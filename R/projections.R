# Projection models and walk-forward backtest.
#
# For a target week T, each position gets its own linear regression fit ONLY on
# games from the preceding ~3 seasons (widx < T). Features (see features.R) are
# recency-weighted averages of the player's own points and usage, the team's
# implied total and spread, opponent strength vs the position, and how much
# usage is missing because teammates are ruled out. The prediction is expected
# points *if the player plays*; the injury layer turns that into an expected
# value (see injuries.R / project_slate()).

# Chosen on 2023-24 only; confirmed on untouched 2025-26 (see README "Accuracy").
OFF_FEATURES <- c("m_pts", "m_tgt", "m_car", "m_att", "itt", "spr", "home", "opp_def", "n_c",
                  "miss_tgt", "miss_car", "miss_att", "int_itt", "int_def")
DST_FEATURES <- c("m_dst", "m_dsk", "opp_sksuf", "opp_score_m", "opp_itt", "spr", "is_home")
PRED_MAX <- c(QB = 45, RB = 45, WR = 45, TE = 40, DST = 30)

.fit_lm <- function(train, feats, min_rows) {
  if (nrow(train) < min_rows) return(NULL)
  gone <- setdiff(feats, names(train))
  if (length(gone)) stop("Model feature(s) not in the panel: ", paste(gone, collapse = ", "), call. = FALSE)
  ok <- feats[vapply(feats, function(f) stats::sd(train[[f]], na.rm = TRUE) > 1e-9, logical(1))]
  suppressWarnings(stats::lm(stats::reformulate(ok, "y"), data = train))
}

#' Predict every row of the panel whose widx == T. Rows with an outcome are scored
#' against it in backtests; rows without (is_future) are live projections.
predict_offense_week <- function(P, T, window = 60, min_rows = 150, features = OFF_FEATURES) {
  te <- P[P$widx == T, , drop = FALSE]
  if (!nrow(te)) return(te)
  te$pred <- NA_real_
  for (p in POS_OFF) {
    i <- which(te$pos == p)
    if (!length(i)) next
    tr <- P[!P$is_future & P$widx < T & P$widx >= T - window & P$pos == p & !is.na(P$y), , drop = FALSE]
    fit <- .fit_lm(tr, features, min_rows)
    te$pred[i] <- if (is.null(fit)) te$m_pts[i] else suppressWarnings(stats::predict(fit, te[i, , drop = FALSE]))
  }
  te$pred <- pmin(pmax(te$pred, 0), unname(PRED_MAX[te$pos]))
  te
}

predict_dst_week <- function(tp, T, window = 60, min_rows = 150) {
  te <- tp[tp$widx == T, , drop = FALSE]
  if (!nrow(te)) return(te)
  tr <- tp[tp$widx < T & tp$widx >= T - window & !is.na(tp$y_dst), , drop = FALSE]
  tr$y <- tr$y_dst
  fit <- .fit_lm(tr, DST_FEATURES, min_rows)
  te$pred <- if (is.null(fit)) te$m_dst else suppressWarnings(stats::predict(fit, te))
  te$pred <- pmin(pmax(te$pred, -2), PRED_MAX[["DST"]])
  te
}

# ---- walk-forward backtest ---------------------------------------------------

#' Re-create what the model would have projected before each past week and compare
#' with what happened. Priors come only from seasons before `eval_seasons`.
backtest_projections <- function(ps, ts, gm, inj, site, eval_seasons, eval_weeks = 1:18, verbose = TRUE,
                                 features = OFF_FEATURES, ...) {
  bp <- build_panel(ps, ts, gm, inj, site, prior_upto = wk_index(min(eval_seasons), 1) - 1, ...)
  P <- bp$panel; tp <- bp$team_panel
  targets <- sort(unique(P$widx[P$season %in% eval_seasons & P$week %in% eval_weeks & !P$is_future]))
  off <- vector("list", length(targets)); dst <- vector("list", length(targets))
  for (i in seq_along(targets)) {
    off[[i]] <- predict_offense_week(P, targets[i], features = features)
    dst[[i]] <- predict_dst_week(tp, targets[i])
    dst[[i]] <- dst[[i]][!is.na(dst[[i]]$y_dst), ]
    if (verbose && i %% 10 == 0) cat(sprintf("  backtest %d/%d weeks\n", i, length(targets)))
  }
  list(offense = do.call(rbind, off), dst = do.call(rbind, dst), site = site)
}

.metrics <- function(y, pred, base) {
  c(n = length(y), mae = mean(abs(y - pred)), mae_base = mean(abs(y - base)),
    rmse = sqrt(mean((y - pred)^2)), rmse_base = sqrt(mean((y - base)^2)),
    bias = mean(pred - y), r = stats::cor(y, pred), r_base = stats::cor(y, base))
}

# The fixed evaluation population: players who scored >= min_avg on a recent unshrunk average.
.relevant <- function(bt, min_avg = 5) {
  o <- bt$offense
  o[!is.na(o$y) & o$n_prior >= 1 & !is.na(o$ref_avg) & o$ref_avg >= min_avg, ]
}

#' Accuracy summary by position. `min_avg` keeps DFS-relevant players only (recent
#' average points >= min_avg); `base` is a plain recency-weighted average with fixed settings.
summarize_backtest <- function(bt, min_avg = 5) {
  o <- .relevant(bt, min_avg)
  rows <- lapply(POS_OFF, function(p) { z <- o[o$pos == p, ]; c(pos = p, round(.metrics(z$y, z$pred, z$base_pts), 3)) })
  all <- c(pos = "ALL", round(.metrics(o$y, o$pred, o$base_pts), 3))
  d <- bt$dst
  dr <- c(pos = "DST", round(.metrics(d$y_dst, d$pred, d$m_dst), 3))
  out <- as.data.frame(do.call(rbind, c(rows, list(all, dr))), stringsAsFactors = FALSE)
  out[-1] <- lapply(out[-1], as.numeric)
  out
}

# Mean predicted vs mean actual by prediction bucket (is the model calibrated?).
calibration_table <- function(bt, min_avg = 5, buckets = 8) {
  o <- .relevant(bt, min_avg)
  b <- cut(o$pred, stats::quantile(o$pred, seq(0, 1, length.out = buckets + 1)), include.lowest = TRUE)
  data.frame(bucket = levels(b), n = as.vector(table(b)),
             mean_pred = round(tapply(o$pred, b, mean), 2), mean_actual = round(tapply(o$y, b, mean), 2),
             row.names = NULL)
}

# Average within-week rank correlation (what matters for picking players in a slate).
weekly_rank_corr <- function(bt, min_avg = 5) {
  o <- .relevant(bt, min_avg)
  key <- paste(o$season, o$week, o$pos)
  r <- vapply(split(seq_len(nrow(o)), key), function(i) {
    if (length(i) < 8) return(c(NA, NA))
    c(stats::cor(o$pred[i], o$y[i], method = "spearman"), stats::cor(o$base_pts[i], o$y[i], method = "spearman"))
  }, numeric(2))
  pos <- sub("^[0-9]+ [0-9]+ ", "", colnames(r))
  data.frame(pos = unique(pos),
             rho_model = round(vapply(unique(pos), function(p) mean(r[1, pos == p], na.rm = TRUE), 0), 3),
             rho_base  = round(vapply(unique(pos), function(p) mean(r[2, pos == p], na.rm = TRUE), 0), 3),
             row.names = NULL)
}
