# Feature building for the projection model.
#
# EVERY feature for a game is computed from games strictly before it, so a single
# pass over history is leak-free and the same code serves backtests (past weeks)
# and live projections (upcoming week = rows with no outcome yet).

POS_OFF <- c("QB", "RB", "WR", "TE")

wk_index <- function(season, week) (season - 2000) * 20 + week   # monotone across seasons

# ---- decayed averages ------------------------------------------------------

# For rows sorted by (grp, time): decayed sum/weight of x over strictly earlier rows
# of the same group. NA outcomes are skipped (future games). The state decays extra
# at each season boundary (offseason uncertainty). Also returns the post-game state.
ewma_group <- function(x, grp, season, d = 0.88, d_season = 0.75) {
  n <- length(x)
  num <- den <- cnt <- pnum <- pden <- numeric(n)
  nc <- dc <- cc <- 0
  for (i in seq_len(n)) {
    if (i == 1L || grp[i] != grp[i - 1L]) { nc <- dc <- cc <- 0 }
    else if (season[i] != season[i - 1L]) { nc <- nc * d_season; dc <- dc * d_season }
    num[i] <- nc; den[i] <- dc; cnt[i] <- cc
    if (!is.na(x[i])) { nc <- d * nc + x[i]; dc <- d * dc + 1; cc <- cc + 1 }
    pnum[i] <- nc; pden[i] <- dc
  }
  list(num = num, den = den, n = cnt, post_num = pnum, post_den = pden)
}

shrink <- function(num, den, prior, k) (num + k * prior) / (den + k)

# ---- player-game history ---------------------------------------------------

build_player_games <- function(ps, site) {
  d <- ps[ps$position %in% c(POS_OFF, "FB"), , drop = FALSE]
  d$position[d$position == "FB"] <- "RB"
  data.frame(
    pid = d$player_id, name = d$player_display_name, pos = d$position,
    team = normalize_team(d$team), opp = normalize_team(d$opponent_team),
    season = d$season, week = d$week, widx = wk_index(d$season, d$week),
    y = score_offense(d, site),
    tgt = .num(d, "targets"), car = .num(d, "carries"), att = .num(d, "attempts"),
    stringsAsFactors = FALSE
  )
}

# ---- team-game panel: schedule context + defense-vs-position + DST target ---

build_team_panel <- function(gm, ts, pg, seasons) {
  g <- gm[gm$season %in% seasons, , drop = FALSE]
  g$home_team <- normalize_team(g$home_team); g$away_team <- normalize_team(g$away_team)
  has <- !is.na(g$spread_line) & !is.na(g$total_line)
  sp  <- ifelse(has, g$spread_line, 0)           # positive = home favoured (nflverse convention)
  tot <- ifelse(has, g$total_line, 45)
  mk <- function(home) {
    s <- if (home) sp else -sp
    data.frame(team = if (home) g$home_team else g$away_team, opp = if (home) g$away_team else g$home_team,
               season = g$season, week = g$week, is_home = as.numeric(home), spr = s,
               itt = (tot + s) / 2, opp_itt = (tot - s) / 2, has_line = has,
               score = if (home) g$home_score else g$away_score,
               opp_score = if (home) g$away_score else g$home_score, stringsAsFactors = FALSE)
  }
  tp <- rbind(mk(TRUE), mk(FALSE))
  tp$widx <- wk_index(tp$season, tp$week)

  # DST outcomes from team stats (own defense) and the opponent's defensive TDs
  tsn <- ts; tsn$team <- normalize_team(tsn$team)
  tsn$blocks <- .num(tsn, "def_fg_blocks") + .num(tsn, "def_punt_blocks") + .num(tsn, "def_pat_blocks")
  cols <- c("def_sacks", "def_interceptions", "fumble_recovery_opp", "def_tds", "special_teams_tds",
            "def_safeties", "blocks", "sacks_suffered")
  key_t <- paste(tsn$team, tsn$season, tsn$week)
  m <- match(paste(tp$team, tp$season, tp$week), key_t)
  for (cn in cols) tp[[cn]] <- ifelse(is.na(m), NA, .num(tsn, cn)[m])
  mo <- match(paste(tp$opp, tp$season, tp$week), key_t)
  opp_def_tds <- ifelse(is.na(mo), NA, .num(tsn, "def_tds")[mo])
  # points allowed by the defense: opponent score minus its own pick-six/fumble-return TDs (7 pts each)
  tp$points_allowed <- pmax(tp$opp_score - 7 * opp_def_tds, 0)
  tp$def_fg_blocks <- tp$blocks
  tp$y_dst <- ifelse(is.na(tp$points_allowed) | is.na(tp$def_sacks), NA, score_dst(tp, "draftkings"))

  # points each position group scored against this defense
  for (p in POS_OFF) {
    a <- tapply(pg$y[pg$pos == p], paste(pg$opp[pg$pos == p], pg$season[pg$pos == p], pg$week[pg$pos == p]), sum)
    tp[[paste0("allowed_", p)]] <- as.numeric(a[paste(tp$team, tp$season, tp$week)])
  }
  # allowed_* are only meaningful when we have the game's stats
  played <- !is.na(tp$y_dst)
  for (p in POS_OFF) tp[[paste0("allowed_", p)]][!played] <- NA

  tp <- tp[order(tp$team, tp$widx), ]
  rownames(tp) <- NULL
  tp
}

# Priors (league means) from rows up to `upto` (inclusive widx), so evaluation
# seasons never see their own future.
compute_priors <- function(pg, tp, upto = Inf) {
  r <- pg[pg$widx <= upto, ]
  t <- tp[tp$widx <= upto & !is.na(tp$y_dst), ]
  list(
    pg = lapply(split(r, r$pos), function(z) c(y = mean(z$y), tgt = mean(z$tgt), car = mean(z$car), att = mean(z$att))),
    allowed = sapply(POS_OFF, function(p) mean(t[[paste0("allowed_", p)]], na.rm = TRUE)),
    dst = mean(t$y_dst), dsk = mean(t$def_sacks), sksuf = mean(t$sacks_suffered), score = mean(t$score, na.rm = TRUE)
  )
}

add_team_features <- function(tp, pri) {
  g <- tp$team
  e <- function(x, d = 0.9, ds = 0.7) ewma_group(x, g, tp$season, d, ds)
  s <- e(tp$y_dst);          tp$m_dst   <- shrink(s$num, s$den, pri$dst, 3)
  s <- e(tp$def_sacks);      tp$m_dsk   <- shrink(s$num, s$den, pri$dsk, 3)
  s <- e(tp$sacks_suffered); tp$m_sksuf <- shrink(s$num, s$den, pri$sksuf, 3)
  s <- e(tp$score);          tp$m_score <- shrink(s$num, s$den, pri$score, 3)
  for (p in POS_OFF) {
    s <- e(tp[[paste0("allowed_", p)]])
    tp[[paste0("def_", p)]] <- shrink(s$num, s$den, pri$allowed[[p]], 3) - pri$allowed[[p]]
  }
  # opponent's offense traits, joined by (opp, widx)
  mo <- match(paste(tp$opp, tp$widx), paste(tp$team, tp$widx))
  tp$opp_sksuf <- tp$m_sksuf[mo]; tp$opp_score_m <- tp$m_score[mo]
  tp
}

# ---- teammates ruled out ---------------------------------------------------

# For each (team, week): usage that is missing because teammates are Out/Doubtful.
# Uses each absent player's usage as of his last game (must be recent).
build_miss <- function(panel_hist, inj, out_w = c(Out = 1, Doubtful = 0.8), max_gap = 6) {
  inj <- inj[inj$report_status %in% names(out_w) & inj$position %in% c(POS_OFF, "FB") & !is.na(inj$gsis_id), , drop = FALSE]
  empty <- data.frame(team = character(), widx = numeric(), miss_tgt = numeric(), miss_car = numeric(), miss_att = numeric())
  if (!nrow(inj)) return(list(team = empty, player = cbind(gsis_id = character(), empty)))
  inj$widx <- wk_index(inj$season, inj$week); inj$team <- normalize_team(inj$team)
  ids <- unique(c(panel_hist$pid, inj$gsis_id))
  ph <- panel_hist[order(match(panel_hist$pid, ids), panel_hist$widx), ]
  keys <- match(ph$pid, ids) * 1e4 + ph$widx
  q <- match(inj$gsis_id, ids) * 1e4 + inj$widx - 0.5
  loc <- findInterval(q, keys)
  ok <- loc >= 1
  loc[!ok] <- 1
  ok <- ok & (ph$pid[loc] == inj$gsis_id) & (inj$widx - ph$widx[loc] <= max_gap)
  w <- unname(out_w[inj$report_status]) * ok
  inj$miss_tgt <- w * ph$post_tgt[loc]; inj$miss_car <- w * ph$post_car[loc]; inj$miss_att <- w * ph$post_att[loc]
  inj <- inj[!duplicated(inj[, c("gsis_id", "widx")]), ]
  list(team = stats::aggregate(cbind(miss_tgt, miss_car, miss_att) ~ team + widx, data = inj, FUN = sum),
       player = inj[, c("gsis_id", "widx", "team", "miss_tgt", "miss_car", "miss_att")])
}

# ---- the panel: history + rows to predict -----------------------------------

#' @param future data.frame(pid, name, pos, team, season, week) for rows to predict
#'               (no outcome yet); opponent comes from the schedule.
build_panel <- function(ps, ts, gm, inj, site, future = NULL, prior_upto = Inf, k = 2,
                        decay = 0.80, season_decay = 0.75) {
  pg <- build_player_games(ps, site)
  seasons <- sort(unique(c(pg$season, if (!is.null(future)) future$season)))
  tp  <- build_team_panel(gm, ts, pg, seasons)
  pri <- compute_priors(pg, tp, prior_upto)
  tp  <- add_team_features(tp, pri)

  P <- pg
  P$is_future <- FALSE
  if (!is.null(future) && nrow(future)) {
    f <- data.frame(pid = future$pid, name = future$name, pos = future$pos, team = normalize_team(future$team),
                    season = future$season, week = future$week, widx = wk_index(future$season, future$week))
    f$opp <- tp$opp[match(paste(f$team, f$widx), paste(tp$team, tp$widx))]
    f <- f[!is.na(f$opp), ]
    for (cn in c("y", "tgt", "car", "att")) f[[cn]] <- NA_real_
    f$is_future <- TRUE
    P <- rbind(P, f[, names(P)])
  }
  P <- P[order(P$pid, P$widx), ]
  rownames(P) <- NULL

  pr <- function(stat, pos) vapply(pos, function(p) { v <- pri$pg[[p]][[stat]]; if (is.null(v)) NA_real_ else v }, numeric(1))
  for (st in c("y", "tgt", "car", "att")) {
    s <- ewma_group(P[[st]], P$pid, P$season, decay, season_decay)
    pri_v <- pr(st, P$pos)
    P[[paste0("m_", st)]] <- shrink(s$num, s$den, pri_v, k)
    P[[paste0("post_", st)]] <- shrink(s$post_num, s$post_den, pri_v, k)
    if (st == "y") { P$n_prior <- s$n; P$w_prior <- s$den }
  }
  names(P)[names(P) == "m_y"] <- "m_pts"
  P$n_c <- pmin(P$n_prior, 8)
  # Fixed reference columns, deliberately independent of the tuned settings above, so
  # backtests always judge the same player population against the same baseline.
  s0 <- ewma_group(P$y, P$pid, P$season, 0.88, 0.75)
  P$base_pts <- shrink(s0$num, s0$den, pr("y", P$pos), 2)          # "plain rolling average"
  P$ref_avg  <- ifelse(s0$den > 0, s0$num / s0$den, NA_real_)       # unshrunk recent average

  # game context
  mt <- match(paste(P$team, P$widx), paste(tp$team, tp$widx))
  P$itt <- tp$itt[mt]; P$spr <- tp$spr[mt]; P$home <- tp$is_home[mt]; P$has_line <- tp$has_line[mt]
  P$itt[is.na(P$itt)] <- 22.5; P$spr[is.na(P$spr)] <- 0; P$home[is.na(P$home)] <- 0
  mo <- match(paste(P$opp, P$widx), paste(tp$team, tp$widx))
  P$opp_def <- vapply(seq_len(nrow(P)), function(i) tp[[paste0("def_", P$pos[i])]][mo[i]], numeric(1))
  P$opp_def[is.na(P$opp_def)] <- 0

  # interactions: a good player's output moves more with game environment
  P$int_itt <- P$m_pts * (P$itt - 22.5) / 10
  P$int_def <- P$m_pts * P$opp_def / 10

  # teammates out
  ms <- build_miss(P[!P$is_future, ], inj)
  mm   <- match(paste(P$team, P$widx), paste(ms$team$team, ms$team$widx))
  self <- match(paste(P$pid, P$widx), paste(ms$player$gsis_id, ms$player$widx))   # don't count a player as his own absent teammate
  for (cn in c("miss_tgt", "miss_car", "miss_att")) {
    P[[cn]] <- pmax(ifelse(is.na(mm), 0, ms$team[[cn]][mm]) - ifelse(is.na(self), 0, ms$player[[cn]][self]), 0)
  }

  list(panel = P, team_panel = tp, priors = pri)
}
