# Cross-checking your projections against outside ones, and later against what happened.
#
# Two different questions:
#   1. Do I agree with other projection sources?  -> compare_projections()
#      Agreement is a sanity check (catches data errors, stale injury info, role changes),
#      NOT proof of accuracy: two sources can be wrong together.
#   2. Who was actually more accurate?            -> evaluate_comparison(), after games are played.
#      Run it weekly and keep the results; that is what tells you how much to trust each
#      source (and whether blending them helps).
#
# Where benchmark projections come from: DraftKings does not publish a projection file, and
# FanDuel Research shows projections as a web table. Copy such a table into a CSV (columns:
# name, position, team, proj) or use any other provider's export. Your salary files' season
# averages (FPPG / AvgPointsPerGame) can be passed in too, but they are backward-looking.

NFL_TEAMS <- data.frame(
  abbr = c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GB","HOU","IND","JAX","KC",
           "LAC","LAR","LV","MIA","MIN","NE","NO","NYG","NYJ","PHI","PIT","SEA","SF","TB","TEN","WAS"),
  nick = c("cardinals","falcons","ravens","bills","panthers","bears","bengals","browns","cowboys","broncos",
           "lions","packers","texans","colts","jaguars","chiefs","chargers","rams","raiders","dolphins",
           "vikings","patriots","saints","giants","jets","eagles","steelers","seahawks","49ers","buccaneers",
           "titans","commanders"), stringsAsFactors = FALSE)

# "Philadelphia Eagles" / "Eagles" / "PHI" / "Eagles D/ST" -> "PHI"
team_from_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  ab <- normalize_team(x)
  out <- ifelse(ab %in% NFL_TEAMS$abbr, ab, NA_character_)
  for (i in which(is.na(out))) {
    hit <- NFL_TEAMS$abbr[vapply(NFL_TEAMS$nick, function(n) grepl(n, x[i], fixed = TRUE), logical(1))]
    if (length(hit) == 1) out[i] <- hit
  }
  out
}

read_benchmark <- function(x, source = NULL) {
  df <- if (is.character(x)) utils::read.csv(x, stringsAsFactors = FALSE, check.names = FALSE) else as.data.frame(x, stringsAsFactors = FALSE)
  names(df) <- tolower(trimws(names(df)))
  ren <- function(from, to) { h <- intersect(from, names(df)); if (length(h) && !to %in% names(df)) names(df)[names(df) == h[1]] <<- to }
  ren(c("player", "playername", "player name", "full_name"), "name")
  ren(c("pos"), "position")
  ren(c("tm", "teamabbrev", "team_abbrev", "teamabbr"), "team")
  ren(c("fpts", "fp", "points", "projection", "projected", "projpts", "proj_pts", "fantasy points", "fdpts", "dkpts"), "proj")
  miss <- setdiff(c("name", "position", "proj"), names(df))
  if (length(miss)) stop("Benchmark file needs column(s): ", paste(miss, collapse = ", "),
                         " (found: ", paste(names(df), collapse = ", "), ")", call. = FALSE)
  df$position <- normalize_position(df$position)
  df$proj <- suppressWarnings(as.numeric(df$proj))
  df$team <- if ("team" %in% names(df)) normalize_team(df$team) else NA_character_
  dst <- df$position == "DST"
  df$team[dst] <- ifelse(is.na(df$team[dst]) | !(df$team[dst] %in% NFL_TEAMS$abbr), team_from_text(df$name[dst]), df$team[dst])
  df$key <- ifelse(dst, paste0("DST|", df$team), paste0(df$position, "|", .name_key(df$name)))
  df <- df[!is.na(df$proj) & !duplicated(df$key), c("name", "position", "team", "proj", "key")]
  attr(df, "source") <- if (is.null(source)) "benchmark" else source
  df
}

.mine_key <- function(m) ifelse(m$position == "DST", paste0("DST|", m$team), paste0(m$position, "|", .name_key(m$name)))

#' @param mine        output of project_slate() (or any table with name, position, team, proj)
#' @param benchmarks  named list of data frames / CSV paths, e.g. list(fanduel = "fd.csv")
#' @param compare_col which of your columns to compare ("proj_active" = points if he plays, the
#'                    usual meaning of outside projections; "proj" = injury-weighted)
compare_projections <- function(mine, benchmarks, compare_col = NULL, abs_thresh = 3, rel_thresh = 0.25, min_proj = 4) {
  m <- as.data.frame(mine, stringsAsFactors = FALSE)
  if (is.null(compare_col)) compare_col <- if ("proj_active" %in% names(m)) "proj_active" else "proj"
  m$key <- .mine_key(m)
  if (is.null(names(benchmarks)) || any(names(benchmarks) == "")) stop("Name each benchmark, e.g. list(fanduel = ...)", call. = FALSE)
  tab <- data.frame(name = m$name, position = m$position, team = m$team, mine = m[[compare_col]], stringsAsFactors = FALSE)
  for (cn in c("salary", "status", "p_play", "flags", "proj_source", "proj")) if (cn %in% names(m)) tab[[cn]] <- m[[cn]]
  if ("proj" %in% names(tab)) names(tab)[names(tab) == "proj"] <- "mine_weighted"   # injury-weighted, kept for scoring
  srcs <- names(benchmarks)
  for (s in srcs) {
    b <- read_benchmark(benchmarks[[s]], s)
    tab[[s]] <- b$proj[match(m$key, b$key)]
  }
  S <- as.matrix(tab[srcs])
  tab$consensus <- ifelse(rowSums(!is.na(S)) > 0, rowMeans(S, na.rm = TRUE), NA_real_)
  tab$diff <- tab$mine - tab$consensus
  tab$pct  <- ifelse(!is.na(tab$consensus) & tab$consensus > 0, tab$diff / tab$consensus, NA_real_)
  big <- !is.na(tab$diff) & abs(tab$diff) >= pmax(abs_thresh, rel_thresh * tab$consensus) & pmax(tab$mine, tab$consensus) >= min_proj
  zero <- !is.na(tab$consensus) & rowSums(S <= 0.5, na.rm = TRUE) > 0 & tab$mine >= 5
  spread <- if (length(srcs) > 1) apply(S, 1, function(r) if (sum(!is.na(r)) > 1) diff(range(r, na.rm = TRUE)) else 0) else rep(0, nrow(tab))
  note <- ifelse(zero, "benchmark has him at ~0: Out/inactive?  check news",
          ifelse(big & tab$diff > 0, "I'm higher", ifelse(big, "I'm lower", "")))
  note <- ifelse(spread >= pmax(abs_thresh, 0.35 * ifelse(is.na(tab$consensus), 0, tab$consensus)) & note == "",
                 "benchmarks disagree with each other", note)
  tab$note <- note

  rel <- !is.na(tab$consensus) & pmax(tab$mine, tab$consensus) >= min_proj
  summ <- do.call(rbind, lapply(srcs, function(s) {
    ok <- rel & !is.na(tab[[s]])
    d <- tab$mine[ok] - tab[[s]][ok]
    data.frame(source = s, matched = sum(!is.na(tab[[s]])), compared = sum(ok),
               bias = round(mean(d), 2), mean_abs_gap = round(mean(abs(d)), 2), rmsd = round(sqrt(mean(d^2)), 2),
               r = round(stats::cor(tab$mine[ok], tab[[s]][ok]), 3),
               rho = round(stats::cor(tab$mine[ok], tab[[s]][ok], method = "spearman"), 3),
               flagged = sum(tab$note[ok] != ""), stringsAsFactors = FALSE)
  }))
  tab <- tab[order(-abs(ifelse(is.na(tab$diff), 0, tab$diff)) * (tab$note != "")), ]
  rownames(tab) <- NULL
  structure(list(table = tab, summary = summ, sources = srcs, compare_col = compare_col,
                 season = attr(mine, "season"), week = attr(mine, "week"), site = attr(mine, "site"),
                 unmatched_mine = sum(rowSums(!is.na(S)) == 0 & tab$mine >= min_proj)),
            class = "dfs_comparison")
}

print.dfs_comparison <- function(x, n = 15, ...) {
  cat(sprintf("Comparing my '%s' with: %s\n\n", x$compare_col, paste(x$sources, collapse = ", ")))
  print(x$summary, row.names = FALSE)
  cat("\n  bias = mine minus source (positive: I'm more optimistic); r/rho = correlation / rank correlation\n")
  fl <- x$table[x$table$note != "", ]
  cat(sprintf("\n%d players flagged. Largest gaps:\n", nrow(fl)))
  cols <- intersect(c("name", "position", "team", "mine", x$sources, "consensus", "diff", "status", "note"), names(fl))
  d <- fl[seq_len(min(n, nrow(fl))), cols, drop = FALSE]
  num <- vapply(d, is.numeric, logical(1)); d[num] <- lapply(d[num], round, 1)
  print(d, row.names = FALSE)
  if (x$unmatched_mine > 0) cat(sprintf("\n%d of my players priced as relevant were not found in any benchmark.\n", x$unmatched_mine))
  invisible(x)
}

# ---- after the games: who was right? -----------------------------------------

#' Actual fantasy points for a completed week, in the same shape as projections.
actual_points <- function(season, week, site) {
  site <- site_key(site)
  seas <- (season - 1):season
  ps <- load_nfl_player_stats(seas); ts <- load_nfl_team_stats(seas); gm <- load_nfl_games()
  pg <- build_player_games(ps, site)
  a <- pg[pg$season == season & pg$week == week, ]
  off <- data.frame(name = a$name, position = a$pos, team = a$team, actual = a$y, stringsAsFactors = FALSE)
  tp <- build_team_panel(gm, ts, pg, season)
  t <- tp[tp$week == week & !is.na(tp$y_dst), ]
  dst <- data.frame(name = t$team, position = "DST", team = t$team, actual = t$y_dst, stringsAsFactors = FALSE)
  out <- rbind(off, dst)
  out$key <- .mine_key(out)
  attr(out, "teams_played") <- t$team       # only these teams' games are finished
  out
}

#' Score every source against what happened.
#'
#' IMPORTANT: `cmp` must be made BEFORE kickoff (see save_snapshot()). Running project_slate()
#' again after the games gives you the actual results back, not a forecast, so those rows are
#' skipped here. Players on the slate who did not play count as 0 (what a roster spot earns),
#' so sources that knew about injuries are rewarded.
evaluate_comparison <- function(cmp, season = cmp$season, week = cmp$week, site = cmp$site, min_proj = 4) {
  if (is.null(season) || is.null(week) || is.null(site)) stop("Give season, week and site.", call. = FALSE)
  act <- actual_points(season, week, site)
  t <- cmp$table
  t$key <- .mine_key(t)
  t$actual <- act$actual[match(t$key, act$key)]
  t$actual[is.na(t$actual) & t$position != "DST"] <- 0
  finished <- t$team %in% attr(act, "teams_played")
  if (!any(finished)) stop(sprintf("No games from %d week %d are in the data yet; try again after they are played.", season, week), call. = FALSE)
  circular <- if ("proj_source" %in% names(t)) t$proj_source %in% c("actual (game played)", "no game this week") else rep(FALSE, nrow(t))
  if (any(circular)) message(sum(circular), " rows were made after their game was played and are skipped; ",
                             "save the projections before kickoff to evaluate them fairly.")
  # 'mine (if plays)' is directly comparable with outside projections; 'mine (injury-weighted)'
  # adds the chance-of-playing scaling, so the gap between the two rows is what the injury layer is worth.
  if ("mine_weighted" %in% names(t)) t[["mine (injury-weighted)"]] <- t$mine_weighted
  names(t)[names(t) == "mine"] <- "mine (if plays)"
  cols <- intersect(c("mine (injury-weighted)", "mine (if plays)", cmp$sources, "consensus"), names(t))
  keep <- !is.na(t$actual) & finished & !circular & pmax(t[["mine (if plays)"]], t$consensus, na.rm = TRUE) >= min_proj
  for (s in cmp$sources) keep <- keep & !is.na(t[[s]])          # same players for every source
  if (!any(keep)) stop("Nothing to score (no players with all sources present).", call. = FALSE)
  res <- do.call(rbind, lapply(cols, function(cn) {
    e <- t[[cn]][keep] - t$actual[keep]
    data.frame(source = cn, n = sum(keep), mae = round(mean(abs(e)), 3), rmse = round(sqrt(mean(e^2)), 3),
               bias = round(mean(e), 3), r = round(stats::cor(t[[cn]][keep], t$actual[keep]), 3), stringsAsFactors = FALSE)
  }))
  res$season <- season; res$week <- week
  res[order(res$rmse), ]
}

#' Save this week's projections (and the comparison, if any) before the games, so they can be
#' scored afterwards. Writes snapshots/<site>_<season>_wk<week>.rds
save_snapshot <- function(cmp, dir = "snapshots") {
  if (!inherits(cmp, "dfs_comparison")) stop("Pass the result of compare_projections().", call. = FALSE)
  if (is.null(cmp$season)) stop("The comparison has no season/week; build it from project_slate() output.", call. = FALSE)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(dir, sprintf("%s_%d_wk%02d.rds", cmp$site, cmp$season, cmp$week))
  saveRDS(cmp, f)
  invisible(f)
}

#' Score every saved snapshot whose games have been played, week by week and overall.
#' This is the honest answer to "are my projections accurate, and are they better than FanDuel's?"
evaluate_snapshots <- function(dir = "snapshots", site = NULL, ...) {
  fs <- list.files(dir, pattern = "\\.rds$", full.names = TRUE)
  if (!is.null(site)) fs <- fs[grepl(paste0("^", site), basename(fs))]
  if (!length(fs)) stop("No snapshots in '", dir, "'. Use save_snapshot() before kickoff.", call. = FALSE)
  per <- lapply(fs, function(f) tryCatch(evaluate_comparison(readRDS(f), ...), error = function(e) {
    message("skipped ", basename(f), ": ", conditionMessage(e)); NULL }))
  per <- do.call(rbind, per[!vapply(per, is.null, logical(1))])
  if (is.null(per)) stop("No snapshot could be scored yet (games not in the data?).", call. = FALSE)
  w <- per$n
  overall <- do.call(rbind, lapply(split(per, per$source), function(z)
    data.frame(source = z$source[1], weeks = nrow(z), players = sum(z$n),
               mae = round(stats::weighted.mean(z$mae, z$n), 3),
               rmse = round(sqrt(stats::weighted.mean(z$rmse^2, z$n)), 3),
               bias = round(stats::weighted.mean(z$bias, z$n), 3), stringsAsFactors = FALSE)))
  list(by_week = per, overall = overall[order(overall$rmse), ])
}

#' Simple weighted blend of your projection and the benchmark consensus.
blend_projections <- function(cmp, w_mine = 0.5) {
  t <- cmp$table
  ifelse(is.na(t$consensus), t$mine, w_mine * t$mine + (1 - w_mine) * t$consensus)
}
