# End-to-end: salary file in -> injury-aware projections out, ready for optimize_lineups().

# Match slate rows to nflverse player ids (needed for history AND for injury reports).
# Offensive players: normalised name + position (narrowed by team), then name alone if unique.
# Defenses: by team. `aliases` is an optional data frame (alias, canonical) for odd name spellings.
map_slate_players <- function(S, ps, ro, aliases = NULL) {
  a <- ps[ps$position %in% c(POS_OFF, "FB"), c("player_id", "player_display_name", "position", "team", "season", "week")]
  a <- a[order(a$player_id, a$season, a$week), ]; a <- a[!duplicated(a$player_id, fromLast = TRUE), ]
  c1 <- data.frame(pid = a$player_id, name = a$player_display_name, pos = a$position,
                   team = normalize_team(a$team), recency = wk_index(a$season, a$week), stringsAsFactors = FALSE)
  r <- ro[!is.na(ro$gsis_id) & ro$gsis_id != "" & ro$position %in% c(POS_OFF, "FB"), , drop = FALSE]
  r <- r[order(r$gsis_id, r$season, r$week), ]; r <- r[!duplicated(r$gsis_id, fromLast = TRUE), ]
  c2 <- data.frame(pid = r$gsis_id, name = r$full_name, pos = r$position,
                   team = normalize_team(r$team), recency = wk_index(r$season, r$week), stringsAsFactors = FALSE)
  cand <- rbind(c1, c2)
  cand <- cand[order(-cand$recency), ]; cand <- cand[!duplicated(cand$pid), ]
  cand$pos[cand$pos == "FB"] <- "RB"
  cand$nkey <- .name_key(cand$name)

  nm <- S$name
  if (!is.null(aliases) && nrow(aliases)) {
    hit <- match(tolower(nm), tolower(aliases$alias)); nm[!is.na(hit)] <- aliases$canonical[hit[!is.na(hit)]]
  }
  skey <- .name_key(nm)
  pid <- rep(NA_character_, nrow(S)); type <- rep(NA_character_, nrow(S))
  for (i in seq_len(nrow(S))) {
    if (S$position[i] == "DST") next
    h <- which(cand$nkey == skey[i] & cand$pos == S$position[i])
    how <- "name+position"
    if (length(h) > 1) h <- h[cand$team[h] == S$team[i]]
    if (!length(h)) { h <- which(cand$nkey == skey[i]); how <- "name only" ; if (length(h) > 1) h <- h[cand$team[h] == S$team[i]] }
    if (length(h) >= 1) { pid[i] <- cand$pid[h[1]]; type[i] <- how }
  }
  list(pid = pid, type = type, model_pos = cand$pos[match(pid, cand$pid)])
}

.to_table <- function(x) if (is.character(x)) utils::read.csv(x, stringsAsFactors = FALSE) else x

#' Project a DraftKings or FanDuel slate.
#'
#' @param slate    data frame: id, name, position, team, opponent, salary
#'                 (from load_dk_salaries() / load_fd_players())
#' @param site     "draftkings" or "fanduel" (decides scoring)
#' @param season,week default: the next week with unplayed games
#' @param overrides path or data frame of late news (see read_overrides())
#' @param aliases  path or data frame (alias, canonical) for names that don't match
#' @return the slate plus: proj (injury-adjusted expected points; feed this to the optimizer),
#'   proj_active (points if he plays), p_play, status, practice, injury, flags, ...
project_slate <- function(slate, site = c("draftkings", "fanduel"), season = NULL, week = NULL,
                          overrides = NULL, aliases = NULL, history_seasons = 5, verbose = TRUE) {
  site <- match.arg(site)
  say <- function(...) if (verbose) cat(..., "\n")
  gm <- load_nfl_games()
  if (is.null(season) || is.null(week)) { u <- upcoming_week(gm); season <- u$season; week <- u$week }
  seas <- seq(season - history_seasons + 1, season)
  say(sprintf("Projecting %s, season %d week %d (history: %d-%d)", site, season, week, min(seas), max(seas)))
  ps <- load_nfl_player_stats(seas); ts <- load_nfl_team_stats(seas)
  inj <- load_nfl_injuries(seas); ro <- load_nfl_rosters(season)

  S <- as.data.frame(slate, stringsAsFactors = FALSE); names(S) <- tolower(names(S))
  miss <- setdiff(c("id", "name", "position", "team", "opponent", "salary"), names(S))
  if (length(miss)) stop("Slate is missing column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if ("injury" %in% names(S)) names(S)[names(S) == "injury"] <- "fd_injury"      # FanDuel's own flag
  S$id <- as.character(S$id); S$position <- normalize_position(S$position)
  S$team <- normalize_team(S$team); S$opponent <- normalize_team(S$opponent); S$salary <- as.numeric(S$salary)
  S <- S[S$position %in% c(POS_OFF, "DST"), , drop = FALSE]; rownames(S) <- NULL

  aliases <- .to_table(aliases)
  m <- map_slate_players(S, ps, ro, aliases); S$pid <- m$pid; S$match_type <- m$type
  unmatched <- S$position != "DST" & is.na(S$pid)
  say(sprintf("Matched %d of %d skill players to nflverse ids.", sum(S$position != "DST" & !unmatched), sum(S$position != "DST")))
  if (any(unmatched & S$salary >= 4000)) {
    say(sprintf("  Unmatched players priced $4,000+ (add them to an aliases file if they are real): %s",
                paste(utils::head(S$name[unmatched & S$salary >= 4000], 10), collapse = ", ")))
  }

  ov <- resolve_overrides(read_overrides_any(overrides), S)
  # Teammate effects (usage freed up when a player is out) must react to the same information
  # that drives p_play: your overrides, plus FanDuel's Out/Doubtful flags where the NFL report
  # has not caught up. (FanDuel flags only feed the model's inputs; they never touch overrides.)
  ov_feat <- if (!is.null(ov)) ov[, c("row", "status")] else NULL
  if ("fd_injury" %in% names(S)) {
    fs <- rep(NA_character_, nrow(S)); h <- !is.na(S$fd_injury) & S$fd_injury != ""
    fs[h] <- normalize_status(S$fd_injury[h])
    cur <- inj[inj$season == season & inj$week == week & !is.na(inj$gsis_id), , drop = FALSE]
    offs <- .severity(normalize_status(cur$report_status[match(S$pid, cur$gsis_id)]))
    rows <- which(fs %in% c("Doubtful", "Out") & !is.na(S$pid) & (is.na(offs) | .severity(fs) > offs))
    rows <- setdiff(rows, if (!is.null(ov)) ov$row else integer(0))
    if (length(rows)) ov_feat <- rbind(ov_feat, data.frame(row = rows, status = fs[rows], stringsAsFactors = FALSE))
  }
  inj_eff <- apply_overrides_to_inj(inj, ov_feat, S, season, week)

  # rows to predict: matched skill players that haven't played yet this week
  T <- wk_index(season, week)
  done_pids <- unique(ps$player_id[ps$season == season & ps$week == week])
  sk <- which(S$position != "DST" & !is.na(S$pid))
  fut <- sk[!(S$pid[sk] %in% done_pids)]
  future <- data.frame(pid = S$pid[fut], name = S$name[fut], pos = m$model_pos[fut], team = S$team[fut],
                       season = season, week = week, stringsAsFactors = FALSE)
  bp <- build_panel(ps, ts, gm, inj_eff, site, future = future)
  te  <- predict_offense_week(bp$panel, T)
  dte <- predict_dst_week(bp$team_panel, T)

  S$proj_active <- NA_real_; S$n_prior <- NA_real_; S$itt <- NA_real_; S$proj_source <- NA_character_
  S$game_opp <- NA_character_
  k <- match(S$pid, te$pid)
  isk <- S$position != "DST" & !is.na(k)
  S$proj_active[isk] <- te$pred[k[isk]]; S$n_prior[isk] <- te$n_prior[k[isk]]; S$itt[isk] <- te$itt[k[isk]]
  S$game_opp[isk] <- te$opp[k[isk]]
  S$proj_source[isk] <- ifelse(te$is_future[k[isk]], "model", "actual (game played)")
  S$proj_active[isk & !te$is_future[k]] <- te$y[k[isk & !te$is_future[k]]]
  kd <- match(S$team, dte$team); isd <- S$position == "DST" & !is.na(kd)
  S$proj_active[isd] <- ifelse(is.na(dte$y_dst[kd[isd]]), dte$pred[kd[isd]], dte$y_dst[kd[isd]])
  S$proj_source[isd] <- ifelse(is.na(dte$y_dst[kd[isd]]), "model", "actual (game played)")
  S$itt[isd] <- dte$itt[kd[isd]]; S$game_opp[isd] <- dte$opp[kd[isd]]

  # teams with no game this week
  teams_playing <- unique(bp$team_panel$team[bp$team_panel$widx == T])
  nogame <- !(S$team %in% teams_playing)
  S$proj_active[nogame] <- 0; S$proj_source[nogame] <- "no game this week"

  # fallback for matched-less rookies etc.: salary-based within position
  need <- which(is.na(S$proj_active) & !nogame)
  for (p in unique(S$position[need])) {
    ref <- S[!is.na(S$proj_active) & S$position == p & S$proj_source == "model" & (is.na(S$n_prior) | S$n_prior >= 3), ]
    if (nrow(ref) >= 10) {
      f <- stats::lm(proj_active ~ salary, data = ref); i <- need[S$position[need] == p]
      S$proj_active[i] <- pmax(stats::predict(f, S[i, , drop = FALSE]), 0); S$proj_source[i] <- "salary-based estimate"
    }
  }

  # injuries
  rates <- estimate_play_rates(inj, ps)
  st <- injury_status(S, season, week, inj, ov, rates)
  dst <- S$position == "DST"
  st$status[dst] <- "Active"; st$p_play[dst] <- 1; st$status_flag[dst] <- NA; st$status_source[dst] <- NA
  played <- S$proj_source %in% c("actual (game played)", "no game this week")
  st$p_play[played] <- ifelse(S$proj_source[played] == "no game this week", 0, 1)
  S <- cbind(S, st)
  S$proj <- round(S$proj_active * S$p_play, 2)
  S$proj_active <- round(S$proj_active, 2)

  # flags a human should look at
  fl <- vapply(seq_len(nrow(S)), function(i) {
    f <- character(0)
    if (!is.na(S$status_flag[i])) f <- c(f, S$status_flag[i])
    if (S$status[i] %in% c("Questionable", "Doubtful")) f <- c(f, sprintf("%s, plays ~%d%%", S$status[i], round(100 * S$p_play[i])))
    if (identical(S$proj_source[i], "salary-based estimate")) f <- c(f, "no stats history: salary-based estimate")
    if (!is.na(S$n_prior[i]) && S$n_prior[i] == 0 && identical(S$proj_source[i], "model")) f <- c(f, "no NFL stats history")
    if (!is.na(S$n_prior[i]) && S$n_prior[i] > 0 && S$n_prior[i] < 3 && identical(S$proj_source[i], "model")) f <- c(f, "very little history")
    if (!is.na(S$game_opp[i]) && !is.na(S$opponent[i]) && S$game_opp[i] != S$opponent[i]) f <- c(f, "opponent differs from schedule")
    if (identical(S$proj_source[i], "no game this week")) f <- c(f, "team has no game this week")
    if (identical(S$proj_source[i], "actual (game played)")) f <- character(0)     # already played: nothing to check
    paste(f, collapse = "; ")
  }, character(1))
  S$flags <- fl
  S$game_opp <- NULL

  attr(S, "season") <- season; attr(S, "week") <- week; attr(S, "site") <- site
  attr(S, "injury_age_min") <- injury_data_age(season)
  attr(S, "injury_coverage") <- injury_coverage(inj, season, week)
  w <- injury_coverage_note(attr(S, "injury_coverage"))
  if (!is.null(w) && verbose) cat("\n", w, "\n\n", sep = "")
  class(S) <- c("dfs_projections", "data.frame")
  S
}

read_overrides_any <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.character(x)) return(read_overrides(x))
  x <- as.data.frame(x, stringsAsFactors = FALSE); names(x) <- tolower(names(x))
  for (cn in c("p_play", "note", "team", "name", "id")) if (!cn %in% names(x)) x[[cn]] <- NA
  x$status <- normalize_status(x$status)
  x
}

print.dfs_projections <- function(x, n = 4, ...) {
  if (is.null(attr(x, "site"))) return(print.data.frame(x))            # a subset lost its attributes
  d <- as.data.frame(x)
  cat(sprintf("%s projections, %d week %d: %d players\n", attr(x, "site"), attr(x, "season"), attr(x, "week"), nrow(d)))
  cat(sprintf("  status: %s\n", paste(sprintf("%s %d", names(table(d$status)), as.integer(table(d$status))), collapse = " | ")))
  cat(sprintf("  flagged for a look: %d\n", sum(d$flags != "")))
  np <- sum(d$proj_source %in% "actual (game played)")
  if (np) cat(sprintf("  %d players' games are already played: their 'proj' is the actual result (use playable_slate() for lineups)\n", np))
  w <- injury_coverage_note(attr(x, "injury_coverage"))
  if (!is.null(w)) cat("  ", w, "\n", sep = "")
  cat("\n")
  for (p in c("QB", "RB", "WR", "TE", "DST")) {
    z <- d[d$position == p, ]; z <- z[order(-z$proj), ][seq_len(min(n, nrow(z))), c("name", "team", "salary", "proj", "p_play", "status")]
    cat(p, ":\n"); print(z, row.names = FALSE)
  }
  invisible(x)
}

#' A stand-in slate built from real nflverse players (salaries are SYNTHETIC, derived from recent
#' production) so you can try the whole workflow before you have a real salary file.
demo_slate <- function(site = c("draftkings", "fanduel"), season = NULL, week = NULL, min_avg = 5) {
  site <- match.arg(site)
  gm <- load_nfl_games()
  if (is.null(season) || is.null(week)) { u <- upcoming_week(gm); season <- u$season; week <- u$week }
  g <- gm[gm$season == season & gm$week == week, ]
  if (!nrow(g)) stop("No games found for ", season, " week ", week, call. = FALSE)
  ps <- load_nfl_player_stats(c(season - 1, season))
  pg <- build_player_games(ps, site)
  wi <- wk_index(season, week)
  rec <- pg[pg$widx < wi & pg$widx >= wi - 5, ]
  ag <- stats::aggregate(y ~ pid + name + pos + team, rec, function(v) c(m = mean(v), n = length(v)))
  ag <- do.call(data.frame, ag); names(ag)[5:6] <- c("m", "n")
  ag <- ag[ag$n >= 2 & ag$m >= min_avg, ]
  gh <- normalize_team(g$home_team); ga <- normalize_team(g$away_team)
  tm <- data.frame(team = c(gh, ga), opp = c(ga, gh), stringsAsFactors = FALSE)
  ag$team <- normalize_team(ag$team); ag$opponent <- tm$opp[match(ag$team, tm$team)]
  ag <- ag[!is.na(ag$opponent), ]
  # Salaries: rank of recent production within the position, mapped onto typical price ranges.
  rng <- if (site == "fanduel") list(QB = c(6500, 9500), RB = c(4500, 9500), WR = c(4500, 9500), TE = c(4000, 7500), DST = c(3000, 5000))
         else                   list(QB = c(5000, 7800), RB = c(4000, 9000), WR = c(3500, 9000), TE = c(2800, 6500), DST = c(2200, 4000))
  sal <- numeric(nrow(ag))
  for (p in POS_OFF) {
    i <- which(ag$pos == p); if (!length(i)) next
    pct <- (rank(ag$m[i], ties.method = "average") - 1) / max(length(i) - 1, 1)
    sal[i] <- round((rng[[p]][1] + (rng[[p]][2] - rng[[p]][1]) * pct^1.3) / 100) * 100
  }
  out <- data.frame(id = as.character(seq_len(nrow(ag))), name = ag$name, position = ag$pos, team = ag$team,
                    opponent = ag$opponent, salary = sal, stringsAsFactors = FALSE)
  dst <- data.frame(id = paste0("D", seq_len(nrow(tm))), name = paste(tm$team, "DST"), position = "DST",
                    team = tm$team, opponent = tm$opp, salary = round(mean(rng$DST) / 100) * 100, stringsAsFactors = FALSE)
  structure(rbind(out, dst), season = season, week = week)
}

#' The rows you can actually build lineups from: not Out, has a game, and the game has not been
#' played yet (for games already played, `proj` shows what happened, not a forecast).
playable_slate <- function(proj) {
  d <- as.data.frame(proj, stringsAsFactors = FALSE)
  keep <- d$proj > 0 & !(d$proj_source %in% c("actual (game played)", "no game this week"))
  d[keep, , drop = FALSE]
}
