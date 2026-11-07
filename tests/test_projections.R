# Tests for the projection, injury and comparison layers.
# Sourced by run_tests.R, or run alone:  Rscript tests/test_projections.R
# Tests that need nflverse data use the local cache / download it, and are skipped (with a
# message) when it cannot be loaded, so the rest still run offline.

if (!exists("check")) {
  args <- commandArgs(FALSE); this <- sub("^--file=", "", args[grep("^--file=", args)])
  root <- normalizePath(file.path(dirname(this), ".."))
  invisible(lapply(list.files(file.path(root, "R"), "\\.R$", full.names = TRUE), source))
  n_pass <- 0L; n_fail <- 0L
  check <- function(desc, cond) {
    ok <- isTRUE(cond); if (ok) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
    cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
  }
  section <- function(s) cat(sprintf("\n--- %s ---\n", s))
  standalone <- TRUE
}
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))

# ---------------------------------------------------------------------------
section("Decayed averages (hand-computed)")
e <- ewma_group(c(10, 20, 30), c("a", "a", "a"), c(1, 1, 1), d = 0.5, d_season = 0.5)
check("first game has no history", e$num[1] == 0 && e$den[1] == 0 && e$n[1] == 0)
check("2nd game sees only game 1 (10 / 1)", near(e$num[2], 10) && near(e$den[2], 1))
check("3rd game: num = 0.5*10+20 = 25, weight = 1.5", near(e$num[3], 25) && near(e$den[3], 1.5) && e$n[3] == 2)
check("post-game state includes the game itself (42.5 / 1.75)", near(e$post_num[3], 42.5) && near(e$post_den[3], 1.75))
e <- ewma_group(c(10, 20), c("a", "a"), c(1, 2), d = 1, d_season = 0.5)
check("season boundary decays the state (5 / 0.5)", near(e$num[2], 5) && near(e$den[2], 0.5))
e <- ewma_group(c(10, NA, 30), c("a", "a", "a"), c(1, 1, 1), d = 0.5, d_season = 0.5)
check("a game with no outcome yet is skipped, not counted as zero", near(e$num[3], 10) && near(e$den[3], 1) && e$n[3] == 1)
e <- ewma_group(c(10, 99), c("a", "b"), c(1, 1), d = 0.5, d_season = 0.5)
check("state does not leak across players", e$num[2] == 0 && e$n[2] == 0)

# ---------------------------------------------------------------------------
section("Teammates out (hand-computed)")
ph <- data.frame(pid = c("A", "A", "B"), widx = c(100, 101, 100), post_tgt = c(8, 9, 3), post_car = c(0, 0, 10), post_att = 0)
inj <- data.frame(gsis_id = c("A", "B"), season = 2005, week = c(2, 20), team = "XYZ", position = c("WR", "RB"),
                  report_status = c("Out", "Doubtful"), stringsAsFactors = FALSE)
m <- build_miss(ph, inj)                                      # A out at widx 102: last game was 101 -> 9 targets
check("Out player's last-game usage is credited to his team that week", near(m$team$miss_tgt[m$team$widx == 102], 9))
check("far-away absence (more than 6 weeks since his last game) counts for nothing", near(m$team$miss_car[m$team$widx == 120], 0))
inj2 <- data.frame(gsis_id = "B", season = 2005, week = 2, team = "XYZ", position = "RB", report_status = "Doubtful", stringsAsFactors = FALSE)
m2 <- build_miss(ph, inj2)
check("Doubtful counts at 80% (10 carries -> 8)", near(m2$team$miss_car, 8))
inj3 <- data.frame(gsis_id = "B", season = 2005, week = 2, team = "XYZ", position = "RB", report_status = "Questionable", stringsAsFactors = FALSE)
check("Questionable players are not treated as missing", nrow(build_miss(ph, inj3)$team) == 0)

# ---------------------------------------------------------------------------
section("Injury statuses and chance of playing")
check("status aliases", identical(normalize_status(c("Q", "o", "IR", "doubtful", "", NA, "Probable", "GTD")),
      c("Questionable", "Out", "Out", "Doubtful", "Active", "Active", "Active", "Questionable")))
check("unknown status text becomes NA (so overrides can reject it)", is.na(normalize_status("banana")))
rates <- list(
  cell = data.frame(status = c("Questionable", "Questionable", "None"), practice = c("Limited", "DNP", "Full"),
                    n = c(100, 10, 500), played = c(70, 2, 450), rate = c(70.5 / 101, 2.5 / 11, 450.5 / 501)),
  status = data.frame(status = c("Questionable", "None", "Doubtful", "Out"), n = c(200, 800, 50, 300),
                      played = c(120, 700, 1, 0), rate = c(0.6, 0.87, 0.03, 0)))
p <- lookup_play_prob(c("Questionable", "Questionable", "Active", "Out", "Doubtful"),
                      c("Limited", "DNP", "Full", "DNP", "Full"), rates)
check("uses the status+practice cell when it has >= 25 cases", near(p[1], 70.5 / 101))
check("falls back to the status-level rate for thin cells", near(p[2], 0.6))
check("Active uses the 'no designation' rate for his practice level", near(p[3], 450.5 / 501))
check("Out is always zero; Doubtful uses its rate", p[4] == 0 && near(p[5], 0.03))

slate <- data.frame(pid = c("P1", "P2", "P3", "P4", "P5"), name = paste("Player", 1:5), position = "WR",
                    team = "KC", id = as.character(1:5), stringsAsFactors = FALSE)
inj <- data.frame(season = 2026, week = 9, game_type = "REG", team = "KC", gsis_id = c("P1", "P2", "P3"),
                  position = "WR", report_status = c("Questionable", "Out", ""),
                  practice_status = c("Limited Participation in Practice", "Did Not Participate In Practice", "Full Participation in Practice"),
                  report_primary_injury = c("Knee", "Hamstring", NA), stringsAsFactors = FALSE)
st <- injury_status(slate, 2026, 9, inj, NULL, rates)
check("Questionable + Limited practice looks up its historical rate", near(st$p_play[1], 70.5 / 101))
check("Out -> 0 and the injury is carried along", st$status[2] == "Out" && st$p_play[2] == 0 && st$injury[2] == "Hamstring")
check("on the practice report with no designation -> 'Active' with that practice class", st$status[3] == "Active" && st$practice[3] == "Full")
check("players not on any report are treated as healthy (0.99)", near(st$p_play[4], 0.99) && st$status_source[4] == "no report")

slate$fd_injury <- c("O", "", "Q", "D", "")                       # FanDuel's indicator
st <- injury_status(slate, 2026, 9, inj, NULL, rates)
check("worst status wins: NFL Questionable + FanDuel Out -> Out", st$status[1] == "Out" && st$p_play[1] == 0)
check("a disagreement between sources is flagged", grepl("disagree", st$status_flag[1]))
check("FanDuel-only flag counts when the NFL report has nothing on the player", st$status[4] == "Doubtful" && st$status_source[4] == "FanDuel")
check("FanDuel's blank indicator is not an opinion (NFL Out stays Out)", st$status[2] == "Out")

ov <- data.frame(name = c("Player 2", "Player 4", "Player 5"), status = c("Active", "Out", "Questionable"),
                 p_play = c(NA, NA, 0.4), note = c("cleared", NA, "coach says GTD"), team = NA, id = NA, stringsAsFactors = FALSE)
ov <- resolve_overrides(read_overrides_any(ov), slate)
st <- injury_status(slate, 2026, 9, inj, ov, rates)
check("override beats the NFL report (Out -> Active, 0.98)", st$status[2] == "Active" && near(st$p_play[2], 0.98) && grepl("NFL report said Out", st$status_flag[2]))
check("override beats FanDuel", st$status[4] == "Out" && st$p_play[4] == 0)
check("override p_play is respected", near(st$p_play[5], 0.4) && st$status_note[5] == "coach says GTD")
check("overrides are recorded as the source", all(st$status_source[c(2, 4, 5)] == "your override"))
inj_eff <- apply_overrides_to_inj(inj, ov[ov$name == "Player 4", ], slate, 2026, 9)
check("overrides feed the injury table the model uses (teammates react)",
      any(inj_eff$gsis_id == "P4" & inj_eff$report_status == "Out"))

tf <- tempfile(fileext = ".csv")
writeLines(c("name,status,p_play,note", "Player 1,out,,x", "Player 2,gtd,0.5,y"), tf)
o <- read_overrides(tf)
check("override file: statuses normalised", identical(o$status, c("Out", "Questionable")) && near(o$p_play[2], 0.5))
writeLines(c("name,status", "Player 1,wat"), tf)
check("override file: nonsense status is an error, not silently ignored", inherits(try(read_overrides(tf), silent = TRUE), "try-error"))
check("missing override file means no overrides", is.null(read_overrides(file.path(tempdir(), "nope.csv"))))
check("a name that matches nobody warns", {
  w <- tryCatch(resolve_overrides(read_overrides_any(data.frame(name = "Nobody Here", status = "Out")), slate), warning = function(w) "warned")
  identical(w, "warned") })

# was the official report actually published?
mk <- function(wk, share) data.frame(season = 2026, week = wk, report_status = c(rep("Out", round(100 * share)), rep("", 100 - round(100 * share))))
hist <- do.call(rbind, lapply(1:5, mk, share = 0.45))
check("injury coverage: a normal week is complete", isTRUE(injury_coverage(rbind(hist, mk(6, 0.42)), 2026, 6)$complete))
cv <- injury_coverage(rbind(hist, mk(6, 0.03)), 2026, 6)
check("injury coverage: a week with almost no designations is flagged incomplete", identical(cv$complete, FALSE) && !is.null(injury_coverage_note(cv)))

# ---------------------------------------------------------------------------
section("Benchmark comparison (hand-computed)")
b <- read_benchmark(data.frame(Player = c("Patrick Mahomes II", "Philadelphia Eagles", "Josh Allen"), Pos = c("QB", "D/ST", "QB"),
                               Team = c("KC", "", "BUF"), FPTS = c("22.5", "8", "bad"), stringsAsFactors = FALSE), "fd")
check("benchmark columns are recognised by common aliases", all(c("name", "position", "team", "proj", "key") %in% names(b)))
check("defence rows are keyed by team, even when only the team name is given", "DST|PHI" %in% b$key)
check("rows with a non-numeric projection are dropped", nrow(b) == 2)
check("names are matched ignoring suffixes (Mahomes II)", paste0("QB|", .name_key("Patrick Mahomes")) %in% b$key)
check("a benchmark with no projection column is an error",
      inherits(try(read_benchmark(data.frame(name = "x", position = "QB")), silent = TRUE), "try-error"))

mine <- data.frame(name = c("Alpha QB", "Bravo RB", "Charlie WR", "Delta TE", "KC DST"), position = c("QB", "RB", "WR", "TE", "DST"),
                   team = c("KC", "BUF", "MIA", "NYJ", "KC"), proj = c(20, 20, 8, 5, 9), proj_active = c(20, 20, 8, 5, 9), stringsAsFactors = FALSE)
bench <- data.frame(name = c("Alpha QB", "Bravo RB", "Charlie WR", "Delta TE", "Kansas City Chiefs"), position = c("QB", "RB", "WR", "TE", "DEF"),
                    team = c("KC", "BUF", "MIA", "NYJ", ""), proj = c(21, 10, 0, 5.5, 9.5), stringsAsFactors = FALSE)
cmp <- compare_projections(mine, list(src = bench))
t <- cmp$table
check("every player is found, including the defence by team name", sum(!is.na(t$src)) == 5)
d <- c(20 - 21, 20 - 10, 8 - 0, 5 - 5.5, 9 - 9.5)
check("bias = mean(mine - source) = 1.4 (hand-computed)", near(cmp$summary$bias, round(mean(d), 2)))
check("rmsd matches by hand", near(cmp$summary$rmsd, round(sqrt(mean(d^2)), 2)))
note <- setNames(t$note, t$name)
check("a 10-point gap is flagged 'I'm higher'", note[["Bravo RB"]] == "I'm higher")
check("a source at ~0 for a player I project at 8 is flagged as a possible Out", grepl("Out", note[["Charlie WR"]]))
check("small gaps are not flagged", all(note[c("Alpha QB", "Delta TE", "KC DST")] == ""))
check("flagged rows sort first, largest gap first", t$name[1] == "Charlie WR" || t$name[1] == "Bravo RB")
two <- compare_projections(mine, list(a = bench, b = transform(bench, proj = proj + c(0, 2, 2, 0, 0))))
check("consensus is the mean of the sources", near(two$table$consensus[two$table$name == "Bravo RB"], 11))
check("blend: 50/50 of mine and consensus", near(blend_projections(cmp, 0.5)[t$name == "Bravo RB"], 15))
check("sources must be named", inherits(try(compare_projections(mine, list(bench)), silent = TRUE), "try-error"))

section("Scoring sources against results (mock results)")
real_actual_points <- actual_points
actual_points <- function(season, week, site) {
  a <- data.frame(name = c("Alpha QB", "Bravo RB", "Charlie WR", "Delta TE", "KC"), position = c("QB", "RB", "WR", "TE", "DST"),
                  team = c("KC", "BUF", "MIA", "NYJ", "KC"), actual = c(21, 10, 0, 6, 9), stringsAsFactors = FALSE)
  a$key <- .mine_key(a); attr(a, "teams_played") <- c("KC", "BUF", "MIA", "NYJ"); a
}
mine2 <- mine; mine2$proj_source <- "model"; mine2$proj <- c(20, 20, 0, 5, 9)       # my injury layer zeroed Charlie
cm <- compare_projections(mine2, list(src = bench)); cm$season <- 2026; cm$week <- 9; cm$site <- "fanduel"
ev <- evaluate_comparison(cm, min_proj = 0)
rm_ <- function(src) ev$rmse[ev$source == src]
check("source that equals the result scores zero error on those players",
      near(sqrt(mean(c(20 - 21, 20 - 10, 8 - 0, 5 - 6, 9 - 9)^2)), rm_("mine (if plays)"), 1e-3))
check("knowing Charlie was out (injury-weighted) beats the unweighted number", rm_("mine (injury-weighted)") < rm_("mine (if plays)"))
mine3 <- mine2; mine3$proj_source[1] <- "actual (game played)"
cm3 <- compare_projections(mine3, list(src = bench)); cm3$season <- 2026; cm3$week <- 9; cm3$site <- "fanduel"
check("rows projected after their game are skipped (they would be circular)",
      suppressMessages(evaluate_comparison(cm3, min_proj = 0)$n[1]) == 4)
dir <- tempfile(); save_snapshot(cm, dir)
check("snapshots round-trip through evaluate_snapshots", {
  r <- evaluate_snapshots(dir, min_proj = 0); nrow(r$overall) == 4 && r$overall$weeks[1] == 1 })
actual_points <- real_actual_points

# ---------------------------------------------------------------------------
section("No look-ahead (needs nflverse data)")
data_ok <- tryCatch({ gm <- load_nfl_games(); stopifnot(nrow(gm) > 1000); TRUE }, error = function(e) FALSE)
if (!data_ok) {
  cat("[SKIP] nflverse data not available (offline?) - skipping data-dependent tests\n")
} else {
  seas <- 2024:2025; SZ <- 2025; WK <- 10
  ps <- load_nfl_player_stats(seas); ts <- load_nfl_team_stats(seas); inj <- load_nfl_injuries(seas)
  pri_to <- wk_index(SZ, 1) - 1
  num_cols <- function(d) names(d)[vapply(d, is.numeric, logical(1)) & !(names(d) %in% c("season", "week"))]

  base <- build_panel(ps, ts, gm, inj, "fanduel", prior_upto = pri_to)
  Pb <- base$panel; T0 <- wk_index(SZ, WK)
  feats <- c(OFF_FEATURES, "base_pts", "n_prior")

  # (1) Changing what happened IN the target week or later must not change the target week's features.
  ps2 <- ps; hit <- ps2$season == SZ & ps2$week >= WK
  for (cn in intersect(c("passing_yards", "rushing_yards", "receiving_yards", "targets", "carries", "attempts", "receptions"), names(ps2)))
    ps2[[cn]][hit] <- ps2[[cn]][hit] * 3 + 50
  ts2 <- ts; ht <- ts2$season == SZ & ts2$week >= WK
  for (cn in num_cols(ts2)) ts2[[cn]][ht] <- ts2[[cn]][ht] + 7
  gm2 <- gm; hg <- gm2$season == SZ & gm2$week >= WK
  gm2$home_score[hg] <- gm2$home_score[hg] + 21; gm2$away_score[hg] <- gm2$away_score[hg] + 14
  pert <- build_panel(ps2, ts2, gm2, inj, "fanduel", prior_upto = pri_to)$panel
  k0 <- paste(Pb$pid, Pb$widx); k1 <- paste(pert$pid, pert$widx)
  tgt <- which(Pb$widx == T0 & !Pb$is_future); j <- match(k0[tgt], k1)
  worst <- max(vapply(feats, function(f) max(abs(Pb[[f]][tgt] - pert[[f]][j]), na.rm = TRUE), numeric(1)))
  check(sprintf("features for week %d ignore week-%d results (%d players, %d features, max diff %.1e)", WK, WK, length(tgt), length(feats), worst),
        length(tgt) > 300 && worst < 1e-9)
  nxt <- which(Pb$widx == T0 + 1 & !Pb$is_future); jn <- match(k0[nxt], k1)
  check("...but week WK+1 features DO move (the perturbation is real, so the test above is meaningful)",
        max(abs(Pb$m_pts[nxt] - pert$m_pts[jn]), na.rm = TRUE) > 0.1)
  check("outcome for the target week is untouched in the base panel", all(!is.na(Pb$y[tgt])))

  # (2) A panel built from data that stops before the week gives the same features for that week.
  keep_ps <- !(ps$season == SZ & ps$week >= WK); keep_ts <- !(ts$season == SZ & ts$week >= WK)
  keep_inj <- !(inj$season == SZ & inj$week > WK)                    # the week's own report is pre-game information
  gm3 <- gm; gm3$home_score[hg] <- NA; gm3$away_score[hg] <- NA
  fut <- data.frame(pid = Pb$pid[tgt], name = Pb$name[tgt], pos = Pb$pos[tgt], team = Pb$team[tgt], season = SZ, week = WK)
  tr <- build_panel(ps[keep_ps, ], ts[keep_ts, ], gm3, inj[keep_inj, ], "fanduel", future = fut, prior_upto = pri_to)$panel
  trk <- paste(tr$pid, tr$widx); jj <- match(k0[tgt], trk)
  ok <- !is.na(jj)
  d2 <- max(vapply(setdiff(feats, "n_prior"), function(f) max(abs(Pb[[f]][tgt][ok] - tr[[f]][jj[ok]]), na.rm = TRUE), numeric(1)))
  check(sprintf("a live-style panel (data stops before the week) reproduces the backtest features (%d players, max diff %.1e)", sum(ok), d2),
        sum(ok) > 300 && d2 < 1e-9)

  # (3) Predictions for the week use only earlier games.
  p0 <- predict_offense_week(Pb, T0)
  P2 <- Pb; P2$y[P2$widx >= T0] <- P2$y[P2$widx >= T0] * 5 + 10
  p1 <- predict_offense_week(P2, T0)
  check("predictions for a week do not change if that week's outcomes change", near(p0$pred, p1$pred, 1e-9))
  dd <- predict_dst_week(base$team_panel, T0); tp2 <- base$team_panel; tp2$y_dst[tp2$widx >= T0] <- 99
  check("defence predictions likewise", near(dd$pred, predict_dst_week(tp2, T0)$pred, 1e-9))

  # (4) Whole-pipeline check on a slate built from real players.
  section("project_slate end to end")
  gmx <- gm[gm$season == SZ & gm$week == WK, ]
  rec <- build_player_games(ps, "fanduel"); rec <- rec[rec$season == SZ & rec$week < WK & rec$week >= WK - 4, ]
  ag <- stats::aggregate(y ~ pid + name + pos + team, rec, mean); ag <- ag[ag$y >= 6, ]
  oppm <- rbind(data.frame(team = gmx$home_team, opp = gmx$away_team), data.frame(team = gmx$away_team, opp = gmx$home_team))
  ag$team <- normalize_team(ag$team); ag$opponent <- normalize_team(oppm$opp[match(ag$team, normalize_team(oppm$team))]); ag <- ag[!is.na(ag$opponent), ]
  sl <- data.frame(id = as.character(seq_len(nrow(ag))), name = ag$name, position = ag$pos, team = ag$team, opponent = ag$opponent,
                   salary = round((4000 + 400 * ag$y) / 100) * 100, stringsAsFactors = FALSE)
  dsl <- data.frame(id = paste0("d", seq_len(nrow(oppm))), name = paste(normalize_team(oppm$team), "DST"), position = "DST",
                    team = normalize_team(oppm$team), opponent = normalize_team(oppm$opp), salary = 4000, stringsAsFactors = FALSE)
  sl <- rbind(sl, dsl)
  # Make the target week look like the future: hide its results from the loaders, then score the forecast.
  orig <- list(ps = load_nfl_player_stats, ts = load_nfl_team_stats, inj = load_nfl_injuries)
  hide <- function(d) d[!(d$season == SZ & d$week >= WK), , drop = FALSE]
  load_nfl_player_stats <- function(seasons) hide(orig$ps(seasons))
  load_nfl_team_stats   <- function(seasons) hide(orig$ts(seasons))
  load_nfl_injuries     <- function(seasons) { d <- orig$inj(seasons); d[!(d$season == SZ & d$week > WK), , drop = FALSE] }
  res <- ov_res <- NULL
  top <- sl[sl$position == "WR", ]; top <- top[order(-top$salary), ][1, ]
  ovd <- data.frame(name = top$name, status = "Out", team = top$team)
  err <- tryCatch({
    res <- suppressMessages(project_slate(sl, "fanduel", season = SZ, week = WK, verbose = FALSE))
    ov_res <- suppressMessages(project_slate(sl, "fanduel", season = SZ, week = WK, overrides = ovd, verbose = FALSE))
    NULL }, error = function(e) conditionMessage(e))
  load_nfl_player_stats <- orig$ps; load_nfl_team_stats <- orig$ts; load_nfl_injuries <- orig$inj    # restore
  if (!is.null(err)) { cat("project_slate failed:", err, "\n"); check("project_slate runs on a hidden-results week", FALSE) } else {
    check("every slate row gets a projection", !anyNA(res$proj) && nrow(res) == nrow(sl))
    check("proj = proj_active x p_play", near(res$proj, round(res$proj_active * res$p_play, 2), 0.011))
    check("chance of playing is a probability", all(res$p_play >= 0 & res$p_play <= 1))
    check("defences are present and always 'play'", all(res$p_play[res$position == "DST"] == 1))
    check("with results hidden, nothing is labelled as an actual", !any(res$proj_source == "actual (game played)", na.rm = TRUE))
    act <- build_player_games(ps, "fanduel"); act <- act[act$season == SZ & act$week == WK, ]
    sk <- res[res$position != "DST", ]; a <- act$y[match(sk$pid, act$pid)]; a[is.na(a)] <- 0
    ok <- sk$p_play > 0.9
    cat(sprintf("  forecast vs actual, %d players: r = %.2f, mean error = %.2f, RMSE = %.2f\n",
                sum(ok), stats::cor(sk$proj_active[ok], a[ok]), mean(sk$proj_active[ok] - a[ok]), sqrt(mean((sk$proj_active[ok] - a[ok])^2))))
    check("live-path forecasts are positively correlated with what happened", sum(ok) > 100 && stats::cor(sk$proj_active[ok], a[ok]) > 0.25)
    check("live-path forecasts are not biased by more than 2 points", abs(mean(sk$proj_active[ok] - a[ok])) < 2)
    o <- ov_res[ov_res$name == top$name, ]
    check("Out override: that player is projected at 0 and sourced to the override", o$proj == 0 && o$status_source == "your override")
    tm <- res$team == top$team & res$position %in% c("WR", "TE", "RB", "QB") & res$name != top$name
    check("Out override: his teammates' if-active projections go up, not down",
          sum(ov_res$proj_active[tm]) > sum(res$proj_active[tm]) - 1e-9 && sum(ov_res$proj_active[tm]) > sum(res$proj_active[tm]))
  }
}

if (isTRUE(exists("standalone"))) {
  cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail)); if (n_fail > 0) quit(status = 1)
}
