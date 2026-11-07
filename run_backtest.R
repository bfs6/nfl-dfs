# How good are the model's projections, honestly?  Walk-forward backtest: for every past week,
# the model sees only what was known before kickoff, then is scored on what happened.
#
#   Rscript run_backtest.R [draftkings|fanduel] [first_season] [last_season]

args <- commandArgs(FALSE)
this <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(this)) dirname(normalizePath(this)) else getwd()
invisible(lapply(list.files(file.path(root, "R"), "\\.R$", full.names = TRUE), source))
options(dfs.cache_dir = file.path(root, "cache"))

a <- commandArgs(trailingOnly = TRUE)
site <- if (length(a) >= 1) a[1] else "fanduel"
last <- if (length(a) >= 3) as.integer(a[3]) else .current_season()
first <- if (length(a) >= 2) as.integer(a[2]) else last - 1
seas <- (first - 2):last                                    # two extra seasons of history before the first scored one

ps <- load_nfl_player_stats(seas); ts <- load_nfl_team_stats(seas)
gm <- load_nfl_games(); inj <- load_nfl_injuries(seas)
bt <- backtest_projections(ps, ts, gm, inj, site, eval_seasons = first:last)

cat("\nOffense: model vs a plain rolling average, players averaging >= 5 points\n")
print(summarize_backtest(bt))
cat("\nCalibration: when the model says X, what happens on average?\n")
print(calibration_table(bt))
cat("\nWithin-week ranking skill (average rank correlation of projection and result, per week and position):\n")
print(weekly_rank_corr(bt))
