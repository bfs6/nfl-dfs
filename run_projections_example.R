# Projections + injuries + benchmark check + lineups, in one pass.
#
#   Rscript run_projections_example.R                 # demo: real players, synthetic salaries
#   Rscript run_projections_example.R fanduel my_salaries.csv
#
# First run downloads nflverse data (~80 MB over a few files) into ./cache.

args <- commandArgs(FALSE)
this <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(this)) dirname(normalizePath(this)) else getwd()
invisible(lapply(list.files(file.path(root, "R"), "\\.R$", full.names = TRUE), source))
options(dfs.cache_dir = file.path(root, "cache"))
dir.create(file.path(root, "output"), showWarnings = FALSE)

a <- commandArgs(trailingOnly = TRUE)
site        <- if (length(a) >= 1) a[1] else "fanduel"
salary_file <- if (length(a) >= 2) a[2] else NULL

# ---- 1. the slate -------------------------------------------------------------
slate <- if (!is.null(salary_file)) {
  if (site == "fanduel") load_fd_players(salary_file, drop_out = FALSE) else load_dk_salaries(salary_file)   # keep FanDuel "O" players so their absence lifts teammates
} else {
  cat("No salary file given: using a DEMO slate (real players, made-up salaries).\n")
  demo_slate(site)
}

# ---- 2. your late news (edit data/overrides.csv; see data/overrides_example.csv) -----
overrides <- file.path(root, "data", "overrides.csv")        # optional; ignored if missing
aliases   <- file.path(root, "data", "aliases.csv")          # optional; for names that fail to match
if (!file.exists(aliases)) aliases <- NULL

# ---- 3. projections (includes the injury adjustment) ---------------------------
proj <- project_slate(slate, site, overrides = overrides, aliases = aliases)
print(proj)

# ---- 4. who might not play? ------------------------------------------------------
cat("\n===== INJURY CHECK (look at this again right before lock) =====\n")
print(injury_report(proj, min_proj = 6))

# ---- 5. cross-check against other projections -------------------------------------
# Put FanDuel Research / any other source in a CSV with name, position, team, proj columns.
bench_files <- c(fanduel_research = file.path(root, "data", "benchmark_fanduel.csv"),
                 other            = file.path(root, "data", "benchmark_other.csv"))
bench_files <- as.list(bench_files[file.exists(bench_files)])
if (length(bench_files)) {
  cmp <- compare_projections(proj, bench_files)
  print(cmp)
  save_snapshot(cmp, file.path(root, "snapshots"))      # so you can score everyone after the games
} else {
  cat("\n(No benchmark files in data/: add benchmark_fanduel.csv to compare. See data/benchmark_example.csv.)\n")
}

# ---- 6. lineups from the injury-adjusted projections -------------------------------
players <- playable_slate(proj)                         # drops Out players, byes and games already played
res <- optimize_lineups(players, site, n = 10, min_unique = 3, randomness = 0.12,
                        max_exposure = 0.5, stack_min = 1, bringback_min = 1, seed = 1)
print(res)
if (!nrow(res$lineups)) stop("No lineups could be built from this slate (check salaries/positions).", call. = FALSE)
stopifnot(nrow(validate_result(res)) == 0)
write_lineups_csv(res, file.path(root, "output", paste0(site, "_projected_lineups.csv")), what = "names")
cat("\nWrote output/", site, "_projected_lineups.csv\n", sep = "")

# ---- 7. after the games: how accurate was everyone? ------------------------------------
#   print(evaluate_snapshots(file.path(root, "snapshots")))
