# Demo: build 20 lineups for DraftKings and FanDuel from a SYNTHETIC slate
# (fake players, so the numbers mean nothing) and write upload-style CSVs.
#
#   Rscript run_example.R          (or open in RStudio and source it)

args <- commandArgs(FALSE)
this <- sub("^--file=", "", args[grep("^--file=", args)])
root <- if (length(this)) dirname(normalizePath(this)) else getwd()
invisible(lapply(list.files(file.path(root, "R"), "\\.R$", full.names = TRUE), source))

dir.create(file.path(root, "output"), showWarnings = FALSE)

for (site in c("draftkings", "fanduel")) {
  cat("\n==================== ", toupper(site), " ====================\n", sep = "")

  slate <- make_sample_slate(site)                 # <- replace with your real data (see below)

  res <- optimize_lineups(
    slate, site,
    n             = 20,
    min_unique    = 3,      # each lineup differs from every other by >= 3 players
    randomness    = 0.12,   # projection noise before each solve (0 = identical to pure optimum)
    max_exposure  = 0.5,    # no player in more than half the lineups
    stack_min     = 1,      # QB + at least 1 WR/TE from his team
    bringback_min = 1,      # ...and at least 1 WR/TE from the opposing team
    seed          = 42
  )
  print(res)

  bad <- validate_result(res)                       # independent re-check against the site rules
  stopifnot(nrow(bad) == 0)

  write_lineups_csv(res, file.path(root, "output", paste0(site, "_lineups_upload.csv")), what = "ids")
  write_lineups_csv(res, file.path(root, "output", paste0(site, "_lineups_names.csv")),  what = "names")
}
cat("\nWrote CSVs to ", file.path(root, "output"), "\n", sep = "")

# Save the synthetic slates once so you can see the expected column layout.
for (site in c("draftkings", "fanduel")) {
  f <- file.path(root, "data", paste0("sample_slate_", site, ".csv"))
  if (!file.exists(f)) utils::write.csv(make_sample_slate(site), f, row.names = FALSE)
}

# ---- With real data -------------------------------------------------------
# 1. Download the salary file from the site (DraftKings: DKSalaries.csv from the
#    contest's "Draft Team" page; FanDuel: the player-list CSV from the contest).
# 2. Get projections from wherever you source them: a CSV with columns
#       name, position, team, proj          (optional: stdev, max_exp, own)
#    Defenses are matched by team, so keep the team column filled for them.
#
#   slate <- load_dk_salaries("DKSalaries.csv")        # or load_fd_players("FanDuel-NFL-....csv")
#   proj  <- read_projections("my_projections.csv")
#   slate <- merge_projections(slate, proj)            # prints who failed to match
#   res   <- optimize_lineups(slate, "draftkings", n = 20, ...)
#   write_lineups_csv(res, "output/dk_upload.csv")
