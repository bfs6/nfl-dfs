# Scoring unit tests with hand-computed answers. Sourced by run_tests.R, or run alone:
#   Rscript tests/test_scoring.R

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

section("Scoring (hand-computed stat lines)")

# QB: 320 pass yd, 3 TD, 1 INT; 30 rush yd, 1 rush TD; 1 fumble lost
qb <- data.frame(passing_yards = 320, passing_tds = 3, passing_interceptions = 1,
                 rushing_yards = 30, rushing_tds = 1, sack_fumbles_lost = 1)
# 12.8 + 12 - 1 + 3 + 6 + 3(300yd bonus) - fumble
check("DraftKings QB line = 12.8+12-1+3+6-1+3 = 34.8", isTRUE(all.equal(score_offense(qb, "draftkings"), 34.8)))
check("FanDuel QB line = 12.8+12-1+3+6-2 = 30.8 (no bonus, fumble -2)", isTRUE(all.equal(score_offense(qb, "fanduel"), 30.8)))

# RB: 122 rush yd, 2 TD, 6 rec, 56 rec yd, 1 fumble lost  (a classic textbook line)
rb <- data.frame(rushing_yards = 122, rushing_tds = 2, receptions = 6, receiving_yards = 56, rushing_fumbles_lost = 1)
# DK: 12.2 + 12 + 6 + 5.6 - 1 + 3 = 37.8     FD: 12.2 + 12 + 3 + 5.6 - 2 = 30.8
check("DraftKings RB line = 37.8 (full PPR, 100-yd bonus)", isTRUE(all.equal(score_offense(rb, "draftkings"), 37.8)))
check("FanDuel RB line = 30.8 (half PPR, no bonus)", isTRUE(all.equal(score_offense(rb, "fanduel"), 30.8)))

# WR at exactly 100 yards gets the bonus; 99 does not
wr <- data.frame(receptions = c(5, 5), receiving_yards = c(100, 99), receiving_tds = 0)
check("DK 100-yard receiving bonus applies at exactly 100, not 99",
      isTRUE(all.equal(score_offense(wr, "draftkings"), c(5 + 10 + 3, 5 + 9.9))))
check("FD has no yardage bonus", isTRUE(all.equal(score_offense(wr, "fanduel"), c(2.5 + 10, 2.5 + 9.9))))

check("missing stat columns are treated as zero", score_offense(data.frame(x = 1), "draftkings") == 0)

# Defense: 3 sacks, 1 INT, 1 fumble rec, 1 defensive TD, 1 safety, 13 points allowed
d <- data.frame(def_sacks = 3, def_interceptions = 1, fumble_recovery_opp = 1, def_tds = 1,
                def_safeties = 1, points_allowed = 13)
check("DST line = 3+2+2+6+2+4(7-13 pts allowed) = 19", score_dst(d, "draftkings") == 19)
pa <- points_allowed_score(c(0, 1, 6, 7, 13, 14, 20, 21, 27, 28, 34, 35, 50))
check("points-allowed tiers: 10,7,7,4,4,1,1,0,0,-1,-1,-4,-4", identical(as.numeric(pa), c(10, 7, 7, 4, 4, 1, 1, 0, 0, -1, -1, -4, -4)))

if (isTRUE(exists("standalone"))) {
  cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail)); if (n_fail > 0) quit(status = 1)
}
