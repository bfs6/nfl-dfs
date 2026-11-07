# Run with:  Rscript tests/run_tests.R      (no testthat needed)

args <- commandArgs(FALSE)
this <- sub("^--file=", "", args[grep("^--file=", args)])
root <- normalizePath(file.path(dirname(this), ".."))
invisible(lapply(list.files(file.path(root, "R"), "\\.R$", full.names = TRUE), source))

n_pass <- 0L; n_fail <- 0L
check <- function(desc, cond) {
  ok <- isTRUE(cond)
  if (ok) n_pass <<- n_pass + 1L else n_fail <<- n_fail + 1L
  cat(sprintf("[%s] %s\n", if (ok) "PASS" else "FAIL", desc))
}
section <- function(s) cat(sprintf("\n--- %s ---\n", s))

solvers <- c("Rglpk", "lpSolve")[c(requireNamespace("Rglpk", quietly = TRUE),
                                    requireNamespace("lpSolve", quietly = TRUE))]
cat("Solvers under test:", paste(solvers, collapse = ", "), "\n")

pairwise_overlap <- function(res) {
  M <- vapply(res$idx, function(i) tabulate(i, nrow(res$players)), integer(nrow(res$players)))
  ov <- crossprod(M); diag(ov) <- 0L; ov
}

# ---------------------------------------------------------------------------
section("Full-size synthetic slate: rules, uniqueness, exposure")
for (site in c("draftkings", "fanduel")) {
  for (sv in solvers) {
    dat <- make_sample_slate(site)
    res <- optimize_lineups(dat, site, n = 40, min_unique = 3, randomness = 0.15,
                            max_exposure = 0.5, stack_min = 1, bringback_min = 1,
                            seed = 7, solver = sv, verbose = FALSE)
    tag <- sprintf("[%s/%s]", site, sv)
    check(paste(tag, "returns all 40 lineups"), nrow(res$lineups) == 40)
    check(paste(tag, "every lineup passes the independent rule check"), nrow(validate_result(res)) == 0)
    check(paste(tag, "no two lineups share more than 6 players (min_unique = 3)"), max(pairwise_overlap(res)) <= 6)
    check(paste(tag, "max player exposure <= 50%"), max(res$exposure$lineups) <= 20)
    check(paste(tag, "all lineups have a bring-back"), all(res$lineups$bringback >= 1))
    check(paste(tag, "all lineups have a QB stack"), all(res$lineups$qb_stack >= 1))
    base <- optimize_lineups(dat, site, n = 1, randomness = 0, stack_min = 1, bringback_min = 1,
                             solver = sv, verbose = FALSE)
    check(paste(tag, "lineup 1 is the pure max-projection lineup"),
          isTRUE(all.equal(res$lineups$proj[1], base$lineups$proj[1])))
  }
}

# ---------------------------------------------------------------------------
section("Solvers agree (deterministic mode)")
if (length(solvers) == 2) {
  for (site in c("draftkings", "fanduel")) {
    dat <- make_sample_slate(site, seed = 3)
    a <- optimize_lineups(dat, site, n = 10, randomness = 0, solver = "Rglpk", verbose = FALSE)
    b <- optimize_lineups(dat, site, n = 10, randomness = 0, solver = "lpSolve", verbose = FALSE)
    check(sprintf("[%s] Rglpk and lpSolve give the same 10 projected totals", site),
          isTRUE(all.equal(a$lineups$proj, b$lineups$proj)))
  }
} else cat("(skipped: needs both solvers)\n")

# ---------------------------------------------------------------------------
section("Locks, bans, per-player caps, reproducibility, edge cases")
dat <- make_sample_slate("draftkings", seed = 5)
star <- dat$name[which.max(dat$proj)]
other <- dat$name[order(-dat$proj)][2]
r <- optimize_lineups(dat, "draftkings", n = 12, lock = star, ban = other, randomness = 0.1, seed = 1, verbose = FALSE)
check("locked player appears in every lineup", all(apply(r$lineups[, r$cfg$slots], 1, function(x) star %in% x)))
check("banned player appears in no lineup", !any(unlist(r$lineups[, r$cfg$slots]) == other))

dat2 <- dat; dat2$max_exp <- NA_real_; dat2$max_exp[dat2$name == star] <- 0.1
r <- optimize_lineups(dat2, "draftkings", n = 20, randomness = 0.2, seed = 2, verbose = FALSE)
check("per-player max_exp column is respected (<= 2 of 20)",
      sum(unlist(r$lineups[, r$cfg$slots]) == star) <= 2)

r1 <- optimize_lineups(dat, "fanduel", n = 6, randomness = 0.2, seed = 99, verbose = FALSE)
r2 <- optimize_lineups(dat, "fanduel", n = 6, randomness = 0.2, seed = 99, verbose = FALSE)
check("same seed reproduces identical lineups", identical(r1$ids, r2$ids))

dat3 <- dat; dat3$stdev <- dat3$proj * 0.3
r <- optimize_lineups(dat3, "draftkings", n = 5, randomness = 1, seed = 4, verbose = FALSE)
check("stdev column accepted and lineups valid", nrow(r$lineups) == 5 && nrow(validate_result(r)) == 0)

r <- suppressWarnings(optimize_lineups(dat, "draftkings", n = 5, min_salary = 60000, verbose = FALSE))
check("impossible constraints end gracefully with 0 lineups (no crash)", nrow(r$lineups) == 0)
w <- tryCatch(optimize_lineups(dat, "draftkings", n = 3, min_salary = 60000, verbose = FALSE),
              warning = function(w) conditionMessage(w))
check("...and say why in a warning", is.character(w) && grepl("no feasible lineup", w))

# ---------------------------------------------------------------------------
section("Brute-force optimality on a tiny slate (4 teams, 2 games)")

make_tiny <- function(seed) {
  set.seed(seed)
  teams <- c("AAA", "BBB", "CCC", "DDD"); opp <- c(AAA = "BBB", BBB = "AAA", CCC = "DDD", DDD = "CCC")
  spec <- data.frame(position = c("QB", "RB", "RB", "WR", "WR", "TE", "DST"),
                     lo = c(5000, 5000, 3500, 5000, 3500, 3000, 2000),
                     hi = c(8000, 9000, 5500, 9000, 5500, 6000, 3500), stringsAsFactors = FALSE)
  rows <- list()
  for (t in teams) for (r in seq_len(nrow(spec))) {
    sal <- round(runif(1, spec$lo[r], spec$hi[r]) / 100) * 100
    rows[[length(rows) + 1]] <- data.frame(
      id = sprintf("%s-%d", t, r), name = sprintf("%s %s%d", t, spec$position[r], r),
      position = spec$position[r], team = t, opponent = opp[[t]], salary = sal,
      proj = round(sal / 1000 * 2.4 * exp(rnorm(1, 0, 0.25)), 1), stringsAsFactors = FALSE)
  }
  prepare_players(do.call(rbind, rows))
}

# Enumerate every lineup that satisfies all rules EXCEPT the salary cap.
enumerate_lineups <- function(P, cfg, o) {
  teams <- unique(P$team); games <- unique(P$game)
  fx <- which(P$position %in% c("RB", "WR", "TE"))
  C <- utils::combn(fx, 7)
  pos <- matrix(P$position[C], nrow = 7)
  nrb <- colSums(pos == "RB"); nwr <- colSums(pos == "WR"); nte <- colSums(pos == "TE")
  C <- C[, nrb >= 2 & nrb <= 3 & nwr >= 3 & nwr <= 4 & nte >= 1 & nte <= 2, drop = FALSE]
  N <- ncol(C)
  cnt <- function(vals, keys, mask = NULL) {   # per-key count of combo players, keys x N
    t(vapply(keys, function(k) colSums(matrix(vals[C] == k & (if (is.null(mask)) TRUE else mask[C]), nrow = 7)),
             numeric(N)))
  }
  tc <- cnt(P$team, teams); gc <- cnt(P$game, games)
  stk <- cnt(P$team, teams, P$position %in% o$stack_positions)
  bb  <- cnt(P$team, teams, P$position %in% o$bringback_positions)
  off <- cnt(P$team, teams, P$position %in% c("RB", "WR", "TE"))
  sal <- colSums(matrix(P$salary[C], nrow = 7)); prj <- colSums(matrix(P$proj[C], nrow = 7))
  mpt <- if (!is.null(o$max_per_team)) o$max_per_team else cfg$max_per_team

  M <- list(); S <- list(); R <- list()
  for (q in which(P$position == "QB")) for (d in which(P$position == "DST")) {
    tq <- match(P$team[q], teams); td <- match(P$team[d], teams)
    gq <- match(P$game[q], games); gd <- match(P$game[d], games)
    tc2 <- tc; tc2[tq, ] <- tc2[tq, ] + 1; tc2[td, ] <- tc2[td, ] + 1
    gc2 <- gc; gc2[gq, ] <- gc2[gq, ] + 1; gc2[gd, ] <- gc2[gd, ] + 1
    ok <- colSums(tc2 > 0) >= cfg$min_teams & colSums(gc2 > 0) >= cfg$min_games
    if (!is.na(mpt)) ok <- ok & colSums(tc2 > mpt) == 0
    if (o$stack_min > 0) ok <- ok & stk[tq, ] >= o$stack_min
    if (o$bringback_min > 0) ok <- ok & bb[match(P$opponent[q], teams), ] >= o$bringback_min
    if (o$no_dst_vs_offense) {
      ok <- ok & off[match(P$opponent[d], teams), ] == 0 & P$team[q] != P$opponent[d]
    }
    if (any(ok)) {
      M[[length(M) + 1]] <- rbind(C[, ok, drop = FALSE], q, d)
      S[[length(S) + 1]] <- sal[ok] + P$salary[q] + P$salary[d]
      R[[length(R) + 1]] <- prj[ok] + P$proj[q] + P$proj[d]
    }
  }
  list(M = do.call(cbind, M), salary = unlist(S), proj = unlist(R))
}

best_sequence <- function(all, cap, lineups, min_unique, nP) {
  v <- all$salary <= cap
  M <- all$M[, v, drop = FALSE]; sc <- all$proj[v]
  vapply(seq_along(lineups), function(k) {
    keep <- rep(TRUE, ncol(M))
    for (prev in lineups[seq_len(k - 1)]) {
      ind <- logical(nP); ind[prev] <- TRUE
      keep <- keep & colSums(matrix(ind[M], nrow = 9)) <= 9 - min_unique
    }
    max(sc[keep])
  }, numeric(1))
}

scenarios <- list(
  strategy_on  = list(stack_min = 1, stack_positions = c("WR", "TE"), bringback_min = 1,
                      bringback_positions = c("WR", "TE"), no_dst_vs_offense = TRUE),
  strategy_off = list(stack_min = 0, stack_positions = c("WR", "TE"), bringback_min = 0,
                      bringback_positions = c("WR", "TE"), no_dst_vs_offense = FALSE)
)
for (site in c("draftkings", "fanduel")) {
  for (sc_name in names(scenarios)) {
    o <- scenarios[[sc_name]]
    P <- make_tiny(11)
    cfg <- site_config(site)
    all <- enumerate_lineups(P, cfg, o)
    cfg$cap <- round(stats::quantile(all$salary, 0.4) / 100) * 100   # make the cap bind
    for (sv in solvers) {
      res <- optimize_lineups(P, cfg, n = 8, min_unique = 3, randomness = 0, solver = sv, verbose = FALSE,
                              stack_min = o$stack_min, bringback_min = o$bringback_min,
                              no_dst_vs_offense = o$no_dst_vs_offense)
      got  <- vapply(res$idx, function(i) sum(P$proj[i]), numeric(1))
      want <- best_sequence(all, cfg$cap, res$idx, 3, nrow(P))
      check(sprintf("[%s/%s/%s] 8 sequential lineups match brute-force optimum (%d candidate lineups)",
                    site, sc_name, sv, sum(all$salary <= cfg$cap)),
            length(got) == 8 && isTRUE(all.equal(got, want)))
      check(sprintf("[%s/%s/%s] and all pass the independent rule check", site, sc_name, sv),
            nrow(validate_result(res)) == 0)
    }
  }
}

# ---------------------------------------------------------------------------
section("File loaders (synthetic files shaped like the site downloads)")
td <- tempfile("dfs"); dir.create(td)
for (site in c("draftkings", "fanduel")) {
  dat <- make_sample_slate(site, seed = 8)
  if (site == "draftkings") {
    gi <- vapply(seq_len(nrow(dat)), function(r) {
      a <- dat$team[r]; b <- dat$opponent[r]; sprintf("%s@%s 11/09/2026 01:00PM ET", min(a, b), max(a, b))
    }, character(1))
    raw <- data.frame(Position = dat$position, `Name + ID` = sprintf("%s (%s)", dat$name, dat$id),
                      Name = dat$name, ID = dat$id, `Roster Position` = ifelse(dat$position %in% c("RB","WR","TE"), paste0(dat$position, "/FLEX"), dat$position),
                      Salary = dat$salary, `Game Info` = gi, TeamAbbrev = ifelse(dat$team == "LAR", "LA", dat$team),
                      AvgPointsPerGame = round(dat$proj * 0.9, 2), check.names = FALSE)
    f <- file.path(td, "DKSalaries.csv"); utils::write.csv(raw, f, row.names = FALSE)
    slate <- load_dk_salaries(f)
  } else {
    raw <- data.frame(Id = dat$id, Position = ifelse(dat$position == "DST", "D", dat$position),
                      `First Name` = sub(" .*", "", dat$name), Nickname = dat$name,
                      `Last Name` = sub("^[^ ]+ ", "", dat$name), FPPG = round(dat$proj * 0.9, 2), Played = 10,
                      Salary = dat$salary, Game = paste0(dat$opponent, "@", dat$team), Team = dat$team,
                      Opponent = dat$opponent, `Injury Indicator` = "", check.names = FALSE, stringsAsFactors = FALSE)
    raw$`Injury Indicator`[5] <- "O"
    f <- file.path(td, "FanDuel-players.csv"); utils::write.csv(raw, f, row.names = FALSE)
    slate <- load_fd_players(f)
  }
  expect_n <- nrow(dat) - (site == "fanduel")
  check(sprintf("[%s] loader reads %d players (FD drops the 'O' player)", site, expect_n), nrow(slate) == expect_n)
  m <- match(slate$id, dat$id)
  check(sprintf("[%s] loader recovers team/opponent/salary/position correctly", site),
        all(slate$team == dat$team[m]) && all(slate$opponent == dat$opponent[m]) &&
        all(slate$salary == dat$salary[m]) && all(slate$position == dat$position[m]))

  pr <- data.frame(Name = ifelse(dat$position == "DST", dat$team, toupper(dat$name)),   # names in a different case
                   Position = ifelse(dat$position == "DST", "DEF", dat$position),
                   Team = dat$team, Proj = dat$proj)
  pr <- pr[sample(nrow(pr)), ]; names(pr) <- tolower(names(pr))
  merged <- merge_projections(slate, pr, verbose = FALSE)
  check(sprintf("[%s] merge matches every player (incl. defenses by team)", site), !anyNA(merged$proj))
  res <- optimize_lineups(merged, site, n = 5, randomness = 0.05, seed = 1, verbose = FALSE)
  check(sprintf("[%s] file -> merge -> optimize works end to end", site),
        nrow(res$lineups) == 5 && nrow(validate_result(res)) == 0)
  out <- file.path(td, paste0(site, "_upload.csv")); write_lineups_csv(res, out)
  back <- utils::read.csv(out, colClasses = "character", check.names = FALSE)
  check(sprintf("[%s] upload CSV has 9 id columns and 5 rows", site), ncol(back) == 9 && nrow(back) == 5)
}

for (f in c("test_scoring.R", "test_projections.R")) source(file.path(root, "tests", f), local = FALSE)

cat(sprintf("\n%d passed, %d failed\n", n_pass, n_fail))
if (n_fail > 0) quit(status = 1)
