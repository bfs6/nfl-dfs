# Core optimizer: build a 0/1 integer program, solve it repeatedly, and collect
# n different lineups.
#
# One binary variable per player (x_i = 1 means "in the lineup").
# Objective: maximize the sum of (possibly perturbed) projections.
# Constraints: salary cap, position counts, team/game rules for the site,
# optional QB stacking / bring-back, no-defense-vs-your-own-offense, locks.
# Each later lineup also gets "uniqueness" cuts against earlier lineups and
# per-player exposure caps.

# ---- constraint building ---------------------------------------------------

# Convert a list of rows (cols, vals, dir, rhs) into sparse triplets.
.assemble_rows <- function(rows) {
  if (!length(rows)) {
    return(list(i = integer(0), j = integer(0), v = numeric(0),
                dir = character(0), rhs = numeric(0), n_row = 0L))
  }
  lens <- vapply(rows, function(r) length(r$cols), integer(1))
  list(
    i     = rep.int(seq_along(rows), lens),
    j     = as.integer(unlist(lapply(rows, `[[`, "cols"))),
    v     = as.numeric(unlist(lapply(rows, `[[`, "vals"))),
    dir   = vapply(rows, `[[`, character(1), "dir"),
    rhs   = vapply(rows, `[[`, numeric(1), "rhs"),
    n_row = length(rows)
  )
}

build_model <- function(P, cfg, opts) {
  n <- nrow(P)
  rows <- list()
  add <- function(cols, vals, dir, rhs) {
    rows[[length(rows) + 1L]] <<- list(cols = cols, vals = vals, dir = dir, rhs = rhs)
  }
  n_aux <- 0L
  new_aux <- function(k) {
    idx <- n + n_aux + seq_len(k)
    n_aux <<- n_aux + k
    idx
  }

  # Salary cap (and optional floor), roster size
  add(seq_len(n), P$salary, "<=", cfg$cap)
  if (!is.null(opts$min_salary)) add(seq_len(n), P$salary, ">=", opts$min_salary)
  add(seq_len(n), rep(1, n), "==", cfg$n_players)

  # Position counts. min == max becomes an equality; otherwise a range, which is
  # how the FLEX spot is expressed (see sites.R).
  for (pos in names(cfg$pos_min)) {
    cols <- which(P$position == pos)
    if (!length(cols)) stop("No ", pos, " in the player pool.", call. = FALSE)
    lo <- cfg$pos_min[[pos]]; hi <- cfg$pos_max[[pos]]
    if (lo == hi) {
      add(cols, rep(1, length(cols)), "==", lo)
    } else {
      add(cols, rep(1, length(cols)), ">=", lo)
      add(cols, rep(1, length(cols)), "<=", hi)
    }
  }

  # Max players from one team (FanDuel: 4)
  mpt <- if (!is.null(opts$max_per_team)) opts$max_per_team else cfg$max_per_team
  if (!is.na(mpt)) {
    for (t in unique(P$team)) {
      cols <- which(P$team == t)
      if (length(cols) > mpt) add(cols, rep(1, length(cols)), "<=", mpt)
    }
  }

  # "At least k distinct games / teams" via indicator variables y_g <= sum(x in g)
  at_least_k_groups <- function(group, k) {
    g <- unique(group)
    y <- new_aux(length(g))
    for (m in seq_along(g)) {
      cols <- which(group == g[m])
      add(c(y[m], cols), c(1, rep(-1, length(cols))), "<=", 0)
    }
    add(y, rep(1, length(y)), ">=", k)
  }
  if (cfg$min_games > 1L) at_least_k_groups(P$game, cfg$min_games)
  teams_implied <- cfg$min_games >= cfg$min_teams ||
    (!is.na(mpt) && ceiling(cfg$n_players / mpt) >= cfg$min_teams)
  if (cfg$min_teams > 1L && !teams_implied) at_least_k_groups(P$team, cfg$min_teams)

  qbs <- which(P$position == "QB")

  # QB stack: each QB used needs >= stack_min pass catchers from his own team.
  if (opts$stack_min > 0) {
    for (j in qbs) {
      pool <- setdiff(which(P$team == P$team[j] & P$position %in% opts$stack_positions), j)
      add(c(pool, j), c(rep(1, length(pool)), -opts$stack_min), ">=", 0)
    }
  }
  # Bring-back: ...and >= bringback_min players from the opposing team.
  if (opts$bringback_min > 0) {
    for (j in qbs) {
      pool <- which(P$team == P$opponent[j] & P$position %in% opts$bringback_positions)
      add(c(pool, j), c(rep(1, length(pool)), -opts$bringback_min), ">=", 0)
    }
  }

  # Don't pair a defense with offensive players it plays against.
  if (opts$no_dst_vs_offense) {
    offense <- P$position %in% c("QB", "RB", "WR", "TE")
    for (j in which(P$position == "DST")) {
      for (i in which(offense & P$team == P$opponent[j])) add(c(i, j), c(1, 1), "<=", 1)
    }
  }

  # Locked players must be in every lineup.
  for (i in opts$lock_idx) add(i, 1, "==", 1)

  list(base = .assemble_rows(rows), n_var = n + n_aux, n_aux = n_aux, n = n)
}

# ---- solvers ---------------------------------------------------------------

.choose_solver <- function(solver = "auto") {
  have <- c(Rglpk   = requireNamespace("Rglpk", quietly = TRUE) && requireNamespace("slam", quietly = TRUE),
            lpSolve = requireNamespace("lpSolve", quietly = TRUE))
  if (solver == "auto") {
    if (!any(have)) stop("Install a solver: install.packages(\"Rglpk\") or install.packages(\"lpSolve\")", call. = FALSE)
    return(names(have)[have][1])
  }
  if (!solver %in% names(have)) stop("Unknown solver '", solver, "'. Use 'auto', 'Rglpk' or 'lpSolve'.", call. = FALSE)
  if (!have[[solver]]) stop("Solver '", solver, "' is not installed.", call. = FALSE)
  solver
}

# All variables are binary. `trip` holds the constraints as sparse triplets.
.solve_milp <- function(obj, trip, n_var, solver) {
  if (solver == "Rglpk") {
    mat <- slam::simple_triplet_matrix(trip$i, trip$j, trip$v, nrow = trip$n_row, ncol = n_var)
    res <- Rglpk::Rglpk_solve_LP(obj, mat, trip$dir, trip$rhs,
                                 types = rep("B", n_var), max = TRUE)
    return(list(ok = res$status == 0, x = res$solution, obj = res$optimum))
  }
  A <- matrix(0, nrow = trip$n_row, ncol = n_var)
  A[cbind(trip$i, trip$j)] <- trip$v
  res <- lpSolve::lp("max", obj, A, sub("==", "=", trip$dir, fixed = TRUE), trip$rhs, all.bin = TRUE)
  list(ok = res$status == 0, x = res$solution, obj = res$objval)
}

.solve_model <- function(model, obj_x, extra_rows, solver) {
  ex <- .assemble_rows(extra_rows)
  b  <- model$base
  trip <- list(
    i = c(b$i, ex$i + b$n_row), j = c(b$j, ex$j), v = c(b$v, ex$v),
    dir = c(b$dir, ex$dir), rhs = c(b$rhs, ex$rhs), n_row = b$n_row + ex$n_row
  )
  .solve_milp(c(obj_x, rep(0, model$n_aux)), trip, model$n_var, solver)
}

# ---- randomness ------------------------------------------------------------

# Nudge each projection by Gaussian noise. `randomness` is a fraction of the
# projection (0.10 -> sd of 10% of proj), or a multiplier on `stdev` if the
# player table has a stdev column.
.perturb <- function(proj, randomness, stdev = NULL) {
  if (randomness <= 0) return(proj)
  sd <- if (is.null(stdev)) randomness * proj else randomness * ifelse(is.na(stdev), proj, stdev)
  pmax(proj + stats::rnorm(length(proj), 0, sd), 0)
}

# ---- main entry point ------------------------------------------------------

#' Generate n lineups.
#'
#' @param players   data frame (see prepare_players for columns)
#' @param site      "draftkings", "fanduel", or a custom config list
#' @param n         number of lineups
#' @param min_unique every lineup differs from every earlier one by >= this many players
#' @param randomness noise on projections before each solve (0 = deterministic)
#' @param max_exposure share of the n lineups a player may appear in (1 = no cap);
#'                     a max_exp column in `players` overrides it per player
#' @param stack_min, stack_positions  QB needs this many of these positions from his team
#' @param bringback_min, bringback_positions  ...and this many from the opposing team
#' @param no_dst_vs_offense forbid a defense alongside players it faces
#' @param max_per_team override the site's per-team limit
#' @param min_salary require at least this much salary spent
#' @param lock, ban  player ids or names forced in / kept out of every lineup
#' @param first_lineup_optimal make lineup 1 the pure max-projection lineup
#' @param solver "auto", "Rglpk" or "lpSolve"
#' @param seed set for reproducible noise (sets the global RNG seed)
optimize_lineups <- function(players, site = "draftkings", n = 20,
                             min_unique = 2, randomness = 0.10, max_exposure = 1,
                             stack_min = 1, stack_positions = c("WR", "TE"),
                             bringback_min = 0, bringback_positions = c("WR", "TE"),
                             no_dst_vs_offense = TRUE, max_per_team = NULL, min_salary = NULL,
                             lock = NULL, ban = NULL, first_lineup_optimal = TRUE,
                             solver = "auto", seed = NULL, verbose = TRUE) {
  cfg <- if (is.list(site)) site else site_config(site)
  stopifnot(n >= 1, min_unique >= 1, min_unique <= cfg$n_players,
            randomness >= 0, max_exposure > 0, max_exposure <= 1)
  if (!is.null(seed)) set.seed(seed)
  solver <- .choose_solver(solver)

  P <- prepare_players(players, cfg)
  ban_idx <- resolve_players(P, ban, "banned player")
  if (length(ban_idx)) {
    P <- P[-ban_idx, , drop = FALSE]
    rownames(P) <- NULL
  }
  lock_idx <- resolve_players(P, lock, "locked player")

  opts <- list(stack_min = stack_min, stack_positions = stack_positions,
               bringback_min = bringback_min, bringback_positions = bringback_positions,
               no_dst_vs_offense = no_dst_vs_offense, max_per_team = max_per_team,
               min_salary = min_salary, lock_idx = lock_idx)
  model <- build_model(P, cfg, opts)

  # Per-player exposure caps (in lineups); locked players are exempt.
  me  <- rep(max_exposure, nrow(P))
  if ("max_exp" %in% names(P)) me <- ifelse(is.na(P$max_exp), me, P$max_exp)
  cap <- ifelse(me > 0, pmax(1L, floor(me * n + 1e-9)), 0L)
  cap[lock_idx] <- n

  counts <- integer(nrow(P))
  cuts   <- list()
  lineups <- list()
  stdev <- if ("stdev" %in% names(P)) P$stdev else NULL
  t0 <- Sys.time()

  for (k in seq_len(n)) {
    obj <- if (k == 1 && first_lineup_optimal) P$proj else .perturb(P$proj, randomness, stdev)
    capped <- which(counts >= cap)
    bans <- lapply(capped, function(i) list(cols = i, vals = 1, dir = "<=", rhs = 0))
    sol <- .solve_model(model, obj, c(cuts, bans), solver)
    if (!sol$ok) {
      warning(sprintf("Stopped after %d of %d lineups: no feasible lineup left under the current ",
                      k - 1L, n),
              "constraints (try a lower min_unique, higher max_exposure, or looser stacking).",
              call. = FALSE)
      break
    }
    idx <- which(sol$x[seq_len(nrow(P))] > 0.5)
    if (length(idx) != cfg$n_players) stop("Solver returned a lineup of the wrong size.", call. = FALSE)
    lineups[[k]] <- idx
    counts[idx]  <- counts[idx] + 1L
    cuts[[k]] <- list(cols = idx, vals = rep(1, length(idx)), dir = "<=",
                      rhs = cfg$n_players - min_unique)
    if (verbose && k %% 25 == 0) cat(sprintf("  ... %d/%d lineups (%.1fs)\n", k, n,
                                             as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }

  structure(list(
    lineups  = lineup_table(P, lineups, cfg, stack_positions, bringback_positions),
    ids      = lineup_ids(P, lineups, cfg),
    exposure = exposure_table(P, lineups),
    idx      = lineups,
    players  = P,
    cfg      = cfg,
    opts     = c(opts, list(min_unique = min_unique, randomness = randomness,
                            max_exposure = max_exposure, n_requested = n, solver = solver)),
    seconds  = as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ), class = "dfs_lineups")
}
