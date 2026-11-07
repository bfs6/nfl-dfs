# Independent rule checker. It does NOT use the optimizer's constraint matrix:
# it re-checks a finished lineup directly against the site rules, so a bug in
# the model can't hide itself. Used by the tests, and handy for checking
# lineups that came from anywhere else.

#' @param L data frame of the lineup's players (rows of the player table)
#' @param cfg site config; @param opts the same strategy options given to optimize_lineups
#' @return character vector of problems (empty = valid)
validate_lineup <- function(L, cfg, opts = list()) {
  o <- utils::modifyList(list(stack_min = 0, stack_positions = c("WR", "TE"),
                              bringback_min = 0, bringback_positions = c("WR", "TE"),
                              no_dst_vs_offense = FALSE, max_per_team = NULL,
                              min_salary = NULL), opts)
  bad <- character(0)
  flag <- function(msg) bad <<- c(bad, msg)

  if (nrow(L) != cfg$n_players) flag(sprintf("has %d players, needs %d", nrow(L), cfg$n_players))
  if (anyDuplicated(L$id)) flag("duplicate player")
  if (sum(L$salary) > cfg$cap) flag(sprintf("salary %d over cap %d", sum(L$salary), cfg$cap))
  if (!is.null(o$min_salary) && sum(L$salary) < o$min_salary) flag("under salary floor")

  for (pos in names(cfg$pos_min)) {
    c_ <- sum(L$position == pos)
    if (c_ < cfg$pos_min[[pos]] || c_ > cfg$pos_max[[pos]]) flag(sprintf("%d %s out of range", c_, pos))
  }
  if (any(!L$position %in% names(cfg$pos_min))) flag("ineligible position")

  mpt <- if (!is.null(o$max_per_team)) o$max_per_team else cfg$max_per_team
  if (!is.na(mpt) && any(table(L$team) > mpt)) flag("too many players from one team")
  if (length(unique(L$team)) < cfg$min_teams) flag("not enough distinct teams")
  if (length(unique(L$game)) < cfg$min_games) flag("not enough distinct games")

  qb <- L[L$position == "QB", ]
  if (nrow(qb) == 1) {
    if (o$stack_min > 0 &&
        sum(L$team == qb$team & L$position %in% o$stack_positions) < o$stack_min) flag("QB stack too small")
    if (o$bringback_min > 0 &&
        sum(L$team == qb$opponent & L$position %in% o$bringback_positions) < o$bringback_min) flag("no bring-back")
  }
  if (o$no_dst_vs_offense) {
    dst <- L[L$position == "DST", ]
    if (nrow(dst) == 1 &&
        any(L$position %in% c("QB", "RB", "WR", "TE") & L$team == dst$opponent)) flag("defense faces own offense")
  }
  bad
}

# Validate every lineup in an optimize_lineups() result. Returns a data frame of
# failures (zero rows = all good).
validate_result <- function(res) {
  fails <- list()
  for (k in seq_along(res$idx)) {
    pr <- validate_lineup(res$players[res$idx[[k]], , drop = FALSE], res$cfg, res$opts)
    if (length(pr)) fails[[length(fails) + 1L]] <- data.frame(lineup = k, problem = pr)
  }
  if (length(fails)) do.call(rbind, fails) else data.frame(lineup = integer(0), problem = character(0))
}
