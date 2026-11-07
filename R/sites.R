# Site rules for NFL "Classic" (full-slate) contests.
#
# Confirmed against public rule summaries in Nov 2026:
#   DraftKings: $50,000 cap, 9 players (QB, 2 RB, 3 WR, TE, FLEX, DST),
#               players from at least 2 teams AND at least 2 different games.
#   FanDuel:    $60,000 cap, 9 players (QB, 2 RB, 3 WR, TE, FLEX, DEF),
#               at most 4 players from one team, at least 3 teams.
# Re-check at the start of each season; sites occasionally tweak rules.
#
# FLEX (RB/WR/TE) is modelled with position *ranges*: with 9 players total, the
# minimums below add up to 8, so exactly one extra RB, WR or TE fills the FLEX.
#
# Internally the defense position is always called "DST" (FanDuel's "D"/"DEF"
# is renamed on the way in and back to "DEF" on the way out).

site_config <- function(site = c("draftkings", "fanduel")) {
  site <- match.arg(site)

  common <- list(
    n_players = 9L,
    pos_min   = c(QB = 1L, RB = 2L, WR = 3L, TE = 1L, DST = 1L),
    pos_max   = c(QB = 1L, RB = 3L, WR = 4L, TE = 2L, DST = 1L),
    # Order lineups are written out in (matches both sites' upload layouts)
    slots     = c("QB", "RB1", "RB2", "WR1", "WR2", "WR3", "TE", "FLEX", "DST")
  )

  switch(site,
    draftkings = c(list(
      site          = "draftkings",
      cap           = 50000,
      max_per_team  = NA_integer_,     # no per-team limit
      min_teams     = 2L,
      min_games     = 2L,
      upload_header = c("QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX", "DST")
    ), common),

    fanduel = c(list(
      site          = "fanduel",
      cap           = 60000,
      max_per_team  = 4L,
      min_teams     = 3L,
      min_games     = 1L,
      upload_header = c("QB", "RB", "RB", "WR", "WR", "WR", "TE", "FLEX", "DEF")
    ), common)
  )
}
