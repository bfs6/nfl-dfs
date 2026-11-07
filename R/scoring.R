# Fantasy scoring for NFL Classic contests, applied to nflverse stat lines.
#
#   DraftKings: full PPR, fumble lost -1, +3 bonus at 300 pass / 100 rush / 100 rec yards.
#   FanDuel:    half PPR, fumble lost -2, no yardage bonuses.
# Shared: 0.04/pass yd, 4/pass TD, -1/INT, 0.1/rush-or-rec yd, 6/TD, 2/two-point
# conversion, 6/return TD. Defense tiers are identical on both sites (below).
# (Checked against public scoring summaries, Nov 2026.)

.num <- function(d, col) {
  if (!col %in% names(d)) return(rep(0, nrow(d)))
  x <- suppressWarnings(as.numeric(d[[col]]))
  x[is.na(x)] <- 0
  x
}

site_key <- function(site) {
  if (is.list(site)) site <- site$site
  match.arg(site, c("draftkings", "fanduel"))
}

#' Fantasy points for offensive players (QB/RB/WR/TE) from nflverse weekly columns.
score_offense <- function(d, site) {
  site <- site_key(site)
  dk <- site == "draftkings"
  pass_yd <- .num(d, "passing_yards"); rush_yd <- .num(d, "rushing_yards"); rec_yd <- .num(d, "receiving_yards")
  fum_lost <- .num(d, "sack_fumbles_lost") + .num(d, "rushing_fumbles_lost") + .num(d, "receiving_fumbles_lost")
  two_pt   <- .num(d, "passing_2pt_conversions") + .num(d, "rushing_2pt_conversions") + .num(d, "receiving_2pt_conversions")

  pts <- 0.04 * pass_yd + 4 * .num(d, "passing_tds") - 1 * .num(d, "passing_interceptions") +
         0.1 * rush_yd + 6 * .num(d, "rushing_tds") +
         (if (dk) 1 else 0.5) * .num(d, "receptions") + 0.1 * rec_yd + 6 * .num(d, "receiving_tds") +
         6 * .num(d, "special_teams_tds") + 6 * .num(d, "fumble_recovery_tds") +
         2 * two_pt + (if (dk) -1 else -2) * fum_lost
  if (dk) pts <- pts + 3 * (pass_yd >= 300) + 3 * (rush_yd >= 100) + 3 * (rec_yd >= 100)
  pts
}

#' Points-allowed tiers (same on both sites).
points_allowed_score <- function(pa) {
  ifelse(pa <= 0, 10, ifelse(pa <= 6, 7, ifelse(pa <= 13, 4, ifelse(pa <= 20, 1,
         ifelse(pa <= 27, 0, ifelse(pa <= 34, -1, -4))))))
}

#' Defense/special-teams points from team-week stats.
#' Needs: def_sacks, def_interceptions, fumble_recovery_opp, def_tds, def_safeties,
#' special_teams_tds, blocked kicks, and `points_allowed` (already net of the
#' opponent's own defensive TDs; see build_team_games()).
score_dst <- function(d, site) {
  site_key(site)  # same scoring on both sites; argument kept for symmetry
  blocks <- .num(d, "def_fg_blocks") + .num(d, "def_punt_blocks") + .num(d, "def_pat_blocks")
  1 * .num(d, "def_sacks") + 2 * .num(d, "def_interceptions") + 2 * .num(d, "fumble_recovery_opp") +
    6 * .num(d, "def_tds") + 6 * .num(d, "special_teams_tds") + 2 * .num(d, "def_safeties") +
    2 * blocks + points_allowed_score(.num(d, "points_allowed"))
}
