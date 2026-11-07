# Free data from the nflverse project, read with base R only (no nflreadr needed).
# Files are downloaded once and cached; the current season's files are refreshed
# when they get old. Set options(dfs.cache_dir = "path") to move the cache.

.nfl_base <- "https://github.com/nflverse/nflverse-data/releases/download"

.cache_dir <- function() {
  d <- getOption("dfs.cache_dir", "cache")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

# Download `url` into the cache unless a fresh copy is there.
nfl_fetch <- function(url, max_age_hours = Inf, verbose = FALSE) {
  dest <- file.path(.cache_dir(), gsub("[^A-Za-z0-9._-]", "_", sub("^https?://", "", url)))
  fresh <- file.exists(dest) &&
    as.numeric(difftime(Sys.time(), file.mtime(dest), units = "hours")) < max_age_hours
  if (!fresh) {
    if (verbose) message("Downloading ", url)
    tmp <- paste0(dest, ".part")
    ok <- tryCatch({
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
      file.size(tmp) > 0
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) file.rename(tmp, dest)
    else {
      unlink(tmp)
      if (!file.exists(dest)) stop("Could not download ", url, call. = FALSE)
      warning("Download failed; using older cached copy of ", basename(dest), call. = FALSE)
    }
  }
  dest
}

.current_season <- function() {
  d <- Sys.Date(); y <- as.integer(format(d, "%Y"))
  if (as.integer(format(d, "%m")) >= 3) y else y - 1L
}

# Past seasons never change; the current one refreshes every few hours.
.age_for <- function(season, hours = 6) if (season >= .current_season()) hours else Inf

.read_seasons <- function(tag, pattern, seasons, hours = 6, keep = NULL) {
  out <- lapply(seasons, function(s) {
    f <- nfl_fetch(sprintf("%s/%s/%s", .nfl_base, tag, sprintf(pattern, s)), .age_for(s, hours))
    d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    if (!is.null(keep)) d <- d[, intersect(keep, names(d)), drop = FALSE]
    d
  })
  all_cols <- unique(unlist(lapply(out, names)))
  out <- lapply(out, function(d) { for (m in setdiff(all_cols, names(d))) d[[m]] <- NA; d[, all_cols, drop = FALSE] })
  do.call(rbind, out)
}

load_nfl_player_stats <- function(seasons) {
  keep <- c("player_id", "player_display_name", "position", "season", "week", "season_type", "team",
            "opponent_team", "completions", "attempts", "passing_yards", "passing_tds", "passing_interceptions",
            "sacks_suffered", "sack_fumbles_lost", "passing_2pt_conversions", "carries", "rushing_yards",
            "rushing_tds", "rushing_fumbles_lost", "rushing_2pt_conversions", "receptions", "targets",
            "receiving_yards", "receiving_tds", "receiving_fumbles_lost", "receiving_2pt_conversions",
            "special_teams_tds", "fumble_recovery_tds")
  d <- .read_seasons("stats_player", "stats_player_week_%d.csv", seasons, keep = keep)
  d[d$season_type == "REG", , drop = FALSE]
}

load_nfl_team_stats <- function(seasons) {
  d <- .read_seasons("stats_team", "stats_team_week_%d.csv", seasons)
  d[d$season_type == "REG", , drop = FALSE]
}

load_nfl_injuries <- function(seasons) {
  d <- .read_seasons("injuries", "injuries_%d.csv", seasons, hours = 1)
  d[d$game_type == "REG" | is.na(d$game_type), , drop = FALSE]
}

load_nfl_rosters <- function(seasons) {
  .read_seasons("weekly_rosters", "roster_weekly_%d.csv", seasons, hours = 12,
                keep = c("season", "week", "team", "position", "full_name", "gsis_id", "status", "game_type"))
}

# All seasons in one file, includes spread_line / total_line / scores / rest / roof.
load_nfl_games <- function() {
  f <- nfl_fetch(sprintf("%s/schedules/games.csv", .nfl_base), 3)
  d <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  d[d$game_type == "REG", , drop = FALSE]
}

# The next week that still has unplayed games in the current season.
upcoming_week <- function(games = load_nfl_games()) {
  s <- max(games$season)
  g <- games[games$season == s & is.na(games$home_score), ]
  list(season = s, week = if (nrow(g)) min(g$week) else max(games$week[games$season == s]))
}
