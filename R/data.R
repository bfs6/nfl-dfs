# Getting player data into one standard shape.
#
# Standard columns used everywhere downstream:
#   id, name, position (QB/RB/WR/TE/DST), team, opponent, salary, proj
# Optional columns that are honoured if present:
#   game      game key (built from team/opponent if missing)
#   stdev     projection std-dev; scales the noise in optimize_lineups()
#   max_exp   per-player max exposure, 0-1 (overrides the global max_exposure)
# Any other columns (ownership, etc.) are carried along untouched.

# ---- small helpers ---------------------------------------------------------

.col_key <- function(s) gsub("[^a-z0-9]", "", tolower(s))

# Find a column by loose name matching ("Game Info" == "game_info" == "GAMEINFO").
.pick <- function(df, candidates, required = TRUE) {
  hit <- match(.col_key(candidates), .col_key(names(df)))
  hit <- hit[!is.na(hit)]
  if (!length(hit)) {
    if (required) stop("Couldn't find a column like: ",
                       paste(candidates, collapse = " / "), call. = FALSE)
    return(NULL)
  }
  df[[hit[1]]]
}

normalize_team <- function(x) {
  x <- toupper(trimws(as.character(x)))
  map <- c(JAC = "JAX", WSH = "WAS", ARZ = "ARI", LA = "LAR", STL = "LAR",
           SD = "LAC", SDG = "LAC", OAK = "LV", LVR = "LV", GBP = "GB",
           KCC = "KC", NEP = "NE", NOS = "NO", SFO = "SF", TBB = "TB")
  hit <- x %in% names(map)
  x[hit] <- unname(map[x[hit]])
  x
}

normalize_position <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x[x %in% c("D", "DEF", "D/ST", "DST", "DEFENSE", "D-ST")] <- "DST"
  x[x == "PK"] <- "K"
  x
}

# "Mike Smith Jr." -> "mikesmith"  (for matching names across sources)
.name_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9 ]", "", x)
  x <- gsub("\\s+(jr|sr|ii|iii|iv)$", "", trimws(x))
  gsub("\\s+", "", x)
}

# ---- the standard player table ---------------------------------------------

# Validates, cleans and standardises a player data frame. Safe to call twice.
prepare_players <- function(df, cfg = NULL, min_proj = 0) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- tolower(names(df))
  req  <- c("name", "position", "team", "opponent", "salary", "proj")
  miss <- setdiff(req, names(df))
  if (length(miss)) {
    stop("Player data is missing column(s): ", paste(miss, collapse = ", "),
         "\nExpected at least: ", paste(c("id", req), collapse = ", "), call. = FALSE)
  }
  if (!"id" %in% names(df)) df$id <- seq_len(nrow(df))

  df$id       <- as.character(df$id)
  df$name     <- trimws(as.character(df$name))
  df$position <- normalize_position(df$position)
  df$team     <- normalize_team(df$team)
  df$opponent <- normalize_team(df$opponent)
  df$salary   <- suppressWarnings(as.numeric(df$salary))
  df$proj     <- suppressWarnings(as.numeric(df$proj))
  if (!"game" %in% names(df)) {
    df$game <- ifelse(is.na(df$team) | is.na(df$opponent), NA_character_,
                      paste(pmin(df$team, df$opponent), pmax(df$team, df$opponent), sep = "@"))
  }

  keep <- !is.na(df$salary) & !is.na(df$proj) & df$proj > min_proj &
          !is.na(df$team) & !is.na(df$opponent)
  if (!is.null(cfg)) keep <- keep & df$position %in% names(cfg$pos_min)
  df <- df[keep, , drop = FALSE]
  rownames(df) <- NULL

  if (anyDuplicated(df$id)) {
    stop("Duplicate player ids: ",
         paste(utils::head(unique(df$id[duplicated(df$id)]), 5), collapse = ", "), call. = FALSE)
  }
  df
}

# Resolve a user-supplied vector of ids and/or names to row numbers.
resolve_players <- function(P, keys, what = "player") {
  if (is.null(keys) || !length(keys)) return(integer(0))
  keys <- as.character(keys)
  idx <- vapply(keys, function(k) {
    h <- which(P$id == k)
    if (!length(h)) h <- which(tolower(P$name) == tolower(k))
    if (length(h) != 1L) NA_integer_ else h
  }, integer(1))
  if (anyNA(idx)) {
    stop("Couldn't uniquely match ", what, "(s): ",
         paste(keys[is.na(idx)], collapse = ", "), call. = FALSE)
  }
  unname(idx)
}

# ---- reading real files ----------------------------------------------------
# These loaders are written from the public layout of each site's salary file.
# If a column isn't found you'll get a clear error; adjusting the mapping is a
# few lines. Salary files carry no projections; use merge_projections() next.

# DraftKings "DKSalaries.csv" (Position, Name + ID, Name, ID, Roster Position,
# Salary, Game Info, TeamAbbrev, AvgPointsPerGame)
load_dk_salaries <- function(path) {
  raw  <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  gi   <- as.character(.pick(raw, "Game Info"))
  ok   <- grepl("^[A-Za-z]+@[A-Za-z]+", gi)
  away <- ifelse(ok, sub("^([A-Za-z]+)@([A-Za-z]+).*$", "\\1", gi), NA_character_)
  home <- ifelse(ok, sub("^([A-Za-z]+)@([A-Za-z]+).*$", "\\2", gi), NA_character_)
  team <- normalize_team(.pick(raw, c("TeamAbbrev", "TeamAbbr", "Team")))
  away <- normalize_team(away); home <- normalize_team(home)
  data.frame(
    id        = as.character(.pick(raw, "ID")),
    name      = trimws(as.character(.pick(raw, "Name"))),
    position  = normalize_position(.pick(raw, "Position")),
    team      = team,
    opponent  = ifelse(team == away, home, away),
    salary    = as.numeric(.pick(raw, "Salary")),
    stringsAsFactors = FALSE
  )
}

# FanDuel player-list CSV (Id, Position, First Name, Nickname, Last Name, FPPG,
# Played, Salary, Game, Team, Opponent, Injury Indicator, ...)
load_fd_players <- function(path, drop_out = TRUE) {
  raw   <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  nick  <- .pick(raw, "Nickname", required = FALSE)
  first <- .pick(raw, "First Name", required = FALSE)
  last  <- .pick(raw, "Last Name", required = FALSE)
  name  <- if (!is.null(nick)) nick else paste(first, last)
  out <- data.frame(
    id       = as.character(.pick(raw, "Id")),
    name     = trimws(as.character(name)),
    position = normalize_position(.pick(raw, "Position")),
    team     = normalize_team(.pick(raw, "Team")),
    opponent = normalize_team(.pick(raw, "Opponent")),
    salary   = as.numeric(.pick(raw, "Salary")),
    stringsAsFactors = FALSE
  )
  inj <- .pick(raw, "Injury Indicator", required = FALSE)
  det <- .pick(raw, "Injury Details", required = FALSE)
  out$injury      <- if (is.null(inj)) NA_character_ else toupper(trimws(as.character(inj)))
  out$injury_note <- if (is.null(det)) NA_character_ else trimws(as.character(det))
  if (drop_out) out <- out[!(out$injury %in% c("O", "IR")), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Projections CSV: needs name, position, proj. Add team for defenses (they are
# matched by team, not name). Optional: stdev, max_exp, own, ...
read_projections <- function(path) {
  pr <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(pr) <- tolower(trimws(names(pr)))
  pr
}

# Attach projections to a salary file. Offensive players match on normalised
# name + position; defenses match on team.
merge_projections <- function(slate, projections, verbose = TRUE) {
  pr <- as.data.frame(projections, stringsAsFactors = FALSE)
  names(pr) <- tolower(names(pr))
  miss <- setdiff(c("name", "position", "proj"), names(pr))
  if (length(miss)) stop("Projections are missing column(s): ", paste(miss, collapse = ", "), call. = FALSE)

  pr$position <- normalize_position(pr$position)
  pr$team <- if ("team" %in% names(pr)) normalize_team(pr$team) else NA_character_
  dst_team <- ifelse(pr$position == "DST" & is.na(pr$team), normalize_team(pr$name), pr$team)

  make_key <- function(name, pos, team) {
    ifelse(pos == "DST", paste0("DST|", team), paste0(pos, "|", .name_key(name)))
  }
  pr$.key    <- make_key(pr$name, pr$position, dst_team)
  slate$.key <- make_key(slate$name, slate$position, slate$team)

  if (anyDuplicated(pr$.key)) {
    warning("Duplicate projection rows for the same player; keeping the first of each.", call. = FALSE)
    pr <- pr[!duplicated(pr$.key), , drop = FALSE]
  }

  m <- match(slate$.key, pr$.key)
  for (col in setdiff(names(pr), c("name", "position", "team", ".key"))) {
    slate[[col]] <- pr[[col]][m]
  }

  if (verbose) {
    miss_rows <- is.na(m) & slate$salary >= 4000
    cat(sprintf("Matched %d of %d salary-file players to projections.\n", sum(!is.na(m)), nrow(slate)))
    if (any(miss_rows)) {
      cat(sprintf("%d players priced $4,000+ had no projection (they will be dropped), e.g.: %s\n",
                  sum(miss_rows),
                  paste(utils::head(slate$name[miss_rows], 8), collapse = ", ")))
    }
  }
  slate$.key <- NULL
  slate
}

# ---- synthetic slate for demos and tests -----------------------------------
# Fake players ("DAL WR2") with salary-linked projections plus noise. This is
# NOT real data; it just has realistic structure so the pipeline can be run.
make_sample_slate <- function(site = c("draftkings", "fanduel"), seed = 1, n_games = 10) {
  site <- match.arg(site)
  stopifnot(n_games >= 2, n_games <= 16)
  set.seed(seed)

  all_teams <- c("ARI","ATL","BAL","BUF","CAR","CHI","CIN","CLE","DAL","DEN","DET","GB",
                 "HOU","IND","JAX","KC","LAC","LAR","LV","MIA","MIN","NE","NO","NYG",
                 "NYJ","PHI","PIT","SEA","SF","TB","TEN","WAS")
  tm   <- sample(all_teams, 2 * n_games)
  away <- tm[seq_len(n_games)]
  home <- tm[n_games + seq_len(n_games)]
  opp  <- stats::setNames(c(home, away), c(away, home))

  spec <- data.frame(
    position = c("QB", "RB","RB","RB", "WR","WR","WR","WR","WR", "TE","TE", "DST"),
    depth    = c(1,     1,   2,   3,    1,   2,   3,   4,   5,    1,   2,    1),
    lo       = c(5000,  5500,4000,3000, 6000,4800,3800,3000,2500, 3500,2500, 2000),
    hi       = c(8200,  9000,6000,4500, 9200,7000,5500,4300,3500, 6500,3200, 3800),
    stringsAsFactors = FALSE
  )
  pts_per_k <- c(QB = 2.6, RB = 2.3, WR = 2.3, TE = 2.1, DST = 2.5)
  scale <- if (site == "fanduel") 1.2 else 1

  rows <- list()
  for (t in names(opp)) {
    boost <- exp(stats::rnorm(1, 0, 0.05))
    for (r in seq_len(nrow(spec))) {
      dk_sal <- round(stats::runif(1, spec$lo[r], spec$hi[r]) / 100) * 100
      sal    <- round(dk_sal * scale / 100) * 100
      proj   <- dk_sal / 1000 * pts_per_k[[spec$position[r]]] * exp(stats::rnorm(1, 0, 0.2)) * boost
      rows[[length(rows) + 1L]] <- data.frame(
        name = sprintf("%s %s%d", t, spec$position[r], spec$depth[r]),
        position = spec$position[r], team = t, opponent = opp[[t]],
        salary = sal, proj = round(proj, 1), stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows)
  out$id <- if (site == "draftkings") as.character(10000000L + seq_len(nrow(out)))
            else sprintf("99999-%06d", seq_len(nrow(out)))
  out[, c("id", "name", "position", "team", "opponent", "salary", "proj")]
}
