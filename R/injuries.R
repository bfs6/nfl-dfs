# Injury layer: from raw reports to "how likely is this player to play, and how
# sure are we?".
#
# Sources, combined per player:
#   1. Official NFL injury report (nflverse): game status + final practice status.
#   2. FanDuel's injury indicator, if you loaded a FanDuel player list.
#   3. YOUR overrides (data/overrides.csv): late news, inactive lists, beat reporters.
# Rules (kept simple on purpose):
#   - An override always wins.
#   - Otherwise the WORST status across sources wins (Out > Doubtful > Questionable > Active);
#     including an Out player costs a whole roster spot, missing a healthy one costs little.
#   - Chance of playing for Questionable/Doubtful comes from history: how often players with
#     that exact designation + final practice status actually played (estimate_play_rates()).
# Official reports update a few times a week and final inactives come ~90 minutes before
# kickoff, so run this again close to lock and add what you've heard to the overrides file.

STATUS_LEVELS <- c("Active", "Questionable", "Doubtful", "Out")

normalize_status <- function(x) {
  x <- toupper(trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  out[is.na(x) | x %in% c("", "NA", "ACTIVE", "HEALTHY", "P", "PROBABLE", "CLEARED", "FULL")] <- "Active"
  out[x %in% c("Q", "QUESTIONABLE", "GTD")] <- "Questionable"
  out[x %in% c("D", "DOUBTFUL")] <- "Doubtful"
  out[x %in% c("O", "OUT", "IR", "INACTIVE", "PUP", "PUP-R", "NFI", "NFI-R", "SUS", "SUSPENDED", "RESERVE")] <- "Out"
  out
}

.severity <- function(s) match(s, STATUS_LEVELS)          # NA stays NA

.practice_class <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", "NoReport",
         ifelse(grepl("Full", x), "Full", ifelse(grepl("Limited", x), "Limited",
                ifelse(grepl("Did Not", x), "DNP", "NoReport"))))
}

# ---- historical play rates ---------------------------------------------------

#' How often did players with a given designation + final practice status actually
#' play? "Played" = has a stats row that week. Only players who produced stats in
#' the previous five weeks are counted, so healthy scratches don't dilute the rates.
estimate_play_rates <- function(inj, ps) {
  d <- inj[inj$position %in% c(POS_OFF, "FB") & !is.na(inj$gsis_id), , drop = FALSE]
  d$widx <- wk_index(d$season, d$week)
  have <- paste(ps$player_id, wk_index(ps$season, ps$week))
  d$played <- paste(d$gsis_id, d$widx) %in% have
  recent <- Reduce(`|`, lapply(1:5, function(j) paste(d$gsis_id, d$widx - j) %in% have))
  d <- d[recent, , drop = FALSE]
  d$status   <- ifelse(d$report_status %in% c("Out", "Doubtful", "Questionable"), d$report_status, "None")
  d$practice <- .practice_class(d$practice_status)
  cell <- stats::aggregate(played ~ status + practice, data = d, FUN = function(v) c(n = length(v), k = sum(v)))
  cell <- data.frame(cell[1:2], n = cell$played[, "n"], played = cell$played[, "k"])
  cell$rate <- (cell$played + 0.5) / (cell$n + 1)
  st <- stats::aggregate(played ~ status, data = d, FUN = function(v) c(n = length(v), k = sum(v)))
  st <- data.frame(status = st$status, n = st$played[, "n"], played = st$played[, "k"])
  st$rate <- (st$played + 0.5) / (st$n + 1)
  list(cell = cell, status = st)
}

#' Look up P(play). Uses the status+practice cell when it has >= 25 cases, else the
#' status-level rate.
lookup_play_prob <- function(status, practice, rates, min_n = 25) {
  stat_key <- ifelse(status == "Active", "None", status)
  out <- rates$status$rate[match(stat_key, rates$status$status)]
  i <- match(paste(stat_key, practice), paste(rates$cell$status, rates$cell$practice))
  use <- !is.na(i) & rates$cell$n[i] >= min_n
  out[use] <- rates$cell$rate[i[use]]
  out[status == "Out"] <- 0
  out[is.na(out)] <- 0.99
  out
}

# ---- overrides ----------------------------------------------------------------

#' Read your late-news file. Columns: name (or id), status, optional p_play (0-1),
#' optional team (to disambiguate), optional note.
#' Status: Out / Doubtful / Questionable / Active (aliases: IR, inactive, GTD, probable ...).
read_overrides <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  ov <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(ov) <- tolower(trimws(names(ov)))
  if (!nrow(ov)) return(NULL)
  if (!"status" %in% names(ov) || !any(c("name", "id") %in% names(ov))) {
    stop("Overrides need a 'status' column and a 'name' or 'id' column.", call. = FALSE)
  }
  ov$status_raw <- ov$status
  ov$status <- normalize_status(ov$status)
  if (anyNA(ov$status)) stop("Unrecognised status in overrides: ", paste(unique(ov$status_raw[is.na(ov$status)]), collapse = ", "), call. = FALSE)
  for (cn in c("p_play", "note", "team", "name", "id")) if (!cn %in% names(ov)) ov[[cn]] <- NA
  ov$p_play <- suppressWarnings(as.numeric(ov$p_play))
  ov
}

# Attach each override to a row of the slate (by id, else name, narrowed by team if given).
resolve_overrides <- function(ov, slate) {
  if (is.null(ov) || !nrow(ov)) return(NULL)
  ov$row <- vapply(seq_len(nrow(ov)), function(r) {
    h <- integer(0)
    if (!is.na(ov$id[r]) && nzchar(ov$id[r])) h <- which(slate$id == as.character(ov$id[r]))
    if (!length(h) && !is.na(ov$name[r])) {
      h <- which(.name_key(slate$name) == .name_key(ov$name[r]) & slate$position != "DST")
      if (length(h) > 1 && !is.na(ov$team[r])) h <- h[slate$team[h] == normalize_team(ov$team[r])]
    }
    if (length(h) == 1) h else NA_integer_
  }, integer(1))
  if (anyNA(ov$row)) {
    warning("Override(s) not matched to any slate player (or ambiguous; add a team or id column): ",
            paste(ov$name[is.na(ov$row)], collapse = ", "), call. = FALSE)
  }
  ov[!is.na(ov$row), , drop = FALSE]
}

# Make the injury table reflect overrides, so teammates' projections react too
# (e.g. a WR1 ruled Out late raises the other receivers).
apply_overrides_to_inj <- function(inj, ov, slate, season, week) {
  if (is.null(ov) || !nrow(ov)) return(inj)
  s <- slate[ov$row, , drop = FALSE]
  ok <- !is.na(s$pid)
  if (!any(ok)) return(inj)
  s <- s[ok, , drop = FALSE]; o <- ov[ok, , drop = FALSE]
  inj <- inj[!(inj$gsis_id %in% s$pid & inj$season == season & inj$week == week), , drop = FALSE]
  new <- inj[rep(NA_integer_, nrow(s)), , drop = FALSE]
  rownames(new) <- NULL
  set <- function(col, val) if (col %in% names(new)) new[[col]] <<- val
  set("season", season); set("week", week); set("season_type", "REG"); set("game_type", "REG")
  set("team", s$team); set("gsis_id", s$pid); set("position", s$position)
  set("full_name", s$name); set("report_status", ifelse(o$status == "Active", "", o$status))
  rbind(inj, new)
}

# ---- the per-player status table ----------------------------------------------

#' @param slate must have pid, name, position, team (and optionally `fd_injury` = FanDuel indicator)
injury_status <- function(slate, season, week, inj, ov, rates) {
  n <- nrow(slate)
  cur <- inj[inj$season == season & inj$week == week & !is.na(inj$gsis_id), , drop = FALSE]
  cur <- cur[!duplicated(cur$gsis_id, fromLast = TRUE), , drop = FALSE]
  j <- match(slate$pid, cur$gsis_id)
  on_report <- !is.na(j)

  official <- ifelse(on_report, normalize_status(cur$report_status[j]), NA_character_)
  practice <- ifelse(on_report, .practice_class(cur$practice_status[j]), "NoReport")
  injury   <- ifelse(on_report, trimws(paste(cur$report_primary_injury[j])), NA_character_)
  injury[!is.na(injury) & (injury == "" | injury == "NA")] <- NA
  fd <- rep(NA_character_, n)
  if ("fd_injury" %in% names(slate)) {
    has <- !is.na(slate$fd_injury) & slate$fd_injury != ""          # no FD flag -> no opinion
    fd[has] <- normalize_status(slate$fd_injury[has])
  }

  worst <- pmax(.severity(official), .severity(fd), na.rm = TRUE)
  worst[is.infinite(worst)] <- NA
  status <- ifelse(is.na(worst), "Active", STATUS_LEVELS[worst])
  src <- ifelse(is.na(official) & is.na(fd), "no report",
                ifelse(!is.na(official) & !is.na(fd), "NFL report + FanDuel", ifelse(!is.na(official), "NFL report", "FanDuel")))

  p <- lookup_play_prob(status, practice, rates)
  p[!on_report & status == "Active"] <- 0.99
  disagree <- !is.na(official) & !is.na(fd) & official != fd

  note <- rep(NA_character_, n)
  flag <- ifelse(disagree, paste0("sources disagree (NFL: ", official, ", FanDuel: ", fd, ")"), NA_character_)

  if (!is.null(ov) && nrow(ov)) {
    r <- ov$row
    status[r] <- ov$status
    p[r] <- ifelse(!is.na(ov$p_play), ov$p_play,
                   ifelse(ov$status == "Out", 0, ifelse(ov$status == "Active", 0.98,
                          lookup_play_prob(ov$status, practice[r], rates))))
    src[r] <- "your override"; note[r] <- ov$note
    flag[r] <- paste0("override", ifelse(!is.na(official[r]) & official[r] != ov$status,
                                         paste0(" (NFL report said ", official[r], ")"), ""))
  }
  data.frame(status = status, practice = practice, injury = injury, p_play = p,
             status_source = src, status_flag = flag, status_note = note, stringsAsFactors = FALSE)
}

# When was the injury data last refreshed?
injury_data_age <- function(season) {
  f <- file.path(.cache_dir(), gsub("[^A-Za-z0-9._-]", "_", sub("^https?://", "",
        sprintf("%s/injuries/injuries_%d.csv", .nfl_base, season))))
  if (!file.exists(f)) return(NA)
  as.numeric(difftime(Sys.time(), file.mtime(f), units = "mins"))
}

# Has the week's official injury report actually been published? The free feed shows practice
# participation early in the week; the Out/Doubtful/Questionable designations arrive Friday (or
# the day before a Thursday game). Compare this week's share of rows that carry a designation
# with a normal week. A low share means designations are NOT in yet, so any "Active" status
# really means "no news yet".
injury_coverage <- function(inj, season, week) {
  has <- !is.na(inj$report_status) & trimws(inj$report_status) != ""
  key <- inj$season * 100 + inj$week
  share <- tapply(has, key, mean)
  cur <- share[as.character(season * 100 + week)]
  n_cur <- sum(key == season * 100 + week)
  prev <- share[names(share) < as.character(season * 100 + week)]
  prev <- utils::tail(prev, 12)
  typical <- if (length(prev) >= 3) stats::median(prev) else NA_real_
  complete <- if (is.na(cur) || is.na(typical)) NA else cur >= 0.6 * typical
  list(rows = n_cur, designated = sum(has[key == season * 100 + week]),
       share = unname(cur), typical_share = typical, complete = unname(complete))
}

injury_coverage_note <- function(cov) {
  if (is.null(cov) || is.na(cov$complete)) return(NULL)
  if (cov$complete) return(NULL)
  sprintf(paste0("WARNING: only %d of %d players on this week's NFL injury report have an Out/Doubtful/Questionable ",
                 "designation (a normal week: ~%.0f%%). The official designations do not appear to be published yet, ",
                 "so 'Active' here means 'no news yet'. Check again after the final report, and use the overrides file."),
          cov$designated, cov$rows, 100 * cov$typical_share)
}

#' Slate-level injury report: everyone who is not clearly healthy, most important first.
#' `proj` is the table returned by project_slate().
injury_report <- function(proj, show_active = FALSE, min_proj = 0) {
  d <- proj[proj$position != "DST", , drop = FALSE]
  d <- d[d$proj_active >= min_proj, , drop = FALSE]
  if ("proj_source" %in% names(d)) d <- d[!(d$proj_source %in% c("actual (game played)", "no game this week")), , drop = FALSE]
  # not just designated players: an 'Active' player who did not practise matters too
  if (!show_active) d <- d[d$status != "Active" | !is.na(d$status_flag) | d$p_play < 0.95, , drop = FALSE]
  d <- d[order(-.severity(d$status), -(d$proj_active * (1 - d$p_play)), -d$proj_active), , drop = FALSE]
  d$note <- trimws(paste(ifelse(is.na(d$status_flag), "", d$status_flag), ifelse(is.na(d$status_note), "", d$status_note)))
  d$src <- ifelse(is.na(d$status_source) | d$status_source == "NFL report", "", d$status_source)
  cols <- c("name", "position", "team", "salary", "status", "practice", "injury", "p_play",
            "proj_active", "proj", "src", "note")
  out <- d[, intersect(cols, names(d)), drop = FALSE]
  if (all(out$src == "")) out$src <- NULL
  out$p_play <- round(out$p_play, 2); out$proj_active <- round(out$proj_active, 1); out$proj <- round(out$proj, 1)
  rownames(out) <- NULL
  age <- attr(proj, "injury_age_min")
  attr(out, "note") <- paste(c(sprintf("Official report data is %s old. Re-run close to lock; final inactives post ~90 min before kickoff.",
                               if (is.null(age) || is.na(age)) "of unknown age" else sprintf("%.0f min", age)),
                               injury_coverage_note(attr(proj, "injury_coverage"))), collapse = "\n")
  class(out) <- c("dfs_injury_report", class(out))
  out
}

print.dfs_injury_report <- function(x, ...) {
  cat(attr(x, "note"), "\n\n")
  if (!nrow(x)) cat("No flagged players.\n") else print(as.data.frame(x), row.names = FALSE)
  invisible(x)
}
