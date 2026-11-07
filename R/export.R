# Turning solver output into readable tables and upload files.

# Put a lineup's 9 players into roster-slot order:
# QB, RB1, RB2, WR1, WR2, WR3, TE, FLEX, DST. Within a position the pricier
# player comes first; whoever is left over (3rd RB / 4th WR / 2nd TE) is FLEX.
order_slots <- function(P, idx) {
  take <- function(pos) {
    ii <- idx[P$position[idx] == pos]
    ii[order(-P$salary[ii], -P$proj[ii])]
  }
  qb <- take("QB"); rb <- take("RB"); wr <- take("WR"); te <- take("TE"); dst <- take("DST")
  flex <- c(rb[-(1:2)], wr[-(1:3)], te[-1])
  c(qb, rb[1:2], wr[1:3], te[1], flex, dst)
}

lineup_table <- function(P, idx_list, cfg, stack_positions = c("WR", "TE"),
                         bringback_positions = c("WR", "TE")) {
  if (!length(idx_list)) return(data.frame())
  rows <- lapply(seq_along(idx_list), function(k) {
    s  <- order_slots(P, idx_list[[k]])
    qb <- s[1]
    data.frame(
      lineup = k,
      as.list(stats::setNames(P$name[s], cfg$slots)),
      salary    = sum(P$salary[s]),
      proj      = round(sum(P$proj[s]), 2),
      qb_stack  = sum(P$team[s] == P$team[qb] & P$position[s] %in% stack_positions),
      bringback = sum(P$team[s] == P$opponent[qb] & P$position[s] %in% bringback_positions),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

lineup_ids <- function(P, idx_list, cfg) {
  if (!length(idx_list)) return(data.frame())
  mat <- t(vapply(idx_list, function(idx) P$id[order_slots(P, idx)], character(cfg$n_players)))
  out <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(out) <- cfg$slots
  out
}

exposure_table <- function(P, idx_list) {
  if (!length(idx_list)) return(data.frame())
  cnt <- table(unlist(idx_list))
  i <- as.integer(names(cnt))
  out <- data.frame(name = P$name[i], position = P$position[i], team = P$team[i],
                    salary = P$salary[i], proj = P$proj[i],
                    lineups = as.integer(cnt), exposure = round(as.integer(cnt) / length(idx_list), 3),
                    stringsAsFactors = FALSE)
  out[order(-out$lineups, -out$proj), , drop = FALSE]
}

#' Write lineups to CSV.
#' what = "ids":   one row per lineup, 9 columns of player ids under the site's
#'                 upload header. Paste these into the site's entry template
#'                 (see README: check the layout against your own template).
#' what = "names": human-readable names plus salary/projection summary.
write_lineups_csv <- function(res, path, what = c("ids", "names")) {
  what <- match.arg(what)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (what == "ids") {
    out <- res$ids
    names(out) <- res$cfg$upload_header
  } else {
    out <- res$lineups
  }
  utils::write.csv(out, path, row.names = FALSE)
  invisible(path)
}

print.dfs_lineups <- function(x, n = 5, ...) {
  k <- nrow(x$lineups)
  cat(sprintf("%d %s lineups (requested %d) | %s solver | %.1fs\n",
              k, x$cfg$site, x$opts$n_requested, x$opts$solver, x$seconds))
  if (k) {
    cat(sprintf("Projection: %.1f to %.1f | avg salary $%s of $%s\n",
                min(x$lineups$proj), max(x$lineups$proj),
                format(round(mean(x$lineups$salary)), big.mark = ","),
                format(x$cfg$cap, big.mark = ",")))
    cat(sprintf("\nFirst %d lineups:\n", min(n, k)))
    print(utils::head(x$lineups, n), row.names = FALSE)
    cat("\nMost-used players:\n")
    print(utils::head(x$exposure, 8), row.names = FALSE)
  }
  invisible(x)
}
