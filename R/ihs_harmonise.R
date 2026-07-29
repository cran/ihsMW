#' Harmonise Raw IHS Data
#'
#' Takes a raw data.frame loaded from a Malawi IHS survey round (e.g. from a
#' `.dta` or `.csv` file) and renames its columns to the standard harmonised
#' variable names defined in the bundled crosswalk. This is the first step of
#' almost every `ihsMW` workflow: once column names are harmonised, the same
#' downstream code works on any round.
#'
#' Matching is case-insensitive, so `HHID` and `hhid` are treated as the same
#' variable. Each raw column is claimed by at most one crosswalk entry.
#'
#' @param data A data.frame, typically read from a `.dta` file using
#'   \code{haven::read_dta} or a `.csv` using \code{readr::read_csv}.
#' @param round A character string specifying the IHS round: one of
#'   \code{"IHS2"}, \code{"IHS3"}, \code{"IHS4"}, \code{"IHS5"} or
#'   \code{"IHS6"}.
#' @param extra Logical. If FALSE (default), drops columns that are not in the harmonisation
#' crosswalk or standard ID columns. If TRUE, keeps all original columns.
#'
#' @return A data.frame with columns renamed to standard `harmonised_name`s
#'   where applicable, plus an `ihs_round` column recording which round the
#'   rows came from. Stata variable labels are preserved across the rename.
#'
#' @seealso \code{\link{ihs_search}} to find out what a raw variable is called
#'   in each round before harmonising.
#'
#' @examples
#' # A miniature stand-in for a raw IHS file
#' raw <- data.frame(case_id = c("a", "b"), hhsize = c(4, 6), junk = c(1, 2))
#'
#' # Harmonise, dropping columns absent from the crosswalk
#' ihs_harmonise(raw, round = "IHS6")
#'
#' # Keep everything, including unmapped columns
#' ihs_harmonise(raw, round = "IHS6", extra = TRUE)
#' @export
ihs_harmonise <- function(data, round = "IHS5", extra = FALSE) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame")
  }
  
  round <- check_round(round)
  if (length(round) > 1) {
    cli::cli_abort("{.arg round} must be a single string for ihs_harmonise.")
  }
  
  # Load the crosswalk
  cw <- .load_crosswalk()
  
  # Find the relevant column name in the crosswalk for this round (e.g., "ihs5_name")
  col_name <- paste0(tolower(round), "_name")
  if (!col_name %in% names(cw)) {
    cli::cli_abort("Round {.val {round}} is not fully supported in the crosswalk.")
  }
  
  # Filter crosswalk to variables that exist in this round
  cw_round <- cw[!is.na(cw[[col_name]]), ]
  
  # Build a mapping of raw_name -> harmonised_name
  raw_names_lower <- tolower(names(data))
  
  mapped <- 0
  id_cols <- c("case_id", "hhid", "hh_id", "ea_id", "stratum", "strata",
               "weight", "panelweight")
  keep_cols <- c()

  # Each raw column may be claimed by at most one crosswalk entry. Without this
  # guard, two crosswalk rows whose round names differ only in case would both
  # match the same column: the second rename would silently overwrite the first,
  # and the now-dangling first name would later fail the `keep_cols` subset.
  consumed <- rep(FALSE, length(data))

  # Process labels if present
  labels <- list()
  for (i in seq_along(data)) {
    if (!is.null(attr(data[[i]], "label"))) {
      labels[[names(data)[i]]] <- attr(data[[i]], "label")
    }
  }

  for (i in seq_len(nrow(cw_round))) {
    orig_name <- cw_round[[col_name]][i]
    ind_name <- cw_round$harmonised_name[i]

    match_idx <- which(raw_names_lower == tolower(orig_name) & !consumed)

    if (length(match_idx) > 0) {
      idx <- match_idx[1]
      names(data)[idx] <- ind_name
      consumed[idx] <- TRUE
      keep_cols <- c(keep_cols, ind_name)
      mapped <- mapped + 1

      # Retain label if available
      orig_actual_name <- names(labels)[tolower(names(labels)) == tolower(orig_name)]
      if (length(orig_actual_name) > 0) {
        attr(data[[ind_name]], "label") <- labels[[orig_actual_name[1]]]
      }
    }
  }

  # Belt and braces: never try to keep a column that no longer exists.
  keep_cols <- intersect(keep_cols, names(data))

  if (mapped == 0) {
    cli::cli_warn("No columns were mapped to harmonised names. Are you sure this is an {round} dataset?")
  } else {
    cli::cli_inform("Harmonised {mapped} column{?s} for {round}.")
  }
  
  # Filter if requested
  if (!extra) {
    keep_ids <- names(data)[tolower(names(data)) %in% id_cols]
    keep_final <- unique(c(keep_ids, keep_cols))
    data <- data[, keep_final, drop = FALSE]
  }
  
  data$ihs_round <- round
  
  data
}
