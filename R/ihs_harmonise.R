#' Harmonise Raw IHS Data
#'
#' Takes a raw data.frame loaded from a Malawi IHS survey round (e.g. from a `.dta` file) 
#' and renames its columns to the standard harmonised variable names defined in the crosswalk.
#'
#' @param data A data.frame, typically read from a `.dta` file using \code{haven::read_dta}.
#' @param round A character string specifying the IHS round (e.g., \code{"IHS5"}, \code{"IHS4"}).
#' @param extra Logical. If FALSE (default), drops columns that are not in the harmonisation 
#' crosswalk or standard ID columns. If TRUE, keeps all original columns.
#'
#' @return A data.frame with columns renamed to standard `harmonised_name`s where applicable.
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
  id_cols <- c("case_id", "hhid", "hh_id", "ea_id", "stratum", "weight", "panelweight")
  keep_cols <- c()
  
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
    
    match_idx <- which(raw_names_lower == tolower(orig_name))
    
    if (length(match_idx) > 0) {
      names(data)[match_idx[1]] <- ind_name
      keep_cols <- c(keep_cols, ind_name)
      mapped <- mapped + 1
      
      # Retain label if available
      orig_actual_name <- names(labels)[tolower(names(labels)) == tolower(orig_name)]
      if (length(orig_actual_name) > 0) {
        attr(data[[ind_name]], "label") <- labels[[orig_actual_name[1]]]
      }
    }
  }
  
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
