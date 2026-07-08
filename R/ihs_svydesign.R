#' Create a Survey Design Object for IHS Data
#'
#' Wraps \code{survey::svydesign()} with automatic detection of standard IHS
#' weight, strata, and PSU columns from harmonised data.
#'
#' @param data A data.frame of harmonised IHS data.
#' @param weight_col Character. Column name for survey weights. If \code{NULL}
#'   (default), auto-detected from standard IHS column names.
#' @param strata_col Character. Column name for strata. If \code{NULL}
#'   (default), auto-detected.
#' @param psu_col Character. Column name for PSU/cluster. If \code{NULL}
#'   (default), auto-detected.
#'
#' @return A \code{survey.design2} object from the \pkg{survey} package.
#'
#' @examples
#' \dontrun{
#'   library(survey)
#'   dsgn <- ihs_svydesign(harmonised_data)
#'   survey::svymean(~rexp_cat01, dsgn, na.rm = TRUE)
#' }
#'
#' @export
ihs_svydesign <- function(data, weight_col = NULL, strata_col = NULL,
                           psu_col = NULL) {
  rlang::check_installed("survey", reason = "to create survey design objects.")

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame.")
  }

  data_names <- names(data)
  data_names_lower <- tolower(data_names)

  # Auto-detect weight column
  if (is.null(weight_col)) {
    weight_candidates <- c("hh_wgt", "hhweight", "weight", "panelweight",
                           "hh_wgt_adj", "hhwght")
    weight_col <- .detect_col(data_names, data_names_lower, weight_candidates,
                              "weight")
  } else if (!weight_col %in% data_names) {
    cli::cli_abort("Weight column {.var {weight_col}} not found in data.")
  }

  # Auto-detect strata column
  if (is.null(strata_col)) {
    strata_candidates <- c("stratum", "strata", "strataid", "strat")
    strata_col <- .detect_col(data_names, data_names_lower, strata_candidates,
                              "strata")
  } else if (!strata_col %in% data_names) {
    cli::cli_abort("Strata column {.var {strata_col}} not found in data.")
  }

 # Auto-detect PSU column
  if (is.null(psu_col)) {
    psu_candidates <- c("ea_id", "psu", "cluster", "clusterid")
    psu_col <- .detect_col(data_names, data_names_lower, psu_candidates, "PSU")
  } else if (!psu_col %in% data_names) {
    cli::cli_abort("PSU column {.var {psu_col}} not found in data.")
  }

  if (is.null(weight_col)) {
    cli::cli_abort(c(
      "Could not detect a survey weight column.",
      "i" = "Specify {.arg weight_col} explicitly."
    ))
  }

  # Build the design formula
  if (!is.null(psu_col) && !is.null(strata_col)) {
    cli::cli_inform(c(
      "Creating survey design:",
      "*" = "Weights: {.var {weight_col}}",
      "*" = "Strata: {.var {strata_col}}",
      "*" = "PSU: {.var {psu_col}}"
    ))
    dsgn <- survey::svydesign(
      ids    = stats::reformulate(psu_col),
      strata = stats::reformulate(strata_col),
      weights = stats::reformulate(weight_col),
      data   = data,
      nest   = TRUE
    )
  } else if (!is.null(psu_col)) {
    cli::cli_inform(c(
      "Creating survey design (no strata detected):",
      "*" = "Weights: {.var {weight_col}}",
      "*" = "PSU: {.var {psu_col}}"
    ))
    dsgn <- survey::svydesign(
      ids    = stats::reformulate(psu_col),
      weights = stats::reformulate(weight_col),
      data   = data
    )
  } else {
    cli::cli_inform(c(
      "Creating simple weighted design (no PSU/strata detected):",
      "*" = "Weights: {.var {weight_col}}"
    ))
    dsgn <- survey::svydesign(
      ids    = ~1,
      weights = stats::reformulate(weight_col),
      data   = data
    )
  }

  dsgn
}

#' Detect a column by matching candidate names
#' @noRd
.detect_col <- function(data_names, data_names_lower, candidates, label) {
  for (cand in candidates) {
    idx <- which(data_names_lower == tolower(cand))
    if (length(idx) > 0) {
      found <- data_names[idx[1]]
      cli::cli_inform("Auto-detected {label} column: {.var {found}}")
      return(found)
    }
  }
  NULL
}
