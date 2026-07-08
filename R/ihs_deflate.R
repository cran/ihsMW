#' Deflate Nominal Values to Real Values Using CPI
#'
#' Converts nominal monetary values to real (constant-price) values using
#' Malawi CPI data. By default uses 2019 (IHS5 baseline) as the reference
#' period.
#'
#' @param data A data.frame.
#' @param value_cols Character vector of column names containing monetary
#'   values to deflate.
#' @param round Character string of the IHS round (e.g., \code{"IHS5"}). If
#'   \code{NULL} (default), auto-detected from an \code{ihs_round} column in
#'   the data.
#' @param base_year Numeric. The base year for deflation. Default is
#'   \code{2019} (IHS5 baseline).
#'
#' @return A data.frame with new \code{*_real} suffixed columns containing
#'   deflated values.
#'
#' @examples
#' \dontrun{
#'   # Deflate IHS4 consumption to 2019 prices
#'   real_data <- ihs_deflate(df, value_cols = "rexp_cat01", round = "IHS4")
#' }
#'
#' @export
ihs_deflate <- function(data, value_cols, round = NULL, base_year = 2019) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame.")
  }

  missing_cols <- setdiff(value_cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Columns not found in data: {.var {missing_cols}}")
  }

  # Map rounds to survey midpoint years
  round_year_map <- c(IHS2 = 2004, IHS3 = 2010, IHS4 = 2016, IHS5 = 2019)

  # Determine round(s) to use
  if (is.null(round)) {
    if ("ihs_round" %in% names(data)) {
      rounds_in_data <- unique(data$ihs_round)
      cli::cli_inform("Auto-detected round(s) from {.var ihs_round}: {.val {rounds_in_data}}")
    } else {
      cli::cli_abort(c(
        "Cannot determine IHS round.",
        "i" = "Specify {.arg round} or ensure data has an {.var ihs_round} column."
      ))
    }
  } else {
    round <- check_round(round)
    if (length(round) > 1) {
      cli::cli_abort("{.arg round} must be a single round for {.fn ihs_deflate}.")
    }
    data$.ihs_deflate_round <- round
    rounds_in_data <- round
  }

  cpi <- .load_cpi()

  base_row <- cpi[cpi$year == base_year, ]
  if (nrow(base_row) == 0) {
    cli::cli_abort("Base year {.val {base_year}} not found in CPI table.")
  }
  base_cpi <- base_row$cpi_index[1]

  for (v in value_cols) {
    new_col <- paste0(v, "_real")
    data[[new_col]] <- NA_real_

    if (!is.null(round) && length(round) == 1) {
      # Single round: apply uniform deflation
      survey_year <- round_year_map[[round]]
      if (is.na(survey_year)) {
        cli::cli_abort("No year mapping for round {.val {round}}.")
      }
      round_cpi <- cpi$cpi_index[cpi$year == survey_year]
      if (length(round_cpi) == 0) {
        cli::cli_abort("CPI data not available for year {.val {survey_year}}.")
      }
      factor <- base_cpi / round_cpi
      data[[new_col]] <- data[[v]] * factor
      cli::cli_inform("{.var {v}}: deflation factor = {round(factor, 4)} ({round} -> {base_year})")
    } else {
      # Multiple rounds via ihs_round column
      for (r in rounds_in_data) {
        survey_year <- round_year_map[[r]]
        if (is.na(survey_year)) next
        round_cpi <- cpi$cpi_index[cpi$year == survey_year]
        if (length(round_cpi) == 0) next
        factor <- base_cpi / round_cpi
        idx <- which(data$ihs_round == r)
        data[[new_col]][idx] <- data[[v]][idx] * factor
      }
      cli::cli_inform("Deflated {.var {v}} across {length(rounds_in_data)} round(s) to base year {base_year}.")
    }
  }

  # Clean up temp column

  if (".ihs_deflate_round" %in% names(data)) {
    data$.ihs_deflate_round <- NULL
  }

  data
}

#' Load bundled CPI data
#' @noRd
.load_cpi <- function() {
  cache_key <- "cpi"

  if (exists(cache_key, envir = .ihs_cache)) {
    return(get(cache_key, envir = .ihs_cache))
  }

  cpi_path <- system.file("extdata", "mw_cpi_annual.csv", package = "ihsMW")
  if (cpi_path == "" || !file.exists(cpi_path)) {
    cpi_path <- "inst/extdata/mw_cpi_annual.csv"
    if (!file.exists(cpi_path)) {
      cli::cli_abort("Could not locate mw_cpi_annual.csv. Is the package installed properly?")
    }
  }

  cpi <- readr::read_csv(cpi_path, show_col_types = FALSE)
  assign(cache_key, cpi, envir = .ihs_cache)
  cpi
}
