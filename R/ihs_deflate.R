#' Deflate Nominal Values to Real Values Using CPI
#'
#' Converts nominal monetary values to real (constant-price) values using
#' Malawi CPI data. By default uses 2019 (the IHS5 fieldwork year) as the
#' reference period, so figures from every round are expressed in 2019 kwacha
#' and can be compared directly.
#'
#' Each round is deflated using its survey midpoint year: IHS2 = 2004,
#' IHS3 = 2010, IHS4 = 2016, IHS5 = 2019, IHS6 = 2024. The bundled CPI table
#' (\code{inst/extdata/mw_cpi_annual.csv}) is the World Bank WDI series
#' \code{FP.CPI.TOTL} for Malawi, rebased to 2019 = 100, covering 2004-2025.
#'
#' @param data A data.frame.
#' @param value_cols Character vector of column names containing monetary
#'   values to deflate.
#' @param round Character string of the IHS round (e.g., \code{"IHS5"}). If
#'   \code{NULL} (default), auto-detected from an \code{ihs_round} column in
#'   the data - which is what \code{\link{ihs_harmonise}} adds, so pooled
#'   multi-round data.frames deflate correctly without further arguments.
#' @param base_year Numeric. The base year for deflation. Default is
#'   \code{2019} (IHS5 baseline). Must be a year present in the CPI table.
#'
#' @return A data.frame with new \code{*_real} suffixed columns containing
#'   deflated values. Original nominal columns are left untouched.
#'
#' @section Caveat:
#' This is a national CPI deflator. It does not adjust for spatial price
#' variation between districts or between urban and rural areas. If your
#' analysis is sensitive to spatial price differences, use the spatial price
#' index published with each round's consumption aggregate instead.
#'
#' @seealso \code{\link{ihs_harmonise}}, which adds the \code{ihs_round}
#'   column this function reads.
#'
#' @examples
#' # A pooled data.frame carrying an `ihs_round` column deflates in one call
#' pooled <- data.frame(
#'   food_exp  = c(1000, 1000, 1000),
#'   ihs_round = c("IHS4", "IHS5", "IHS6")
#' )
#' ihs_deflate(pooled, value_cols = "food_exp")
#'
#' # Or state the round explicitly for a single-round data.frame
#' ihs_deflate(data.frame(food_exp = 1000), value_cols = "food_exp",
#'             round = "IHS6")
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

  # Map rounds to survey midpoint years (see .IHS_ROUND_YEARS in utils.R)
  round_year_map <- .IHS_ROUND_YEARS

  # Determine round(s) to use
  if (is.null(round)) {
    if ("ihs_round" %in% names(data)) {
      rounds_in_data <- unique(stats::na.omit(data$ihs_round))
      # Validate up front so an unrecognised value produces an actionable
      # message rather than a "subscript out of bounds" error deeper down.
      check_round(rounds_in_data)
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

  # Warn once, not per column, if any round involved is deflated against a CPI
  # figure the World Bank may still revise. Better to know the number can move
  # than to discover it when a reviewer reruns the analysis a year later.
  .warn_provisional(cpi, round_year_map, rounds_in_data, base_year)

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

#' Tell the user when a deflation rests on a provisional CPI figure
#'
#' The CPI table carries a `provisional` flag for years the World Bank may
#' still revise (roughly the two most recent). Deflating against one is fine -
#' it is the best figure available - but the resulting real values can shift
#' when the series is refreshed, which matters for reproducibility.
#'
#' @noRd
.warn_provisional <- function(cpi, round_year_map, rounds_in_data, base_year) {
  if (!"provisional" %in% names(cpi)) {
    return(invisible(NULL))
  }

  years <- unique(c(
    unlist(round_year_map[intersect(rounds_in_data, names(round_year_map))]),
    base_year
  ))
  prov <- cpi$year[cpi$provisional %in% TRUE & cpi$year %in% years]

  if (length(prov) == 0) {
    return(invisible(NULL))
  }

  retrieved <- if ("retrieved" %in% names(cpi)) unique(cpi$retrieved)[1] else NA
  cli::cli_inform(c(
    "!" = "{cli::qty(length(prov))}CPI for {.val {prov}} {?is/are} provisional and may be revised.",
    "i" = if (!is.na(retrieved)) {
      "Bundled series retrieved {retrieved}. Rebuild with {.file data-raw/02_build_cpi.R} to refresh."
    } else {
      "Rebuild with {.file data-raw/02_build_cpi.R} to refresh."
    }
  ))
  invisible(prov)
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
