#' Generate Summary Statistics Table
#'
#' Produces a publication-ready summary statistics table for numeric variables
#' in IHS data. Supports grouping by a factor variable and optional survey
#' weights.
#'
#' @param data A data.frame.
#' @param vars Character vector of column names to summarise. If \code{NULL}
#'   (default), all numeric columns are included.
#' @param by Optional character string of a grouping variable (e.g.,
#'   \code{"region"}, \code{"urban"}).
#' @param weights Optional character string of a column containing survey
#'   weights for weighted means and SDs.
#'
#' @return A data.frame of summary statistics with columns: \code{variable},
#'   \code{n}, \code{mean}, \code{sd}, \code{median}, \code{min}, \code{max},
#'   \code{pct_missing}. If \code{by} is specified, an additional grouping
#'   column is included.
#'
#' @section Weighted statistics:
#' When \code{weights} is supplied the mean and SD are weighted; \code{n},
#' \code{median}, \code{min}, \code{max} and \code{pct_missing} stay
#' unweighted, since they describe the sample rather than the population.
#' For weighted quantiles or design-correct standard errors, build a design
#' with \code{\link{ihs_svydesign}} and use the \pkg{survey} package.
#'
#' @seealso \code{\link{ihs_svydesign}} for design-based inference.
#'
#' @examples
#' hh <- data.frame(
#'   hhsize   = c(4, 6, 3, 5, 7, 2),
#'   food_exp = c(1200, 3400, 900, 2100, 4500, 700),
#'   region   = c("North", "North", "Central", "Central", "South", "South"),
#'   hh_wgt   = c(1.2, 0.8, 1.5, 1.1, 0.9, 1.3)
#' )
#'
#' # Basic summary of every numeric column (weight columns are excluded)
#' ihs_report(hh)
#'
#' # Selected variables, grouped by region
#' ihs_report(hh, vars = "food_exp", by = "region")
#'
#' # Survey-weighted means and SDs
#' ihs_report(hh, vars = c("hhsize", "food_exp"), weights = "hh_wgt")
#'
#' @export
ihs_report <- function(data, vars = NULL, by = NULL, weights = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame.")
  }

  # Default to all numeric columns
  if (is.null(vars)) {
    vars <- names(data)[vapply(data, is.numeric, logical(1))]
    # Exclude weight columns from auto-selection
    weight_like <- c("hh_wgt", "hhweight", "weight", "panelweight")
    vars <- setdiff(vars, weight_like)
    if (length(vars) == 0) {
      cli::cli_abort("No numeric columns found in data.")
    }
  }

  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    cli::cli_abort("Variables not found in data: {.var {missing_vars}}")
  }

  if (!is.null(weights) && !weights %in% names(data)) {
    cli::cli_abort("Weight column {.var {weights}} not found in data.")
  }

  if (!is.null(by) && !by %in% names(data)) {
    cli::cli_abort("Grouping variable {.var {by}} not found in data.")
  }

  # Compute stats
  if (is.null(by)) {
    result <- .compute_stats(data, vars, weights)
  } else {
    groups <- unique(data[[by]])
    groups <- groups[!is.na(groups)]
    results_list <- lapply(groups, function(g) {
      sub <- data[data[[by]] == g, ]
      stats <- .compute_stats(sub, vars, weights)
      stats$group <- g
      stats
    })
    result <- do.call(rbind, results_list)
    # Reorder columns so group is first
    result <- result[, c("group", setdiff(names(result), "group")), drop = FALSE]
    names(result)[names(result) == "group"] <- by
  }

  n_vars <- length(vars)
  if (is.null(by)) {
    cli::cli_inform("Summary statistics for {n_vars} variable{?s}.")
  } else {
    n_groups <- length(unique(data[[by]]))
    cli::cli_inform("Summary statistics for {n_vars} variable{?s} across {n_groups} group{?s}.")
  }

  rownames(result) <- NULL
  result
}

#' Compute summary statistics for a set of variables
#' @noRd
.compute_stats <- function(data, vars, weights = NULL) {
  stats_list <- lapply(vars, function(v) {
    x <- data[[v]]
    n_total <- length(x)
    n_missing <- sum(is.na(x))
    x_clean <- x[!is.na(x)]
    n_obs <- length(x_clean)

    if (n_obs == 0) {
      return(data.frame(
        variable = v, n = 0L, mean = NA_real_, sd = NA_real_,
        median = NA_real_, min = NA_real_, max = NA_real_,
        pct_missing = 100, stringsAsFactors = FALSE
      ))
    }

    if (!is.null(weights)) {
      w <- data[[weights]][!is.na(x)]
      w_sum <- sum(w, na.rm = TRUE)
      if (w_sum > 0) {
        w_mean <- sum(x_clean * w, na.rm = TRUE) / w_sum
        w_var <- sum(w * (x_clean - w_mean)^2, na.rm = TRUE) / w_sum
        w_sd <- sqrt(w_var)
      } else {
        w_mean <- mean(x_clean)
        w_sd <- stats::sd(x_clean)
      }
      stat_mean <- w_mean
      stat_sd <- w_sd
    } else {
      stat_mean <- mean(x_clean)
      stat_sd <- stats::sd(x_clean)
    }

    data.frame(
      variable = v,
      n = n_obs,
      mean = round(stat_mean, 4),
      sd = round(stat_sd, 4),
      median = round(stats::median(x_clean), 4),
      min = round(min(x_clean), 4),
      max = round(max(x_clean), 4),
      pct_missing = round(100 * n_missing / n_total, 2),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, stats_list)
}
