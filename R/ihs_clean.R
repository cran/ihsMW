#' Clean and Harmonise IHS Data
#'
#' This wrapper function applies standard cleaning procedures to Malawi IHS data.
#' It handles missing value conversions, winsorization of continuous variables,
#' and returns an audit log of all transformations applied.
#'
#' @param data A data.frame (typically loaded from a `.dta` file)
#' @param winsorize_vars Character vector of continuous variables to winsorize (e.g., consumption, harvest)
#' @param winsorize_by Optional character string of a grouping variable (e.g., region) for stratified winsorization
#' @param probs Numeric vector of length 2 specifying the lower and upper quantiles for winsorization. Default is `c(0.01, 0.99)`.
#' 
#' @return A data.frame with cleaning applied. The returned object has an `ihs_audit` attribute
#' containing a log of modifications: initial and final dimensions, how many
#' values were recoded to `NA` in each column, and how many observations each
#' winsorized variable had capped at the lower and upper bound.
#'
#' @seealso \code{\link{ihs_standardize_missing}} and
#'   \code{\link{ihs_winsorize}}, which this function wraps.
#'
#' @examples
#' survey <- data.frame(
#'   region   = rep(c("North", "South"), each = 50),
#'   food_exp = c(rnorm(50, 100, 10), rnorm(50, 500, 50))
#' )
#' # A refusal code and an implausible outlier
#' survey$food_exp[1] <- -99
#' survey$food_exp[60] <- 99999
#'
#' clean <- ihs_clean(survey, winsorize_vars = "food_exp",
#'                    winsorize_by = "region")
#'
#' # Winsorized values land in a new `_w` column; the raw column is preserved
#' head(clean[, c("food_exp", "food_exp_w")])
#'
#' # Inspect what was changed
#' str(attr(clean, "ihs_audit"))
#'
#' @export
ihs_clean <- function(data, winsorize_vars = NULL, winsorize_by = NULL, probs = c(0.01, 0.99)) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame")
  }
  
  audit_log <- list()
  audit_log$initial_rows <- nrow(data)
  audit_log$initial_cols <- ncol(data)
  
  # Step 1: Standardize missing values (-99, -98, etc. -> NA)
  data <- ihs_standardize_missing(data)
  audit_log$missing_conversions <- attr(data, "ihs_missing_conversions")
  attr(data, "ihs_missing_conversions") <- NULL
  
  # Step 2: Winsorize requested variables
  if (!is.null(winsorize_vars)) {
    data <- ihs_winsorize(data, vars = winsorize_vars, by = winsorize_by, probs = probs)
    audit_log$winsorized_vars <- attr(data, "ihs_winsorized_vars")
    attr(data, "ihs_winsorized_vars") <- NULL
  }
  
  audit_log$final_rows <- nrow(data)
  audit_log$final_cols <- ncol(data)
  
  attr(data, "ihs_audit") <- audit_log
  data
}

#' Standardize Survey Missing Codes
#'
#' Converts common survey missing codes (like -99 for "Refused" or -98 for
#' "Don't Know") into standard R `NA` values to prevent them from skewing
#' numeric calculations. The codes recognised are -99, -98, -97, 999, 998 and
#' 997. Only numeric columns are touched; character columns pass through
#' untouched.
#'
#' @param data A data.frame
#'
#' @return A data.frame with missing values standardized, carrying an
#'   `ihs_missing_conversions` attribute recording how many values were
#'   recoded in each affected column.
#'
#' @section Warning:
#' 999 is a legitimate value for some variables - a plot area in square metres,
#' a price in kwacha. Check your variables before relying on the blanket recode,
#' and pass only the affected columns through this function if in doubt.
#'
#' @examples
#' df <- data.frame(age = c(34, -99, 41, 998), village = c("A", "B", "C", "D"))
#' clean <- ihs_standardize_missing(df)
#' clean$age
#'
#' # How many values were recoded, by column
#' attr(clean, "ihs_missing_conversions")
#' @export
ihs_standardize_missing <- function(data) {
  missing_codes <- c(-99, -98, -97, 999, 998, 997)
  conversions <- list()
  
  for (col in names(data)) {
    if (is.numeric(data[[col]])) {
      is_missing <- data[[col]] %in% missing_codes
      if (any(is_missing, na.rm = TRUE)) {
        conversions[[col]] <- sum(is_missing, na.rm = TRUE)
        data[[col]][is_missing] <- NA
      }
    }
  }
  
  attr(data, "ihs_missing_conversions") <- conversions
  data
}

#' Winsorize Continuous Variables
#'
#' Caps extreme outliers at specified percentiles. Crucially, this function allows for 
#' stratified winsorization (e.g., by region) to avoid over-trimming poor/rich areas, 
#' and it creates new `_w` suffixed columns to preserve raw data provenance.
#'
#' @param data A data.frame
#' @param vars Character vector of column names to winsorize
#' @param by Optional grouping variable name (e.g., "region") for stratified thresholds
#' @param probs Numeric vector of lower and upper quantiles. Default `c(0.01, 0.99)`
#'
#' @return A data.frame with new `*_w` columns added, carrying an
#'   `ihs_winsorized_vars` attribute recording how many observations were
#'   capped at each bound.
#'
#' @section Why stratify:
#' A single national 99th percentile treats the richest rural households as
#' outliers because the urban distribution sits far above them. Passing `by`
#' computes the cut-points separately within each stratum, which preserves the
#' shape of both distributions.
#'
#' @examples
#' df <- data.frame(
#'   region = rep(c("North", "South"), each = 50),
#'   cons   = c(rnorm(50, 100, 10), rnorm(50, 500, 50))
#' )
#'
#' # Global thresholds
#' g <- ihs_winsorize(df, vars = "cons", probs = c(0.05, 0.95))
#'
#' # Thresholds computed within each region
#' s <- ihs_winsorize(df, vars = "cons", by = "region", probs = c(0.05, 0.95))
#'
#' # The raw column is never modified
#' identical(g$cons, df$cons)
#' attr(s, "ihs_winsorized_vars")
#' @export
ihs_winsorize <- function(data, vars, by = NULL, probs = c(0.01, 0.99)) {
  if (length(probs) != 2 || probs[1] >= probs[2]) {
    cli::cli_abort("{.arg probs} must be a numeric vector of length 2 where lower < upper (e.g. c(0.01, 0.99))")
  }
  
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    cli::cli_abort("Variables not found in data: {.var {missing_vars}}")
  }
  
  if (!is.null(by) && !by %in% names(data)) {
    cli::cli_abort("Grouping variable {.var {by}} not found in data")
  }
  
  audit <- list()
  
  for (v in vars) {
    if (!is.numeric(data[[v]])) {
      cli::cli_warn("Skipping {.var {v}} as it is not numeric.")
      next
    }
    
    new_col <- paste0(v, "_w")
    data[[new_col]] <- data[[v]]
    
    total_capped_low <- 0
    total_capped_high <- 0
    
    if (is.null(by)) {
      q <- stats::quantile(data[[v]], probs = probs, na.rm = TRUE, names = FALSE)
      
      low_idx <- which(data[[v]] < q[1])
      high_idx <- which(data[[v]] > q[2])
      
      data[[new_col]][low_idx] <- q[1]
      data[[new_col]][high_idx] <- q[2]
      
      total_capped_low <- length(low_idx)
      total_capped_high <- length(high_idx)
    } else {
      groups <- unique(data[[by]])
      for (g in groups) {
        if (is.na(g)) next
        
        idx <- which(data[[by]] == g)
        if (length(idx) == 0) next
        
        q <- stats::quantile(data[[v]][idx], probs = probs, na.rm = TRUE, names = FALSE)
        
        low_idx <- idx[which(data[[v]][idx] < q[1])]
        high_idx <- idx[which(data[[v]][idx] > q[2])]
        
        data[[new_col]][low_idx] <- q[1]
        data[[new_col]][high_idx] <- q[2]
        
        total_capped_low <- total_capped_low + length(low_idx)
        total_capped_high <- total_capped_high + length(high_idx)
      }
    }
    
    audit[[v]] <- list(
      capped_lower = total_capped_low,
      capped_upper = total_capped_high
    )
  }
  
  attr(data, "ihs_winsorized_vars") <- audit
  data
}
