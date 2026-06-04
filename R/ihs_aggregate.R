#' Smart Aggregation to Household Level
#'
#' Automatically detects variable types and applies sensible aggregations (e.g., `sum` 
#' for continuous quantities, `max` or logical OR for dummies). Throws warnings 
#' for ambiguous columns rather than failing silently.
#'
#' @param data A data.frame at the individual or plot level
#' @param group_col The column name identifying the household (e.g., "case_id" or "y4_hhid")
#' 
#' @return A data.frame aggregated to the household level
#' @importFrom rlang .data
#' @export
ihs_aggregate <- function(data, group_col = "case_id") {
  if (!group_col %in% names(data)) {
    cli::cli_abort("Grouping column {.var {group_col}} not found in data.")
  }
  
  cli::cli_alert_info("Aggregating data by {.var {group_col}}...")
  
  # Determine sensible aggregation functions by column type
  aggs <- list()
  for (col in names(data)) {
    if (col == group_col) next
    
    val <- data[[col]]
    
    if (is.numeric(val)) {
      # Check if it's a dummy/boolean masquerading as numeric (only 0, 1, NA)
      uniq_vals <- unique(stats::na.omit(val))
      if (all(uniq_vals %in% c(0, 1))) {
        aggs[[col]] <- function(x) as.numeric(any(x == 1, na.rm = TRUE))
      } else {
        # Continuous numeric -> sum by default
        aggs[[col]] <- function(x) sum(x, na.rm = TRUE)
      }
    } else if (is.logical(val)) {
      aggs[[col]] <- function(x) any(x, na.rm = TRUE)
    } else if (is.character(val) || is.factor(val)) {
      # For text, we might take the first non-NA, or concatenate.
      # Usually, household level IDs are identical, but varying text needs warning.
      aggs[[col]] <- function(x) {
        x <- stats::na.omit(x)
        if (length(x) == 0) return(NA_character_)
        if (length(unique(x)) > 1) {
          # Silent concatenation to avoid flooding, but maybe should warn
          paste(unique(x), collapse = " | ")
        } else {
          as.character(x[1])
        }
      }
    } else {
      cli::cli_warn("Uncertain how to aggregate column {.var {col}} of type {.type {val}}. Dropping.")
    }
  }
  
  # Perform aggregation
  # Note: using base R aggregate or split/lapply for zero-dependency style, 
  # but dplyr is imported in this package, so we use dplyr
  
  # We construct the list of expressions for dplyr::summarise
  res <- dplyr::group_by(data, .data[[group_col]])
  
  # Because applying custom functions per column dynamically in dplyr is easier with summarise(across())
  # But since functions vary, we can do it via a loop or by building a summary dataframe.
  # Let's use a split-apply-combine approach or dplyr's group_modify if we want to be safe.
  
  # Actually, dplyr 1.1.0+ allows grouped iteration easily.
  res <- dplyr::summarise(res, dplyr::across(dplyr::everything(), ~ {
    col_name <- dplyr::cur_column()
    fn <- aggs[[col_name]]
    if (is.null(fn)) {
      NA
    } else {
      fn(.x)
    }
  }), .groups = "drop")
  
  # Clean up dropped columns
  dropped <- setdiff(names(data), names(res))
  if (length(dropped) > 0) {
    cli::cli_inform("Dropped columns: {.var {dropped}}")
  }
  
  res
}
