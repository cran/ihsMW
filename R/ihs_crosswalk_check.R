#' Check the comparability of variables across IHS rounds
#'
#' Evaluates the completeness and comparability of variables across the 
#' available IHS rounds (IHS2, IHS3, IHS4, IHS5) using the bundled crosswalk.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints a summary report 
#'   to the console using \code{cli}.
#'
#' @return A \code{tibble} containing the full crosswalk. If \code{verbose} 
#'   is \code{TRUE}, also prints a summary.
#'
#' @examples
#' \dontrun{
#'   # Check the crosswalk and print a report
#'   cw <- ihs_crosswalk_check()
#' }
#'
#' @export
ihs_crosswalk_check <- function(verbose = TRUE) {
  cw <- .load_crosswalk()
  total_vars <- nrow(cw)
  
  all_names_cols <- grep("^ihs[1-5]_name$", names(cw), value = TRUE)
  cw$n_rounds_avail <- rowSums(!is.na(cw[, all_names_cols, drop = FALSE]))
  max_rounds <- length(all_names_cols)
  
  all_max_rounds <- sum(cw$n_rounds_avail == max_rounds)
  all_max_pct <- round(100 * all_max_rounds / total_vars, 1)
  
  if (verbose) {
    cli::cli_h1("Crosswalk Check Report")
    cli::cli_text("Total harmonised variables: {.val {total_vars}}")
    cli::cli_text("Variables present in all {max_rounds} rounds: {.val {all_max_rounds}} ({all_max_pct}%)")
    
    cli::cli_h2("Variables by Availability")
    for (i in max_rounds:1) {
      cnt <- sum(cw$n_rounds_avail == i)
      bars <- paste(rep("\u2588", round((cnt / total_vars) * 20)), collapse = "")
      cli::cli_text("{i} round{?s}: {bars} ({cnt})")
    }
    
    needs_review_cnt <- sum(cw$needs_review, na.rm = TRUE)
    if (needs_review_cnt > 0) {
      cli::cli_h2("Variables Needing Review")
      cli::cli_alert_warning("{needs_review_cnt} variable{?s} flagged for review:")
      review_vars <- cw[cw$needs_review, ]
      
      # Use message instead of print for CRAN compliance
      msg_out <- paste(utils::capture.output(print(utils::head(review_vars[, c("harmonised_name", "label", "topic")], 5))), collapse = "\n")
      message(msg_out)
      
      if (needs_review_cnt > 5) cli::cli_text("... and {needs_review_cnt - 5} more.")
      
      cli::cli_alert_info("Review {.file data-raw/ihs_crosswalk_working.csv} to resolve flags.")
    } else {
      cli::cli_alert_success("No variables flagged for review! Crosswalk is clean.")
    }
  }
  
  invisible(cw)
}
