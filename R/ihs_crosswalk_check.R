#' Check the comparability of variables across IHS rounds
#'
#' Evaluates the completeness and comparability of variables across the
#' available IHS rounds (IHS2, IHS3, IHS4, IHS5, IHS6) using the bundled
#' crosswalk. Use it to see how many variables are genuinely comparable
#' across the full series before committing to a pooled analysis.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints a summary report
#'   to the console using \code{cli}.
#'
#' @return Invisibly, a \code{tibble} containing the full crosswalk with one
#'   \code{ihs*_name} column per round plus an \code{n_rounds_avail} column
#'   counting how many rounds each variable appears in. If \code{verbose}
#'   is \code{TRUE}, also prints a summary to the console.
#'
#' @seealso \code{\link{ihs_search}} to look up individual variables.
#'
#' @examples
#' # Return the crosswalk without printing the report
#' cw <- ihs_crosswalk_check(verbose = FALSE)
#' nrow(cw)
#'
#' # Variables available in every round
#' sum(cw$n_rounds_avail == max(cw$n_rounds_avail))
#'
#' @export
ihs_crosswalk_check <- function(verbose = TRUE) {
  cw <- .load_crosswalk()
  total_vars <- nrow(cw)
  
  all_names_cols <- grep("^ihs[0-9]+_name$", names(cw), value = TRUE)
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

    cli::cli_h2("Coverage by Round")
    for (nm in all_names_cols) {
      rnd <- toupper(sub("_name$", "", nm))
      cli::cli_text("{rnd}: {.val {sum(!is.na(cw[[nm]]))}} variables")
    }

    # IHS6 is collected in Survey Solutions, which splits a "select all that
    # apply" question into one binary column per option. Those look like new
    # variables but are the same question re-encoded, so report them apart
    # from genuinely new content.
    if ("ihs6_expansion_of" %in% names(cw)) {
      exp_cnt <- sum(!is.na(cw$ihs6_expansion_of))
      if (exp_cnt > 0) {
        parents <- length(unique(stats::na.omit(cw$ihs6_expansion_of)))
        cli::cli_h2("IHS6 Multi-Select Expansions")
        cli::cli_alert_info(paste(
          "{exp_cnt} IHS6 column{?s} {?is/are} Survey Solutions expansion{?s}",
          "of {parents} parent question{?s}, not new variables."
        ))
        cli::cli_text("See the {.code ihs6_expansion_of} column for the parent.")
      }
    }

    needs_review_cnt <- sum(cw$needs_review, na.rm = TRUE)
    if (needs_review_cnt > 0) {
      cli::cli_h2("Variables Needing Review")
      cli::cli_alert_warning(paste(
        "{needs_review_cnt} variable{?s} flagged for review",
        "(single-round variables whose concept has not been verified",
        "against an earlier instrument):"
      ))
      review_vars <- cw[cw$needs_review, ]

      # Use message instead of print for CRAN compliance
      msg_out <- paste(utils::capture.output(print(utils::head(review_vars[, c("harmonised_name", "label", "topic")], 5))), collapse = "\n")
      message(msg_out)

      if (needs_review_cnt > 5) cli::cli_text("... and {needs_review_cnt - 5} more.")

      cli::cli_alert_info(
        "Filter the returned tibble on {.code needs_review} to inspect them all."
      )
    } else {
      cli::cli_alert_success("No variables flagged for review! Crosswalk is clean.")
    }

    # Mapping confidence summary
    if ("mapped" %in% names(cw)) {
      mapped_cnt <- sum(cw$mapped, na.rm = TRUE)
      tentative_cnt <- sum(!cw$mapped, na.rm = TRUE)
      cli::cli_h2("Mapping Confidence")
      cli::cli_alert_success("Confidently mapped: {.val {mapped_cnt}}")
      if (tentative_cnt > 0) {
        cli::cli_alert_warning("Tentative mappings: {.val {tentative_cnt}}")
      }
    }
  }
  
  invisible(cw)
}
