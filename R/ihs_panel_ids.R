#' Get Standard Panel ID Columns for an IHS Round
#'
#' Returns the standard household, individual, and enumeration area ID column
#' names used in a given IHS round. This is a convenience helper for users
#' building longitudinal panels or merging modules.
#'
#' @param round A character string specifying the IHS round: one of
#'   \code{"IHS2"}, \code{"IHS3"}, \code{"IHS4"}, \code{"IHS5"} or
#'   \code{"IHS6"}. Use \code{"all"} for a comparison across all rounds.
#'
#' @return If a single round is specified, a named character vector of standard
#'   ID columns with elements \code{hh_id}, \code{indiv_id}, \code{ea_id},
#'   \code{strata} and \code{weight}. If \code{"all"}, a data.frame comparing
#'   ID columns across rounds.
#'
#' @section Round differences:
#' Two names change across the series and catch people out:
#' \itemize{
#'   \item the household weight is \code{hhweight} in IHS2 and IHS3 but
#'     \code{hh_wgt} from IHS4 onwards;
#'   \item the design stratum is \code{stratum} in IHS2-IHS5 but \code{strata}
#'     in IHS6.
#' }
#' \code{\link{ihs_svydesign}} handles both automatically.
#'
#' @examples
#' # Get IHS6 ID columns
#' ihs_panel_ids("IHS6")
#'
#' # Compare across all rounds - note the stratum rename in IHS6
#' ihs_panel_ids("all")
#'
#' @export
ihs_panel_ids <- function(round = "IHS5") {
  # Define the canonical ID columns per round
  id_map <- list(
    IHS2 = c(
      hh_id      = "case_id",
      indiv_id   = "PID",
      ea_id      = "ea_id",
      strata     = "stratum",
      weight     = "hhweight"
    ),
    IHS3 = c(
      hh_id      = "case_id",
      indiv_id   = "PID",
      ea_id      = "ea_id",
      strata     = "stratum",
      weight     = "hhweight"
    ),
    IHS4 = c(
      hh_id      = "case_id",
      indiv_id   = "PID",
      ea_id      = "ea_id",
      strata     = "stratum",
      weight     = "hh_wgt"
    ),
    IHS5 = c(
      hh_id      = "case_id",
      indiv_id   = "PID",
      ea_id      = "ea_id",
      strata     = "stratum",
      weight     = "hh_wgt"
    ),
    # IHS6 (2024/25) renamed the design stratum column from `stratum` to
    # `strata`. Everything else carries over from IHS5 unchanged.
    IHS6 = c(
      hh_id      = "case_id",
      indiv_id   = "PID",
      ea_id      = "ea_id",
      strata     = "strata",
      weight     = "hh_wgt"
    )
  )

  if (length(round) == 1 && round == "all") {
    # Build a comparison data.frame
    roles <- names(id_map[[1]])
    df <- data.frame(role = roles, stringsAsFactors = FALSE)
    for (r in names(id_map)) {
      df[[r]] <- id_map[[r]]
    }
    cli::cli_inform("Panel ID columns across all supported IHS rounds.")
    return(df)
  }

  round <- check_round(round)
  if (length(round) > 1) {
    cli::cli_abort("{.arg round} must be a single round or {.val all}.")
  }

  ids <- id_map[[round]]
  cli::cli_inform("Standard ID columns for {.val {round}}:")
  ids
}
