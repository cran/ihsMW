#' Search across all IHS rounds for variables manually mapped
#'
#' @description
#' Searches the manual harmonisation crosswalk bundled within \code{ihsMW} for specific variables.
#'
#' @param keyword A single search string to find (case-insensitive).
#' @param round Limits the search to variables that exist in a specific round.
#'   Valid inputs are \code{"IHS2"}, \code{"IHS3"}, \code{"IHS4"},
#'   \code{"IHS5"}, and \code{"IHS6"}. Defaults to \code{NULL} (all rounds).
#' @param fields A character vector of fields to include in the search. Valid fields are \code{"name"}, \code{"label"}, and \code{"module"}.
#'
#' @return A tibble with one row per matching harmonised variable and one
#'   \code{ihs*_name} column per survey round, plus \code{n_rounds} (how many
#'   rounds the variable appears in) and \code{needs_review}.
#' @export
#'
#' @examples
#' ihs_search("consumption")
#' ihs_search("expenditure", round = "IHS5")
#' ihs_search("age", fields = c("name", "label"))
#'
#' # Variables that are new in IHS6
#' ihs_search("livestock", round = "IHS6")
ihs_search <- function(keyword, round = NULL, fields = c("name", "label", "module")) {
  fields <- rlang::arg_match(fields, multiple = TRUE)
  
  if (!is.character(keyword) || length(keyword) != 1 || !nzchar(keyword)) {
    cli::cli_abort("`keyword` must be a single, non-empty character string.")
  }
  
  cw <- .load_crosswalk()
  
  if (!is.null(round)) {
    round <- check_round(round)
  }
  
  match_mask <- rep(FALSE, nrow(cw))
  
  if ("name" %in% fields) {
    match_mask <- match_mask | grepl(keyword, cw$harmonised_name, ignore.case = TRUE, perl = TRUE)
  }
  if ("label" %in% fields) {
    match_mask <- match_mask | grepl(keyword, cw$label, ignore.case = TRUE, perl = TRUE)
  }
  if ("module" %in% fields) {
    match_mask <- match_mask | grepl(keyword, cw$module, ignore.case = TRUE, perl = TRUE)
  }
  
  res <- cw[match_mask, ]
  
  if (!is.null(round)) {
    valid_rows <- rep(FALSE, nrow(res))
    for (r in round) {
      col_name <- paste0(tolower(r), "_name")
      if (col_name %in% names(res)) {
        valid_rows <- valid_rows | !is.na(res[[col_name]])
      }
    }
    res <- res[valid_rows, ]
  }
  
  # Round columns are discovered rather than hard-coded so that adding a future
  # round to the crosswalk needs no change here.
  round_cols <- grep("^ihs[0-9]+_name$", names(cw), value = TRUE)

  # `ihs6_expansion_of` is only present in crosswalks built from v1.1.0 onwards.
  extras <- intersect("ihs6_expansion_of", names(cw))

  res <- res |>
    dplyr::select(dplyr::all_of(c(
      "harmonised_name", "label", "module", "topic",
      round_cols, extras,
      "n_rounds", "needs_review"
    ))) |>
    dplyr::arrange(dplyr::desc(.data$n_rounds), .data$harmonised_name)

  if (nrow(res) == 0) {
    cli::cli_inform(c(
      "No variables found matching {.val {keyword}}.",
      ">" = "Try a broader term or check spelling.",
      "i" = "Use {.fn ihs_crosswalk_check} to browse the full crosswalk."
    ))
  } else {
    cli::cli_inform("Found {nrow(res)} variable{?s} matching {.val {keyword}}.")
  }
  
  res
}


