# Note: IHS1 (1997/98) is intentionally excluded from these constants because
# it is not currently available.
.IHS_ROUNDS <- c("IHS2", "IHS3", "IHS4", "IHS5")

#' Validate requested rounds
#'
#' @noRd
check_round <- function(round) {
  if (length(round) == 1 && round == "all") {
    return(.IHS_ROUNDS)
  }
  
  if ("IHS1" %in% round) {
    cli::cli_abort(c(
      "IHS1 (1997/98) is not currently supported.",
      "i" = "Supported rounds are: {.val {(.IHS_ROUNDS)}}"
    ), class = "ihsMW_bad_round")
  }
  
  invalid <- setdiff(round, .IHS_ROUNDS)
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "Invalid round(s) specified: {.val {invalid}}",
      "i" = "Supported rounds are: {.val {(.IHS_ROUNDS)}}"
    ), class = "ihsMW_bad_round")
  }
  
  round
}

