# Note: IHS1 (1997/98) is intentionally excluded from these constants because
# it is not currently available.
.IHS_ROUNDS <- c("IHS2", "IHS3", "IHS4", "IHS5", "IHS6")

# Survey midpoint year for each round, used for CPI deflation and for
# documenting which price base a round's monetary variables are expressed in.
.IHS_ROUND_YEARS <- c(
  IHS2 = 2004,
  IHS3 = 2010,
  IHS4 = 2016,
  IHS5 = 2019,
  IHS6 = 2024
)

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

