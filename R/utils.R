# Note: IHS1 (1997/98) is intentionally excluded from these constants because
# it is not currently available via the World Bank Microdata Library (NADA) API.
.IHS_ROUNDS <- c("IHS2", "IHS3", "IHS4", "IHS5")

.IHS_IDNOS <- c(
  IHS2 = "MWI_2004_IHS-II_v01_M",
  IHS3 = "MWI_2010_IHS-III_v01_M",
  IHS4 = "MWI_2016_IHS-IV_v03_M",
  IHS5 = "MWI_2019_IHS-V_v06_M"
)

#' Get the cache directory for ihsMW
#'
#' @noRd
ihs_cache_dir <- function() {
  dir <- rappdirs::user_cache_dir("ihsMW")
  if (!dir.exists(dir)) {
    success <- dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!success && !dir.exists(dir)) {
      cli::cli_abort("Failed to create cache directory at {.path {dir}}.")
    }
  }
  dir
}

#' Get the base URL for the NADA API
#'
#' @noRd
ihs_base_url <- function() {
  getOption("ihsMW.base_url", default = "https://microdata.worldbank.org/index.php/api/")
}

#' Validate requested rounds
#'
#' @noRd
check_round <- function(round) {
  if (length(round) == 1 && round == "all") {
    return(.IHS_ROUNDS)
  }
  
  if ("IHS1" %in% round) {
    cli::cli_abort(c(
      "IHS1 (1997/98) is not currently available via the NADA API.",
      "i" = "Supported rounds are: {.val {(.IHS_ROUNDS)}}",
      ">" = paste0("See {.url https://github.com/vituk123/ihsMW/issues/1} ",
                   "for progress on IHS1 support.")
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

#' Validate requested file format
#'
#' @noRd
check_format <- function(format) {
  valid_formats <- c("parquet", "rds", "csv")
  if (!all(format %in% valid_formats)) {
    invalid <- setdiff(format, valid_formats)
    cli::cli_abort(c(
      "Invalid format(s) specified: {.val {invalid}}",
      "i" = "Valid formats are: {.val {valid_formats}}"
    ))
  }
  format
}

#' Generate User-Agent string for API requests
#'
#' @noRd
.ihs_user_agent <- function() {
  paste0(
    "ihsMW/", utils::packageVersion("ihsMW"),
    " (https://github.com/vituk123/ihsMW)"
  )
}

#' Get the study IDNo for a valid round
#'
#' @noRd
.ihs_study_idno <- function(round) {
  if (!round %in% names(.IHS_IDNOS)) {
    cli::cli_abort("Invalid round {.val {round}} passed to .ihs_study_idno().")
  }
  .IHS_IDNOS[[round]]
}
