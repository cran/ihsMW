#' Set up World Bank Microdata API Key
#'
#' @description
#' The World Bank Microdata Library uses API keys for authenticated endpoints.
#' The key is stored in the environment variable \env{WORLDBANK_MICRODATA_KEY}.
#'
#' If \code{key} is \code{NULL}, this function prints an interactive guide to
#' obtaining an API key. If a key is provided, the function validates it against
#' the NADA API, saves it to the session, and appends it to your \code{~/.Renviron}
#' file for future sessions.
#'
#' @param key A single string containing your World Bank Microdata API key. Defaults to \code{NULL}.
#'
#' @return Invisibly returns the API key (if provided) or \code{NULL}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Print interactive setup guide
#' ihs_auth()
#'
#' # Set your API key
#' ihs_auth("paste_your_key_here")
#' }
ihs_auth <- function(key = NULL) {
  if (is.null(key)) {
    cli::cli_h1("Setting up your World Bank Microdata API key")
    cli::cli_ol(c(
      "Go to {.url https://microdata.worldbank.org}",
      "Click \"Login / Register\" and create a free account",
      "Once logged in, click your name \U2192 Profile",
      "Scroll to \"API Keys\" and click \"Generate API Key\"",
      "Copy the key and run: {.code ihs_auth(\"paste_your_key_here\")}"
    ))
    
    if (interactive()) {
      ans <- readline("Open the website now? (y/n): ")
      if (tolower(ans) %in% c("y", "yes")) {
        utils::browseURL("https://microdata.worldbank.org/index.php/auth/profile")
      }
    }
    return(invisible(NULL))
  }
  
  if (!is.character(key) || length(key) != 1 || !nzchar(key)) {
    cli::cli_abort("`key` must be a non-empty single string.")
  }
  
  # Test the key against the NADA API
  req <- httr2::request(paste0(ihs_base_url(), "catalog/MWI_2019_IHS-V_v06_M/data_files"))
  req <- httr2::req_headers(req, Authorization = paste("Bearer", key))
  req <- httr2::req_user_agent(req, .ihs_user_agent())
  # Prevent httr2 from throwing on HTTP errors so we can handle 401 manually
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    cli::cli_warn(c(
      "Could not validate API key due to network error:",
      "x" = e$message,
      "i" = "Saving key anyway."
    ))
    NULL
  })
  
  if (!is.null(resp)) {
    if (httr2::resp_status(resp) == 401) {
      cli::cli_abort(c(
        "API key rejected by the World Bank server.",
        "i" = "Your token was rejected as invalid.",
        ">" = "Ensure your token is valid and run {.fn ihs_auth} to update it."
      ), class = "ihsMW_auth_error")
    } else if (httr2::resp_status(resp) >= 400) {
      cli::cli_warn(c(
        sprintf("API returned HTTP status %s during key validation.", httr2::resp_status(resp)),
        "i" = "Saving key anyway."
      ))
    }
  }
  
  # Save to current session
  Sys.setenv(WORLDBANK_MICRODATA_KEY = key)
  
  # Append to ~/.Renviron
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")
  existing <- character()
  if (file.exists(renviron_path)) {
    existing <- readLines(renviron_path)
  }
  
  # Remove any existing WORLDBANK_MICRODATA_KEY line
  existing <- existing[!grepl("^WORLDBANK_MICRODATA_KEY=", existing)]
  new_line <- paste0('WORLDBANK_MICRODATA_KEY="', key, '"')
  writeLines(c(existing, new_line), renviron_path)
  
  cli::cli_alert_success("API key saved to ~/.Renviron")
  cli::cli_inform("You won't need to run {.fn ihs_auth} again.")
  
  invisible(key)
}

#' Retrieve current API key
#'
#' @noRd
.ihs_key <- function() {
  key <- Sys.getenv("WORLDBANK_MICRODATA_KEY")
  if (nzchar(key)) {
    return(key)
  }
  
  cli::cli_abort(c(
    "No World Bank Microdata API key found.",
    "i" = "You need a registered key to authenticate data downloads.",
    ">" = "Run {.fn ihs_auth} to automatically set up your key."
  ), class = "ihsMW_no_key")
}

#' Set up World Bank Microdata API Key (Alias)
#'
#' @description
#' A wrapper for \code{ihs_auth()} meant for use in scripted or non-interactive environments.
#'
#' @param key A single string containing your World Bank Microdata API key.
#'
#' @return Invisibly returns the API key.
#' @export
#'
#' @examples
#' \dontrun{
#' ihs_key_set("paste_your_key_here")
#' }
ihs_key_set <- function(key) {
  ihs_auth(key = key)
}
