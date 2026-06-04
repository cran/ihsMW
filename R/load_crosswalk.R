#' Internal function to load the bundled crosswalk
#'
#' @noRd
.load_crosswalk <- function() {
  cache_key <- "crosswalk"
  
  if (exists(cache_key, envir = .ihs_cache)) {
    return(get(cache_key, envir = .ihs_cache))
  }
  
  cw_path <- system.file("extdata", "ihs_crosswalk.csv", package = "ihsMW")
  if (cw_path == "" || !file.exists(cw_path)) {
    cw_path <- "inst/extdata/ihs_crosswalk.csv"
    if (!file.exists(cw_path)) {
       cli::cli_abort("Could not locate ihs_crosswalk.csv. Is the package installed properly?")
    }
  }
  
  cw <- readr::read_csv(cw_path, show_col_types = FALSE)
  assign(cache_key, cw, envir = .ihs_cache)
  cw
}
