#' Display Information About Cached IHS Data
#'
#' @description
#' Scans the internal \code{ihsMW} cache directory and reports on any previously
#' downloaded datasets.
#'
#' @return A \code{tibble} summarizing the cached files.
#' @export
#'
#' @examples
#' \dontrun{
#' ihs_cache_info()
#' }
ihs_cache_info <- function() {
  cache_dir <- ihs_cache_dir()
  files <- list.files(cache_dir, recursive = TRUE, full.names = TRUE)
  
  if (length(files) == 0) {
    return(dplyr::tibble(
      round = character(),
      module = character(),
      format = character(),
      size_mb = numeric(),
      cached_at = as.POSIXct(character())
    ))
  }
  
  # Extract info
  file_info <- file.info(files)
  
  res <- dplyr::tibble(
    round = basename(dirname(files)),
    module = tools::file_path_sans_ext(basename(files)),
    format = tools::file_ext(files),
    size_mb = round(file_info$size / (1024^2), 2),
    cached_at = file_info$mtime
  )
  
  res
}

#' Clear Cached IHS Data
#'
#' @description
#' Removes downloaded datasets from the internal package cache. This is useful for freeing
#' up disk space. You can clear the cache for specific rounds or entirely.
#'
#' @param round A specific round to clear (e.g. \code{"IHS5"}). If \code{NULL}, asks for confirmation
#'   to clear all IHS data depending on the interactivity of the session. Defaults to \code{NULL}.
#'
#' @return Invisibly returns \code{NULL}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all
#' ihs_cache_clear()
#'
#' # Clear only IHS3 data
#' ihs_cache_clear(round = "IHS3")
#' }
ihs_cache_clear <- function(round = NULL) {
  cache_dir <- ihs_cache_dir()
  
  if (is.null(round)) {
    if (interactive()) {
      ans <- readline("Clear ALL cached IHS data? (y/n): ")
      if (!tolower(ans) %in% c("y", "yes")) {
        cli::cli_inform("Cancelled.")
        return(invisible(NULL))
      }
    }
    
    files_to_remove <- list.files(cache_dir, full.names = TRUE, recursive = TRUE)
    if (length(files_to_remove) == 0) {
      cli::cli_inform("Cache is already empty.")
      return(invisible(NULL))
    }
    
    total_size <- sum(file.info(files_to_remove)$size) / (1024^2)
    unlink(files_to_remove)
    
    cli::cli_alert_success(
      "Cleared {length(files_to_remove)} files ({round(total_size, 2)} MB) from cache."
    )
    
  } else {
    round <- check_round(round)
    total_removed_files <- 0
    total_removed_size <- 0
    
    for (r in round) {
      r_dir <- file.path(cache_dir, r)
      if (dir.exists(r_dir)) {
        files <- list.files(r_dir, full.names = TRUE, recursive = TRUE)
        if (length(files) > 0) {
          total_removed_size <- total_removed_size + sum(file.info(files)$size) / (1024^2)
          total_removed_files <- total_removed_files + length(files)
          unlink(files)
        }
      }
    }
    
    cli::cli_alert_success(
      "Cleared {total_removed_files} files ({round(total_removed_size, 2)} MB) from cache for round {paste(round, collapse = ', ')}."
    )
  }
  
  invisible(NULL)
}
