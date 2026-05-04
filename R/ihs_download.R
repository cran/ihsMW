#' Fetch and cache a module from a specific round
#'
#' @noRd
.ihs_fetch <- function(round, module, format = "parquet", cache = TRUE, progress = TRUE) {
  # STEP 1: RESOLVE IDNo
  idno <- .ihs_study_idno(round)
  
  # STEP 2: CHECK CACHE
  cache_dir <- file.path(ihs_cache_dir(), round)
  cache_path <- file.path(cache_dir, paste0(module, ".", format))
  
  if (cache && file.exists(cache_path)) {
    if (progress) {
      cli::cli_inform("Loading {round}/{module} from cache ({.path {cache_path}})")
    }
    return(.read_cached(cache_path, format))
  }
  
  # STEP 3: GET FILE ID
  files <- .nada_data_files(idno)
  
  # Partial, case-insensitive match
  matched <- files[grepl(tolower(module), tolower(files$file_name), fixed = TRUE), ]
  
  if (nrow(matched) == 0) {
    cli::cli_abort(c(
      "Module {.val {module}} not found in {.val {round}}.",
      ">" = paste0("Run {.code ihs_modules('", round, "')} to see available modules."),
      "i" = "Note: module names are case-insensitive."
    ), class = "ihsMW_module_not_found")
  }
  
  file_id <- matched$file_id[1]
  
  # STEP 4: BUILD DOWNLOAD URL
  req <- .nada_req(paste0("catalog/", idno, "/download/", file_id), auth = TRUE)
  
  # STEP 5: DOWNLOAD
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile()
  
  if (progress) {
    cli::cli_progress_step("Downloading {round}/{module} from World Bank Microdata Library...")
  }
  
  # Prevent httr2 from throwing standard HTTP errors to intercept manually if needed
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  
  resp <- tryCatch({
    httr2::req_perform(req)
  }, error = function(e) {
    if (file.exists(tmp)) unlink(tmp)
    cli::cli_abort(c(
      "Failed to perform download request for {.val {module}}.",
      "x" = e$message
    ))
  })
  
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    if (file.exists(tmp)) unlink(tmp)
    if (status == 401) {
      cli::cli_abort("Unauthorised (401). Run {.fn ihs_auth} to check your API key.")
    } else if (status == 403) {
      cli::cli_abort("Forbidden (403). Ensure you have accepted the data conditions at the website.")
    } else {
      cli::cli_abort("Failed to download module with HTTP status {status}.")
    }
  }
  
  writeBin(httr2::resp_body_raw(resp), tmp)
  
  # Safely move to cache
  file.copy(tmp, cache_path, overwrite = TRUE)
  unlink(tmp)
  
  # STEP 6: READ AND RETURN
  return(.read_cached(cache_path, format))
}

#' Internal function to read known formats
#'
#' @noRd
.read_cached <- function(path, format) {
  switch(format,
    parquet = arrow::read_parquet(path),
    rds     = readRDS(path),
    csv     = readr::read_csv(path, show_col_types = FALSE),
    cli::cli_abort("Unknown format: {.val {format}}")
  )
}
