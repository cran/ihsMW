# R/nada_api.R

.nada_cache <- new.env(parent = emptyenv())

#' Build base httr2 request for NADA API
#'
#' @noRd
.nada_req <- function(path, auth = FALSE) {
  req <- httr2::request(ihs_base_url())
  req <- httr2::req_url_path_append(req, path)
  req <- httr2::req_user_agent(req, .ihs_user_agent())
  req <- httr2::req_timeout(req, 30)
  req <- httr2::req_retry(req, max_tries = 3, backoff = ~ 2)
  
  if (auth) {
    req <- httr2::req_headers(req, Authorization = paste("Bearer", .ihs_key()))
  }
  
  req
}

#' Perform request and parse response
#'
#' @noRd
.nada_get <- function(path, query = list(), auth = FALSE) {
  req <- .nada_req(path, auth = auth)
  
  if (length(query) > 0) {
    req <- rlang::exec(httr2::req_url_query, req, !!!query)
  }
  
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  
  if (status == 401) {
    cli::cli_abort(c(
      "API key rejected by the World Bank server.",
      "i" = "Your authorization credentials were not accepted.",
      ">" = "Run {.fn ihs_auth} to update your World Bank API key."
    ), class = "ihsMW_auth_error")
  } else if (status == 403) {
    cli::cli_abort(c(
      "Access denied by the World Bank server.",
      "i" = "You might need to accept the data use agreement for this survey.",
      ">" = paste0("Go to {.url https://microdata.worldbank.org} ",
                   "and ensure you have requested access.")
    ), class = "ihsMW_forbidden_error")
  } else if (status == 404) {
    cli::cli_abort(c(
      "Module or file not found: {.val {path}}",
      "i" = "The requested file or resource does not exist for this round.",
      ">" = "Run {.fn ihs_modules} to list valid modules for the round."
    ), class = "ihsMW_not_found_error")
  } else if (status >= 400) {
    cli::cli_abort(c(
      sprintf("NADA API request failed with status %d.", status),
      "i" = "An unexpected HTTP error occurred.",
      ">" = "Check your connection or attempt again later."
    ))
  }
  
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

#' Fetch variables for a study
#'
#' @noRd
.nada_variables <- function(idno) {
  cache_key <- paste0(idno, "_vars")
  if (exists(cache_key, envir = .nada_cache)) {
    return(get(cache_key, envir = .nada_cache))
  }
  
  resp <- .nada_get(paste0("catalog/", idno, "/variables"), auth = FALSE)
  
  # Parse defensively
  raw_vars <- if (!is.null(resp$variables$variable)) {
    resp$variables$variable
  } else if (!is.null(resp$variables)) {
    resp$variables
  } else {
    data.frame()
  }
  
  if (!is.data.frame(raw_vars)) {
    if (is.list(raw_vars)) {
      if (!is.null(names(raw_vars))) raw_vars <- list(raw_vars)
      raw_vars <- dplyr::bind_rows(lapply(raw_vars, function(x) {
        out <- list()
        if (!is.null(x$name)) out$name <- as.character(x$name)
        if (!is.null(x$vid)) out$vid <- as.character(x$vid)
        if (!is.null(x$labl)) out$labl <- as.character(x$labl)
        if (!is.null(x$fid)) out$fid <- as.character(x$fid)
        if (!is.null(x$file_id)) out$file_id <- as.character(x$file_id)
        as.data.frame(out, stringsAsFactors = FALSE)
      }))
    } else {
      raw_vars <- data.frame()
    }
  }
  
  res <- dplyr::tibble(
    var_name = character(),
    label = character(),
    file_name = character()
  )
  
  if (nrow(raw_vars) > 0) {
    var_name <- if (!is.null(raw_vars$name)) raw_vars$name else if (!is.null(raw_vars$vid)) raw_vars$vid else rep(NA_character_, nrow(raw_vars))
    label <- if (!is.null(raw_vars$labl)) raw_vars$labl else rep(NA_character_, nrow(raw_vars))
    file_name <- if (!is.null(raw_vars$fid)) raw_vars$fid else if (!is.null(raw_vars$file_id)) raw_vars$file_id else rep(NA_character_, nrow(raw_vars))
    
    res <- dplyr::tibble(
      var_name = as.character(var_name),
      label = as.character(label),
      file_name = as.character(file_name)
    )
  }
  
  assign(cache_key, res, envir = .nada_cache)
  res
}

#' Fetch data files for a study
#'
#' @noRd
.nada_data_files <- function(idno) {
  cache_key <- paste0(idno, "_data_files")
  if (exists(cache_key, envir = .nada_cache)) {
    return(get(cache_key, envir = .nada_cache))
  }
  
  resp <- .nada_get(paste0("catalog/", idno, "/datafiles"), auth = FALSE)
  
  raw_files <- if (is.data.frame(resp)) {
    resp
  } else if (!is.null(resp$dataset$data_files)) {
    resp$dataset$data_files
  } else if (!is.null(resp$data_files)) {
    resp$data_files
  } else if (!is.null(resp$dataset)) {
    resp$dataset
  } else {
    data.frame()
  }
  
  if (!is.data.frame(raw_files) && is.list(raw_files)) {
    if (!is.null(names(raw_files))) raw_files <- list(raw_files)
    raw_files <- dplyr::bind_rows(lapply(raw_files, function(x) {
      out <- list()
      if (!is.null(x$file_id)) out$file_id <- as.character(x$file_id)
      if (!is.null(x$id))      out$id      <- as.character(x$id)
      if (!is.null(x$file_name)) out$file_name <- as.character(x$file_name)
      if (!is.null(x$name))    out$name    <- as.character(x$name)
      if (!is.null(x$format))  out$format  <- as.character(x$format)
      as.data.frame(out, stringsAsFactors = FALSE)
    }))
  }
  
  res <- dplyr::tibble(
    file_id = character(),
    file_name = character(),
    format = character()
  )
  
  if (is.data.frame(raw_files) && nrow(raw_files) > 0) {
    file_id <- if (!is.null(raw_files$file_id)) raw_files$file_id else if (!is.null(raw_files$id)) raw_files$id else rep(NA_character_, nrow(raw_files))
    file_name <- if (!is.null(raw_files$file_name)) raw_files$file_name else if (!is.null(raw_files$name)) raw_files$name else rep(NA_character_, nrow(raw_files))
    format <- if (!is.null(raw_files$format)) raw_files$format else rep(NA_character_, nrow(raw_files))
    
    res <- dplyr::tibble(
      file_id = as.character(file_id),
      file_name = as.character(file_name),
      format = as.character(format)
    )
  }
  
  assign(cache_key, res, envir = .nada_cache)
  res
}

#' Search NADA catalog
#'
#' @noRd
.nada_search <- function(keyword, country = "mwi") {
  resp <- .nada_get("catalog/search", query = list(sk = keyword, country = country, ps = 10), auth = FALSE)
  
  res <- dplyr::tibble(
    idno = character(),
    title = character(),
    nation = character(),
    year_start = character(),
    year_end = character()
  )
  
  raw_search <- if (!is.null(resp$result$rows)) {
    resp$result$rows
  } else if (!is.null(resp$rows)) {
    resp$rows
  } else if (!is.null(resp$result)) {
    resp$result
  } else if (is.data.frame(resp)) {
    resp
  } else {
    data.frame()
  }
  
  if (!is.data.frame(raw_search) && is.list(raw_search)) {
    if (!is.null(names(raw_search))) raw_search <- list(raw_search)
    raw_search <- dplyr::bind_rows(lapply(raw_search, function(x) {
      out <- list()
      if (!is.null(x$idno)) out$idno <- as.character(x$idno)
      if (!is.null(x$title)) out$title <- as.character(x$title)
      if (!is.null(x$nation)) out$nation <- as.character(x$nation)
      if (!is.null(x$year_start)) out$year_start <- as.character(x$year_start)
      if (!is.null(x$year_end)) out$year_end <- as.character(x$year_end)
      as.data.frame(out, stringsAsFactors = FALSE)
    }))
  }
  
  if (is.data.frame(raw_search) && nrow(raw_search) > 0) {
    idno <- if (!is.null(raw_search$idno)) raw_search$idno else rep(NA_character_, nrow(raw_search))
    title <- if (!is.null(raw_search$title)) raw_search$title else rep(NA_character_, nrow(raw_search))
    nation <- if (!is.null(raw_search$nation)) raw_search$nation else rep(NA_character_, nrow(raw_search))
    year_start <- if (!is.null(raw_search$year_start)) raw_search$year_start else rep(NA_character_, nrow(raw_search))
    year_end <- if (!is.null(raw_search$year_end)) raw_search$year_end else rep(NA_character_, nrow(raw_search))
    
    res <- dplyr::tibble(
      idno = as.character(idno),
      title = as.character(title),
      nation = as.character(nation),
      year_start = as.character(year_start),
      year_end = as.character(year_end)
    )
  }
  
  res
}
