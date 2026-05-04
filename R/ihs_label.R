#' Fetch specific variable label locally or remotely
#'
#' @description
#' Quickly deciphers what an individual variable physically measures. Looks through the offline dataset initially via harmonised mappings and seamlessly falls through NADA otherwise.
#'
#' @param variable A single character variable map or harmonised standard to inspect precisely.
#' @param round The physical survey round to tie it structurally to if verifying non-harmonised entries. Default \code{"IHS5"}.
#'
#' @return The extracted label mapping directly against what the variable corresponds natively to.
#' @export
#'
#' @examples
#' ihs_label("rexp_cat01")
ihs_label <- function(variable, round = "IHS5") {
  if (length(round) != 1 || round == "all") {
    cli::cli_abort("`round` must be a single specific round.")
  }
  round <- check_round(round)
  
  if (!is.character(variable) || length(variable) != 1 || !nzchar(variable)) {
    cli::cli_abort("`variable` must be a single non-empty character string.")
  }
  
  cw <- .load_crosswalk()
  
  exact <- cw[cw$harmonised_name == variable, ]
  if (nrow(exact) > 0 && !is.na(exact$label[1])) {
    return(exact$label[1])
  }
  
  vars <- .nada_variables(.ihs_study_idno(round))
  exact_nada <- vars[vars$var_name == variable, ]
  if (nrow(exact_nada) > 0 && !is.na(exact_nada$label[1])) {
    return(exact_nada$label[1])
  }
  
  all_names <- unique(c(cw$harmonised_name, vars$var_name))
  all_names <- all_names[!is.na(all_names)]
  
  if (length(all_names) > 0) {
    dists <- stringdist::stringdist(variable, all_names, method = "jw")
    closest <- all_names[order(dists)[1:3]]
    cli::cli_warn(c(
      "Variable {.val {variable}} not found in {.val {round}}.",
      "i" = "Did you mean one of: {.val {closest}}?"
    ))
  } else {
    cli::cli_warn("Variable {.val {variable}} not found in {.val {round}}.")
  }
  
  return(NA_character_)
}

#' Inspect available modules for a study
#'
#' @description
#' Profiles the file hierarchy explicitly for each data survey pulling nested variables counts efficiently per dataset.
#'
#' @param round A specific round to fetch dataset structures safely scoped against. (e.g. \code{"IHS5"}).
#'
#' @return Invisibly returns a tibble mapping underlying module variables mapping locally.
#' @export
#'
#' @examples
#' \dontrun{
#' ihs_modules("IHS5")
#' }
ihs_modules <- function(round = "IHS5") {
  if (length(round) != 1 || round == "all") {
    cli::cli_abort("`round` must be a single specific round.")
  }
  round <- check_round(round)
  
  files <- .nada_data_files(.ihs_study_idno(round))
  vars <- .nada_variables(.ihs_study_idno(round))
  
  if (nrow(vars) > 0 && "file_name" %in% names(vars)) {
    var_counts <- as.data.frame(table(file_name = vars$file_name), stringsAsFactors = FALSE)
    names(var_counts)[names(var_counts) == "Freq"] <- "n_variables"
    
    files <- files |>
      dplyr::left_join(var_counts, by = "file_name")
  } else {
    files$n_variables <- 0L
  }
  
  files$n_variables[is.na(files$n_variables)] <- 0L
  
  res <- files |>
    dplyr::select(module_name = file_name, file_id, n_variables, format)
  
  for (i in seq_len(nrow(res))) {
    cli::cli_inform("{res$module_name[i]}: {res$n_variables[i]} variable{?s}")
  }
  
  invisible(res)
}
