#' @title Download Malawi IHS microdata
#' @description
#'   The main interface to the ihsMW package. Downloads one or more IHS variables
#'   across one or more survey rounds, applies cross-round harmonisation, and
#'   returns the data in the requested format.
#'
#' @param indicator Character vector of harmonised variable names. Use
#'   \code{\link{ihs_search}} to discover variable names.
#' @param round Character vector of IHS rounds to include. One or more of
#'   \code{"IHS2"}, \code{"IHS3"}, \code{"IHS4"}, \code{"IHS5"}, or \code{"all"}.
#'   Note: IHS1 is not currently available via the API. Default: \code{"IHS5"}.
#' @param module Optional character string to restrict to a specific module.
#'   If NULL (default), the correct module is determined automatically from
#'   the crosswalk.
#' @param return Output format: \code{"data.frame"} (default), \code{"list"},
#'   or \code{"survey"}.
#' @param format File format for download and caching: \code{"parquet"} (default),
#'   \code{"rds"}, or \code{"csv"}.
#' @param cache Logical. If TRUE (default), use and populate the disk cache.
#' @param extra Logical. If FALSE (default), return only the requested indicator
#'   columns plus household ID columns. If TRUE, include all variables in the
#'   downloaded module (stratum, cluster, weights, etc.).
#'
#' @return
#'   If \code{return = "data.frame"}: a single \code{data.frame} with an
#'   \code{ihs_round} column.\cr
#'   If \code{return = "list"}: a named list of \code{data.frame}s, one per round.\cr
#'   If \code{return = "survey"}: a \code{tbl_svy} or \code{svydesign} object.
#'
#' @examples
#' \dontrun{
#'   # One-time setup
#'   ihs_auth()
#'
#'   # Download a single variable from the latest round
#'   df <- IHS("rexp_cat01", round = "IHS5")
#'
#'   # Multiple variables, multiple rounds
#'   df <- IHS(c("rexp_cat01", "hh_a02"), round = c("IHS4", "IHS5"))
#'
#'   # All supported rounds
#'   df <- IHS("rexp_cat01", round = "all")
#'
#'   # Return as a named list of data.frames
#'   lst <- IHS("rexp_cat01", round = c("IHS3", "IHS4", "IHS5"), return = "list")
#'
#'   # Include weights and design variables
#'   df <- IHS("rexp_cat01", round = "IHS5", extra = TRUE)
#'
#'   # Use rds format instead of parquet
#'   df <- IHS("rexp_cat01", round = "IHS5", format = "rds")
#' }
#'
#' @seealso
#'   \code{\link{ihs_search}} to find variable names.\cr
#'   \code{IHS_survey} for weighted survey analysis.\cr
#'   \code{ihs_crosswalk_check} to assess cross-round comparability.
#'
#' @export
IHS <- function(
  indicator,
  round    = "IHS5",
  module   = NULL,
  return   = c("data.frame", "list", "survey"),
  format   = c("parquet", "rds", "csv"),
  cache    = TRUE,
  extra    = FALSE
) {
  # STEP 1: VALIDATE ARGUMENTS
  return <- rlang::arg_match(return)
  format <- rlang::arg_match(format)
  round  <- check_round(round)
  
  if (missing(indicator) || length(indicator) == 0) {
    cli::cli_abort(c(
      "Please supply at least one variable name to {.arg indicator}.",
      ">" = "Use {.fn ihs_search} to find variable names.",
      "i" = 'Example: {.code ihs_search("consumption")}'
    ))
  }
  
  # STEP 2: RESOLVE MODULES FROM INDICATORS
  cw <- .load_crosswalk()
  
  valid_indicators <- c()
  indic_modules <- list()
  
  for (ind in indicator) {
    if (!ind %in% cw$harmonised_name) {
      cli::cli_warn(c(
        "'{ind}' not found in the crosswalk. Skipping.",
        "Run {.code ihs_search('{ind}')} to check the name."
      ))
    } else {
      valid_indicators <- c(valid_indicators, ind)
      
      # Determine specific file module name explicitly bypassing standard when needed
      m <- if (!is.null(module)) module else cw$module[cw$harmonised_name == ind][1]
      indic_modules[[ind]] <- m
    }
  }
  
  indicator <- valid_indicators
  
  plan <- dplyr::tibble(round = character(), module = character(), indicators_needed = list())
  
  for (r in round) {
    col_name <- paste0(tolower(r), "_name")
    round_plan <- list()
    
    for (ind in indicator) {
      ind_row <- cw[cw$harmonised_name == ind, ]
      if (col_name %in% names(ind_row) && !is.na(ind_row[[col_name]])) {
        m <- indic_modules[[ind]]
        
        if (is.null(round_plan[[m]])) round_plan[[m]] <- c()
        round_plan[[m]] <- c(round_plan[[m]], ind)
      } else {
        cli::cli_warn("'{ind}' is not available in {r}. Skipping for that round.")
      }
    }
    
    for (m in names(round_plan)) {
      plan <- dplyr::bind_rows(plan, dplyr::tibble(
        round = r,
        module = m,
        indicators_needed = list(round_plan[[m]])
      ))
    }
  }
  
  if (nrow(plan) == 0) {
    cli::cli_abort("No valid indicator/round combinations found. Aborting.")
  }
  
  # STEP 3: FETCH DATA
  if (nrow(plan) > 1) {
    cli::cli_progress_bar(name = "Downloading Modules", total = nrow(plan))
  }
  
  results <- list()
  
  for (i in seq_len(nrow(plan))) {
    r <- plan$round[i]
    m <- plan$module[i]
    
    df_raw <- .ihs_fetch(r, m, format, cache, progress = (nrow(plan) == 1))
    
    if (nrow(plan) > 1) cli::cli_progress_update()
    
    if (is.null(results[[r]])) results[[r]] <- list()
    results[[r]][[m]] <- df_raw
  }
  
  if (nrow(plan) > 1) cli::cli_progress_done()
  
  # STEP 4: HARMONISE AND FILTER COLUMNS
  id_cols <- c("case_id", "hhid", "hh_id", "ea_id")
  
  for (r in names(results)) {
    for (m in names(results[[r]])) {
      df <- results[[r]][[m]]
      inds_needed <- plan$indicators_needed[plan$round == r & plan$module == m][[1]]
      
      df <- .apply_harmonisation(df, r, inds_needed)
      
      keep_cols <- names(df)
      if (!extra) {
        keep_ids <- keep_cols[tolower(keep_cols) %in% id_cols]
        keep_cols <- unique(c(keep_ids, inds_needed))
        keep_cols <- keep_cols[keep_cols %in% names(df)]
        df <- df[, keep_cols, drop = FALSE]
      }
      
      results[[r]][[m]] <- df
    }
  }
  
  # STEP 5: ASSEMBLE OUTPUT BY ROUND
  round_results <- list()
  for (r in names(results)) {
    modules_list <- results[[r]]
    
    if (length(modules_list) == 1) {
      round_results[[r]] <- modules_list[[1]]
    } else {
      base_df <- modules_list[[1]]
      
      for (i in 2:length(modules_list)) {
        next_df <- modules_list[[i]]
        intersecting_ids <- intersect(tolower(names(base_df)), tolower(names(next_df)))
        intersecting_ids <- intersecting_ids[intersecting_ids %in% id_cols]
        
        if (length(intersecting_ids) > 0) {
          by_cols <- sapply(intersecting_ids, function(id_lower) {
            names(next_df)[tolower(names(next_df)) == id_lower][1]
          })
          names(by_cols) <- sapply(intersecting_ids, function(id_lower) {
            names(base_df)[tolower(names(base_df)) == id_lower][1]
          })
          
          pre_join_rows <- nrow(base_df)
          base_df <- dplyr::left_join(base_df, next_df, by = by_cols)
          
          if (nrow(base_df) != pre_join_rows) {
            cli::cli_warn("Join produced unexpected row counts in {r}.")
          }
        } else {
          cli::cli_warn("No common ID columns found to join modules in {r}.")
        }
      }
      
      round_results[[r]] <- base_df
    }
  }
  
  # STEP 6 & 7: RETURN IN REQUESTED FORMAT
  if (return == "data.frame") {
    out <- .bind_rounds(round_results)
    
    cli::cli_inform(c(
      "v" = "Downloaded {length(indicator)} variable{?s} from {length(round_results)} round{?s} of the Malawi IHS.",
      "i" = "Rows: {nrow(out)} | Columns: {ncol(out)}",
      "i" = "Survey weights not included. Use {.fn IHS_survey} for weighted analysis.",
      "i" = "Please cite: NSO Malawi / World Bank LSMS Program."
    ))
    
    return(out)
  } else if (return == "list") {
    cli::cli_inform(c(
      "v" = "Downloaded {length(indicator)} variable{?s} from {length(round_results)} round{?s} of the Malawi IHS.",
      "i" = "Survey weights not included. Use {.fn IHS_survey} for weighted analysis.",
      "i" = "Please cite: NSO Malawi / World Bank LSMS Program."
    ))
    
    cli::cli_inform(
      "Returning a list of {length(round_results)} data.frame{?s}, one per round: {.val {names(round_results)}}"
    )
    return(invisible(round_results))
  } else if (return == "survey") {
    return(IHS_survey(indicator = indicator, round = round, module = module, format = format, cache = cache))
  }
}


