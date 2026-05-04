#' Check Crosswalk Health
#'
#' @description
#' Evaluates the ihsMW crosswalk variable map. Prints a formatted report
#' indicating how many variables are present across rounds, and flags any
#' variables needing manual review.
#'
#' @param verbose Logical. If \code{TRUE} (default), prints the report to the console using \code{message()}.
#'
#' @return A \code{tibble} containing the master crosswalk, returned invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' cw <- ihs_crosswalk_check()
#' }
ihs_crosswalk_check <- function(verbose = TRUE) {
  cw <- .load_crosswalk()
  total_vars <- nrow(cw)
  
  all_names_cols <- grep("^ihs[1-5]_name$", names(cw), value = TRUE)
  cw$n_rounds_avail <- rowSums(!is.na(cw[, all_names_cols, drop = FALSE]))
  max_rounds <- length(all_names_cols)
  
  all_max_rounds <- sum(cw$n_rounds_avail == max_rounds)
  all_max_pct <- round(100 * all_max_rounds / total_vars, 1)
  
  if (verbose) {
    cli::cli_h1("Crosswalk Check Report")
    cli::cli_text("Total harmonised variables: {.val {total_vars}}")
    cli::cli_text("Variables present in all {max_rounds} rounds: {.val {all_max_rounds}} ({all_max_pct}%)")
    
    cli::cli_h2("Variables by Availability")
    for (i in max_rounds:1) {
      cnt <- sum(cw$n_rounds_avail == i)
      bars <- paste(rep("\u2588", round((cnt / total_vars) * 20)), collapse = "")
      cli::cli_text("{i} round{?s}: {bars} ({cnt})")
    }
    
    needs_review_cnt <- sum(cw$needs_review, na.rm = TRUE)
    if (needs_review_cnt > 0) {
      cli::cli_h2("Variables Needing Review")
      cli::cli_alert_warning("{needs_review_cnt} variable{?s} flagged for review:")
      review_vars <- cw[cw$needs_review, ]
      
      # Use message instead of print for CRAN compliance
      msg_out <- paste(utils::capture.output(print(utils::head(review_vars[, c("harmonised_name", "label", "topic")], 5))), collapse = "\n")
      message(msg_out)
      
      if (needs_review_cnt > 5) cli::cli_text("... and {needs_review_cnt - 5} more.")
      
      cli::cli_alert_info("Review {.file data-raw/ihs_crosswalk_working.csv} to resolve flags.")
    } else {
      cli::cli_alert_success("No variables flagged for review! Crosswalk is clean.")
    }
  }
  
  invisible(cw)
}

#' @noRd
.load_crosswalk <- function() {
  cache_key <- "crosswalk"
  if (exists(cache_key, envir = .nada_cache)) {
    return(get(cache_key, envir = .nada_cache))
  }
  
  cw_path <- system.file("extdata", "ihs_crosswalk.csv", package = "ihsMW")
  if (!file.exists(cw_path)) {
    cw_path <- "inst/extdata/ihs_crosswalk.csv"
    if (!file.exists(cw_path)) {
       cli::cli_abort("Could not locate ihs_crosswalk.csv. Is the package installed properly?")
    }
  }
  
  cw <- readr::read_csv(cw_path, show_col_types = FALSE)
  assign(cache_key, cw, envir = .nada_cache)
  cw
}

#' @noRd
.resolve_indicators <- function(indicator, round) {
  cw <- .load_crosswalk()
  col_name <- paste0(tolower(round), "_name")
  
  map <- character()
  
  for (ind in indicator) {
    cw_row <- cw[cw$harmonised_name == ind, ]
    
    if (nrow(cw_row) == 0) {
      cli::cli_warn("'{ind}' not found in the crosswalk. Skipping.")
      map[ind] <- NA_character_
    } else {
      orig_name <- as.character(cw_row[[col_name]][1])
      
      if (is.na(orig_name)) {
        cli::cli_warn("'{ind}' is not available in {round}. Skipping.")
        map[ind] <- NA_character_
      } else {
        map[ind] <- orig_name
      }
      
      if (isTRUE(cw_row$needs_review[1])) {
        cli::cli_warn(c(
          "'{ind}' is flagged for manual review in the crosswalk.",
          "i" = "Check {.fn ihs_crosswalk_check} for details before interpreting results."
        ))
      }
    }
  }
  
  map
}

#' @noRd
.apply_harmonisation <- function(df, round, indicator) {
  map <- .resolve_indicators(indicator, round)
  
  map <- map[!is.na(map)]
  if (length(map) == 0) return(df)
  
  df_names_lower <- tolower(names(df))
  
  labels <- list()
  for (i in seq_along(df)) {
    if (!is.null(attr(df[[i]], "label"))) {
      labels[[names(df)[i]]] <- attr(df[[i]], "label")
    }
  }
  
  for (i in seq_along(map)) {
    ind <- names(map)[i]
    orig_name <- map[i]
    
    match_idx <- which(df_names_lower == tolower(orig_name))
    if (length(match_idx) > 0) {
      names(df)[match_idx[1]] <- ind
      
      orig_actual_name <- names(labels)[tolower(names(labels)) == tolower(orig_name)]
      if (length(orig_actual_name) > 0) {
         attr(df[[ind]], "label") <- labels[[orig_actual_name[1]]]
      }
    }
  }
  
  df
}

#' @noRd
.bind_rounds <- function(round_list) {
  for (r in names(round_list)) {
     if (nrow(round_list[[r]]) > 0) {
        round_list[[r]]$ihs_round <- r
     }
  }
  
  all_cols <- unique(unlist(lapply(round_list, names)))
  
  for (col in all_cols) {
    col_data <- list()
    rounds_with_col <- c()
    for (r in names(round_list)) {
      if (col %in% names(round_list[[r]])) {
        col_data[[r]] <- round_list[[r]][[col]]
        rounds_with_col <- c(rounds_with_col, r)
      }
    }
    
    if (length(col_data) > 1) {
      types <- vapply(col_data, function(x) class(x)[1], character(1))
      if (length(unique(types)) > 1) {
         msg <- paste0(rounds_with_col, " (", types, ")", collapse = ", ")
         cli::cli_warn("Type mismatch for column {.val {col}}: {msg}. Coercing to most general type using vctrs::vec_cast_common().")
         
         common <- tryCatch({
            rlang::exec(vctrs::vec_cast_common, !!!col_data)
         }, error = function(e) {
            lapply(col_data, as.character)
         })
         
         for (r_idx in seq_along(rounds_with_col)) {
           r <- rounds_with_col[r_idx]
           round_list[[r]][[col]] <- common[[r_idx]]
         }
      }
    }
  }
  
  dplyr::bind_rows(round_list)
}
