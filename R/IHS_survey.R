#' @noRd
# NOTE: These should be verified against the Basic Information Documents for each
# round and may need adjustment.
.ihs_weight_vars <- tibble::tribble(
  ~round,  ~weight_var,  ~strata_var,    ~cluster_var, ~notes,
  "IHS2",  "hhwght",     "strata",       "psu",        "Verified from data columns",
  "IHS3",  "hh_wgt",     "strata",       "ea_id",      "Verified from data columns",
  "IHS4",  "hh_wgt",     NA_character_,  "ea_id",      "strata_var not found in data - verify against BID before use",
  "IHS5",  "hh_wgt",     NA_character_,  "ea_id",      "strata_var not found in data - verify against BID before use"
)

#' Create a survey design object for Malawi IHS data
#'
#' @description
#' Creates a complex survey design object using the \code{survey} and \code{srvyr} packages.
#' Automatically incorporates the appropriate sampling weights, strata, and clusters
#' for the requested round to enable statistically sound national estimations natively.
#'
#' @param indicator Character vector of harmonised variable names.
#' @param round A single round string (e.g. \code{"IHS5"}) or \code{"all"}.
#' @param ... Additional arguments passed to \code{\link{IHS}}, such as \code{module} or \code{format}.
#'
#' @return A \code{tbl_svy} object if the \code{srvyr} package is installed,
#'   otherwise a \code{svydesign} object from the \code{survey} package. If multiple
#'   rounds are requested, returns a named list of survey objects.
#'
#' @note
#' Survey weights differ across IHS rounds and reflect the complex sample design
#' of each survey. Estimates produced using this function are representative at
#' the national, urban/rural, regional, and district level for each round
#' independently. Do not pool weights across rounds without consulting the
#' relevant Basic Information Document for each round.
#' Cite the sampling methodology: NSO Malawi (year), IHS[N] Basic Information
#' Document. National Statistical Office, Zomba, Malawi.
#'
#' @examples
#' \dontrun{
#'   svy <- IHS_survey("rexp_cat01", round = "IHS5")
#'   survey::svymean(~rexp_cat01, design = svy)
#'   svy |> srvyr::summarise(mean_cons = srvyr::survey_mean(rexp_cat01))
#' }
#' 
#' @export
IHS_survey <- function(indicator, round = "IHS5", ...) {
  round <- check_round(round)
  
  if (length(round) > 1) {
    if (identical(sort(round), sort(.IHS_ROUNDS))) {
      cli::cli_warn(c(
        "Survey objects cannot be pooled across rounds automatically.",
        "i" = "Returning a list of survey objects, one per round."
      ))
    }
    
    out_list <- list()
    for (r in round) {
      out_list[[r]] <- IHS_survey(indicator = indicator, round = r, ...)
    }
    return(out_list)
  }
  
  if (!rlang::is_installed("survey")) {
    cli::cli_abort("The {.pkg survey} package is required to create survey designs. Please install it.")
  }
  
  # Fetch data ensuring extra fields (like weights) are acquired properly targeting standard data.frame outputs
  df <- IHS(indicator = indicator, round = round, extra = TRUE, return = "data.frame", ...)
  
  wt_info <- .ihs_weight_vars[.ihs_weight_vars$round == round, ]
  if (nrow(wt_info) == 0) {
    cli::cli_abort("Survey design information not found for {.val {round}}.")
  }
  
  col_w <- wt_info$weight_var[1]
  col_s <- wt_info$strata_var[1]
  col_c <- wt_info$cluster_var[1]
  
  missing_cols <- c()
  df_names <- tolower(names(df))
  
  if (!is.na(col_w) && !tolower(col_w) %in% df_names) missing_cols <- c(missing_cols, col_w)
  if (!is.na(col_s) && !tolower(col_s) %in% df_names) missing_cols <- c(missing_cols, col_s)
  if (!is.na(col_c) && !tolower(col_c) %in% df_names) missing_cols <- c(missing_cols, col_c)
  
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing survey design variables in the downloaded data: {.val {missing_cols}}",
      "i" = "These variables are required to create a survey object for {.val {round}}.",
      ">" = "Ensure the relevant module containing these variables is available."
    ))
  }
  
  # Align case map properly with targeted string properties
  col_w_actual <- if (!is.na(col_w)) names(df)[df_names == tolower(col_w)][1] else NA
  col_s_actual <- if (!is.na(col_s)) names(df)[df_names == tolower(col_s)][1] else NA
  col_c_actual <- if (!is.na(col_c)) names(df)[df_names == tolower(col_c)][1] else NA
  
  f_weights <- if (!is.na(col_w_actual)) stats::as.formula(paste0("~", col_w_actual)) else NULL
  f_strata  <- if (!is.na(col_s_actual)) stats::as.formula(paste0("~", col_s_actual)) else NULL
  f_cluster <- if (!is.na(col_c_actual)) stats::as.formula(paste0("~", col_c_actual)) else ~1
  
  svy <- survey::svydesign(
    ids = f_cluster,
    strata = f_strata,
    weights = f_weights,
    data = df,
    nest = TRUE
  )
  
  if (rlang::is_installed("srvyr")) {
    svy <- srvyr::as_survey_design(svy)
    return(svy)
  } else {
    cli::cli_inform("The {.pkg srvyr} package is strongly recommended for tidy-style survey analysis. Returning a base {.cls svydesign} object.")
    return(svy)
  }
}
