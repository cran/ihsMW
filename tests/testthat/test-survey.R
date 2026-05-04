# test-survey.R
# Tests for IHS_survey() survey design object creation with mocked IHS().

mock_survey_df_5 <- data.frame(
  case_id  = paste0("HH", 1:20),
  rexp_cat01 = runif(20, 10000, 500000),
  hh_wgt = runif(20, 0.5, 3.0),
  ea_id    = paste0("EA", rep(1:10, each = 2)),
  ihs_round = rep("IHS5", 20),
  stringsAsFactors = FALSE
)

mock_survey_df_3 <- data.frame(
  case_id  = paste0("HH", 1:20),
  rexp_cat01 = runif(20, 10000, 500000),
  hh_wgt = runif(20, 0.5, 3.0),
  strata  = rep(c("STR1", "STR2", "STR3", "STR4"), each = 5),
  ea_id    = paste0("EA", rep(1:10, each = 2)),
  ihs_round = rep("IHS3", 20),
  stringsAsFactors = FALSE
)

test_that("IHS_survey returns a survey object for IHS5 (NA strata) and IHS3 (all vars)", {
  local_mocked_bindings(IHS = function(indicator, round, ...) {
    if (round == "IHS5") return(mock_survey_df_5)
    if (round == "IHS3") return(mock_survey_df_3)
  })

  res5 <- suppressMessages(IHS_survey("rexp_cat01", round = "IHS5"))
  res3 <- suppressMessages(IHS_survey("rexp_cat01", round = "IHS3"))

  if (rlang::is_installed("srvyr")) {
    expect_s3_class(res5, "tbl_svy")
    expect_s3_class(res3, "tbl_svy")
  } else {
    expect_s3_class(res5, "survey.design")
    expect_s3_class(res3, "survey.design")
  }
})

test_that("IHS_survey returns list and warns when multiple rounds requested", {
  mock_IHS_multiround <- function(indicator, round, ...) {
    wt_info <- .ihs_weight_vars[.ihs_weight_vars$round == round, ]
    df <- data.frame(
      case_id  = paste0("HH", 1:20),
      rexp_cat01 = runif(20, 10000, 500000),
      ihs_round = rep(round, 20),
      stringsAsFactors = FALSE
    )
    if (!is.na(wt_info$weight_var[1])) df[[wt_info$weight_var[1]]] <- runif(20, 0.5, 3.0)
    if (!is.na(wt_info$strata_var[1])) df[[wt_info$strata_var[1]]] <- rep(c("STR1", "STR2", "STR3", "STR4"), each = 5)
    if (!is.na(wt_info$cluster_var[1])) df[[wt_info$cluster_var[1]]] <- paste0("EA", rep(1:10, each = 2))
    df
  }
  local_mocked_bindings(IHS = mock_IHS_multiround)

  expect_warning(
    result <- suppressMessages(IHS_survey("rexp_cat01", round = "all")),
    "cannot be pooled"
  )
  expect_type(result, "list")
  expect_true(length(result) >= 2)
})

test_that("IHS_survey aborts when weight columns are missing", {
  local_mocked_bindings(IHS = function(...) {
    df <- mock_survey_df_5
    df$hh_wgt <- NULL
    df
  })
  expect_error(
    suppressMessages(IHS_survey("rexp_cat01", round = "IHS5")),
    "Missing survey design variables.*hh_wgt"
  )
})

test_that("IHS_survey aborts when strata column is missing (for a round targeting one)", {
  local_mocked_bindings(IHS = function(...) {
    df <- mock_survey_df_3
    df$strata <- NULL
    df
  })
  expect_error(
    suppressMessages(IHS_survey("rexp_cat01", round = "IHS3")),
    "Missing survey design variables.*strata"
  )
})

test_that(".ihs_weight_vars has entries for all supported rounds", {
  for (r in c("IHS2", "IHS3", "IHS4", "IHS5")) {
    wt_row <- .ihs_weight_vars[.ihs_weight_vars$round == r, ]
    expect_equal(nrow(wt_row), 1, info = paste("Missing weight info for", r))
    expect_true(!is.na(wt_row$weight_var) && nzchar(wt_row$weight_var), info = paste("Empty weight_var for", r))
    expect_true(!is.na(wt_row$cluster_var) && nzchar(wt_row$cluster_var), info = paste("Empty cluster_var for", r))
  }
})
