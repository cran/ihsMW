# test-IHS.R
# Tests for the main IHS() entry point with mocked .ihs_fetch() data fetcher.

test_that("IHS returns a data.frame for a valid single-round request", {
  local_mocked_bindings(.ihs_fetch = mock_ihs_fetch)

  result <- suppressMessages(IHS("rexp_cat01", round = "IHS5"))
  expect_s3_class(result, "data.frame")
})

test_that("IHS result has an ihs_round column", {
  local_mocked_bindings(.ihs_fetch = mock_ihs_fetch)

  result <- suppressMessages(IHS("rexp_cat01", round = "IHS5"))
  expect_true("ihs_round" %in% names(result))
  expect_equal(unique(result$ihs_round), "IHS5")
})

test_that("IHS with return = 'list' returns a named list", {
  local_mocked_bindings(.ihs_fetch = mock_ihs_fetch)

  result <- suppressMessages(IHS("rexp_cat01", round = "IHS5", return = "list"))
  expect_type(result, "list")
  expect_named(result)
  expect_true("IHS5" %in% names(result))
})

test_that("IHS with extra = FALSE excludes weight/stratum columns", {
  local_mocked_bindings(.ihs_fetch = mock_ihs_fetch)

  result <- suppressMessages(IHS("rexp_cat01", round = "IHS5", extra = FALSE))
  # Weight/strata columns should not be in output when extra = FALSE
  extra_cols <- c("strata", "hh_wgt")
  expect_false(any(extra_cols %in% names(result)))
})

test_that("IHS with extra = TRUE includes all columns from the module", {
  local_mocked_bindings(.ihs_fetch = mock_ihs_fetch)

  result <- suppressMessages(IHS("rexp_cat01", round = "IHS5", extra = TRUE))
  # When extra = TRUE, all columns from the fetched data should be present
  expect_true(ncol(result) > 2)
})

test_that("IHS with an invalid round throws ihsMW_bad_round", {
  expect_error(
    IHS("rexp_cat01", round = "IHS99"),
    class = "ihsMW_bad_round"
  )
})

test_that("IHS with an unknown indicator aborts with meaningful error", {
  local_mocked_bindings(.ihs_fetch = mock_ihs_fetch)

  # IHS() first warns about the unknown indicator, then aborts because

  # no valid indicator/round combinations remain after filtering.
  expect_error(
    suppressMessages(
      withCallingHandlers(
        IHS("totally_fake_indicator_xyz", round = "IHS5"),
        warning = function(w) {
          expect_match(conditionMessage(w), "not found")
          invokeRestart("muffleWarning")
        }
      )
    ),
    "No valid indicator"
  )
})
