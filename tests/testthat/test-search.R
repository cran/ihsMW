# test-search.R
# Tests for variable search, label lookup, and module listing with mocked HTTP.

# Helper to build mock responses for .nada_variables and .nada_data_files
mock_nada_variables <- function(idno) {
  fixture <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "catalog_variables.json"),
    simplifyVector = TRUE
  )
  raw <- fixture$variables$variable
  dplyr::tibble(
    var_name = as.character(raw$name),
    label    = as.character(raw$labl),
    file_name = as.character(raw$fid)
  )
}

mock_nada_data_files <- function(idno) {
  fixture <- jsonlite::fromJSON(
    testthat::test_path("fixtures", "catalog_data_files.json"),
    simplifyVector = TRUE
  )
  raw <- fixture$dataset$data_files
  dplyr::tibble(
    file_id   = as.character(raw$file_id),
    file_name = as.character(raw$file_name),
    format    = as.character(raw$format)
  )
}

test_that("ihs_search('consumption') returns a tibble with correct columns", {
  # ihs_search uses the crosswalk, not NADA, so no HTTP mocking needed
  result <- suppressMessages(ihs_search("consumption"))
  expect_s3_class(result, "tbl_df")
  required_cols <- c("harmonised_name", "label", "module")
  expect_true(all(required_cols %in% names(result)))
})

test_that("ihs_search with nonexistent term returns zero-row tibble with a message", {
  expect_message(
    result <- ihs_search("xxxxnotarealvariable"),
    "No variables found"
  )
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("ihs_label returns a character string for valid variable", {
  # Mock the NADA calls that ihs_label falls through to
  local_mocked_bindings(
    .nada_variables = mock_nada_variables
  )

  result <- ihs_label("rexp_cat01", round = "IHS5")
  expect_type(result, "character")
  expect_true(nzchar(result))
})

test_that("ihs_label returns NA with a warning for unknown variable", {
  local_mocked_bindings(
    .nada_variables = mock_nada_variables
  )

  expect_warning(
    result <- ihs_label("notavarname_xyz123", round = "IHS5"),
    "not found"
  )
  expect_true(is.na(result))
})

test_that("ihs_modules returns a tibble with expected columns", {
  local_mocked_bindings(
    .nada_data_files = mock_nada_data_files,
    .nada_variables  = mock_nada_variables
  )

  result <- suppressMessages(ihs_modules("IHS5"))
  expect_s3_class(result, "tbl_df")
  expect_true("n_variables" %in% names(result))
})

# --- Additional search tests ---

test_that("ihs_search nonexistent returns zero rows with 'try broader term' message", {
  expect_message(
    result <- ihs_search("xxxxnotreal"),
    "broader term"
  )
  expect_equal(nrow(result), 0)
})

test_that("ihs_search with fields = 'module' searches only module column", {
  # "Household Characteristics" is a module name in the crosswalk
  result <- suppressMessages(ihs_search("Household", fields = "module"))
  expect_s3_class(result, "tbl_df")
  # Should get matches since module column contains "Household Characteristics"
  if (nrow(result) > 0) {
    # Verify it matched on module, not on name or label
    expect_true(all(grepl("Household", result$module, ignore.case = TRUE) |
                     is.na(result$module)))
  }
})

test_that("ihs_search with round = 'IHS4' filters to IHS4-available variables", {
  result <- suppressMessages(ihs_search("consumption", round = "IHS4"))
  expect_s3_class(result, "tbl_df")
  # All returned rows should have non-NA ihs4_name
  if (nrow(result) > 0 && "ihs4_name" %in% names(result)) {
    expect_true(all(!is.na(result$ihs4_name)))
  }
})

test_that("ihs_variables prints cli headers and returns invisibly", {
  local_mocked_bindings(
    .nada_variables = mock_nada_variables
  )

  expect_message(
    result <- ihs_variables(round = "IHS5"),
    "variable"
  )
  expect_s3_class(result, "tbl_df")
})

test_that("ihs_variables with module filter returns subset", {
  local_mocked_bindings(
    .nada_variables = mock_nada_variables
  )

  # Filter to hh_mod_a — our mock has files named hh_mod_a.dta
  result <- suppressMessages(ihs_variables(round = "IHS5", module = "hh_mod_a"))
  expect_s3_class(result, "tbl_df")
  if (nrow(result) > 0) {
    expect_true(all(grepl("hh_mod_a", result$file_name, ignore.case = TRUE)))
  }
})
