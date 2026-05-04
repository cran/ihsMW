# test-download.R
# Tests for .ihs_fetch() download logic with mocked network and file system.

# Small mock data_files response
mock_data_files_for_dl <- function(idno) {
  dplyr::tibble(
    file_id   = c("F1", "F2"),
    file_name = c("hh_mod_a_filt.dta", "consumption_aggregate.dta"),
    format    = c("Stata", "Stata")
  )
}

test_that(".ihs_fetch returns a data.frame when mock download succeeds", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)
  local_mocked_bindings(.nada_data_files = mock_data_files_for_dl)

  # Mock the download pipeline: .nada_req, httr2::req_perform, etc.
  test_df <- data.frame(case_id = c("A", "B"), val = c(1, 2))
  local_mocked_bindings(
    req_perform = function(...) httr2::response(status_code = 200, body = charToRaw("{}")),
    resp_status = function(...) 200L,
    resp_body_raw = function(...) serialize(test_df, NULL),
    .package = "httr2"
  )

  # Also mock .read_cached to return test data from the cache path
  local_mocked_bindings(.read_cached = function(path, format) test_df)
  # Mock .nada_req to return a basic request
  local_mocked_bindings(.nada_req = function(path, auth = FALSE) {
    req <- httr2::request("https://example.com/mock")
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    req
  })

  result <- suppressMessages(.ihs_fetch("IHS5", "hh_mod_a", format = "rds", cache = TRUE))
  expect_s3_class(result, "data.frame")
})

test_that(".ihs_fetch loads from cache on second call", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)

  # Pre-populate cache
  cache_subdir <- file.path(tmp_dir, "IHS5")
  dir.create(cache_subdir, recursive = TRUE)
  test_df <- data.frame(case_id = c("X", "Y"), val = c(10, 20))
  saveRDS(test_df, file.path(cache_subdir, "hh_mod_a.rds"))

  # Should NOT call .nada_data_files at all since cached
  result <- suppressMessages(.ihs_fetch("IHS5", "hh_mod_a", format = "rds", cache = TRUE))
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$case_id, c("X", "Y"))
})

test_that(".ihs_fetch throws ihsMW_module_not_found for missing module", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)
  local_mocked_bindings(.nada_data_files = mock_data_files_for_dl)

  expect_error(
    suppressMessages(.ihs_fetch("IHS5", "totally_nonexistent_module", format = "rds")),
    class = "ihsMW_module_not_found"
  )
})

test_that(".ihs_fetch constructs correct cache path", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)

  expected_path <- file.path(tmp_dir, "IHS5", "hh_mod_a.rds")

  # Pre-populate the exact expected path
  dir.create(file.path(tmp_dir, "IHS5"), recursive = TRUE)
  saveRDS(data.frame(x = 1), expected_path)

  # Should load from the exact expected path
  result <- suppressMessages(.ihs_fetch("IHS5", "hh_mod_a", format = "rds", cache = TRUE))
  expect_s3_class(result, "data.frame")
})

test_that(".read_cached handles rds format", {
  tmp <- withr::local_tempfile(fileext = ".rds")
  test_df <- data.frame(a = 1:3, b = letters[1:3])
  saveRDS(test_df, tmp)

  result <- .read_cached(tmp, "rds")
  expect_equal(result, test_df)
})

test_that(".read_cached handles csv format", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  test_df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  readr::write_csv(test_df, tmp)

  result <- .read_cached(tmp, "csv")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
})

test_that(".read_cached throws error for unknown format", {
  expect_error(.read_cached("fake.xlsx", "xlsx"), "Unknown format")
})
