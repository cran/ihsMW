# test-cache.R
# Tests for cache info/clear operations using temporary directories.

test_that("ihs_cache_info() returns empty tibble when cache is empty", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)

  result <- ihs_cache_info()
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("ihs_cache_info() returns tibble with correct columns when files exist", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)

  # Create fake cached files
  ihs5_dir <- file.path(tmp_dir, "IHS5")
  dir.create(ihs5_dir, recursive = TRUE)
  saveRDS(data.frame(x = 1:5), file.path(ihs5_dir, "hh_mod_a.rds"))
  saveRDS(data.frame(y = 1:3), file.path(ihs5_dir, "consumption.rds"))

  result <- suppressMessages(ihs_cache_info())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(c("round", "module", "format", "size_mb", "cached_at") %in% names(result)))
  expect_true(all(result$round == "IHS5"))
  expect_true(all(result$format == "rds"))
})

test_that("ihs_cache_clear(round = 'IHS5') deletes only IHS5 files", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)

  # Create files in two rounds
  dir.create(file.path(tmp_dir, "IHS4"), recursive = TRUE)
  dir.create(file.path(tmp_dir, "IHS5"), recursive = TRUE)
  saveRDS(1, file.path(tmp_dir, "IHS4", "mod_a.rds"))
  saveRDS(2, file.path(tmp_dir, "IHS5", "mod_a.rds"))

  suppressMessages(ihs_cache_clear(round = "IHS5"))

  # IHS5 file gone, IHS4 file still present
  expect_false(file.exists(file.path(tmp_dir, "IHS5", "mod_a.rds")))
  expect_true(file.exists(file.path(tmp_dir, "IHS4", "mod_a.rds")))
})

test_that("ihs_cache_clear() with round = NULL clears everything in non-interactive mode", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)

  dir.create(file.path(tmp_dir, "IHS4"), recursive = TRUE)
  dir.create(file.path(tmp_dir, "IHS5"), recursive = TRUE)
  saveRDS(1, file.path(tmp_dir, "IHS4", "data.rds"))
  saveRDS(2, file.path(tmp_dir, "IHS5", "data.rds"))

  # In non-interactive mode, readline() is not called; the function proceeds
  suppressMessages(ihs_cache_clear(round = NULL))

  remaining <- list.files(tmp_dir, recursive = TRUE)
  expect_equal(length(remaining), 0)
})

test_that("ihs_cache_clear() reports correct stats", {
  tmp_dir <- withr::local_tempdir()
  local_mocked_bindings(ihs_cache_dir = function() tmp_dir)

  dir.create(file.path(tmp_dir, "IHS5"), recursive = TRUE)
  saveRDS(data.frame(x = rnorm(100)), file.path(tmp_dir, "IHS5", "data.rds"))

  expect_message(
    ihs_cache_clear(round = "IHS5"),
    "Cleared"
  )
})
