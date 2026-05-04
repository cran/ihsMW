# test-utils.R
# Tests for round/format validation helpers and cache directory management.

test_that("check_round('IHS5') passes silently", {
  expect_equal(check_round("IHS5"), "IHS5")
})

test_that("check_round('all') returns all supported rounds", {
  result <- check_round("all")
  expect_equal(result, c("IHS2", "IHS3", "IHS4", "IHS5"))
})

test_that("check_round(c('IHS4', 'IHS5')) passes silently", {
  expect_equal(check_round(c("IHS4", "IHS5")), c("IHS4", "IHS5"))
})

test_that("check_round('IHS6') throws ihsMW_bad_round", {
  expect_error(check_round("IHS6"), class = "ihsMW_bad_round")
})

test_that("check_round('IHS1') throws ihsMW_bad_round", {
  expect_error(check_round("IHS1"), class = "ihsMW_bad_round")
})

test_that("check_format('parquet') passes silently", {
  expect_equal(check_format("parquet"), "parquet")
})

test_that("check_format('xlsx') throws an error", {
  expect_error(check_format("xlsx"))
})

test_that("ihs_cache_dir() returns an existing directory", {
  dir <- ihs_cache_dir()
  expect_type(dir, "character")
  expect_true(dir.exists(dir))
})
