# test-utils.R
# Tests for round validation helpers.

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

