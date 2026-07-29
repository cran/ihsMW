test_that("ihs_deflate creates _real columns with correct deflation", {
  df <- data.frame(
    consumption = c(1000, 2000),
    ihs_round = c("IHS4", "IHS5")
  )

  result <- ihs_deflate(df, value_cols = "consumption")
  expect_true("consumption_real" %in% names(result))

  # IHS5 (2019) -> base 2019, factor = 1.0

  expect_equal(result$consumption_real[result$ihs_round == "IHS5"], 2000)

  # IHS4 (2016) -> base 2019: prices rose, so real 2019 value exceeds nominal
  expect_true(result$consumption_real[result$ihs_round == "IHS4"] > 1000)
})

test_that("ihs_deflate handles IHS6 and orders rounds monotonically", {
  df <- data.frame(
    v = rep(1000, 5),
    ihs_round = c("IHS2", "IHS3", "IHS4", "IHS5", "IHS6")
  )
  result <- ihs_deflate(df, value_cols = "v")

  # Malawi CPI rises monotonically over 2004-2024, so a fixed nominal amount
  # buys steadily less: deflated values must fall across successive rounds.
  expect_true(all(diff(result$v_real) < 0))

  # IHS6 (2024) is after the 2019 base, so 1000 nominal is worth < 1000 real
  expect_true(result$v_real[result$ihs_round == "IHS6"] < 1000)
  expect_true(result$v_real[result$ihs_round == "IHS5"] == 1000)
})

test_that("ihs_deflate rejects an unknown round in ihs_round", {
  df <- data.frame(v = 1000, ihs_round = "IHS9")
  expect_error(ihs_deflate(df, value_cols = "v"), class = "ihsMW_bad_round")
})

test_that("the bundled CPI table carries provenance", {
  cpi <- .load_cpi()
  expect_true(all(c("year", "cpi_index", "source", "retrieved", "provisional")
                  %in% names(cpi)))
  expect_equal(cpi$cpi_index[cpi$year == 2019], 100)
  expect_true(all(diff(cpi$cpi_index) > 0))
  # The most recent years are the ones the World Bank may still revise.
  expect_true(all(cpi$year[cpi$provisional] >= max(cpi$year) - 1))
})

test_that("ihs_deflate flags a provisional CPI year but still deflates", {
  # IHS6 maps to 2024, which is provisional in the bundled series.
  expect_message(
    out <- ihs_deflate(data.frame(v = 1000), value_cols = "v", round = "IHS6"),
    "provisional"
  )
  expect_true(is.finite(out$v_real))

  # IHS5 maps to 2019, long settled - no such message.
  expect_no_message(
    ihs_deflate(data.frame(v = 1000), value_cols = "v", round = "IHS5"),
    message = "provisional"
  )
})

test_that(".warn_provisional is silent on a table without the flag", {
  # Older bundled crosswalks/CPI tables have no `provisional` column.
  old <- data.frame(year = 2019, cpi_index = 100)
  expect_silent(
    .warn_provisional(old, .IHS_ROUND_YEARS, "IHS5", 2019)
  )
})

test_that("ihs_deflate works with explicit round argument", {
  df <- data.frame(expenditure = c(500, 1000))

  result <- ihs_deflate(df, value_cols = "expenditure", round = "IHS3")
  expect_true("expenditure_real" %in% names(result))

  # IHS3 (2010), factor = 100/22.1 ~ 4.525
  expect_true(all(result$expenditure_real > result$expenditure))
})

test_that("ihs_deflate errors on missing columns", {
  df <- data.frame(x = 1)
  expect_error(ihs_deflate(df, value_cols = "nonexistent"), "not found")
})

test_that("ihs_deflate errors without round information", {
  df <- data.frame(consumption = 100)
  expect_error(ihs_deflate(df, value_cols = "consumption"), "Cannot determine")
})

test_that("ihs_deflate handles multiple value columns", {
  df <- data.frame(
    food = c(100, 200),
    nonfood = c(50, 75),
    ihs_round = c("IHS5", "IHS5")
  )

  result <- ihs_deflate(df, value_cols = c("food", "nonfood"))
  expect_true("food_real" %in% names(result))
  expect_true("nonfood_real" %in% names(result))
  # Base year 2019 = IHS5, factor = 1.0
  expect_equal(result$food_real, result$food)
})
