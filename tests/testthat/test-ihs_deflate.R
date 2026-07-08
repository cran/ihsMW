test_that("ihs_deflate creates _real columns with correct deflation", {
  df <- data.frame(
    consumption = c(1000, 2000),
    ihs_round = c("IHS4", "IHS5")
  )

  result <- ihs_deflate(df, value_cols = "consumption")
  expect_true("consumption_real" %in% names(result))

  # IHS5 (2019) -> base 2019, factor = 1.0

  expect_equal(result$consumption_real[result$ihs_round == "IHS5"], 2000)

  # IHS4 (2016) -> base 2019, factor = 100/69.1 ~ 1.447
  expect_true(result$consumption_real[result$ihs_round == "IHS4"] > 1000)
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
