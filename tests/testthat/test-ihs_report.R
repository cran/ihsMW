test_that("ihs_report summarizes numeric variables correctly", {
  df <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(10, 20, NA, 40, 50),
    category = c("A", "A", "B", "B", "C")
  )

  # Basic report
  res <- ihs_report(df, vars = c("x", "y"))
  expect_s3_class(res, "data.frame")
  expect_equal(res$variable, c("x", "y"))
  expect_equal(res$n, c(5, 4))
  expect_equal(res$mean, c(3, 30))
  expect_equal(res$pct_missing, c(0, 20))

  # Default to all numeric variables
  res_all <- ihs_report(df)
  expect_equal(res_all$variable, c("x", "y"))
})

test_that("ihs_report supports grouping by a variable", {
  df <- data.frame(
    x = c(10, 20, 30, 40),
    grp = c("A", "A", "B", "B")
  )

  res <- ihs_report(df, vars = "x", by = "grp")
  expect_equal(names(res)[1], "grp")
  expect_equal(res$grp, c("A", "B"))
  expect_equal(res$mean, c(15, 35))
})

test_that("ihs_report supports survey weighting", {
  df <- data.frame(
    x = c(10, 20, 30),
    w = c(1, 2, 1)
  )

  # Weighted mean: (10*1 + 20*2 + 30*1) / 4 = 80 / 4 = 20
  # Weighted variance: (1*(10-20)^2 + 2*(20-20)^2 + 1*(30-20)^2) / 4 = (100 + 0 + 100) / 4 = 50
  # Weighted SD: sqrt(50) = 7.0711
  res <- ihs_report(df, vars = "x", weights = "w")
  expect_equal(res$mean, 20)
  expect_equal(res$sd, round(sqrt(50), 4))
})

test_that("ihs_report validates input parameters", {
  df <- data.frame(
    x = c(1, 2, 3)
  )

  expect_error(ihs_report(123), "must be a data.frame")
  expect_error(ihs_report(df, vars = "nonexistent"), "Variables not found in data")
  expect_error(ihs_report(df, weights = "nonexistent"), "Weight column.*not found")
  expect_error(ihs_report(df, by = "nonexistent"), "Grouping variable.*not found")
})
