test_that("ihs_svydesign creates survey design with explicit columns", {
  skip_if_not_installed("survey")

  df <- data.frame(
    case_id = 1:20,
    ea_id = rep(1:5, each = 4),
    stratum = rep(c("urban", "rural"), each = 10),
    hh_wgt = runif(20, 0.5, 3),
    consumption = rnorm(20, 5000, 1000)
  )

  dsgn <- ihs_svydesign(df, weight_col = "hh_wgt",
                          strata_col = "stratum", psu_col = "ea_id")
  expect_s3_class(dsgn, "survey.design2")
})

test_that("ihs_svydesign auto-detects standard columns", {
  skip_if_not_installed("survey")

  df <- data.frame(
    case_id = 1:20,
    ea_id = rep(1:5, each = 4),
    stratum = rep(c("urban", "rural"), each = 10),
    hh_wgt = runif(20, 0.5, 3),
    consumption = rnorm(20, 5000, 1000)
  )

  dsgn <- ihs_svydesign(df)
  expect_s3_class(dsgn, "survey.design2")
})

test_that("ihs_svydesign errors on non-data.frame", {
  expect_error(ihs_svydesign(list(a = 1)), "must be a data.frame")
})

test_that("ihs_svydesign errors when weight column not found", {
  df <- data.frame(x = 1:5)
  expect_error(ihs_svydesign(df, weight_col = "nonexistent"), "not found")
})

test_that("ihs_svydesign falls back to simple design without PSU/strata", {
  skip_if_not_installed("survey")

  df <- data.frame(
    hh_wgt = runif(10, 0.5, 3),
    consumption = rnorm(10, 5000, 1000)
  )

  dsgn <- ihs_svydesign(df)
  expect_s3_class(dsgn, "survey.design2")
})
