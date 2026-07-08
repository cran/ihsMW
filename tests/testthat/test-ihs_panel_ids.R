test_that("ihs_panel_ids returns named vector for single round", {
  ids <- ihs_panel_ids("IHS5")
  expect_type(ids, "character")
  expect_named(ids, c("hh_id", "indiv_id", "ea_id", "strata", "weight"))
  expect_equal(ids[["hh_id"]], "case_id")
  expect_equal(ids[["weight"]], "hh_wgt")
})

test_that("ihs_panel_ids returns different weights for IHS2 vs IHS5", {
  ids2 <- ihs_panel_ids("IHS2")
  ids5 <- ihs_panel_ids("IHS5")
  expect_equal(ids2[["weight"]], "hhweight")
  expect_equal(ids5[["weight"]], "hh_wgt")
})

test_that("ihs_panel_ids returns comparison data.frame for 'all'", {
  df <- ihs_panel_ids("all")
  expect_s3_class(df, "data.frame")
  expect_true("role" %in% names(df))
  expect_true(all(c("IHS2", "IHS3", "IHS4", "IHS5") %in% names(df)))
  expect_equal(nrow(df), 5)
})

test_that("ihs_panel_ids errors on invalid round", {
  expect_error(ihs_panel_ids("IHS9"), class = "ihsMW_bad_round")
})
