# test-crosswalk.R
# Tests for crosswalk loading, indicator resolution, health checks, and round binding.

test_that(".load_crosswalk() returns a tibble with required columns", {
  cw <- .load_crosswalk()
  expect_s3_class(cw, "tbl_df")
  expect_true(all(c("harmonised_name", "label", "module") %in% names(cw)))
})

test_that(".resolve_indicators returns a named character vector for valid indicator", {
  result <- .resolve_indicators("rexp_cat01", "IHS5")
  expect_type(result, "character")
  expect_named(result)
  expect_equal(names(result), "rexp_cat01")
})

test_that(".resolve_indicators returns NA with a warning for nonexistent variable", {
  expect_warning(
    result <- .resolve_indicators("nonexistent_var_xyz", "IHS5"),
    "not found"
  )
  expect_true(is.na(result[["nonexistent_var_xyz"]]))
})

test_that("ihs_crosswalk_check() returns a tibble invisibly", {
  result <- suppressMessages(ihs_crosswalk_check())
  expect_s3_class(result, "tbl_df")
})

test_that(".bind_rounds() correctly binds two mock data.frames", {
  df1 <- data.frame(case_id = c("A", "B"), val = c(1, 2))
  df2 <- data.frame(case_id = c("C", "D"), val = c(3, 4))
  round_list <- list(IHS4 = df1, IHS5 = df2)

  result <- .bind_rounds(round_list)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)
  expect_true("ihs_round" %in% names(result))
  expect_equal(result$ihs_round, c("IHS4", "IHS4", "IHS5", "IHS5"))
})

test_that(".bind_rounds() warns when column types differ across rounds", {
  df1 <- data.frame(case_id = c("A", "B"), val = c(1L, 2L))
  df2 <- data.frame(case_id = c("C", "D"), val = c("x", "y"))
  round_list <- list(IHS4 = df1, IHS5 = df2)

  expect_warning(.bind_rounds(round_list), "Type mismatch")
})
