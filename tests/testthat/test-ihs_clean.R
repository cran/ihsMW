test_that("ihs_standardize_missing works", {
  df <- data.frame(a = c(1, -99, 3, 999), b = c("A", "B", "C", "D"))
  clean_df <- ihs_standardize_missing(df)
  
  expect_equal(clean_df$a, c(1, NA, 3, NA))
  expect_equal(clean_df$b, c("A", "B", "C", "D"))
  
  audit <- attr(clean_df, "ihs_missing_conversions")
  expect_equal(audit$a, 2)
})

test_that("ihs_winsorize works with and without groups", {
  set.seed(123)
  df <- data.frame(
    region = rep(c("North", "South"), each = 50),
    cons = c(rnorm(50, 100, 10), rnorm(50, 500, 50))
  )
  # Introduce outliers
  df$cons[1] <- 1000
  df$cons[51] <- 5000
  
  # Global winsorization
  w_global <- ihs_winsorize(df, vars = "cons", probs = c(0.05, 0.95))
  expect_true("cons_w" %in% names(w_global))
  expect_true(max(w_global$cons_w) < 5000)
  
  # Grouped winsorization
  w_grouped <- ihs_winsorize(df, vars = "cons", by = "region", probs = c(0.05, 0.95))
  
  # The max in North should be much lower than max in South
  max_north <- max(w_grouped$cons_w[w_grouped$region == "North"])
  max_south <- max(w_grouped$cons_w[w_grouped$region == "South"])
  expect_true(max_north < max_south)
})

test_that("ihs_convert_units works", {
  df <- data.frame(
    qty = c(2, 5, 10),
    unit = c(2, 3, 99),
    crop = c(1, 1, 3)
  )
  
  conv <- suppressWarnings(ihs_convert_units(df, "qty", "unit", "crop"))
  
  # Using the real conversion factors (Region Central default): crop=1, unit=2 -> factor 50
  expect_equal(conv$qty_kg[1], 2 * 50)
  # crop=1, unit=3 -> factor 90
  expect_equal(conv$qty_kg[2], 5 * 90)
  # Unmapped gets NA
  expect_true(is.na(conv$qty_kg[3]))
})

test_that("ihs_aggregate works", {
  df <- data.frame(
    case_id = c("A", "A", "B"),
    harvest = c(10, 20, 50), # sum
    has_radio = c(0, 1, 0),  # dummy -> 1, 0
    name = c("John", "Jane", "Bob") # character
  )
  
  agg <- suppressMessages(ihs_aggregate(df, "case_id"))
  
  expect_equal(nrow(agg), 2)
  expect_equal(agg$harvest[agg$case_id == "A"], 30)
  expect_equal(agg$has_radio[agg$case_id == "A"], 1)
  expect_equal(agg$has_radio[agg$case_id == "B"], 0)
})

test_that("ihs_clean wrapper works", {
  df <- data.frame(
    region = c("N", "N", "S", "S"),
    cons = c(10, -99, 1000, 20)
  )
  
  clean <- suppressWarnings(ihs_clean(df, winsorize_vars = "cons", winsorize_by = "region"))
  
  audit <- attr(clean, "ihs_audit")
  expect_equal(audit$missing_conversions$cons, 1)
  expect_true(!is.null(audit$winsorized_vars$cons))
})
