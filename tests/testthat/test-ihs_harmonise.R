test_that("ihs_harmonise renames columns correctly", {
  # Create mock raw data matching IHS4
  raw_data <- data.frame(
    case_id = "123",
    af_bio_12 = 1, # Should become 'af_bio_12_x'
    unrelated_col = 99
  )
  attr(raw_data$af_bio_12, "label") <- "Biomass"
  
  # Harmonise and filter out unrelated
  harmonised <- ihs_harmonise(raw_data, round = "IHS4", extra = FALSE)
  
  expect_true("af_bio_12_x" %in% names(harmonised))
  expect_false("af_bio_12" %in% names(harmonised))
  expect_false("unrelated_col" %in% names(harmonised))
  expect_equal(harmonised$ihs_round, "IHS4")
  
  # Check label retention
  expect_equal(attr(harmonised$af_bio_12_x, "label"), "Biomass")
  
  # Harmonise and keep unrelated
  harmonised_extra <- ihs_harmonise(raw_data, round = "IHS4", extra = TRUE)
  expect_true("unrelated_col" %in% names(harmonised_extra))
})

test_that("ihs_harmonise warns on no matches", {
  raw_data <- data.frame(a = 1, b = 2)
  expect_warning(ihs_harmonise(raw_data, round = "IHS4"), "No columns were mapped")
})
