test_that("ihs_search works", {
  # It should return a data.frame from the internal crosswalk
  res <- suppressMessages(ihs_search("consumption"))
  expect_true(is.data.frame(res))
  expect_true(nrow(res) > 0)
  expect_true("harmonised_name" %in% names(res))
  
  # Check fields param
  res_module <- suppressMessages(ihs_search("f1", fields = "module"))
  expect_true(nrow(res_module) > 0)
})

test_that("ihs_crosswalk_check works", {
  # Should run without error and return tibble
  cw <- suppressMessages(ihs_crosswalk_check(verbose = FALSE))
  expect_true(is.data.frame(cw))
})
