test_that("ihs_merge joins two data.frames with auto-detected keys", {
  df1 <- data.frame(case_id = c("A", "B", "C"), income = c(100, 200, 300))
  df2 <- data.frame(case_id = c("A", "B", "D"), harvest = c(10, 20, 30))

  merged <- ihs_merge(df1, df2)
  expect_equal(nrow(merged), 3)
  expect_true(all(c("income", "harvest") %in% names(merged)))
  expect_true(is.na(merged$harvest[merged$case_id == "C"]))
  expect_true(!is.null(attr(merged, "ihs_merge_log")))
})

test_that("ihs_merge works with explicit 'by' argument", {
  df1 <- data.frame(id = c("A", "B"), x = 1:2)
  df2 <- data.frame(id = c("A", "B"), y = 3:4)

  merged <- ihs_merge(df1, df2, by = "id")
  expect_equal(nrow(merged), 2)
  expect_equal(merged$x, 1:2)
  expect_equal(merged$y, 3:4)
})

test_that("ihs_merge supports inner and full join types", {
  df1 <- data.frame(case_id = c("A", "B"), x = 1:2)
  df2 <- data.frame(case_id = c("B", "C"), y = 3:4)

  inner <- ihs_merge(df1, df2, type = "inner")
  expect_equal(nrow(inner), 1)
  expect_equal(inner$case_id, "B")

  full <- ihs_merge(df1, df2, type = "full")
  expect_equal(nrow(full), 3)
})

test_that("ihs_merge does not warn about row growth on a full join", {
  # A full join over partly disjoint keys legitimately returns more rows than
  # either input; warning about it trained users to ignore a real signal.
  df1 <- data.frame(case_id = c("A", "B"), x = 1:2)
  df2 <- data.frame(case_id = c("C", "D"), y = 3:4)

  expect_no_warning(ihs_merge(df1, df2, type = "full"))
  expect_equal(nrow(ihs_merge(df1, df2, type = "full")), 4)
})

test_that("ihs_merge warns when shared non-key columns get suffixed", {
  df1 <- data.frame(case_id = c("A", "B"), region = 1:2, x = 1:2)
  df2 <- data.frame(case_id = c("A", "B"), region = 1:2, y = 3:4)

  expect_warning(ihs_merge(df1, df2), "suffixed")

  merged <- suppressWarnings(ihs_merge(df1, df2))
  expect_true(all(c("region.x", "region.y") %in% names(merged)))

  # Naming the shared column as a join key avoids the duplication entirely
  expect_no_warning(ihs_merge(df1, df2, by = c("case_id", "region")))
})

test_that("ihs_merge still warns about many-to-many left joins", {
  df1 <- data.frame(case_id = c("A", "A"), x = 1:2)
  df2 <- data.frame(case_id = c("A", "A"), y = 3:4)
  expect_warning(ihs_merge(df1, df2), "expanded rows")
})

test_that("ihs_merge errors on fewer than 2 data.frames", {
  df1 <- data.frame(case_id = "A", x = 1)
  expect_error(ihs_merge(df1), "at least 2")
})

test_that("ihs_merge warns on row expansion", {
  df1 <- data.frame(case_id = c("A", "A", "B"), x = 1:3)
  df2 <- data.frame(case_id = c("A", "A", "B"), y = 4:6)

  expect_warning(ihs_merge(df1, df2), "expanded rows")
})

test_that("ihs_merge merges three data.frames sequentially", {
  df1 <- data.frame(case_id = c("A", "B"), a = 1:2)
  df2 <- data.frame(case_id = c("A", "B"), b = 3:4)
  df3 <- data.frame(case_id = c("A", "B"), c = 5:6)

  merged <- ihs_merge(df1, df2, df3)
  expect_equal(ncol(merged), 4)
  expect_equal(nrow(merged), 2)
})
