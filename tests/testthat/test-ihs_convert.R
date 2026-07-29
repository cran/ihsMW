test_that("the kilogram identity is derived from the bundled table", {
  cf <- .load_conversion_factors()

  # Unit 1 is KILOGRAM: factor 1 for every crop and region in the table.
  expect_equal(.identity_unit(cf), 1)
  expect_true(all(cf$factor[cf$unit_code == 1] == 1))
})

test_that("the identity is withdrawn if the table stops supporting it", {
  cf <- .load_conversion_factors()

  # If a future NSO release gave the kilogram unit a crop-specific factor,
  # the rule must switch itself off rather than assert a wrong 1:1.
  tampered <- cf
  tampered$factor[which(tampered$unit_code == 1)[1]] <- 0.5
  expect_null(.identity_unit(tampered))

  # A unit that is 1 for only a handful of crops is not well enough attested.
  thin <- cf[cf$unit_code != 1, ]
  thin <- rbind(thin, data.frame(region = 1, crop_code = 1:3, unit_code = 99,
                                 condition = 1, factor = 1))
  expect_null(.identity_unit(thin))
})

test_that("quantities already in kilograms pass through unchanged", {
  df <- data.frame(
    crop_code = c(1, 48, 999),   # known crop, IHS6-only crop, unknown crop
    unit_code = c(1, 1, 1),
    quantity  = c(7, 40, 12.5)
  )

  out <- suppressWarnings(suppressMessages(
    ihs_convert_units(df, "quantity", "unit_code", "crop_code")
  ))

  # The factor table has no rows for crops 48 or 999, but a kilogram is a
  # kilogram - these must convert, not return NA.
  expect_equal(out$quantity_kg, out$quantity)
})

test_that("the identity does not leak to other units", {
  df <- data.frame(crop_code = 999, unit_code = 2, quantity = 1)

  out <- suppressWarnings(suppressMessages(
    ihs_convert_units(df, "quantity", "unit_code", "crop_code")
  ))

  # Unit 2 is a 50 kg bag, whose factor varies by crop (19 to 52.5). An
  # unknown crop must stay NA rather than be silently treated as 1 kg.
  expect_true(is.na(out$quantity_kg))
})

test_that("explicit factors still win over the identity", {
  df <- data.frame(crop_code = c(1, 1), unit_code = c(2, 3), quantity = c(2, 5))

  out <- suppressWarnings(suppressMessages(
    ihs_convert_units(df, "quantity", "unit_code", "crop_code")
  ))

  expect_equal(out$quantity_kg[1], 100)  # crop 1, unit 2 -> 50
  expect_equal(out$quantity_kg[2], 450)  # crop 1, unit 3 -> 90
})

test_that("label-valued code columns abort with a helpful error", {
  # Some IHS CSV distributions export value labels instead of numeric codes.
  df <- data.frame(
    crop_code = c("MAIZE LOCAL", "BEANS"),
    unit_code = c("50 KG BAG", "PAIL (LARGE)"),
    quantity  = c(2, 3)
  )

  expect_error(
    ihs_convert_units(df, "quantity", "unit_code", "crop_code"),
    class = "ihsMW_labelled_column"
  )
})

test_that("unmappable combinations return NA and are named", {
  df <- data.frame(crop_code = 999, unit_code = 998, quantity = 10)

  expect_warning(
    out <- ihs_convert_units(df, "quantity", "unit_code", "crop_code"),
    "Failed to map"
  )
  expect_true(is.na(out$quantity_kg))

  expect_error(
    ihs_convert_units(df, "quantity", "unit_code", "crop_code",
                      unmapped = "error"),
    "Failed to map"
  )
  expect_no_warning(
    ihs_convert_units(df, "quantity", "unit_code", "crop_code",
                      unmapped = "ignore")
  )
})

test_that("missing quantities are not reported as conversion failures", {
  df <- data.frame(crop_code = c(1, 1), unit_code = c(2, 2), quantity = c(NA, 4))

  # Nothing to convert is not the same as failing to convert.
  expect_no_warning(suppressMessages(
    out <- ihs_convert_units(df, "quantity", "unit_code", "crop_code")
  ))
  expect_true(is.na(out$quantity_kg[1]))
  expect_equal(out$quantity_kg[2], 200)
})
