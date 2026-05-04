# test-auth.R
# Tests for API key authentication: setting, retrieving, and error handling.

test_that("ihs_auth(key) sets the environment variable", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "")

  # Mock the HTTP validation call so no real network request fires
  local_mocked_bindings(
    req_perform = function(...) {
      resp <- httr2::response(status_code = 200, body = charToRaw("{}"))
      resp
    },
    .package = "httr2"
  )

  # Use a temp .Renviron so we don't clobber the real one
  tmp_renviron <- withr::local_tempfile()
  withr::local_envvar(HOME = dirname(tmp_renviron))
  file.create(file.path(dirname(tmp_renviron), ".Renviron"))

  result <- ihs_auth(key = "abc123")

  expect_equal(Sys.getenv("WORLDBANK_MICRODATA_KEY"), "abc123")
  expect_equal(result, "abc123")
})

test_that(".ihs_key() returns the key when set", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "test_key_abc")

  expect_equal(.ihs_key(), "test_key_abc")
})

test_that(".ihs_key() throws ihsMW_no_key when not set", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "")

  expect_error(.ihs_key(), class = "ihsMW_no_key")
})

test_that(".ihs_key() error message mentions ihs_auth", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "")

  err <- tryCatch(.ihs_key(), error = function(e) e)
  expect_match(conditionMessage(err), "ihs_auth", fixed = TRUE)
})

test_that("ihs_auth() in non-interactive mode with key = NULL does NOT call readline", {
  # In non-interactive mode (testthat runs non-interactively), running
  # ihs_auth(key = NULL) should just print the guide and return NULL
  result <- ihs_auth(key = NULL)
  expect_null(result)
})

test_that("ihs_auth(key = 'valid_key') with mocked 200 response saves the key", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "")

  local_mocked_bindings(
    req_perform = function(...) httr2::response(status_code = 200, body = charToRaw("{}")),
    .package = "httr2"
  )

  tmp_renviron <- withr::local_tempfile()
  withr::local_envvar(HOME = dirname(tmp_renviron))
  file.create(file.path(dirname(tmp_renviron), ".Renviron"))

  result <- ihs_auth(key = "valid_test_key")
  expect_equal(Sys.getenv("WORLDBANK_MICRODATA_KEY"), "valid_test_key")
  expect_equal(result, "valid_test_key")
})

test_that("ihs_auth(key = 'bad_key') with mocked 401 aborts with ihsMW_auth_error", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "")

  local_mocked_bindings(
    req_perform = function(...) httr2::response(status_code = 401, body = charToRaw("{}")),
    .package = "httr2"
  )

  expect_error(
    ihs_auth(key = "bad_key"),
    class = "ihsMW_auth_error"
  )
})

test_that("ihs_key_set is equivalent to ihs_auth(key = ...)", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "")

  local_mocked_bindings(
    req_perform = function(...) httr2::response(status_code = 200, body = charToRaw("{}")),
    .package = "httr2"
  )

  tmp_renviron <- withr::local_tempfile()
  withr::local_envvar(HOME = dirname(tmp_renviron))
  file.create(file.path(dirname(tmp_renviron), ".Renviron"))

  result <- ihs_key_set("alias_test_key")
  expect_equal(Sys.getenv("WORLDBANK_MICRODATA_KEY"), "alias_test_key")
  expect_equal(result, "alias_test_key")
})
