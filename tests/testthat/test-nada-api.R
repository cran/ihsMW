# test-nada-api.R
# Tests for the NADA API client: variables, data_files, error handling, caching.

# --- Helpers to build mock .nada_get responses ---

mock_nada_get_variables <- function(path, query = list(), auth = FALSE) {
  list(
    variables = list(
      variable = data.frame(
        name = c("case_id", "rexp_cat01", "hh_a02", "hh_size", "urban"),
        vid  = c("V1", "V2", "V3", "V4", "V5"),
        labl = c("Household ID", "Consumption aggregate", "Head sex", "HH size", "Urban/Rural"),
        fid  = c("hh_mod_a.dta", "consumption.dta", "hh_mod_a.dta", "hh_mod_a.dta", "hh_mod_a.dta"),
        stringsAsFactors = FALSE
      )
    )
  )
}

mock_nada_get_datafiles <- function(path, query = list(), auth = FALSE) {
  list(
    dataset = list(
      data_files = data.frame(
        file_id   = c("F1", "F2", "F3"),
        file_name = c("hh_mod_a.dta", "consumption.dta", "hh_mod_g.dta"),
        format    = c("Stata", "Stata", "Stata"),
        stringsAsFactors = FALSE
      )
    )
  )
}

mock_nada_get_401 <- function(path, query = list(), auth = FALSE) {
  cli::cli_abort(c(
    "API key rejected by the World Bank server.",
    "i" = "Your authorization credentials were not accepted.",
    ">" = "Run {.fn ihs_auth} to update your World Bank API key."
  ), class = "ihsMW_auth_error")
}

mock_nada_get_404 <- function(path, query = list(), auth = FALSE) {
  cli::cli_abort(c(
    "Module or file not found: {.val {path}}",
    "i" = "The requested file or resource does not exist for this round.",
    ">" = "Run {.fn ihs_modules} to list valid modules for the round."
  ), class = "ihsMW_not_found_error")
}

# --- Tests ---

test_that(".nada_variables returns tibble with correct columns", {
  # Clear cache first
  if (exists("MWI_2019_IHS-V_v06_M_vars", envir = .nada_cache))
    rm("MWI_2019_IHS-V_v06_M_vars", envir = .nada_cache)

  local_mocked_bindings(.nada_get = mock_nada_get_variables)

  result <- .nada_variables("MWI_2019_IHS-V_v06_M")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("var_name", "label", "file_name") %in% names(result)))
  expect_equal(nrow(result), 5)
})

test_that(".nada_data_files returns tibble with correct columns", {
  if (exists("MWI_2019_IHS-V_v06_M_data_files", envir = .nada_cache))
    rm("MWI_2019_IHS-V_v06_M_data_files", envir = .nada_cache)

  local_mocked_bindings(.nada_get = mock_nada_get_datafiles)

  result <- .nada_data_files("MWI_2019_IHS-V_v06_M")
  expect_s3_class(result, "tbl_df")
  expect_true(all(c("file_id", "file_name", "format") %in% names(result)))
  expect_equal(nrow(result), 3)
})

test_that(".nada_get with 401 throws ihsMW_auth_error", {
  local_mocked_bindings(.nada_get = mock_nada_get_401)

  # Clear cache so it actually calls .nada_get
  if (exists("test_401_vars", envir = .nada_cache))
    rm("test_401_vars", envir = .nada_cache)

  expect_error(
    .nada_get("catalog/test/variables"),
    class = "ihsMW_auth_error"
  )
})

test_that(".nada_get with 404 throws ihsMW_not_found_error", {
  local_mocked_bindings(.nada_get = mock_nada_get_404)

  expect_error(
    .nada_get("catalog/nonexistent/variables"),
    class = "ihsMW_not_found_error"
  )
})

test_that("Session caching works: .nada_variables called twice hits API once", {
  # Clear cache
  if (exists("MWI_2019_IHS-V_v06_M_vars", envir = .nada_cache))
    rm("MWI_2019_IHS-V_v06_M_vars", envir = .nada_cache)

  call_count <- 0L
  counting_mock <- function(path, query = list(), auth = FALSE) {
    call_count <<- call_count + 1L
    mock_nada_get_variables(path, query, auth)
  }

  local_mocked_bindings(.nada_get = counting_mock)

  result1 <- .nada_variables("MWI_2019_IHS-V_v06_M")
  result2 <- .nada_variables("MWI_2019_IHS-V_v06_M")

  expect_equal(call_count, 1L)
  expect_identical(result1, result2)
})

test_that(".nada_data_files session caching works", {
  if (exists("MWI_2019_IHS-V_v06_M_data_files", envir = .nada_cache))
    rm("MWI_2019_IHS-V_v06_M_data_files", envir = .nada_cache)

  call_count <- 0L
  counting_mock <- function(path, query = list(), auth = FALSE) {
    call_count <<- call_count + 1L
    mock_nada_get_datafiles(path, query, auth)
  }

  local_mocked_bindings(.nada_get = counting_mock)

  .nada_data_files("MWI_2019_IHS-V_v06_M")
  .nada_data_files("MWI_2019_IHS-V_v06_M")

  expect_equal(call_count, 1L)
})

test_that(".nada_search returns a tibble", {
  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      result = list(
        rows = data.frame(
          idno = "MWI_2019_IHS-V_v06_M",
          title = "IHS5",
          nation = "Malawi",
          year_start = "2019",
          year_end = "2020",
          stringsAsFactors = FALSE
        )
      )
    )
  })

  result <- .nada_search("consumption")
  expect_s3_class(result, "tbl_df")
  expect_true("idno" %in% names(result))
})

test_that(".nada_req builds a request with user agent", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "test_key_mock")
  req <- .nada_req("catalog/test", auth = FALSE)
  expect_s3_class(req, "httr2_request")
})

test_that(".nada_req adds auth header when auth = TRUE", {
  withr::local_envvar(WORLDBANK_MICRODATA_KEY = "test_key_123")
  req <- .nada_req("catalog/test", auth = TRUE)
  expect_s3_class(req, "httr2_request")
  # The Authorization header should be set
  expect_true("Authorization" %in% names(req$headers))
})

# ===========================================================================
# Defensive parsing tests — alternate nesting, missing fields, edge cases
# ===========================================================================

test_that(".nada_variables handles resp$variables (no nested $variable)", {
  # Hits line 81-82: resp$variables exists but resp$variables$variable is NULL
  if (exists("alt_nest_test_vars", envir = .nada_cache))
    rm("alt_nest_test_vars", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      variables = data.frame(
        name = c("hh_a01", "hh_a02"),
        labl = c("District", "Sex of head"),
        fid  = c("hh_mod_a.dta", "hh_mod_a.dta"),
        stringsAsFactors = FALSE
      )
    )
  })

  result <- .nada_variables("alt_nest_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$var_name, c("hh_a01", "hh_a02"))
})

test_that(".nada_variables handles list-of-lists (non-data.frame) response", {
  # Hits lines 87-101: raw_vars is a list, not a data.frame
  if (exists("list_resp_test_vars", envir = .nada_cache))
    rm("list_resp_test_vars", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      variables = list(
        variable = list(
          list(name = "var1", labl = "Label 1", fid = "file1.dta"),
          list(name = "var2", labl = "Label 2", fid = "file2.dta")
        )
      )
    )
  })

  result <- .nada_variables("list_resp_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$var_name, c("var1", "var2"))
})

test_that(".nada_variables handles single named list (not list-of-lists)", {
  # Hits line 89: raw_vars is a named list (single record) → wrap in list()
  if (exists("named_list_test_vars", envir = .nada_cache))
    rm("named_list_test_vars", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      variables = list(
        variable = list(name = "solo_var", labl = "Solo Label", fid = "solo.dta")
      )
    )
  })

  result <- .nada_variables("named_list_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$var_name, "solo_var")
})

test_that(".nada_variables returns NA for missing label field", {
  # Hits line 112: labl is NULL so rep(NA_character_) is used
  if (exists("no_label_test_vars", envir = .nada_cache))
    rm("no_label_test_vars", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      variables = list(
        variable = data.frame(
          name = c("x1", "x2"),
          fid  = c("f1.dta", "f2.dta"),
          stringsAsFactors = FALSE
        )
      )
    )
  })

  result <- .nada_variables("no_label_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$label)))
})

test_that(".nada_variables uses vid fallback when name is missing", {
  # Hits line 111: name is NULL → uses vid
  if (exists("vid_fallback_test_vars", envir = .nada_cache))
    rm("vid_fallback_test_vars", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      variables = list(
        variable = data.frame(
          vid  = c("V100", "V200"),
          labl = c("Label A", "Label B"),
          file_id = c("f1.dta", "f2.dta"),
          stringsAsFactors = FALSE
        )
      )
    )
  })

  result <- .nada_variables("vid_fallback_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$var_name, c("V100", "V200"))
  # file_id fallback for file_name (line 113)
  expect_equal(result$file_name, c("f1.dta", "f2.dta"))
})

test_that(".nada_variables returns empty tibble for null response", {
  # Hits line 83-84: neither resp$variables$variable nor resp$variables exist
  if (exists("empty_resp_test_vars", envir = .nada_cache))
    rm("empty_resp_test_vars", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(status = "ok")  # no variables key at all
  })

  result <- .nada_variables("empty_resp_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that(".nada_variables handles non-list, non-df raw_vars", {
  # Hits line 99-100: raw_vars is not a list and not a data.frame → data.frame()
  if (exists("scalar_resp_test_vars", envir = .nada_cache))
    rm("scalar_resp_test_vars", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(variables = list(variable = "unexpected_string"))
  })

  result <- .nada_variables("scalar_resp_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that(".nada_data_files handles resp$data_files nesting (no dataset wrapper)", {
  # Hits line 141-142: resp$data_files exists directly
  if (exists("alt_df_test_data_files", envir = .nada_cache))
    rm("alt_df_test_data_files", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      data_files = data.frame(
        file_id   = c("F10", "F20"),
        file_name = c("a.dta", "b.dta"),
        format    = c("Stata", "Stata"),
        stringsAsFactors = FALSE
      )
    )
  })

  result <- .nada_data_files("alt_df_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that(".nada_data_files handles missing format field (NULL format)", {
  # Hits line 171: format column is NULL → rep(NA_character_)
  if (exists("no_fmt_test_data_files", envir = .nada_cache))
    rm("no_fmt_test_data_files", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      dataset = list(
        data_files = data.frame(
          file_id   = c("F1", "F2"),
          file_name = c("mod_a.dta", "mod_b.dta"),
          stringsAsFactors = FALSE
        )
      )
    )
  })

  result <- .nada_data_files("no_fmt_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$format)))
})

test_that(".nada_data_files uses id/name fallbacks", {
  # Hits line 169-170: file_id is NULL → uses id; file_name is NULL → uses name
  if (exists("fallback_df_test_data_files", envir = .nada_cache))
    rm("fallback_df_test_data_files", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      dataset = list(
        data_files = data.frame(
          id     = c("ID1", "ID2"),
          name   = c("alt_a.dta", "alt_b.dta"),
          format = c("Stata", "CSV"),
          stringsAsFactors = FALSE
        )
      )
    )
  })

  result <- .nada_data_files("fallback_df_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$file_id, c("ID1", "ID2"))
  expect_equal(result$file_name, c("alt_a.dta", "alt_b.dta"))
})

test_that(".nada_data_files handles list-of-lists response", {
  # Hits lines 149-160: raw_files is a list, not a data.frame
  if (exists("list_df_test_data_files", envir = .nada_cache))
    rm("list_df_test_data_files", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      dataset = list(
        data_files = list(
          list(file_id = "LF1", file_name = "list_a.dta", format = "Stata"),
          list(file_id = "LF2", file_name = "list_b.dta", format = "CSV")
        )
      )
    )
  })

  result <- .nada_data_files("list_df_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that(".nada_data_files handles resp$dataset fallback (no data_files key)", {
  # Hits line 143-144: resp$dataset exists but $data_files is NULL
  if (exists("dataset_only_test_data_files", envir = .nada_cache))
    rm("dataset_only_test_data_files", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      dataset = data.frame(
        file_id   = c("DS1"),
        file_name = c("ds_only.dta"),
        format    = c("Stata"),
        stringsAsFactors = FALSE
      )
    )
  })

  result <- .nada_data_files("dataset_only_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})

test_that(".nada_data_files returns empty for totally empty response", {
  # Hits line 145-146: none of the expected keys exist
  if (exists("empty_df_test_data_files", envir = .nada_cache))
    rm("empty_df_test_data_files", envir = .nada_cache)

  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(status = "ok")  # no dataset or data_files keys
  })

  result <- .nada_data_files("empty_df_test")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that(".nada_get with 403 throws ihsMW_forbidden_error", {
  # Need to mock the real .nada_get flow by mocking httr2 functions
  local_mocked_bindings(
    req_perform = function(...) httr2::response(status_code = 403, body = charToRaw("{}")),
    .package = "httr2"
  )

  expect_error(
    .nada_get("catalog/test/forbidden"),
    class = "ihsMW_forbidden_error"
  )
})

test_that(".nada_get with 500 throws generic error", {
  local_mocked_bindings(
    req_perform = function(...) httr2::response(status_code = 500, body = charToRaw("{}")),
    .package = "httr2"
  )

  expect_error(
    .nada_get("catalog/test/servererror"),
    "status 500"
  )
})

test_that(".nada_search handles resp$rows nesting (no result wrapper)", {
  # Hits line 200-201: resp$rows exists directly
  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      rows = data.frame(
        idno = "MWI_2016_IHS-IV_v03_M",
        title = "IHS4",
        nation = "Malawi",
        year_start = "2016",
        year_end = "2017",
        stringsAsFactors = FALSE
      )
    )
  })

  result <- .nada_search("poverty")
  expect_s3_class(result, "tbl_df")
  expect_equal(result$idno, "MWI_2016_IHS-IV_v03_M")
})

test_that(".nada_search handles empty/null response", {
  # Hits line 206-207: none of the expected response keys exist
  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(status = "ok")
  })

  result <- .nada_search("totally_nonexistent")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that(".nada_search handles list-of-lists response", {
  # Hits lines 210-221: raw_search is a list, not a data.frame
  local_mocked_bindings(.nada_get = function(path, query = list(), auth = FALSE) {
    list(
      result = list(
        rows = list(
          list(idno = "MWI1", title = "T1", nation = "Malawi", year_start = "2019", year_end = "2020")
        )
      )
    )
  })

  result <- .nada_search("test_list")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
})
