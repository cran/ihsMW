#' Convert Agricultural Units to Kilograms
#'
#' Converts reported harvest units (e.g., Pails, Oxcarts, Heaps) into standard kilograms 
#' using official NSO crop-specific conversion factors. 
#'
#' @param data A data.frame
#' @param qty_col The name of the column containing the quantity
#' @param unit_col The name of the column containing the unit code or name
#' @param crop_col The name of the column containing the crop code
#' @param unmapped Action to take when a unit cannot be mapped: `"warn"` (default), `"error"`, or `"ignore"`.
#' 
#' @return A data.frame with a new \code{qty_col_kg} column.
#' @export
ihs_convert_units <- function(data, qty_col, unit_col, crop_col, unmapped = "warn") {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame")
  }
  
  if (!all(c(qty_col, unit_col, crop_col) %in% names(data))) {
    missing <- setdiff(c(qty_col, unit_col, crop_col), names(data))
    cli::cli_abort("Columns not found in data: {.var {missing}}")
  }
  
  cf <- .load_conversion_factors()
  
  # Detect region column
  reg_col <- NULL
  for (candidate in c("region", "ihs_region", "survey_region")) {
    if (candidate %in% tolower(names(data))) {
      reg_col <- names(data)[which(tolower(names(data)) == candidate)[1]]
      break
    }
  }
  
  # Detect condition column
  cond_col <- NULL
  for (candidate in c("condition", "crop_condition")) {
    if (candidate %in% tolower(names(data))) {
      cond_col <- names(data)[which(tolower(names(data)) == candidate)[1]]
      break
    }
  }
  
  new_col <- paste0(qty_col, "_kg")
  
  # Standardize matching keys
  data_keys <- data.frame(
    .row_id = seq_len(nrow(data)),
    crop = as.numeric(data[[crop_col]]),
    unit = as.numeric(data[[unit_col]])
  )
  
  if (!is.null(reg_col)) {
    data_keys$region <- as.numeric(data[[reg_col]])
  } else {
    data_keys$region <- 2 # Default to Central region
  }
  
  if (!is.null(cond_col)) {
    data_keys$condition <- as.numeric(data[[cond_col]])
  } else {
    data_keys$condition <- NA_real_
  }
  
  matched_factors <- numeric(nrow(data))
  unmapped_cases <- list()
  
  for (i in seq_len(nrow(data))) {
    qty <- data[[qty_col]][i]
    if (is.na(qty)) {
      matched_factors[i] <- NA_real_
      next
    }
    
    crop <- data_keys$crop[i]
    unit <- data_keys$unit[i]
    region <- data_keys$region[i]
    condition <- data_keys$condition[i]
    
    if (is.na(crop) || is.na(unit)) {
      matched_factors[i] <- NA_real_
      next
    }
    
    # Try exact match with provided region and condition
    match_idx <- which(cf$crop_code == crop & cf$unit_code == unit & cf$region == region)
    
    if (length(match_idx) > 0) {
      cf_sub <- cf[match_idx, ]
      
      # If condition is specified and not NA, filter by condition
      if (!is.na(condition)) {
        cond_match <- which(cf_sub$condition == condition)
        if (length(cond_match) > 0) {
          matched_factors[i] <- cf_sub$factor[cond_match[1]]
          next
        }
      }
      
      # Fallback or preference order for condition: 1 (Shelled) -> 3 (N/A) -> 2 (Unshelled)
      cond_pref <- c(1, 3, 2)
      found <- FALSE
      for (cp in cond_pref) {
        cond_match <- which(cf_sub$condition == cp)
        if (length(cond_match) > 0) {
          matched_factors[i] <- cf_sub$factor[cond_match[1]]
          found <- TRUE
          break
        }
      }
      
      if (found) next
      
      # If still not found, take the first available
      matched_factors[i] <- cf_sub$factor[1]
    } else {
      # Try matching without region (fallback to region = 2)
      match_idx_fallback <- which(cf$crop_code == crop & cf$unit_code == unit & cf$region == 2)
      if (length(match_idx_fallback) > 0) {
        cf_sub <- cf[match_idx_fallback, ]
        if (!is.na(condition)) {
          cond_match <- which(cf_sub$condition == condition)
          if (length(cond_match) > 0) {
            matched_factors[i] <- cf_sub$factor[cond_match[1]]
            next
          }
        }
        cond_pref <- c(1, 3, 2)
        found <- FALSE
        for (cp in cond_pref) {
          cond_match <- which(cf_sub$condition == cp)
          if (length(cond_match) > 0) {
            matched_factors[i] <- cf_sub$factor[cond_match[1]]
            found <- TRUE
            break
          }
        }
        if (found) next
        matched_factors[i] <- cf_sub$factor[1]
      } else {
        # Unmapped
        matched_factors[i] <- NA_real_
        unmapped_key <- paste(crop, unit, sep = "_")
        unmapped_cases[[unmapped_key]] <- (unmapped_cases[[unmapped_key]] %||% 0) + 1
      }
    }
  }
  
  data[[new_col]] <- data[[qty_col]] * matched_factors
  
  if (length(unmapped_cases) > 0 && unmapped != "ignore") {
    msg <- sprintf("Failed to map %d crop-unit combinations.", length(unmapped_cases))
    if (unmapped == "error") {
      cli::cli_abort(msg)
    } else {
      cli::cli_warn(msg)
    }
  }
  
  data
}

.load_conversion_factors <- function() {
  cache_key <- "conversion_factors"
  
  if (exists(cache_key, envir = .ihs_cache)) {
    return(get(cache_key, envir = .ihs_cache))
  }
  
  cf_path <- system.file("extdata", "crop_conversion_factors.csv", package = "ihsMW")
  if (cf_path == "" || !file.exists(cf_path)) {
    cf_path <- "inst/extdata/crop_conversion_factors.csv"
    if (!file.exists(cf_path)) {
       cli::cli_abort("Could not locate crop_conversion_factors.csv. Is the package installed properly?")
    }
  }
  
  cf <- readr::read_csv(cf_path, show_col_types = FALSE)
  assign(cache_key, cf, envir = .ihs_cache)
  cf
}

`%||%` <- function(a, b) if (is.null(a)) b else a

