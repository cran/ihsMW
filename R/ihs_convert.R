#' Convert Agricultural Units to Kilograms
#'
#' Converts reported harvest units (e.g., Pails, Oxcarts, Heaps) into standard
#' kilograms using official NSO crop-specific conversion factors.
#'
#' IHS agriculture modules let households report quantities in whatever unit
#' they actually use, so a raw quantity column mixes kilograms, pails, ox-carts
#' and heaps in the same variable. Summing it directly is meaningless. This
#' function looks up the right factor for each crop, unit, region and shelling
#' condition and returns a comparable kilogram column.
#'
#' @param data A data.frame.
#' @param qty_col The name of the column containing the quantity.
#' @param unit_col The name of the column containing the unit code.
#' @param crop_col The name of the column containing the crop code.
#' @param unmapped Action to take when a crop-unit combination cannot be
#'   mapped: `"warn"` (default), `"error"`, or `"ignore"`.
#'
#' @return A data.frame with a new column named \code{<qty_col>_kg}. Rows whose
#'   crop-unit combination is not in the factor table get \code{NA}, never a
#'   silently wrong number.
#'
#' @section Matching rules:
#' Factors vary by region and by whether the crop was shelled, so the lookup
#' resolves in this order:
#' \enumerate{
#'   \item exact match on crop, unit, region and condition;
#'   \item same crop, unit and region, falling back through the condition
#'     preference order shelled (1), not applicable (3), unshelled (2);
#'   \item the same two steps against the Central region, used as the national
#'     fallback when a crop-unit pair has no factor for the row's own region.
#' }
#' A \code{region} column (or \code{ihs_region} / \code{survey_region}) is
#' detected automatically and used if present. Without one, every row is priced
#' at Central-region factors and the function says so.
#'
#' @seealso \code{\link{ihs_aggregate}} to total the resulting kilogram column
#'   to household level.
#'
#' @examples
#' harvest <- data.frame(
#'   crop_code = c(1, 1, 1),
#'   unit_code = c(1, 2, 3),
#'   quantity  = c(100, 2, 5),
#'   region    = c(1, 2, 2)
#' )
#' ihs_convert_units(harvest, qty_col = "quantity",
#'                   unit_col = "unit_code", crop_col = "crop_code")
#'
#' # Unmappable combinations return NA rather than a wrong number
#' odd <- data.frame(crop_code = 999, unit_code = 999, quantity = 10)
#' suppressWarnings(
#'   ihs_convert_units(odd, "quantity", "unit_code", "crop_code")
#' )
#' @export
ihs_convert_units <- function(data, qty_col, unit_col, crop_col, unmapped = "warn") {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data.frame")
  }

  if (!all(c(qty_col, unit_col, crop_col) %in% names(data))) {
    missing <- setdiff(c(qty_col, unit_col, crop_col), names(data))
    cli::cli_abort("Columns not found in data: {.var {missing}}")
  }

  unmapped <- rlang::arg_match(unmapped, values = c("warn", "error", "ignore"))

  cf <- .load_conversion_factors()

  # Detect region column
  reg_col <- .match_col(names(data), c("region", "ihs_region", "survey_region"))

  # Detect condition column
  cond_col <- .match_col(names(data), c("condition", "crop_condition"))

  new_col <- paste0(qty_col, "_kg")

  n <- nrow(data)
  qty <- .as_code(data[[qty_col]], qty_col)
  crop <- .as_code(data[[crop_col]], crop_col)
  unit <- .as_code(data[[unit_col]], unit_col)

  if (!is.null(reg_col)) {
    region <- suppressWarnings(as.numeric(data[[reg_col]]))
  } else {
    # 2 = Central. Announce the assumption: applying Central factors to
    # Northern or Southern harvests silently biases every converted quantity.
    region <- rep(2, n)
    cli::cli_inform(c(
      "No region column found; using Central-region conversion factors for all rows.",
      "i" = "Add a {.var region} column to use region-specific factors."
    ))
  }

  condition <- if (!is.null(cond_col)) {
    suppressWarnings(as.numeric(data[[cond_col]]))
  } else {
    rep(NA_real_, n)
  }

  # Build two lookup tables once, then resolve every row with vectorised
  # match() calls. The previous implementation walked the factor table row by
  # row, which made conversion the slowest step in a typical pipeline.
  cf_cond_key <- paste(cf$crop_code, cf$unit_code, cf$region, cf$condition,
                       sep = "\r")

  # Per (crop, unit, region) default: first factor found in the condition
  # preference order shelled (1) -> N/A (3) -> unshelled (2), else whatever
  # is available.
  cf_grp_key <- paste(cf$crop_code, cf$unit_code, cf$region, sep = "\r")
  cond_rank <- match(cf$condition, c(1, 3, 2))
  cond_rank[is.na(cond_rank)] <- length(c(1, 3, 2)) + 1L
  ord <- order(cf_grp_key, cond_rank, seq_len(nrow(cf)))
  cf_ord <- cf[ord, , drop = FALSE]
  grp_ord <- cf_grp_key[ord]
  first_in_grp <- !duplicated(grp_ord)
  default_key <- grp_ord[first_in_grp]
  default_factor <- cf_ord$factor[first_in_grp]

  lookup <- function(k, keys, values) values[match(k, keys)]

  # 1. exact crop-unit-region-condition
  factors <- lookup(paste(crop, unit, region, condition, sep = "\r"),
                    cf_cond_key, cf$factor)

  # 2. crop-unit-region default (covers "region exists but condition doesn't")
  todo <- is.na(factors)
  if (any(todo)) {
    factors[todo] <- lookup(paste(crop[todo], unit[todo], region[todo], sep = "\r"),
                            default_key, default_factor)
  }

  # 3. Central-region fallback, only for rows with no factor in their own region
  todo <- is.na(factors)
  if (any(todo)) {
    factors[todo] <- lookup(paste(crop[todo], unit[todo], 2, condition[todo], sep = "\r"),
                            cf_cond_key, cf$factor)
    todo2 <- is.na(factors)
    if (any(todo2)) {
      factors[todo2] <- lookup(paste(crop[todo2], unit[todo2], 2, sep = "\r"),
                               default_key, default_factor)
    }
  }

  # 4. The kilogram identity.
  #
  # The factor table has no row for crops introduced in IHS6, so a quantity
  # already reported in kilograms was being returned as NA. A kilogram is a
  # kilogram whatever the crop, so this is recoverable with certainty - but
  # only assert it if the bundled table agrees, which is checked rather than
  # assumed.
  kg_unit <- .identity_unit(cf)
  if (!is.null(kg_unit)) {
    todo <- is.na(factors) & !is.na(unit) & unit == kg_unit
    if (any(todo)) factors[todo] <- 1
  }

  # Rows with a missing quantity, crop or unit are not conversion failures -
  # there is simply nothing to convert - so exclude them from the report.
  convertible <- !is.na(qty) & !is.na(crop) & !is.na(unit)
  failed <- convertible & is.na(factors)

  data[[new_col]] <- qty * factors

  if (any(failed) && unmapped != "ignore") {
    combos <- unique(paste0("crop ", crop[failed], " / unit ", unit[failed]))
    msg <- c(
      "Failed to map {length(combos)} crop-unit combination{?s} ({sum(failed)} row{?s}).",
      "i" = "Unconverted: {.val {utils::head(combos, 5)}}{if (length(combos) > 5) ' ...' else ''}",
      "i" = "These rows have {.code NA} in {.var {new_col}}."
    )
    if (unmapped == "error") {
      cli::cli_abort(msg)
    } else {
      cli::cli_warn(msg)
    }
  }

  data
}

#' Find the unit code that means "already in kilograms"
#'
#' Returns the unit code whose factor is 1 for every crop and region in the
#' conversion table, or NULL if no such unit exists. Deriving this from the
#' table rather than hard-coding `1` means the rule cannot drift out of step
#' with the bundled data: if a future NSO release gives the kilogram unit a
#' crop-specific factor, this returns NULL and the identity is not applied.
#'
#' @noRd
.identity_unit <- function(cf) {
  by_unit <- split(cf$factor, cf$unit_code)
  all_one <- vapply(by_unit, function(f) all(f == 1), logical(1))
  if (!any(all_one)) {
    return(NULL)
  }
  candidates <- as.numeric(names(by_unit)[all_one])
  # Require the unit to be well attested before trusting it as the identity.
  n_crops <- vapply(candidates, function(u) length(unique(cf$crop_code[cf$unit_code == u])),
                    integer(1))
  candidates <- candidates[n_crops >= 10]
  if (length(candidates) != 1) {
    return(NULL)
  }
  candidates
}

#' Coerce a crop/unit/quantity column to numeric codes, loudly
#'
#' Some IHS CSV distributions (notably the IHS5 release) export value *labels*
#' rather than numeric codes, so `crop_code` arrives as "MAIZE LOCAL" and the
#' unit as "50 KG BAG". Coercing those to numeric yields all NA and the whole
#' conversion silently produces an empty column. Fail loudly instead, and point
#' at the fix.
#'
#' @noRd
.as_code <- function(x, col_name) {
  if (is.factor(x)) x <- as.character(x)
  out <- suppressWarnings(as.numeric(x))

  had_values <- sum(!is.na(x) & (!is.character(x) | nzchar(as.character(x))))
  if (had_values > 0 && all(is.na(out))) {
    examples <- utils::head(unique(stats::na.omit(as.character(x))), 3)
    cli::cli_abort(c(
      "Column {.var {col_name}} holds value labels, not numeric codes.",
      "x" = "Example{?s}: {.val {examples}}",
      "i" = paste(
        "Some IHS CSV exports substitute labels for codes. Read the Stata",
        "({.file .dta}) version with {.fn haven::read_dta}, or recode the",
        "labels to their numeric codes before calling {.fn ihs_convert_units}."
      )
    ), class = "ihsMW_labelled_column")
  }

  out
}

#' Find the first of `candidates` present in `nms`, case-insensitively
#' @noRd
.match_col <- function(nms, candidates) {
  nms_lower <- tolower(nms)
  for (cand in candidates) {
    idx <- which(nms_lower == tolower(cand))
    if (length(idx) > 0) {
      return(nms[idx[1]])
    }
  }
  NULL
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
