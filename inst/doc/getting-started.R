## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("ihsMW")

## ----eval=FALSE---------------------------------------------------------------
# # Install using pak
# pak::pak("vituk123/ihsMW")
# 
# # Or using remotes
# remotes::install_github("vituk123/ihsMW")

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# library(haven)
# 
# # Load the raw Stata file
# raw_data <- read_dta("path/to/IHS5/hh_mod_a_filt.dta")
# 
# # Harmonise variables to standard names
# harmonised_data <- ihs_harmonise(raw_data, round = "IHS5")

## ----eval=FALSE---------------------------------------------------------------
# # Search for consumption-related variables
# ihs_search("consumption")
# 
# # Search for age within a specific round
# ihs_search("age", round = "IHS5")

## ----eval=FALSE---------------------------------------------------------------
# ihs_crosswalk_check()

## ----eval=FALSE---------------------------------------------------------------
# # Convert standard survey missing codes (-99, -98, etc.) to NA
# df_clean <- ihs_standardize_missing(harmonised_data)
# 
# # Winsorize outliers (e.g. food expenditure) stratified by urban/rural.
# # This adds a `food_exp_w` column and leaves `food_exp` untouched.
# df_winsor <- ihs_winsorize(df_clean, vars = "food_exp", by = "urban")
# 
# # Run the master cleaning wrapper which applies both steps and logs changes
# df_cleaned <- ihs_clean(
#   data = harmonised_data,
#   winsorize_vars = c("food_exp", "nonfood_exp"),
#   winsorize_by = "urban"
# )
# 
# # Every transformation is recorded on the result
# str(attr(df_cleaned, "ihs_audit"))

## -----------------------------------------------------------------------------
library(ihsMW)

# A `region` column, if present, is detected automatically and used to pick
# region-specific factors. There is no `region_col` argument.
crop_data <- data.frame(
  crop_code = c(1, 2),
  unit_code = c(3, 4),
  quantity  = c(10, 5),
  region    = c(1, 2)
)

crop_data_kg <- ihs_convert_units(
  data     = crop_data,
  crop_col = "crop_code",
  unit_col = "unit_code",
  qty_col  = "quantity"
)

crop_data_kg

## -----------------------------------------------------------------------------
member_data <- data.frame(
  case_id           = c("A", "A", "B", "B"),
  years_education   = c(8, 12, 4, 6),
  completed_primary = c(1, 1, 0, 1)
)

ihs_aggregate(member_data, group_col = "case_id")

