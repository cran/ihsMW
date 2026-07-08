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
# # Winsorize outliers (e.g. food expenditure) stratified by urban/rural
# df_winsor <- ihs_winsorize(df_clean, value_col = "food_exp", strata_col = "urban")
# 
# # Run the master cleaning wrapper which applies both steps and logs changes
# df_cleaned <- ihs_clean(
#   data = harmonised_data,
#   missing_cols = c("food_exp", "nonfood_exp"),
#   winsorize_cols = "food_exp",
#   strata_col = "urban"
# )

## ----eval=FALSE---------------------------------------------------------------
# # Convert quantities reported in non-standard units to kilograms
# crop_data <- data.frame(
#   crop_code = c(1, 2),
#   unit_code = c(3, 4),
#   quantity = c(10, 5),
#   region = c(1, 2)
# )
# 
# crop_data_kg <- ihs_convert_units(
#   data = crop_data,
#   crop_col = "crop_code",
#   unit_col = "unit_code",
#   qty_col = "quantity",
#   region_col = "region"
# )

## ----eval=FALSE---------------------------------------------------------------
# # Aggregate individual-level education to household level
# hh_edu <- ihs_aggregate(
#   data = member_data,
#   id_cols = "case_id",
#   val_cols = c("years_education", "completed_primary")
# )

