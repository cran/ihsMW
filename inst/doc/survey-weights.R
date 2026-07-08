## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# library(haven)
# 
# # Load and harmonise IHS5 data
# raw_data <- read_dta("path/to/IHS5/hh_mod_a_filt.dta")
# harmonised_data <- ihs_harmonise(raw_data, round = "IHS5")
# 
# # Create survey design object
# # Automatically detects: hh_wgt/hhweight, stratum/strata, and ea_id/psu
# design <- ihs_svydesign(harmonised_data)

## ----eval=FALSE---------------------------------------------------------------
# design <- ihs_svydesign(
#   data = harmonised_data,
#   weight_col = "custom_weight",
#   strata_col = "custom_strata",
#   psu_col = "custom_ea"
# )

## ----eval=FALSE---------------------------------------------------------------
# library(survey)
# 
# # Nationally representative mean of household size
# svymean(~hhsize, design = design, na.rm = TRUE)
# 
# # Nationally representative total of expenditure
# svytotal(~food_exp, design = design, na.rm = TRUE)
# 
# # Calculate means grouped by a factor variable (e.g., region)
# svyby(~food_exp, ~region, design = design, svymean, na.rm = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# library(srvyr)
# 
# # Convert to srvyr design object
# srvyr_design <- as_survey(design)
# 
# # Calculate summary statistics using dplyr verbs
# summary_stats <- srvyr_design |>
#   group_by(region) |>
#   summarise(
#     mean_exp = survey_mean(food_exp, na.rm = TRUE),
#     total_exp = survey_total(food_exp, na.rm = TRUE)
#   )

## ----eval=FALSE---------------------------------------------------------------
# # Generate a summary statistics table with survey weights
# report_tbl <- ihs_report(
#   data = harmonised_data,
#   vars = c("hhsize", "food_exp", "nonfood_exp"),
#   weights = "hh_wgt"
# )
# print(report_tbl)

## ----eval=FALSE---------------------------------------------------------------
# # Grouped weighted summary statistics
# report_grouped <- ihs_report(
#   data = harmonised_data,
#   vars = c("hhsize", "food_exp"),
#   by = "region",
#   weights = "hh_wgt"
# )

