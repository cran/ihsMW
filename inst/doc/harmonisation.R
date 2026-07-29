## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# 
# # Read the crosswalk
# crosswalk <- read.csv(system.file("extdata", "ihs_crosswalk.csv", package = "ihsMW"))
# head(crosswalk)

## ----eval=FALSE---------------------------------------------------------------
# library(haven)
# 
# # Load IHS4 raw household module
# ihs4_raw <- read_dta("path/to/IHS4/hh_mod_a_filt.dta")
# 
# # Harmonise to standard names
# ihs4_clean <- ihs_harmonise(ihs4_raw, round = "IHS4")

## ----eval=FALSE---------------------------------------------------------------
# library(dplyr)
# 
# # Load and harmonise IHS4
# ihs4_raw <- read_dta("path/to/IHS4/hh_mod_a_filt.dta")
# ihs4_harm <- ihs_harmonise(ihs4_raw, round = "IHS4")
# 
# # Load and harmonise IHS5
# ihs5_raw <- read_dta("path/to/IHS5/hh_mod_a_filt.dta")
# ihs5_harm <- ihs_harmonise(ihs5_raw, round = "IHS5")
# 
# # Load and harmonise IHS6
# ihs6_raw <- read_dta("path/to/IHS6/hh_mod_a_filt.dta")
# ihs6_harm <- ihs_harmonise(ihs6_raw, round = "IHS6")
# 
# # Bind rows - ihs_harmonise adds an `ihs_round` column automatically
# pooled_data <- bind_rows(ihs4_harm, ihs5_harm, ihs6_harm)

## -----------------------------------------------------------------------------
library(ihsMW)

cw <- ihs_crosswalk_check(verbose = FALSE)
table(cw$n_rounds_avail)

## ----eval=FALSE---------------------------------------------------------------
# # Load household demographics and crop harvest modules
# hh_demog <- read_dta("path/to/IHS5/hh_mod_a_filt.dta") |> ihs_harmonise("IHS5")
# hh_agri <- read_dta("path/to/IHS5/ag_mod_i.dta") |> ihs_harmonise("IHS5")
# 
# # Merge modules - automatically detects common ID columns (e.g., case_id)
# merged_data <- ihs_merge(hh_demog, hh_agri)

## ----eval=FALSE---------------------------------------------------------------
# # Deflate expenditure variables to 2019 real prices
# real_data <- ihs_deflate(
#   data = pooled_data,
#   value_cols = c("consumption_nominal", "food_exp_nominal")
# )
# # This creates new columns: `consumption_nominal_real` and `food_exp_nominal_real`

## ----eval=FALSE---------------------------------------------------------------
# ihs_crosswalk_check()

## ----eval=FALSE---------------------------------------------------------------
# # Get standard ID columns for IHS5
# ihs_panel_ids("IHS5")

