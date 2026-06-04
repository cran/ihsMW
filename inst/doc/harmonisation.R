## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# 
# # Load the raw crosswalk embedded inside the package
# cw <- read.csv(system.file("extdata", "ihs_crosswalk.csv", package = "ihsMW"))
# head(cw)

## ----eval=FALSE---------------------------------------------------------------
# # Prints a report showing variable availability across rounds
# ihs_crosswalk_check()

## ----eval=FALSE---------------------------------------------------------------
# library(dplyr)
# 
# # Search for consumption and inspect coverage arrays
# ihs_search("consumption") |>
#   select(harmonised_name, ihs2_name, ihs3_name, ihs4_name, ihs5_name)

