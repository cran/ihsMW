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

## ----eval=FALSE---------------------------------------------------------------
# # Extract consumption metrics mapped identically across IHS3, IHS4, and IHS5
# df <- IHS("rexp_cat01", round = c("IHS3", "IHS4", "IHS5"))
# 
# # The output natively includes an `ihs_round` character tracking origins
# df |>
#   group_by(ihs_round) |>
#   summarise(mean_cons = mean(rexp_cat01, na.rm = TRUE))

