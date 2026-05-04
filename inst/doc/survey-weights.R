## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# 
# # Automatically intercept consumption variables and inject structural weighting
# svy <- IHS_survey("rexp_cat01", round = "IHS5")
# 
# # The output natively masks as a tbl_svy allowing tidy-eval manipulation
# class(svy)
# #> [1] "tbl_svy"     "svydesign2"  "svydesign"

## ----eval=FALSE---------------------------------------------------------------
# library(survey)
# 
# # Compute the statistically accurate, nationally representative average
# svymean(~rexp_cat01, design = svy, na.rm = TRUE)
# 
# # Segment the nationally representative consumption by explicit strata
# svyby(~rexp_cat01, ~stratum, svy, svymean, na.rm = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# library(srvyr)
# 
# # Tidy-style summaries mapping the underlying survey dimensions natively
# svy |>
#   group_by(stratum) |>
#   summarise(mean_cons = survey_mean(rexp_cat01, na.rm = TRUE))

## ----eval=FALSE---------------------------------------------------------------
# # Requesting pooled objects targets isolated arrays preserving isolated bounds
# svy_list <- IHS_survey("rexp_cat01", round = c("IHS4", "IHS5"))
# 
# # Apply functional iteration computing the unique representation safely
# lapply(svy_list, function(s) {
#   survey::svymean(~rexp_cat01, design = s, na.rm = TRUE)
# })

