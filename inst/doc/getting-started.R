## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# # Install from GitHub
# pak::pak("vituk123/ihsMW")
# # or
# remotes::install_github("vituk123/ihsMW")

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# 
# # Open the interactive authentication wizard
# ihs_auth()

## ----eval=FALSE---------------------------------------------------------------
# ihs_auth("your_alphanumeric_api_key_goes_here")

## ----eval=FALSE---------------------------------------------------------------
# # Look up variables related to consumption
# ihs_search("consumption")

## ----eval=FALSE---------------------------------------------------------------
# # Find age-related variables specifically monitored during IHS5
# ihs_search("age", round = "IHS5")

## ----eval=FALSE---------------------------------------------------------------
# # Look at all modules administered in IHS5
# ihs_modules("IHS5")

## ----eval=FALSE---------------------------------------------------------------
# ihs_label("rexp_cat01")

## ----eval=FALSE---------------------------------------------------------------
# # Simple extraction targeted against IHS5
# df_simple <- IHS("rexp_cat01", round = "IHS5")

## ----eval=FALSE---------------------------------------------------------------
# # Multi-round pooled extractions mapping harmonisations intelligently
# df_multi <- IHS(c("rexp_cat01", "hh_a02"), round = c("IHS4", "IHS5"))

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# library(dplyr)
# library(ggplot2)
# 
# # Find the consumption variable
# ihs_search("per capita consumption")
# 
# # Download IHS5 consumption data
# df <- IHS("rexp_cat01", round = "IHS5")
# 
# # Quick summary
# df |> summarise(mean_cons = mean(rexp_cat01, na.rm = TRUE))
# 
# # Simple histogram
# ggplot(df, aes(x = rexp_cat01)) +
#   geom_histogram(bins = 50) +
#   labs(title = "Distribution of per capita consumption, Malawi IHS5")

