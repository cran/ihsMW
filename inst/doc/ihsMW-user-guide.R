## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.align = "center"
)
library(ihsMW)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("ihsMW")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("pak")
# pak::pak("vituk123/ihsMW")
# 
# # or
# # install.packages("remotes")
# remotes::install_github("vituk123/ihsMW")

## -----------------------------------------------------------------------------
library(ihsMW)
packageVersion("ihsMW")

## ----eval=FALSE---------------------------------------------------------------
# install.packages(c("haven", "survey", "srvyr"))

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(data.frame(
  Stage = c("Discover", "Discover", "Discover", "Discover",
            "Clean", "Clean", "Clean",
            "Transform", "Transform", "Transform",
            "Analyse", "Analyse", "Analyse"),
  Function = c("ihs_search()", "ihs_crosswalk_check()", "ihs_panel_ids()",
               "ihs_harmonise()",
               "ihs_clean()", "ihs_standardize_missing()", "ihs_winsorize()",
               "ihs_convert_units()", "ihs_aggregate()", "ihs_merge()",
               "ihs_deflate()", "ihs_svydesign()", "ihs_report()"),
  Purpose = c(
    "Find a variable by keyword across every round",
    "See how much of the crosswalk is comparable across rounds",
    "Look up the ID, weight and stratum columns for a round",
    "Rename raw columns to consistent harmonised names",
    "Recode missing codes and winsorize, with an audit trail",
    "Turn -99/-98/999 into NA",
    "Cap outliers at percentiles, optionally within strata",
    "Convert harvest units to kilograms with NSO factors",
    "Roll individual or plot records up to the household",
    "Join modules on auto-detected ID columns",
    "Convert nominal kwacha to real, constant-price kwacha",
    "Build a survey design object for correct standard errors",
    "Produce a weighted summary statistics table"
  ),
  check.names = FALSE
))

## -----------------------------------------------------------------------------
ihs_panel_ids("all")

## ----eval=FALSE---------------------------------------------------------------
# library(haven)
# ag <- haven::read_dta("IHS5/ag_mod_i.dta")   # numeric codes, with labels attached

## -----------------------------------------------------------------------------
survey_data <- data.frame(
  region   = rep(c("North", "South"), each = 50),
  food_exp = c(rnorm(50, 100, 10), rnorm(50, 500, 50))
)
survey_data$food_exp[1]  <- -99      # a refusal code
survey_data$food_exp[60] <- 99999    # an implausible outlier

cleaned <- ihs_clean(survey_data,
                     winsorize_vars = "food_exp",
                     winsorize_by   = "region")

str(attr(cleaned, "ihs_audit"))

## -----------------------------------------------------------------------------
res <- ihs_search("household size")
res[, c("harmonised_name", "label", "ihs2_name", "ihs5_name", "ihs6_name", "n_rounds")]

## -----------------------------------------------------------------------------
nrow(ihs_search("livestock", round = "IHS6"))

## -----------------------------------------------------------------------------
nrow(ihs_search("expenditure", fields = "label"))

## -----------------------------------------------------------------------------
cw <- ihs_crosswalk_check(verbose = FALSE)

nrow(cw)                    # total harmonised variables
table(cw$n_rounds_avail)    # how many rounds each appears in

## -----------------------------------------------------------------------------
# Variables available in every round
head(cw$harmonised_name[cw$n_rounds_avail == 5], 15)

## -----------------------------------------------------------------------------
c(
  `IHS3+IHS4+IHS5`      = sum(!is.na(cw$ihs3_name) & !is.na(cw$ihs4_name) &
                              !is.na(cw$ihs5_name)),
  `IHS4+IHS5+IHS6`      = sum(!is.na(cw$ihs4_name) & !is.na(cw$ihs5_name) &
                              !is.na(cw$ihs6_name)),
  `all five rounds`     = sum(cw$n_rounds_avail == 5)
)

## -----------------------------------------------------------------------------
# Multi-select expansions, with their parent question
exp <- cw[!is.na(cw$ihs6_expansion_of),
          c("harmonised_name", "ihs6_expansion_of")]
head(exp, 5)
nrow(exp)

## -----------------------------------------------------------------------------
ihs_panel_ids("IHS6")

## -----------------------------------------------------------------------------
raw <- data.frame(
  case_id = c("a", "b"),
  hhsize  = c(4, 6),
  junk    = c(1, 2)
)

ihs_harmonise(raw, round = "IHS6")

## -----------------------------------------------------------------------------
ihs_harmonise(raw, round = "IHS6", extra = TRUE)

## -----------------------------------------------------------------------------
hh <- data.frame(case_id = c("A", "B", "C"), hhsize = c(4, 6, 3))
ag <- data.frame(case_id = c("A", "B", "D"), harvest_kg = c(120, 340, 90))

merged <- ihs_merge(hh, ag)
merged

## -----------------------------------------------------------------------------
attr(merged, "ihs_merge_log")

## -----------------------------------------------------------------------------
a <- data.frame(case_id = c("A", "B"), region = 1:2, x = 1:2)
b <- data.frame(case_id = c("A", "B"), region = 1:2, y = 3:4)

names(suppressWarnings(ihs_merge(a, b)))

## -----------------------------------------------------------------------------
names(ihs_merge(a, b, by = c("case_id", "region")))

## -----------------------------------------------------------------------------
df <- data.frame(age = c(34, -99, 41, 998), village = c("A", "B", "C", "D"))
clean <- ihs_standardize_missing(df)
clean$age
attr(clean, "ihs_missing_conversions")

## -----------------------------------------------------------------------------
set.seed(42)
df <- data.frame(
  region = rep(c("North", "South"), each = 50),
  cons   = c(rnorm(50, 100, 10), rnorm(50, 500, 50))
)

global    <- ihs_winsorize(df, vars = "cons", probs = c(0.05, 0.95))
by_region <- ihs_winsorize(df, vars = "cons", by = "region",
                           probs = c(0.05, 0.95))

# The raw column is untouched either way
identical(global$cons, df$cons)

## -----------------------------------------------------------------------------
c(global    = max(global$cons_w[global$region == "North"]),
  stratified = max(by_region$cons_w[by_region$region == "North"]))

## -----------------------------------------------------------------------------
result <- ihs_clean(df, winsorize_vars = "cons", winsorize_by = "region")
names(attr(result, "ihs_audit"))

## -----------------------------------------------------------------------------
harvest <- data.frame(
  crop_code = c(1, 1, 1),
  unit_code = c(1, 2, 3),
  quantity  = c(100, 2, 5),
  region    = c(1, 2, 2)
)

ihs_convert_units(harvest, qty_col = "quantity",
                  unit_col = "unit_code", crop_col = "crop_code")

## -----------------------------------------------------------------------------
kg <- data.frame(crop_code = c(1, 48), unit_code = c(1, 1), quantity = c(7, 40))
ihs_convert_units(kg, "quantity", "unit_code", "crop_code")

## -----------------------------------------------------------------------------
odd <- data.frame(crop_code = 999, unit_code = 999, quantity = 10)
suppressWarnings(
  ihs_convert_units(odd, "quantity", "unit_code", "crop_code")
)

## -----------------------------------------------------------------------------
members <- data.frame(
  case_id   = c("A", "A", "B", "B", "B"),
  income    = c(1000, 500, 200, 300, 250),
  has_radio = c(0, 1, 0, 0, 0),
  district  = c("Lilongwe", "Lilongwe", "Blantyre", "Blantyre", "Blantyre")
)

ihs_aggregate(members, group_col = "case_id")

## -----------------------------------------------------------------------------
pooled <- data.frame(
  food_exp  = c(1000, 1000, 1000, 1000, 1000),
  ihs_round = c("IHS2", "IHS3", "IHS4", "IHS5", "IHS6")
)

ihs_deflate(pooled, value_cols = "food_exp")

## -----------------------------------------------------------------------------
ihs_deflate(data.frame(v = 1000), value_cols = "v", round = "IHS6")

## -----------------------------------------------------------------------------
cpi <- read.csv(system.file("extdata", "mw_cpi_annual.csv", package = "ihsMW"))
tail(cpi[, c("year", "cpi_index", "retrieved", "provisional")], 3)

## ----eval=requireNamespace("survey", quietly = TRUE)--------------------------
hh <- data.frame(
  case_id  = 1:20,
  ea_id    = rep(1:5, each = 4),
  strata   = rep(c("urban", "rural"), each = 10),
  hh_wgt   = runif(20, 0.5, 3),
  food_exp = rnorm(20, 5000, 1000)
)

dsgn <- ihs_svydesign(hh)
survey::svymean(~food_exp, dsgn, na.rm = TRUE)

## -----------------------------------------------------------------------------
hh <- data.frame(
  hhsize   = c(4, 6, 3, 5, 7, 2),
  food_exp = c(1200, 3400, 900, 2100, 4500, 700),
  region   = c("North", "North", "Central", "Central", "South", "South"),
  hh_wgt   = c(1.2, 0.8, 1.5, 1.1, 0.9, 1.3)
)

ihs_report(hh, vars = c("hhsize", "food_exp"))

## -----------------------------------------------------------------------------
ihs_report(hh, vars = "food_exp", by = "region", weights = "hh_wgt")

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# library(haven)
# library(survey)
# 
# # 1. Read
# demog <- haven::read_dta("IHS6/hh_mod_a_filt.dta")
# cons  <- haven::read_dta("IHS6/ihs6_consumptionaggregates.dta")
# 
# # 2. Harmonise (adds `ihs_round`)
# demog_h <- ihs_harmonise(demog, round = "IHS6")
# cons_h  <- ihs_harmonise(cons,  round = "IHS6")
# 
# # 3. Merge, dropping columns that appear in both to avoid .x/.y suffixes
# dupes  <- c("region", "district", "hhsize", "hh_wgt", "ihs_round")
# merged <- ihs_merge(demog_h, cons_h[, setdiff(names(cons_h), dupes)])
# 
# # 4. Clean: recode missing codes, winsorize within region
# clean <- ihs_clean(merged,
#                    winsorize_vars = "rexp_cat011",
#                    winsorize_by   = "region")
# 
# # 5. Deflate to 2019 prices
# real <- ihs_deflate(clean, value_cols = "rexp_cat011_w")
# 
# # 6. Survey design
# dsgn <- ihs_svydesign(real)
# 
# # 7. Report
# ihs_report(real,
#            vars    = c("hhsize", "rexp_cat011_w_real"),
#            by      = "region",
#            weights = "hh_wgt")
# 
# # Design-correct national mean with a standard error
# survey::svymean(~rexp_cat011_w_real, dsgn, na.rm = TRUE)
# 
# # Document what cleaning did, for your appendix
# str(attr(clean, "ihs_audit"))

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# library(haven)
# library(dplyr)
# 
# read_round <- function(path, round) {
#   haven::read_dta(path) |> ihs_harmonise(round = round)
# }
# 
# ihs4 <- read_round("IHS4/ihs4_consumption_aggregate.dta", "IHS4")
# ihs5 <- read_round("IHS5/ihs5_consumption_aggregate.dta", "IHS5")
# ihs6 <- read_round("IHS6/ihs6_consumptionaggregates.dta", "IHS6")
# 
# # Check comparability BEFORE pooling
# cw <- ihs_crosswalk_check(verbose = FALSE)
# comparable <- cw$harmonised_name[
#   !is.na(cw$ihs4_name) & !is.na(cw$ihs5_name) & !is.na(cw$ihs6_name)
# ]
# 
# keep <- intersect(comparable, Reduce(intersect, list(names(ihs4), names(ihs5), names(ihs6))))
# keep <- union(keep, "ihs_round")
# 
# pooled <- bind_rows(ihs4[, keep], ihs5[, keep], ihs6[, keep])
# 
# # One call deflates every round correctly, using `ihs_round`
# pooled <- ihs_deflate(pooled, value_cols = "rexp_cat011")
# 
# # Real food consumption by round, on a common 2019 basis
# ihs_report(pooled, vars = "rexp_cat011_real", by = "ihs_round",
#            weights = "hh_wgt")

## ----eval=FALSE---------------------------------------------------------------
# library(ihsMW)
# library(haven)
# 
# # Plot-level harvest, and the household roster for region
# harvest <- haven::read_dta("IHS6/ag_mod_g.dta")
# roster  <- haven::read_dta("IHS6/hh_mod_a_filt.dta")
# 
# # Attach region so region-specific conversion factors are used
# harvest <- ihs_merge(harvest, roster[, c("case_id", "region")], by = "case_id")
# 
# # Convert mixed units to kilograms
# harvest_kg <- ihs_convert_units(
#   harvest,
#   qty_col  = "ag_g13a",
#   unit_col = "ag_g13b",
#   crop_col = "crop_code",
#   unmapped = "warn"
# )
# 
# # Sum to household level
# hh_production <- ihs_aggregate(
#   harvest_kg[, c("case_id", "ag_g13a_kg")],
#   group_col = "case_id"
# )
# 
# head(hh_production)

## ----eval=FALSE---------------------------------------------------------------
# # WRONG
# ihs_clean(df, winsorize_cols = "x", strata_col = "region")
# ihs_winsorize(df, value_col = "x", strata_col = "region")
# ihs_aggregate(df, id_cols = "case_id", val_cols = "x")
# 
# # RIGHT
# ihs_clean(df, winsorize_vars = "x", winsorize_by = "region")
# ihs_winsorize(df, vars = "x", by = "region")
# ihs_aggregate(df, group_col = "case_id")

## ----eval=FALSE---------------------------------------------------------------
# # WRONG
# rural <- data[data$urban == 0, ]
# dsgn  <- ihs_svydesign(rural)
# 
# # RIGHT
# dsgn  <- ihs_svydesign(data)
# rural <- subset(dsgn, urban == 0)

## ----eval=FALSE---------------------------------------------------------------
# # What do I actually have?
# names(data)
# str(data[, 1:10])
# 
# # Did harmonisation do anything?
# sum(names(data) %in% ihs_crosswalk_check(verbose = FALSE)$harmonised_name)
# 
# # Where did my rows go?
# attr(merged, "ihs_merge_log")
# 
# # What did cleaning change?
# str(attr(cleaned, "ihs_audit"))

## ----eval=FALSE---------------------------------------------------------------
# citation("ihsMW")

## ----echo=FALSE---------------------------------------------------------------
cat("Generated with ihsMW", as.character(packageVersion("ihsMW")),
    "on", format(Sys.Date(), "%d %B %Y"), "\n")

