# tests/testthat/helper-ihsMW.R

# Inject a mock token enforcing safe sandbox limits natively
Sys.setenv(WORLDBANK_MICRODATA_KEY = "test_key_mock")

# Implement a functional mock crosswalk representing explicit targets directly 
mock_crosswalk <- data.frame(
  harmonised_name = c("rexp_cat01", "hh_a02", "hh_size", "urban", "region"),
  ihs1_name = c("rexp_cat011", "q1", NA, "urbrur", "reg"),
  ihs2_name = c("rexp_cat011", "q1", "hhsize", "urbrur", "reg"),
  ihs3_name = c("rexp_cat011", "hh_a02", "hh_size", "urban", "region"),
  ihs4_name = c("rexp_cat01", "hh_a02", "hh_size", "urban", "region"),
  ihs5_name = c("rexp_cat01", "hh_a02", "hh_size", "urban", "region"),
  label = c("Per capita consumption", "Household ID", "Household size", "Urban/Rural", "Region"),
  module = rep("Household Characteristics", 5),
  topic = rep("Demographics", 5),
  needs_review = c(FALSE, FALSE, FALSE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Mocked output dataframe specifically mirroring raw fetch requests accurately
mock_df <- data.frame(
  ihs_round = rep("IHS5", 10),
  rexp_cat01 = runif(10, 15000, 450000),
  hh_size = sample(1:9, 10, replace = TRUE),
  urban = factor(sample(c("Urban", "Rural"), 10, replace = TRUE)),
  ea_id = paste0("EA", sample(100:999, 10, replace = TRUE)),
  strata = paste0("STR", sample(10:99, 10, replace = TRUE)),
  hh_wgt = runif(10, 0.4, 3.1),
  stringsAsFactors = FALSE
)

# Expose a bypass orchestrator completely mitigating raw authenticated REST data targets
mock_ihs_fetch <- function(round, module, format = "parquet", cache = TRUE, progress = TRUE) {
  # Dynamically return structured subset bindings simulating successful raw fetches
  return(mock_df)
}
