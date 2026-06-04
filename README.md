# ihsMW
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ihsMW)](https://CRAN.R-project.org/package=ihsMW)
[![R-CMD-check](https://github.com/vituk123/ihsMW/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vituk123/ihsMW/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The `ihsMW` package provides a robust, offline suite of tools to clean, aggregate, and harmonise data from the Malawi Integrated Household Survey (IHS). It is designed by and for development economists and data scientists, replacing hundreds of lines of brittle, project-specific data wrangling scripts with a single, citable, and defensible pipeline.

*Note: Due to World Bank data access restrictions, this package no longer downloads raw microdata via the NADA API. You must manually download the required `.dta` or `.csv` files from the [World Bank Microdata Library](https://microdata.worldbank.org).*

## Installation

```r
# CRAN (coming soon)
install.packages("ihsMW")

# Development version
devtools::install_github("vituk123/ihsMW")
```

## Quick start

```r
library(ihsMW)
library(haven)

# 1. Load your manually downloaded data
raw_data <- read_dta("path/to/IHS5/hh_mod_a_filt.dta")

# 2. Harmonise column names automatically to the cross-round standard
harmonised_df <- ihs_harmonise(raw_data, round = "IHS5")

# 3. Clean, standardize missing codes, and winsorize extreme outliers
clean_df <- ihs_clean(
  harmonised_df, 
  winsorize_vars = "consumption", 
  winsorize_by = "region", 
  probs = c(0.01, 0.99)
)

# 4. View the audit trail to see exactly what was modified
print(attr(clean_df, "ihs_audit"))
```

## Key features

### 1. Cross-Round Harmonisation
Traditional multi-round analyses require tedious, manual variable mapping. `ihsMW` includes a built-in, curated crosswalk. Pass any raw IHS dataframe into `ihs_harmonise(data, round = "IHS5")`, and the package instantly renames columns to their standard, longitudinal identifiers (e.g., automatically mapping `hh_a02` to `region` or tracking complex changes like `af_bio_12` to `af_bio_12_x` across rounds).

### 2. Defensible Data Cleaning
`ihs_clean()` serves as a master wrapper for standard survey wrangling:
- **Missing Value Standardization**: Converts standard survey missing codes (e.g., `-99`, `999`) to R `NA` values automatically.
- **Stratified Winsorization**: Caps extreme outliers non-destructively. Use `ihs_winsorize()` directly to apply stratified thresholds (e.g., trimming the 99th percentile of consumption separately for urban vs. rural areas to prevent over-trimming poor regions). Original columns are kept, and winsorized columns are appended with a `_w` suffix.
- **Audit Trails**: Every operation is logged and attached as an attribute to your dataframe (`attr(df, "ihs_audit")`), making your cleaning steps highly transparent for academic replication.

### 3. Crop-Specific Unit Conversions
Agricultural productivity analysis is notoriously difficult due to local units (e.g., pails, heaps, oxcarts). `ihs_convert_units()` leverages official NSO crop-specific conversion factors to calculate standard kilograms. A "pail" of groundnuts weighs differently than a "pail" of maize—this function handles the math and warns you if unmapped unit codes exist in your dataset.

### 4. Smart Aggregation
Rolling individual-level data up to the household level is simplified with `ihs_aggregate()`. It automatically detects column types: summing continuous quantities, applying logical `OR` for dummy variables, and warning on ambiguous text columns.

## Citation

To cite `ihsMW` in publications, please use:

```bibtex
@Manual{,
  title = {ihsMW: Clean and Harmonise Malawi Integrated Household Survey Data},
  author = {Vitumbiko Kayuni},
  year = {2026},
  note = {R package version 0.2.0.9000},
  url = {https://github.com/vituk123/ihsMW},
}
```

When publishing research utilizing datasets harmonised or cleaned via `ihsMW`, always cite both the NSO Malawi and the World Bank LSMS. Please consult the respective round's Basic Information Document for the exact citation format.

## Contributing

We welcome additions and mappings! Please report bugs, suggest crosswalk configurations, and propose structural adjustments directly on our [GitHub Issues](https://github.com/vituk123/ihsMW/issues).

## License

MIT
