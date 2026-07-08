# ihsMW
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ihsMW)](https://CRAN.R-project.org/package=ihsMW)
[![R-CMD-check](https://github.com/vituk123/ihsMW/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vituk123/ihsMW/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The `ihsMW` package provides a robust, offline suite of tools to clean, aggregate, and harmonise data from the Malawi Integrated Household Survey (IHS) series. It is designed for development economists and data scientists, replacing hundreds of lines of brittle, project-specific data wrangling scripts with a single, citable, and defensible pipeline.

*Note: Due to World Bank data access restrictions, raw microdata files cannot be distributed inside R packages. You must manually download the required `.dta` or `.csv` files from the [World Bank Microdata Library](https://microdata.worldbank.org).*

## Installation

```r
# Install from CRAN
install.packages("ihsMW")

# Or install the development version from GitHub
# install.packages("pak")
pak::pak("vituk123/ihsMW")
```

## Quick start

Here is a complete end-to-end example showing how to load, harmonise, clean, deflate, design, and report on IHS data:

```r
library(ihsMW)
library(haven)

# 1. Load raw data files (downloaded manually from World Bank)
raw_demog <- read_dta("path/to/IHS5/hh_mod_a_filt.dta")
raw_agri  <- read_dta("path/to/IHS5/ag_mod_i.dta")

# 2. Harmonise column names automatically to the cross-round standard
demog_harm <- ihs_harmonise(raw_demog, round = "IHS5")
agri_harm  <- ihs_harmonise(raw_agri, round = "IHS5")

# 3. Merge modules (automatically detects join keys)
merged_df <- ihs_merge(demog_harm, agri_harm)

# 4. Clean, standardize missing codes, and winsorize extreme outliers
clean_df <- ihs_clean(
  data = merged_df,
  missing_cols = "food_exp",
  winsorize_cols = "food_exp",
  strata_col = "urban"
)

# 5. Deflate nominal values to 2019 real prices
real_df <- ihs_deflate(clean_df, value_cols = "food_exp")

# 6. Create survey design object (automatically detects weights, strata, PSU)
design <- ihs_svydesign(real_df)

# 7. Generate a publication-ready summary statistics table
report_tbl <- ihs_report(
  data = real_df,
  vars = c("hhsize", "food_exp_real"),
  by = "region",
  weights = "hh_wgt"
)
print(report_tbl)
```

## Function Overview

| Function | Category | Description |
|---|---|---|
| `ihs_harmonise()` | Harmonisation | Rename raw .dta columns to harmonised names using the crosswalk. |
| `ihs_search()` | Discovery | Search variable names and labels across rounds. |
| `ihs_crosswalk_check()` | Quality Check | Assess cross-round variable comparability and review flags. |
| `ihs_panel_ids()` | Helper | Get standard household/individual ID columns for any IHS round. |
| `ihs_merge()` | Merging | Merge multiple harmonised dataframes with auto-detected keys. |
| `ihs_deflate()` | Deflation | CPI-based deflation to 2019 prices for real cross-round comparison. |
| `ihs_svydesign()` | Analysis | Set up a survey design object with auto-detected weights/strata/PSU. |
| `ihs_report()` | Analysis | Generate publication-ready weighted summary statistics tables. |
| `ihs_clean()` | Cleaning | Master cleaning wrapper (missing values & winsorization). |
| `ihs_standardize_missing()`| Cleaning | Convert survey missing codes (-99, -98, etc.) to NA. |
| `ihs_winsorize()` | Cleaning | Stratified winsorization with `_w` suffix columns. |
| `ihs_convert_units()` | Agriculture | Crop-specific unit-to-kg conversion using NSO factors. |
| `ihs_aggregate()` | Aggregation | Type-aware aggregation to the household level. |

## Documentation & Vignettes

To learn more about the package features, please consult the vignettes:
- [Getting started with ihsMW](vignettes/getting-started.Rmd)
- [Cross-round harmonisation](vignettes/harmonisation.Rmd)
- [Working with survey weights](vignettes/survey-weights.Rmd)

## Citation

To cite `ihsMW` in publications, please use:

```bibtex
@Manual{,
  title = {ihsMW: Clean and Harmonise Malawi Integrated Household Survey Data},
  author = {Vitumbiko Kayuni},
  year = {2026},
  note = {R package version 0.3.0},
  url = {https://github.com/vituk123/ihsMW},
}
```

When publishing research utilizing datasets harmonised or cleaned via `ihsMW`, always cite both the NSO Malawi and the World Bank LSMS. Please consult the respective round's Basic Information Document for the exact citation format.

## Contributing

We welcome additions and mappings! Please report bugs, suggest crosswalk configurations, and propose structural adjustments directly on our [GitHub Issues](https://github.com/vituk123/ihsMW/issues).

## License

MIT
