# ihsMW
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/ihsMW)](https://CRAN.R-project.org/package=ihsMW)
[![R-CMD-check](https://github.com/vituk123/ihsMW/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vituk123/ihsMW/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The `ihsMW` package provides programmatic access to the Malawi Integrated Household Survey (IHS) microdata hosted by the World Bank. Much like the `WDI` package elegantly bridges World Bank macroscopic development indicators into R, `ihsMW` bridges complex, respondent-level household microdata endpoints. It removes the friction of manual downloads, parsing, and pooling, significantly accelerating research workflows.

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
ihs_auth()                              # one-time setup
ihs_search("consumption")              # find variables
df <- IHS("rexp_cat01", round="IHS5") # download data
head(df)
```

## Key features

Discovering targeted survey questions across thousands of potential columns is notoriously difficult due to arbitrary module definitions. In `ihsMW`, you query intuitive indicators via the built-in search functions directly, retrieving exact metadata bounds including categorical label translations efficiently prior to extracting raw dataset files.

Researchers compiling cross-sectional trends often encounter fractured variable names spanning several disparate releases making chronological bindings intensely fragile. By leveraging the built-in harmonisation engine recursively mapping embedded structures implicitly, querying across multiple rounds binds matrices securely bypassing fragmented architectures entirely out-of-the-box.

Traditional unweighted aggregations mask significant spatial biases inherited during rural stratifications invalidating macroscopic insights structurally. Pulling arrays iteratively utilizing dedicated survey weighting helpers builds `tbl_svy` instances capturing multi-tiered cluster architectures effortlessly. 

## Data access note

Accessing the official database necessitates registering a free researcher account directly through the World Bank Microdata Library at https://microdata.worldbank.org. The `ihs_auth()` function intelligently walks you through generating a secure token natively mapping the credentials persistently out-of-sight ensuring frictionless downloads natively.

## Citation

To cite `ihsMW` in publications, please use:

```bibtex
@Manual{,
  title = {ihsMW: Access Malawi Integrated Household Survey Data in R},
  author = {Vitumbiko Kayuni},
  year = {2026},
  note = {R package version 0.1.0},
  url = {https://github.com/vituk123/ihsMW},
}
```

When publishing research utilizing datasets harmonized or accessed via `ihsMW`, always cite both the NSO Malawi and the World Bank LSMS. Please consult the respective round's Basic Information Document for the exact citation format.

## Contributing

We welcome additions and mappings! Please report bugs, suggest mapping configurations, and propose structural adjustments directly on our [GitHub Issues](https://github.com/vituk123/ihsMW/issues) and consult our CONTRIBUTING.md file.

## License

MIT
