# ihsMW 1.0.0

## Major release
* First stable release on CRAN.
* All features from 0.3.0 development version now stable.

## New features
* `ihs_merge()`: Merge multiple harmonised data.frames with auto-detected join keys.
* `ihs_deflate()`: CPI-based deflation for cross-round real price comparisons.
* `ihs_svydesign()`: One-line creation of `survey.design2` objects with auto-detected weights/strata/PSU.
* `ihs_report()`: Publication-ready summary statistics tables with optional grouping and survey weights.
* `ihs_panel_ids()`: Quick reference for standard ID columns by round.

## Documentation
* Complete rewrite of all vignettes for the offline cleaning workflow.
* Added JOSS paper draft.
* Added GitHub issue templates for crosswalk mappings and conversion factors.

# ihsMW 0.3.0 (development)

## New features
* `ihs_merge()`: Merge multiple harmonised data.frames with auto-detected join keys.
* `ihs_deflate()`: CPI-based deflation for cross-round real price comparisons.
* `ihs_svydesign()`: One-line creation of `survey.design2` objects with auto-detected weights/strata/PSU.
* `ihs_report()`: Publication-ready summary statistics tables with optional grouping and survey weights.
* `ihs_panel_ids()`: Quick reference for standard ID columns by round.

## Documentation
* Complete rewrite of all vignettes for the offline cleaning workflow.
* Added JOSS paper draft.
* Added GitHub issue templates for crosswalk mappings and conversion factors.

# ihsMW 0.2.1

## Bug fixes
* Fixed spelling NOTE for 'winsorization' on CRAN.
* Added `scratch/` to `.Rbuildignore`.

# ihsMW 0.2.0

## Breaking changes
* **Package pivot**: Removed all API download infrastructure. The package is now a 100% offline cleaning and harmonisation suite.
* Removed `IHS()`, `ihs_auth()`, `IHS_survey()`, `ihs_variables()`, `ihs_label()`, `ihs_modules()`, `ihs_cache_info()`, `ihs_cache_clear()`, `mwi_surveys()`.
* Dropped `httr2` and `httptest2` dependencies.

## New features
* `ihs_harmonise()`: Rename raw .dta columns to harmonised names using the crosswalk.
* `ihs_clean()`: Master cleaning wrapper with audit trail.
* `ihs_standardize_missing()`: Convert survey missing codes (-99, -98, etc.) to NA.
* `ihs_winsorize()`: Stratified winsorization with `_w` suffix columns.
* `ihs_convert_units()`: Crop-specific unit-to-kg conversion using official NSO factors.
* `ihs_aggregate()`: Type-aware aggregation to household level.
* Bundled 1,608 official NSO crop conversion factors.
* Crosswalk expanded to 5,829 harmonised variables.

# ihsMW 0.1.0

## Initial release
* API-based download workflow (now removed in v0.2.0).
* `ihs_search()` and `ihs_crosswalk_check()` for variable discovery.
