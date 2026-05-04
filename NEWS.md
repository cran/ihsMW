# ihsMW 0.1.0

## New features

* `IHS()`: Download IHS microdata by variable name, round, and module.
* `IHS_survey()`: Return survey-design objects for weighted analysis.
* `ihs_auth()`: One-time API key setup with interactive browser-based guide.
* `ihs_search()`: Search variable names and labels across rounds via the crosswalk.
* `ihs_variables()`: Browse all variables in a round via the live NADA API.
* `ihs_label()`: Retrieve Stata variable labels from the crosswalk or NADA.
* `ihs_modules()`: List available data modules per round.
* `ihs_crosswalk_check()`: Assess cross-round variable comparability.
* `ihs_cache_info()` and `ihs_cache_clear()`: Manage local data cache.
* `mwi_surveys()`: Overview of supported and planned Malawi surveys.

## Supported surveys

* IHS2 (2004/05) through IHS5 (2019/20) are fully supported via the API.
* IHS1 (1997/98) is not yet available via the World Bank Microdata Library.

## Data access

* All data downloaded directly from the World Bank Microdata Library.
  A free account and API key are required. See `?ihs_auth`.
