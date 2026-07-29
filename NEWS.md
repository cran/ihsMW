# ihsMW 1.1.1

* Replaced four relative links (`../ihsMW-user-guide.pdf` and
  `../ihsMW-user-guide.Rmd`) in the `getting-started` and `ihsMW-user-guide`
  vignettes with absolute URLs to the package website. The relative form
  resolved correctly on the pkgdown site but pointed outside the package when
  the vignettes were installed to `inst/doc/`, which CRAN's incoming checks
  flagged. No user-facing behaviour changes.

Version 1.1.0 was not released; see its entry below for the substantive
changes, all of which are included here.

# ihsMW 1.1.0

## IHS6 support

* Added full support for **IHS6 (2024/25)** across every round-aware function:
  `ihs_harmonise()`, `ihs_search()`, `ihs_panel_ids()`, `ihs_deflate()`,
  `ihs_crosswalk_check()` and `check_round()`.
* The bundled crosswalk gains an `ihs6_name` column with **3,214 IHS6
  variables**, built from the official IHS6 data dictionaries rather than
  inferred. 2,561 map onto existing harmonised variables and 653 are new in
  IHS6, of which 632 are flagged `needs_review` (see below). The crosswalk now
  holds 6,482 variables.
* `ihs_panel_ids("IHS6")` records the design stratum rename from `stratum` to
  `strata`. `ihs_svydesign()` already detected both, so survey designs build
  unchanged on IHS6 data.
* `ihs_deflate()` maps IHS6 to its 2024 survey midpoint year.

## Data corrections

* **The bundled CPI table was wrong and has been rebuilt.** It was labelled
  "WDI" but did not match the WDI series: it understated 2004-2016 by roughly
  7% and understated 2024 by 13.5% (implying 19.6% inflation in 2024 against an
  actual 32.2%). Because `ihs_deflate()` scales every monetary variable by
  `base_cpi / round_cpi`, that error propagated into every real-terms figure
  the package produced. The table is now derived from World Bank WDI
  `FP.CPI.TOTL` rebased to 2019 = 100, covering 2004-2025, and is regenerated
  by `data-raw/02_build_cpi.R`. **Deflated values will change.**
* **The crosswalk's `n_rounds` column was stale** for 5,779 of 5,829 rows
  (99.1%), claiming 4-round coverage for nearly every variable when only 50
  appear in all of IHS2-IHS5. `ihs_search()` displays and sorts by this column,
  so search results systematically misreported cross-round availability. It is
  now recomputed from the round-name columns.

## Data quality and transparency

* **Kilogram quantities now convert for every crop.** Unit code 1 is KILOGRAM
  and carries a factor of exactly 1 for all 34 crops and 3 regions in the NSO
  table, but crops introduced in IHS6 had no row at all, so quantities already
  reported in kilograms came back as `NA`. `ihs_convert_units()` now applies
  the identity, recovering 506 rows (5.3%) across the IHS6 harvest and sales
  modules. The rule is **derived from the bundled table, not hard-coded**: if a
  future NSO release gives the kilogram unit a crop-specific factor, it
  switches itself off.
* **New crosswalk column `ihs6_expansion_of`.** IHS6 is collected in Survey
  Solutions, which splits a "select all that apply" question into one binary
  column per option - IHS5's single `ag_e07` becomes `ag_e07__1`, `ag_e07__4`
  and so on. 342 IHS6 columns are such expansions. They are now labelled with
  their parent question instead of being lumped into `needs_review`, which
  drops from 653 to 632 and, more importantly, now means something specific:
  *new in IHS6 and not cross-checked against an earlier instrument*.
  `ihs_search()` returns the column and `ihs_crosswalk_check()` reports it.
* **CPI provenance.** `mw_cpi_annual.csv` gains `retrieved` and `provisional`
  columns. The World Bank revises recent observations, so the two most recent
  years are flagged, and `ihs_deflate()` says so when a round you are deflating
  depends on one - as IHS6 (2024) currently does.

## Bug fixes

* `ihs_harmonise()` could rename the same raw column twice when two crosswalk
  entries differed only in letter case, silently overwriting the first
  harmonised name and then failing with "Can't subset columns that don't
  exist". Each raw column is now claimed by at most one crosswalk entry.
* `ihs_convert_units()` returned a silent all-`NA` column when the crop or unit
  column contained value labels instead of numeric codes - the case for some
  IHS CSV distributions, including IHS5. It now fails with an explanatory error
  naming the offending values and pointing at the `.dta` distribution.
* `ihs_merge()` no longer warns about row growth on a `type = "full"` join,
  where more rows than either input is the expected result.
* `ihs_merge()` now warns when columns present in more than one input are given
  `.x`/`.y` suffixes. This silently breaks downstream auto-detection: once
  `hh_wgt` becomes `hh_wgt.x`, `ihs_svydesign()` can no longer find the survey
  weight.
* `ihs_deflate()` now validates the `ihs_round` column up front, so an
  unrecognised round produces an actionable message instead of a "subscript out
  of bounds" error.

## Performance

* `ihs_convert_units()` is **5.2x faster** (1.51 s to 0.29 s on 99,988 crop
  records). The row-by-row factor lookup was replaced with a vectorised one;
  output is bit-for-bit identical, verified across IHS4, IHS5 and IHS6
  agriculture modules.

## Documentation

* **New: the ihsMW User Guide** - a complete standalone manual covering
  installation, dependencies, input requirements, output structure, every
  exported function, three worked end-to-end workflows, common mistakes,
  troubleshooting, FAQs and best practices. Available three ways: as an
  [online article](https://vituk123.github.io/ihsMW/articles/ihsMW-user-guide.html),
  as a [downloadable PDF](https://vituk123.github.io/ihsMW/ihsMW-user-guide.pdf),
  and as [R Markdown source](https://vituk123.github.io/ihsMW/ihsMW-user-guide.Rmd).
  All three are rebuilt from one source on every site deploy.
* Fixed argument names in the README and vignettes that did not exist:
  `ihs_clean(missing_cols=, winsorize_cols=, strata_col=)`,
  `ihs_winsorize(value_col=, strata_col=)`,
  `ihs_convert_units(region_col=)` and `ihs_aggregate(id_cols=, val_cols=)`.
  Every documented example now runs as written.
* Added runnable examples to `ihs_clean()`, `ihs_standardize_missing()`,
  `ihs_winsorize()`, `ihs_aggregate()`, `ihs_convert_units()` and
  `ihs_harmonise()`, and replaced the `\dontrun{}` examples in `ihs_merge()`,
  `ihs_report()` and `ihs_svydesign()` with executable ones.
* `ihs_search()` no longer refers users to `ihs_variables()`, removed in 0.2.0.
* Added `benchmarks/`, a reproducible before-and-after benchmark suite with
  publication-quality figures.
* The user guide gains a **Known limitations** section stating plainly what the
  package cannot do and why, including the evidence behind the thin
  full-series coverage: IHS2 is a smaller instrument *and* the crosswalk
  under-links it, because 98% of its rows match on variable name and IHS2 used
  an entirely different naming scheme.
* Removed eight pre-1.0 process notes (`QA_REPORT.md`, `CRAN_FIX_SUMMARY.md`,
  `READY_FOR_*.md` and similar) from the repository root. pkgdown had been
  rendering them into the published website.

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
