# Changelog

## onet2r 0.4.1

- Rebuilt README and article figures with a shared ggplot2 style so
  pkgdown examples render readable, dark-mode-safe output instead of
  base graphics.
- Replaced broad console table dumps in README and vignettes with
  bounded [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)
  tables from actual `onet2r` function results.
- Expanded the longitudinal archive, OEWS, sensitivity, and
  decomposition articles with practical cross-vintage examples using
  packaged fixtures.
- Added pkgdown callouts, branded favicon colors, citation metadata,
  lifecycle badge documentation, and release notes for the v0.4.1 polish
  pass.
- Strengthened fixture tests for task rating scale handling and
  multi-cell aggregation guards.

## onet2r 0.4.0

- [`onet_coverage()`](https://farach.github.io/onet2r/reference/onet_coverage.md)
  and
  [`onet_provenance()`](https://farach.github.io/onet2r/reference/onet_provenance.md)
  expose coverage and provenance metadata through durable accessors
  instead of requiring users to inspect attributes.
- [`onet_decompose_change()`](https://farach.github.io/onet2r/reference/onet_decompose_change.md)
  now returns coverage metadata as an accessor-readable list-column.
- [`onet_join_oews()`](https://farach.github.io/onet2r/reference/onet_join_oews.md),
  [`onet_pums_employment_weights()`](https://farach.github.io/onet2r/reference/onet_pums_employment_weights.md),
  and
  [`onet_weighted_summary()`](https://farach.github.io/onet2r/reference/onet_weighted_summary.md)
  now emit lifecycle guidance toward the vintage-aware
  [`onet_weight_panel_oews()`](https://farach.github.io/onet2r/reference/onet_weight_panel_oews.md),
  [`onet_weight_panel_pums()`](https://farach.github.io/onet2r/reference/onet_weight_panel_pums.md),
  and
  [`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md)
  workflow.
- [`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md)
  now requires a single weight year and one optional cell, with explicit
  `year` and `cell` filters for multi-period or grouped panels.
- [`onet_measure_sensitivity()`](https://farach.github.io/onet2r/reference/onet_measure_sensitivity.md)
  runs a user-supplied measure across alternative bridges, weight
  panels, and task-handling choices, then reports baseline movement with
  provenance.
- Added cross-vintage and task-rating fixtures so tests and articles can
  demonstrate 2010-to-2019 O\*NET-SOC behavior without network access.

## onet2r 0.3.0

- Added CI, README.Rmd, and a reproducible package logo source for
  cleaner release maintenance.
- [`onet_archive_read()`](https://farach.github.io/onet2r/reference/onet_archive_read.md)
  now preserves task and DWA native fields when reading O\*NET Task
  Statements, Task Ratings, Tasks to DWAs, and the 30.3 GWAs to IWAs to
  DWAs file.
- [`onet_crosswalk_bridge()`](https://farach.github.io/onet2r/reference/onet_crosswalk_bridge.md)
  now keeps 8-digit O\*NET-SOC detail codes as the native bridge grain
  and includes derived 6-digit SOC columns only for employment joins.
- [`onet_decompose_change()`](https://farach.github.io/onet2r/reference/onet_decompose_change.md)
  decomposes aggregate change into within, between, interaction, and
  unclassifiable components with a sum-to-total check.
- [`onet_measure()`](https://farach.github.io/onet2r/reference/onet_measure.md),
  [`onet_task_to_occupation()`](https://farach.github.io/onet2r/reference/onet_task_to_occupation.md),
  [`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md),
  and
  [`onet_robustness_diagnostic()`](https://farach.github.io/onet2r/reference/onet_robustness_diagnostic.md)
  add bring-your-own-measure validation, task-to-occupation aggregation,
  employment-weighted aggregation, provenance, and plumbing-sensitivity
  checks.
- [`onet_panel_reconcile()`](https://farach.github.io/onet2r/reference/onet_panel_reconcile.md)
  now classifies transition rows, suppressed rows, new rows, and dropped
  rows explicitly instead of counting them as ordinary matched updates.
- [`onet_reference_soc_resolve()`](https://farach.github.io/onet2r/reference/onet_reference_soc_resolve.md),
  [`onet_weight_panel_oews()`](https://farach.github.io/onet2r/reference/onet_weight_panel_oews.md),
  and
  [`onet_weight_panel_pums()`](https://farach.github.io/onet2r/reference/onet_weight_panel_pums.md)
  create reference-SOC mappings and normalized employment-weight panels
  for OEWS and PUMS workflows.

## onet2r 0.2.0

- Added a longitudinal O\*NET background article and verified reference
  file to help users understand archive panels, taxonomy changes, and
  comparability limits before interpreting release-to-release changes.
- [`onet_archive_download()`](https://farach.github.io/onet2r/reference/onet_archive_download.md)
  downloads text-format O\*NET database archives by release version for
  reproducible local analysis.
- [`onet_archive_read()`](https://farach.github.io/onet2r/reference/onet_archive_read.md)
  reads descriptor tables from downloaded archives into a normalized
  long panel schema and now accepts local ZIP files or extracted archive
  directories for offline workflows.
- [`onet_change_summary()`](https://farach.github.io/onet2r/reference/onet_change_summary.md)
  summarizes reconciled O\*NET changes overall and by SOC job family.
- [`onet_crosswalk_bridge()`](https://farach.github.io/onet2r/reference/onet_crosswalk_bridge.md)
  builds SOC taxonomy bridges across supported O\*NET vintages and
  labels one-to-one, split, and merge mappings.
- [`onet_panel()`](https://farach.github.io/onet2r/reference/onet_panel.md)
  assembles descriptor tables across multiple O\*NET releases for
  longitudinal analysis and can use local archive paths with explicit
  release dates.
- [`onet_panel_reconcile()`](https://farach.github.io/onet2r/reference/onet_panel_reconcile.md)
  classifies release-to-release changes as real updates, stable
  resampling, stale carryforwards, or recode/recalculation flags.
- [`onet_releases()`](https://farach.github.io/onet2r/reference/onet_releases.md)
  lists O\*NET database releases, archive URLs, dictionary URLs, release
  dates, and inferred SOC taxonomy vintages.

## onet2r 0.1.0

- [`onet_cache_use()`](https://farach.github.io/onet2r/reference/onet_cache_use.md),
  [`onet_cache_clear()`](https://farach.github.io/onet2r/reference/onet_cache_clear.md),
  and
  [`onet_rate_limit()`](https://farach.github.io/onet2r/reference/onet_rate_limit.md)
  add optional local response caching and request spacing for repeated
  or bulk O\*NET API workflows.
- [`onet_join_oews()`](https://farach.github.io/onet2r/reference/onet_join_oews.md)
  joins O\*NET occupation tibbles to national BLS Occupational
  Employment and Wage Statistics (OEWS) employment and wage estimates.
- [`onet_oews()`](https://farach.github.io/onet2r/reference/onet_oews.md),
  [`onet_oews_national()`](https://farach.github.io/onet2r/reference/onet_oews_national.md),
  [`onet_oews_state()`](https://farach.github.io/onet2r/reference/onet_oews_state.md),
  [`onet_oews_metro()`](https://farach.github.io/onet2r/reference/onet_oews_metro.md),
  and
  [`onet_oews_industry()`](https://farach.github.io/onet2r/reference/onet_oews_industry.md)
  download and parse BLS OEWS estimates for SOC-level wage and
  employment context.
- [`onet_pums_employment_weights()`](https://farach.github.io/onet2r/reference/onet_pums_employment_weights.md)
  converts ACS/CPS-style PUMS occupation records into SOC-level
  employment weights for O\*NET joins.
- [`onet_weighted_summary()`](https://farach.github.io/onet2r/reference/onet_weighted_summary.md)
  computes employment- and wage-weighted summaries from O\*NET task,
  skill, ability, or work-activity rows.
- Initial development release with O\*NET authentication, occupation
  search, occupation detail helpers, database table access, pagination,
  military crosswalks, and taxonomy mapping.
