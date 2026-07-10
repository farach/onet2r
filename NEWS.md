# onet2r (development version)

## New features

* `onet_resurvey_panel()` restructures a Task Ratings panel into a task by resurvey-cycle frame keyed on the incumbent-survey `source_date`, exposing the occupation survey clock, resurvey events, cycle index, and seam flags.
* `onet_condition_on_resurvey()` labels each row with `selection_reason` (`resurveyed`, `unrevisited`, `taxonomy_seam`, `suppressed`) and an `at_risk` flag, the resurvey denominator for change estimation. The v25.1 SOC carry-forward is treated as a taxonomy seam, not a resurvey.
* `onet_content_change()` is the single, seam-aware source of content metrics between releases: `n_added`, `n_dropped`, `n_retained`, `jaccard`, `churn_rate`, `rating_delta_l2`, and `cosine`. Pairs crossing the v21.0 or v25.1 seam are flagged `safely_comparable = FALSE` so taxonomy churn is not counted as content churn.
* `onet_import_eloundou()` ingests the occupation-level GPT-exposure table from Eloundou et al. (2023), "GPTs are GPTs", and broadcasts it onto the tasks of a caller-supplied panel, returning a task-grain `onet_measure()` object keyed on `(occupation, task)` that feeds `onet_task_to_occupation()`. Every task inherits its occupation's published score; this structurally blind broadcast is the aggregate construction task-aware measures are contrasted against. It is a thin adapter that selects the score column and records provenance without transforming the published values. The MIT-licensed data are never bundled; supply a local `path` or download `url`.
* `onet_import_felten_aioe()` ingests the AI Occupational Exposure (AIOE) scores from Felten, Raj, and Seamans (2021) and broadcasts them onto panel tasks the same way, joining on 6-digit SOC code, following the same download-only, no-transform adapter pattern.
* `onet_measure()` gains an `items = ` / `agg = ` convenience path: given a Task Ratings style panel it builds a task-grain measure in one line. `agg = "targeted"` restricts the panel to a set of target task ids; `agg = "aggregate"` keeps every task. Either way it selects one rating row per task on `scale` (default Importance) and keys on the task id, so the result is exactly the measure the default `data` / `key` / `score` path returns on that same subset, ready for `onet_task_to_occupation()`. The existing `data` / `key` / `score` / `key_type` path is unchanged.
* `onet_resurvey_panel()` and `onet_content_change()` gain an optional `seams = ` argument to override the default Task-Ratings-scoped seam table. The default (`NULL`) reproduces existing output exactly; supplying a table lets non-Task-Ratings inputs such as Work Activities, Work Context, or Abilities drop the v21.0 Task Relevance scale seam that does not apply to them. Cross-vintage SOC seams are still detected from `soc_vintage` regardless.

## Bug fixes

* `onet_content_change()` and `onet_task_to_occupation()` now reject duplicate effective keys instead of silently choosing or blending rows. Task rollups coalesce `release_version` and `version` row by row, reject conflicts, and require exactly one non-missing effective release per call when release metadata is present. `onet_measure_sensitivity()` uses explicit release columns before falling back to named-list labels for multi-vintage provenance (reported in release audit).
* `onet_measure_sensitivity()` now rejects content-change tables and other non-weight inputs at the `weight_panels` boundary with guidance toward employment weight panels and named-list task release inputs. Its documented output contract is scenario aggregate movement, not rank, quintile, variance, or content-drift diagnostics (reported in release audit).
* `onet_oews()` now detects, validates, and caches matching OEWS ZIP files downloaded in the user's browser, and interactive sessions can open the official BLS URL and wait for the ZIP when BLS rejects automated downloads with HTTP 403 (reported manually).
* `onet_oews()` now downloads OEWS ZIP files through the package HTTP client, avoiding RStudio's `.rs.downloadFile()` path that can trigger BLS 403 responses (reported manually).

## Improvements

* Cached API responses are written atomically and corrupt RDS files now fail with a specific cache-clear instruction instead of falling through to network access.
* Cached archive and adapter files without provenance receipts now fail closed when a URL, version, `as_of`, or expected digest is requested. Unconstrained internal reuse warns and records a `legacy_unverified` receipt, while `force = TRUE` replaces the legacy bytes without exposing URL credentials.
* Cached archive readers now acquire matching bytes and receipts and create a private verified snapshot under one cache lock, then retain that exact snapshot through parsing across forced refreshes. Adapter readers continue to consume private verified snapshots, and omitted provenance fields remain unconstrained on reuse. URL credential redaction now matches explicit case-insensitive, percent-decoded parameter names, including OAuth authorization `code`, verifier and consumer credentials, and cloud signatures, without hiding benign parameters such as `author`, `monkey`, or `state`.
* Clean-install validation now builds a source tarball, installs it into temporary libraries outside the repository, exercises every public export with deterministic offline fixtures, and runs twice in pull-request CI.
* O&#42;NET archive and external-adapter downloads now support optional `expected_sha256` and `as_of` verification, write atomic source receipts with URL, commit when inferable, retrieval time, digest, size, and version metadata, and reject changed or mismatched cached sources.
* Pull requests now require installed-package tests with network access blocked plus a complete pkgdown reference check and site build; deployment remains limited to pushes on `main`.

# onet2r 0.4.2

## Bug fixes

* Fixed a regression that broke `onet_crosswalk_bridge()` downloads (`download_crosswalk_file()` was not defined at package level).
* `onet_occupation_details()` now returns a tibble of section titles and URLs instead of an unnamed list.
* `onet_panel_reconcile()` aborts when release dates are missing instead of silently mis-ordering comparisons.

## Improvements

* Archive tables with unrecognized layouts now abort instead of returning all-NA panels.
* OEWS `annual`/`hourly` flags are parsed as logicals and wage-field semantics are documented.
* `onet_change_summary()` gains crosswalk-weighted counts (`n_weighted`, `share_weighted`).
* `onet_measure_aggregate()` reports large unmatched weight-panel employment shares.
* `onet_releases()` is memoised per session, reports archive `format`, and documents the text-archive floor (20.1, October 2015).
* Reconciliation now reports occupations missing from the crosswalk bridge (`coverage_status` values `"unmapped_source"` / `"unmapped_target"`).

## New features

* `onet_data_updates()` downloads the official O&#42;NET Longitudinal Data Updates record, ground truth for which occupations were re-rated in each cycle.

# onet2r 0.4.1

* Rebuilt README and article figures with a shared ggplot2 style so pkgdown examples render readable, dark-mode-safe output instead of base graphics.
* Replaced broad console table dumps in README and vignettes with bounded `knitr::kable()` tables from actual `onet2r` function results.
* Expanded the longitudinal archive, OEWS, sensitivity, and decomposition articles with practical cross-vintage examples using packaged fixtures.
* Added pkgdown callouts, branded favicon colors, citation metadata, lifecycle badge documentation, and release notes for the v0.4.1 polish pass.
* Strengthened fixture tests for task rating scale handling and multi-cell aggregation guards.
* `onet_cache_clear()` can clear API responses, O&#42;NET archives, O&#42;NET crosswalks, OEWS downloads, or the full package cache.
* `onet_change_summary()` now reports the full change-type distribution within each summary group instead of only the modal change type.
* `onet_decompose_change()` now honors comparability flags supplied on either period and treats missing comparability as not safe for within-change attribution.
* `onet_measure_aggregate()` now collapses multiple O&#42;NET detail occupations within a reference SOC before applying employment weights, preventing inflated aggregates and coverage shares above 100 percent.
* `onet_task_to_occupation()` now carries measure id and release metadata into occupation-level rollups so `onet_measure_aggregate()` provenance remains intact.
* `onet_oews()` now uses corrected BLS metro and industry file slugs, writes OEWS downloads atomically, preserves top-coded and suppressed wage flags, and documents OEWS special value markers.
* `onet_weight_panel_oews()` now filters OEWS hierarchy rows to detailed occupations before computing weight shares.
* `onet_weight_panel_pums()` now warns about unfiltered ACS employment universes, drops unweightable SOCP aggregate codes, warns on missing person weights, and warns when replicate standard errors are requested without a full replicate set.

# onet2r 0.4.0

* `onet_coverage()` and `onet_provenance()` expose coverage and provenance metadata through durable accessors instead of requiring users to inspect attributes.
* `onet_decompose_change()` now returns coverage metadata as an accessor-readable list-column.
* `onet_join_oews()`, `onet_pums_employment_weights()`, and `onet_weighted_summary()` now emit lifecycle guidance toward the vintage-aware `onet_weight_panel_oews()`, `onet_weight_panel_pums()`, and `onet_measure_aggregate()` workflow.
* `onet_measure_aggregate()` now requires a single weight year and one optional cell, with explicit `year` and `cell` filters for multi-period or grouped panels.
* `onet_measure_sensitivity()` runs a user-supplied measure across alternative bridges, weight panels, and task-handling choices, then reports baseline movement with provenance.
* Added cross-vintage and task-rating fixtures so tests and articles can demonstrate 2010-to-2019 O&#42;NET-SOC behavior without network access.

# onet2r 0.3.0

* Added CI, README.Rmd, and a reproducible package logo source for cleaner release maintenance.
* `onet_archive_read()` now preserves task and DWA native fields when reading O&#42;NET Task Statements, Task Ratings, Tasks to DWAs, and the 30.3 GWAs to IWAs to DWAs file.
* `onet_crosswalk_bridge()` now keeps 8-digit O&#42;NET-SOC detail codes as the native bridge grain and includes derived 6-digit SOC columns only for employment joins.
* `onet_decompose_change()` decomposes aggregate change into within, between, interaction, and unclassifiable components with a sum-to-total check.
* `onet_measure()`, `onet_task_to_occupation()`, `onet_measure_aggregate()`, and `onet_robustness_diagnostic()` add bring-your-own-measure validation, task-to-occupation aggregation, employment-weighted aggregation, provenance, and plumbing-sensitivity checks.
* `onet_panel_reconcile()` now classifies transition rows, suppressed rows, new rows, and dropped rows explicitly instead of counting them as ordinary matched updates.
* `onet_reference_soc_resolve()`, `onet_weight_panel_oews()`, and `onet_weight_panel_pums()` create reference-SOC mappings and normalized employment-weight panels for OEWS and PUMS workflows.

# onet2r 0.2.0

* Added a longitudinal O&#42;NET background article and verified reference file to help users understand archive panels, taxonomy changes, and comparability limits before interpreting release-to-release changes.
* `onet_archive_download()` downloads text-format O&#42;NET database archives by release version for reproducible local analysis.
* `onet_archive_read()` reads descriptor tables from downloaded archives into a normalized long panel schema and now accepts local ZIP files or extracted archive directories for offline workflows.
* `onet_change_summary()` summarizes reconciled O&#42;NET changes overall and by SOC job family.
* `onet_crosswalk_bridge()` builds SOC taxonomy bridges across supported O&#42;NET vintages and labels one-to-one, split, and merge mappings.
* `onet_panel()` assembles descriptor tables across multiple O&#42;NET releases for longitudinal analysis and can use local archive paths with explicit release dates.
* `onet_panel_reconcile()` classifies release-to-release changes as real updates, stable resampling, stale carryforwards, or recode/recalculation flags.
* `onet_releases()` lists O&#42;NET database releases, archive URLs, dictionary URLs, release dates, and inferred SOC taxonomy vintages.

# onet2r 0.1.0

* `onet_cache_use()`, `onet_cache_clear()`, and `onet_rate_limit()` add optional local response caching and request spacing for repeated or bulk O&#42;NET API workflows.
* `onet_join_oews()` joins O&#42;NET occupation tibbles to national BLS Occupational Employment and Wage Statistics (OEWS) employment and wage estimates.
* `onet_oews()`, `onet_oews_national()`, `onet_oews_state()`, `onet_oews_metro()`, and `onet_oews_industry()` download and parse BLS OEWS estimates for SOC-level wage and employment context.
* `onet_pums_employment_weights()` converts ACS/CPS-style PUMS occupation records into SOC-level employment weights for O&#42;NET joins.
* `onet_weighted_summary()` computes employment- and wage-weighted summaries from O&#42;NET task, skill, ability, or work-activity rows.
* Initial development release with O&#42;NET authentication, occupation search, occupation detail helpers, database table access, pagination, military crosswalks, and taxonomy mapping.
