# onet2r (development version)

## New features

* `onet_oews_bridge()` maps O&#42;NET-SOC codes into the 12 combined codes OEWS has published since May 2021 in place of some detailed occupations, such as `31-1120` Home Health and Personal Care Aides and `25-9045` Teaching Assistants, Except Postsecondary. Pass it as `bridge` to `onet_measure_aggregate()` or `onet_measure_sensitivity()`. The mapping comes from the BLS occupation definitions, so it is the same for national, state, metropolitan, and industry panels, and panels from before May 2021 are rejected. With O&#42;NET 31.0 task ratings and May 2025 national OEWS weights, it raises covered employment from about 92 percent to about 98 percent, and Healthcare Support from about 46 percent to 100 percent (reported in the task-time EDA review).
* `onet_archive_reference()` reads O&#42;NET archive reference tables such as `GWAs to IWAs to DWAs`, `Scales Reference`, and `Task Categories`, which have no O&#42;NET-SOC column and cannot be read by `onet_archive_read()`. In the text archives, DWA titles and scale names appear only in those tables (reported in the task-time EDA review).
* `onet_resurvey_panel()` restructures a Task Ratings panel into a task by resurvey-cycle frame keyed on the incumbent-survey `source_date`, exposing the occupation survey clock, resurvey events, cycle index, and seam flags.
* `onet_condition_on_resurvey()` labels each row with `selection_reason` (`resurveyed`, `unrevisited`, `taxonomy_seam`, `suppressed`) and an `at_risk` flag, the resurvey denominator for change estimation. The v25.1 SOC carry-forward is treated as a taxonomy seam, not a resurvey.
* `onet_content_change()` is the single, seam-aware source of content metrics between releases: `n_added`, `n_dropped`, `n_retained`, `jaccard`, `churn_rate`, `rating_delta_l2`, and `cosine`. Pairs crossing the v25.1 SOC-2010 to SOC-2018 seam are flagged `safely_comparable = FALSE` by default so taxonomy churn is not counted as content churn.
* `onet_import_eloundou()` ingests the occupation-level GPT-exposure table from Eloundou et al. (2023), "GPTs are GPTs", and broadcasts it onto the tasks of a caller-supplied panel, returning a task-grain `onet_measure()` object keyed on `(occupation, task)` that feeds `onet_task_to_occupation()`. Every task inherits its occupation's published score; this structurally blind broadcast is the aggregate construction task-aware measures are contrasted against. It is a thin adapter that selects the score column and records provenance without transforming the published values. The MIT-licensed data are never bundled; supply a local `path` or download `url`.
* `onet_import_felten_aioe()` ingests the AI Occupational Exposure (AIOE) scores from Felten, Raj, and Seamans (2021) and broadcasts them onto panel tasks the same way, joining on 6-digit SOC code, following the same download-only, no-transform adapter pattern.
* `onet_measure()` gains an `items = ` / `agg = ` convenience path: given a Task Ratings style panel it builds a task-grain measure in one line. `agg = "targeted"` restricts the panel to a set of target task ids; `agg = "aggregate"` keeps every task. Either way it selects one rating row per task on `scale` (default Importance) and keys on the task id, so the result is exactly the measure the default `data` / `key` / `score` path returns on that same subset, ready for `onet_task_to_occupation()`. The existing `data` / `key` / `score` / `key_type` path is unchanged.
* `onet_resurvey_panel()` and `onet_content_change()` gain an optional `seams = ` argument to override the default seam table, which contains only the verified v25.1 SOC-2010 to SOC-2018 taxonomy seam. The default (`NULL`) reproduces existing output exactly; supplying a table lets callers add channel-specific or source-specific seam dates, such as a v21.0 row, when they have external evidence a comparison spanning that date needs seam treatment. Cross-vintage SOC seams are still detected from `soc_vintage` regardless.

## Bug fixes

* `onet_oews()` now finds a browser-downloaded OEWS ZIP by checking its exact file name and numbered copies, such as `oesm25nat (1).zip`, before listing the folder. On Windows, `list.files()` can stop early without an error when the R session cannot represent another file name in the folder, so the Downloads fallback could miss ZIPs that were there (reported in the task-time EDA review).
* `onet_measure_aggregate()` now accepts a minimal `bridge` with `from_onet_soc_code` and `reference_soc_code` columns, as documented, instead of failing with a tibble recycling error. Bridge codes and measure keys are standardized before the join, a negative `crosswalk_weight` is rejected, and measure occupations without a bridge row are reported instead of silently left out (reported in the task-time EDA review).
* `onet_measure_aggregate()` now counts `n_occupations` and `n_reference_soc` over the occupations and reference SOCs that contribute to the aggregate after the `year` and `cell` filters. They previously counted every measure key, so every cell of a multi-cell panel reported the national count. Employment of reference SOCs whose score is missing now counts toward the unmatched-employment report (reported in the task-time EDA review).
* `onet_known_seams()` no longer includes a v21.0 / 2016-08-01 row. This corrects unsupported default metadata: independent verification found no evidence that O&#42;NET v21.0 is a proven global content or method seam, and the package's prior description of it as retiring the Task Relevance scale was incorrect. The only package-verified default seam is now the v25.1 SOC-2010 to SOC-2018 taxonomy transition. A default v20.1 -> v21.x comparison with unchanged SOC vintage is no longer seam-flagged solely for crossing that release. Callers with channel-specific evidence can still supply a custom v21.0 row through `seams = ` on `onet_content_change()` or `onet_resurvey_panel()`; such custom seams are not package-verified defaults.
* `onet_cache_clear()` now coordinates with active cache transactions, accepts a configurable wait `timeout`, and uses ownership-checked nonrecursive lock cleanup, preventing concurrent clears and refreshes from deleting replacement locks or separating cached sources from their receipts. Failed transaction registration and teardown retain recoverable state instead of silently leaking markers.
* `onet_content_change()` and `onet_task_to_occupation()` now reject duplicate effective keys instead of silently choosing or blending rows. Task rollups coalesce `release_version` and `version` row by row, reject conflicts, and require exactly one non-missing effective release per call when release metadata is present. `onet_measure_sensitivity()` uses explicit release columns before falling back to named-list labels for multi-vintage provenance (reported in release audit).
* `onet_import_eloundou()` and `onet_import_felten_aioe()` now verify and parse private snapshots of local files, so mutations to the original path cannot separate parsed values from their receipt digest. Download URLs without a safe path filename use an opaque name derived from the raw URL fingerprint, preventing credentials from entering cache and lock paths while preserving source identity and content-based workbook or tab-delimited parsing.
* `onet_measure_sensitivity()` now rejects content-change tables and other non-weight inputs at the `weight_panels` boundary with guidance toward employment weight panels and named-list task release inputs. Its documented output contract is scenario aggregate movement, not rank, quintile, variance, or content-drift diagnostics (reported in release audit).
* `onet_oews()` now detects, validates, and caches matching OEWS ZIP files downloaded in the user's browser, and interactive sessions can open the official BLS URL and wait for the ZIP when BLS rejects automated downloads with HTTP 403 (reported manually).
* `onet_oews()` now downloads OEWS ZIP files through the package HTTP client, avoiding RStudio's `.rs.downloadFile()` path that can trigger BLS 403 responses (reported manually).
* SHA-256 verification now uses a supported cryptographic file implementation on every declared R version, including R 4.1 through R 4.4.

## Improvements

* `onet_archive_read()` now points reference tables without an O&#42;NET-SOC column to `onet_archive_reference()`, and for May 2021 or later OEWS panels the unmatched-employment report from `onet_measure_aggregate()` suggests `onet_oews_bridge()` when no bridge was supplied.
* Cached API responses are written atomically and corrupt RDS files now fail with a specific cache-clear instruction instead of falling through to network access.
* Cached archive and adapter files without provenance receipts now fail closed when a URL, version, `as_of`, or expected digest is requested. Unconstrained internal reuse warns and records a `legacy_unverified` receipt, while `force = TRUE` replaces the legacy bytes without exposing URL credentials.
* Cached archive and adapter readers now copy verified bytes to a private snapshot while holding the cache lock, so a concurrent refresh cannot separate parsed data from its source receipt. Omitted provenance fields remain unconstrained when a verified snapshot is reused. OAuth and cloud credential parameters are matched by explicit normalized names, including authorization `code`, OAuth verifier and consumer credentials, without hiding benign names such as `author` or `monkey`.
* Credential redaction now removes malformed multi-`@` user information through the final authority separator for absolute and `//` network-path URLs while preserving benign `@` characters in paths, queries, and fragments.
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
