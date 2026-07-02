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
