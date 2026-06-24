# onet2r 0.3.0

* Added CI, README.Rmd, and a reproducible package logo source for cleaner release maintenance.
* `onet_archive_read()` now preserves task and DWA native fields when reading O*NET Task Statements, Task Ratings, Tasks to DWAs, and the 30.3 GWAs to IWAs to DWAs file.
* `onet_crosswalk_bridge()` now keeps 8-digit O*NET-SOC detail codes as the native bridge grain and includes derived 6-digit SOC columns only for employment joins.
* `onet_decompose_change()` decomposes aggregate change into within, between, interaction, and unclassifiable components with a sum-to-total check.
* `onet_measure()`, `onet_task_to_occupation()`, `onet_measure_aggregate()`, and `onet_robustness_diagnostic()` add bring-your-own-measure validation, task-to-occupation aggregation, employment-weighted aggregation, provenance, and plumbing-sensitivity checks.
* `onet_panel_reconcile()` now classifies transition rows, suppressed rows, new rows, and dropped rows explicitly instead of counting them as ordinary matched updates.
* `onet_reference_soc_resolve()`, `onet_weight_panel_oews()`, and `onet_weight_panel_pums()` create reference-SOC mappings and normalized employment-weight panels for OEWS and PUMS workflows.

# onet2r 0.2.0

* Added a longitudinal O*NET background article and verified reference file to help users understand archive panels, taxonomy changes, and comparability limits before interpreting release-to-release changes.
* `onet_archive_download()` downloads text-format O*NET database archives by release version for reproducible local analysis.
* `onet_archive_read()` reads descriptor tables from downloaded archives into a normalized long panel schema and now accepts local ZIP files or extracted archive directories for offline workflows.
* `onet_change_summary()` summarizes reconciled O*NET changes overall and by SOC job family.
* `onet_crosswalk_bridge()` builds SOC taxonomy bridges across supported O*NET vintages and labels one-to-one, split, and merge mappings.
* `onet_panel()` assembles descriptor tables across multiple O*NET releases for longitudinal analysis and can use local archive paths with explicit release dates.
* `onet_panel_reconcile()` classifies release-to-release changes as real updates, stable resampling, stale carryforwards, or recode/recalculation flags.
* `onet_releases()` lists O*NET database releases, archive URLs, dictionary URLs, release dates, and inferred SOC taxonomy vintages.

# onet2r 0.1.0

* `onet_cache_use()`, `onet_cache_clear()`, and `onet_rate_limit()` add optional local response caching and request spacing for repeated or bulk O*NET API workflows.
* `onet_join_oews()` joins O*NET occupation tibbles to national BLS Occupational Employment and Wage Statistics (OEWS) employment and wage estimates.
* `onet_oews()`, `onet_oews_national()`, `onet_oews_state()`, `onet_oews_metro()`, and `onet_oews_industry()` download and parse BLS OEWS estimates for SOC-level wage and employment context.
* `onet_pums_employment_weights()` converts ACS/CPS-style PUMS occupation records into SOC-level employment weights for O*NET joins.
* `onet_weighted_summary()` computes employment- and wage-weighted summaries from O*NET task, skill, ability, or work-activity rows.
* Initial development release with O*NET authentication, occupation search, occupation detail helpers, database table access, pagination, military crosswalks, and taxonomy mapping.
