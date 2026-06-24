# onet2r 0.2.0

* Added a longitudinal O*NET background article and verified reference file to help users understand archive panels, taxonomy changes, and comparability limits before interpreting release-to-release changes (no issue).
* `onet_archive_download()` downloads text-format O*NET database archives by release version for reproducible local analysis (no issue).
* `onet_archive_read()` reads descriptor tables from downloaded archives into a normalized long panel schema (no issue).
* `onet_change_summary()` summarizes reconciled O*NET changes overall and by SOC job family (no issue).
* `onet_crosswalk_bridge()` builds SOC taxonomy bridges across supported O*NET vintages and labels one-to-one, split, and merge mappings (no issue).
* `onet_panel()` assembles descriptor tables across multiple O*NET releases for longitudinal analysis (no issue).
* `onet_panel_reconcile()` classifies release-to-release changes as real updates, stable resampling, stale carryforwards, or recode/recalculation flags (no issue).
* `onet_releases()` lists O*NET database releases, archive URLs, dictionary URLs, release dates, and inferred SOC taxonomy vintages (no issue).

# onet2r 0.1.0

* `onet_cache_use()`, `onet_cache_clear()`, and `onet_rate_limit()` add optional local response caching and request spacing for repeated or bulk O*NET API workflows.
* `onet_join_oews()` joins O*NET occupation tibbles to national BLS Occupational Employment and Wage Statistics (OEWS) employment and wage estimates.
* `onet_oews()`, `onet_oews_national()`, `onet_oews_state()`, `onet_oews_metro()`, and `onet_oews_industry()` download and parse BLS OEWS estimates for SOC-level wage and employment context.
* `onet_pums_employment_weights()` converts ACS/CPS-style PUMS occupation records into SOC-level employment weights for O*NET joins.
* `onet_weighted_summary()` computes employment- and wage-weighted summaries from O*NET task, skill, ability, or work-activity rows.
* Initial development release with O*NET authentication, occupation search, occupation detail helpers, database table access, pagination, military crosswalks, and taxonomy mapping.
