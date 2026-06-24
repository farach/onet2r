# onet2r 0.1.0

* `onet_cache_use()`, `onet_cache_clear()`, and `onet_rate_limit()` add optional local response caching and request spacing for repeated or bulk O*NET API workflows.
* `onet_join_oews()` joins O*NET occupation tibbles to national BLS Occupational Employment and Wage Statistics (OEWS) employment and wage estimates.
* `onet_oews()`, `onet_oews_national()`, `onet_oews_state()`, `onet_oews_metro()`, and `onet_oews_industry()` download and parse BLS OEWS estimates for SOC-level wage and employment context.
* `onet_pums_employment_weights()` converts ACS/CPS-style PUMS occupation records into SOC-level employment weights for O*NET joins.
* `onet_weighted_summary()` computes employment- and wage-weighted summaries from O*NET task, skill, ability, or work-activity rows.
* Initial development release with O*NET authentication, occupation search, occupation detail helpers, database table access, pagination, military crosswalks, and taxonomy mapping.
