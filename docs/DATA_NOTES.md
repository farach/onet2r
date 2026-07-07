# Data notes and verified assumptions

These notes record facts checked for the 0.4.1 contracts. Facts that could not
be verified are treated as assumptions and guarded in tests or left out of
automated behavior.

## O&#42;NET archive files

Verified from O&#42;NET Resource Center Data Dictionary pages for release 30.3:

- `Task Statements.txt` includes `O*NET-SOC Code`, `Title`, `Task ID`, `Task`,
  `Task Type`, `Incumbents Responding`, `Date`, and `Domain Source`.
- `Task Type` is `Core` or `Supplemental` when available. Analyst-sourced rows
  may have blank task type and incumbent count.
- `Task Ratings.txt` includes `O*NET-SOC Code`, `Title`, `Task ID`, `Task`,
  `Scale ID`, `Scale Name`, `Category`, `Data Value`, `N`,
  `Standard Error`, confidence bounds, `Recommend Suppress`, `Date`, and
  `Domain Source`.
- Task rating scales include `IM` for importance, `FT` for frequency, and `RT`
  for relevance. Frequency has seven category rows per task.
- In the release 30.3 Task Ratings dictionary example, `RT` is labeled
  `Relevance of Task`. The task aggregation default uses `RT` for relevance
  weights because it is the direct task-to-occupation relevance scale in this
  file.
- The packaged cross-vintage task fixture is tested with `scale = "RT"` and
  asserts that every returned task-rating row carries `Scale ID` equal to `RT`
  and `Scale Name` equal to `Relevance of Task`.
- The DWA reference file changed in release 30.3. `DWA Reference.txt` became
  `GWAs to IWAs to DWAs.txt`, and old `DWA ID` fields became
  `DWA Element ID` fields with a new identifier format.
- `Tasks to DWAs.txt` links occupation tasks to DWA elements. In 30.3 it uses
  `DWA Element ID` and `DWA Element Name`.

Sources:

- https://www.onetcenter.org/dictionary/30.3/excel/task_statements.html
- https://www.onetcenter.org/dictionary/30.3/excel/task_ratings.html
- https://www.onetcenter.org/dictionary/30.3/excel/scales_reference.html
- https://www.onetcenter.org/dictionary/30.3/excel/gwas_to_iwas_to_dwas.html
- https://www.onetcenter.org/dictionary/30.3/excel/tasks_to_dwas.html
- https://www.onetcenter.org/dl_files/30_3_migration_reference.xlsx

## O&#42;NET taxonomy breaks and transition data

Verified from O&#42;NET release and taxonomy pages:

- O&#42;NET 15.1, released in February 2011, introduced the O&#42;NET-SOC 2010
  taxonomy.
- O&#42;NET 21.0, released in August 2016, changed how supplemental-task
  relevance and inclusion were handled in Task Ratings. Tasks that were
  previously carried can enter or leave an occupation on this boundary for
  reasons unrelated to survey re-rating, which inflates apparent task additions
  around the 2016 releases. This is treated as a scale or relevance seam rather
  than content change, and is mitigated downstream by filtering tasks on an
  Importance floor (for example `min_importance = 2`). The `RT`
  Relevance of Task scale itself still ships in later dictionaries; the seam is
  about supplemental-task inclusion, not removal of the scale.
- O&#42;NET 25.1, released in November 2020, introduced the O&#42;NET-SOC 2019
  taxonomy, aligned with 2018 SOC.
- O&#42;NET marks data aggregated from predecessor occupations at the 2019 seam
  with `Domain Source` equal to `Analyst - Transition`.
- Transition rows should not be interpreted as fresh data collection updates.
- The resurvey and content-change verbs treat two seams explicitly: a scale or
  relevance seam at 2016-08-01 (v21.0) and a taxonomy seam at 2020-11-01
  (v25.1). Comparisons crossing either boundary are flagged and marked not
  safely comparable so taxonomy churn is not counted as content churn.

Sources:

- https://www.onetcenter.org/db_releases.html
- https://www.onetcenter.org/taxonomy.html
- https://www.onetcenter.org/reports/Taxonomy2010.html
- https://www.onetcenter.org/reports/Taxonomy2019.html
- Release dates for 20.1 (October 2015), 21.0 (August 2016), and 25.1
  (November 2020) were confirmed against the O&#42;NET Database Releases Archive.
- https://www.onetcenter.org/dictionary/30.3/excel/appendix_updates.html

## SOC, OEWS, and PUMS vintages

Verified from BLS archived pages and Census API variable metadata:

- The 2010 to 2018 SOC crosswalk is published as
  `soc_2010_to_2018_crosswalk.xlsx`. It does not publish split weights.
- OEWS data with reference dates May 2019 and May 2020 are hybrid 2010/2018
  SOC releases. May 2021 is the first full 2018 SOC OEWS release.
- Beginning with the May 2021 release, OEWS publishes most but not all detailed
  2018 SOC occupations. BLS states that to improve data quality OEWS continues
  to aggregate some occupations to the 2018 SOC broad-occupation level or as
  OEWS-specific combinations of detailed occupations. Weight-panel SOCs that are
  published only as broad or combined estimates therefore cannot match a
  detailed O&#42;NET-SOC measure, so their employment stays in the aggregation
  denominator as unmatched. This is the ground truth behind the
  unmatched-employment diagnostic in `onet_measure_aggregate()`.
- OES May 2010 and May 2011 were hybrid 2000/2010 SOC releases. May 2012 is
  the first full 2010 SOC release.
- ACS PUMS `SOCP` uses 2010 SOC in data years 2012 through 2017 and 2018 SOC
  from data year 2018 onward.
- ACS PUMS `SOCP` can include aggregate Census codes with trailing `X`
  characters. These are not six-digit SOC codes and require an external
  allocation crosswalk before they can be matched to O&#42;NET or OEWS.
- ACS PUMS `PWGTP` is the person weight. Person replicate weights are
  `PWGTP1` through `PWGTP80`.
- ACS PUMS employment weights should be built on an employment universe, not on
  raw person records. A common ACS filter is employed civilians,
  `ESR %in% c(1, 2)`, often with age 16 or older.
- ACS PUMS `OCCP` is a Census occupation recode and should not be treated as a
  six-digit SOC code without a separate Census crosswalk.

Sources:

- https://web.archive.org/web/20230528035801/https://www.bls.gov/soc/2018/crosswalks.htm
- https://web.archive.org/web/20230922061615/https://www.bls.gov/soc/socimp.htm
- https://web.archive.org/web/20231209062841/https://www.bls.gov/oes/soc_2018.htm
- https://api.census.gov/data/2022/acs/acs1/pums/variables/SOCP.json
- https://api.census.gov/data/2022/acs/acs1/pums/variables/ESR.json
- https://api.census.gov/data/2022/acs/acs1/pums/variables/OCCP.json
- https://api.census.gov/data/2022/acs/acs1/pums/variables/PWGTP.json

## Assumptions guarded by tests

- When an official bridge has no published weights, equal split weights are
  treated as a transparent fallback, not as BLS employment-share weights.
- Local fixtures are intentionally small. They check schema and arithmetic but
  are not a complete copy of any O&#42;NET, OEWS, or Census file.
- Local `db_24_3_text` and `db_25_1_text` fixtures are synthetic slices used to
  exercise 2010-to-2019 O&#42;NET-SOC behavior. They preserve official column
  names but are not official O&#42;NET data.
- Current package tests do not require network access or API keys.
- Multi-period and multi-cell weight panels must be filtered before
  aggregation. Tests cover the explicit single-year and single-cell errors and
  the successful filtered path.
