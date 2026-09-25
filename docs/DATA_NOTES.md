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

## O&#42;NET text archive layout

The column lists above come from the data dictionary pages for the Excel files.
The tab-delimited text archives use a different layout. Checked directly in
`db_30_2_text.zip`, `db_30_3_text.zip`, and `db_31_0_text.zip` on 2026-09-25:

- Linking and rating files carry codes but not repeated names. `Task
  Statements.txt` has no `Title`. `Task Ratings.txt` has `O*NET-SOC Code`,
  `Task ID`, `Scale ID`, `Category`, `Data Value`, `N`, `Standard Error`,
  confidence bounds, `Recommend Suppress`, `Date`, and `Domain Source`, with no
  `Title`, `Task`, or `Scale Name`. `Tasks to DWAs.txt` has `O*NET-SOC Code`,
  `Task ID`, `DWA Element ID` (`DWA ID` in 30.2), `Date`, and `Domain Source`.
  Descriptor files such as `Abilities.txt` have `Element Name` but no `Title`.
- Names live in other files. Occupation titles are in `Occupation Data.txt` and
  task text in `Task Statements.txt`, which both have an `O*NET-SOC Code`
  column. Scale names are in `Scales Reference.txt`, and DWA titles in `GWAs to
  IWAs to DWAs.txt` (30.3 and 31.0: `GWA Element ID`, `IWA Element ID`,
  `DWA Element ID`, `DWA Element Name`) or `DWA Reference.txt` (30.2:
  `Element ID`, `IWA ID`, `DWA ID`, `DWA Title`). Those reference files have no
  `O*NET-SOC Code` column.
- The 31.0 Excel dictionary page for Tasks to DWAs lists eight columns,
  including `Title`, `Task`, and `DWA Element Name`, while the 31.0 text file has
  five. The text-format dictionary linked from the 31.0 `Read Me.txt`,
  `https://www.onetcenter.org/dictionary/31.0/text/`, returned HTTP 404 on
  2026-09-25.
- With real text archives, `onet_archive_read()` therefore returns `NA` for
  `title`, `scale_name`, `dwa_element_name`, and, in Task Ratings, `task`.
  Titles and task text can be joined back from `Occupation Data` and
  `Task Statements`, which `onet_archive_read()` reads. Scale names and DWA
  titles come from the reference files, which `onet_archive_reference()` reads.
- The bundled `onet-mini` fixtures keep some Excel-style name columns, such as
  `Title` and `Task` in the task fixtures and GWA and IWA names in
  `gwas_to_iwas_to_dwas.txt`. They are readable test data, not an exact copy of
  the text layout.

Sources:

- https://www.onetcenter.org/dl_files/database/db_30_2_text.zip
- https://www.onetcenter.org/dl_files/database/db_30_3_text.zip
- https://www.onetcenter.org/dl_files/database/db_31_0_text.zip
- https://www.onetcenter.org/dictionary/31.0/excel/tasks_to_dwas.html

## O&#42;NET taxonomy breaks and transition data

Verified from O&#42;NET release and taxonomy pages:

- O&#42;NET 15.1, released in February 2011, introduced the O&#42;NET-SOC 2010
  taxonomy.
- O&#42;NET 21.0, released in August 2016, is sometimes cited as changing how
  supplemental-task relevance and inclusion were handled in Task Ratings.
  Independent reconstruction found this is not established as a proven global
  O&#42;NET content or method seam, so the package does not treat v21.0 as a
  package-verified default seam and does not claim it retired the Task
  Relevance scale. v21.0 remains at most a diagnostic release date. A caller
  with channel-specific evidence that a comparison spanning v21.0 needs seam
  treatment can supply a custom row through the `seams = ` argument of
  `onet_resurvey_panel()` or `onet_content_change()`; such custom seams
  require external justification and are not package-verified defaults.
- O&#42;NET 25.1, released in November 2020, introduced the O&#42;NET-SOC 2019
  taxonomy, aligned with 2018 SOC. This is the package's one verified, global
  default seam.
- O&#42;NET marks data aggregated from predecessor occupations at the 2019 seam
  with `Domain Source` equal to `Analyst - Transition`.
- Transition rows should not be interpreted as fresh data collection updates.
- The resurvey and content-change verbs treat one seam by default: the
  taxonomy seam at 2020-11-01 (v25.1). Comparisons crossing that boundary are
  flagged and marked not safely comparable so taxonomy churn is not counted as
  content churn. A default v20.1 -> v21.x comparison with unchanged SOC
  vintage is not seam-flagged merely for crossing v21.0.

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
- The BLS May 2021 OEWS occupation definitions workbook lists the combined
  codes and the 2018 SOC occupations each one includes: `13-1020` (13-1021,
  13-1022, 13-1023), `13-2020` (13-2022, 13-2023), `21-1018` (21-1011,
  21-1014), `25-2052` (25-2055, 25-2056), `25-9045` (25-9042, 25-9043, 25-9049,
  and the 2010 SOC 25-9041), `29-2010` (29-2011, 29-2012), `31-1120` (31-1121,
  31-1122), `39-7010` (39-7011, 39-7012), `47-4090` (47-4091, 47-4099),
  `51-2028` (51-2022, 51-2023), `51-2090` (51-2092, 51-2099), and `53-1047`
  (53-1042, 53-1043, 53-1044, 53-1049). These are the only 12 of its 831
  definitions that list included SOC occupations.
- The May 2023, May 2024, and May 2025 national OEWS files publish the same 12
  combined codes among their detailed rows, and they are the only detailed codes
  with no matching SOC in O&#42;NET 31.0. Seven of them (13-1020, 13-2020,
  29-2010, 31-1120, 39-7010, 47-4090, 51-2090) also appear as OEWS broad rows.
  `onet_oews_bridge()` stores this list, so its mapping does not depend on which
  rows a state, metropolitan, or industry panel publishes. Inferring membership
  from the rows a panel lacks would merge a suppressed neighbor, such as 53-1041
  Aircraft Cargo Handling Supervisors, into 53-1047.
- The May 2019 and May 2020 hybrid structure used other combined codes, and some
  of them span SOC broad occupations. `29-1228` Physicians, All Other; and
  Ophthalmologists, Except Pediatric holds 29-1212, 29-1213, 29-1214, 29-1217,
  29-1222, 29-1224, 29-1229, and 29-1241. `29-1248` Surgeons, Except
  Ophthalmologists holds 29-1242, 29-1243, and 29-1249. `onet_oews_bridge()`
  therefore rejects panels from before May 2021.
- SOC residual "All Other" occupations have codes ending in 9, per principle 8
  and coding guideline 4 of the 2018 SOC classification principles.
- With O&#42;NET 31.0 core-task relevance ratings (910 occupations) and the May
  2025 national OEWS detailed rows (830 codes), 91.9 percent of employment
  matches a score without a bridge and 98.2 percent with `onet_oews_bridge()`.
  Healthcare Support (major group 31) rises from 45.5 to 100 percent. Of the
  remaining 1.8 percent, 1.3 points are residual "All Other" codes and 0.5
  points are occupations that O&#42;NET lists without core-task relevance
  ratings, such as 13-2051 and 29-2042.
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
- https://web.archive.org/web/20240926215234/https://www.bls.gov/oes/occupation_definitions_m2021.xlsx
- https://web.archive.org/web/20220119024644/http://www.bls.gov/oes/oes_2019_hybrid_structure.xlsx
- https://web.archive.org/web/20250101093352/https://www.bls.gov/soc/2018/soc_2018_class_prin_cod_guide.pdf
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
