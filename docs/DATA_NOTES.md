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

## Occupational distinctiveness article

Verified on 2026-10-25 against the public 30.3 text ZIP and its official
Excel-format dictionary:

- `Tasks to DWAs` is a many-to-many task/activity bridge. An occupation must
  be counted once per DWA for occupation document frequency, not once per
  task-link row.
- The downloaded ZIP has SHA-256
  `7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780`.
  It contains 1,016 occupation taxonomy entries, 923 occupations with task
  statements and DWA links, 18,796 occupation-task pairs, and 2,083 linked
  DWAs. Every task in this snapshot has a DWA link. No Task ID is shared
  across occupations in this snapshot.
- Release 30.3 has separate `Essential Skills`, `Transferable Skills`,
  `Education`, and `Training and Experience` files. The dictionary marks
  `Essential Skills` and `Training and Experience` as added in 30.3.
- `Training and Experience` carries element, scale, category, rating,
  suppression, and source-date fields. `Training and Experience Categories`
  defines the category descriptions for Related Work Experience (`RW`),
  On-Site or In-Plant Training (`PT`), and On-The-Job Training (`OJ`).
  Rating values on these categorical scales are percentages of respondents
  choosing each category, not the observed tenure distribution of workers.
- Importance and Level are different scales. Level anchors are
  element-specific. Positive rating presence is not an adequate summary of
  differences in required level.
- The article's binary activity profile and log inverse-prevalence summary
  are research choices. The package does not export a distinctiveness or
  AI-exposure score. Full-archive activity results are distinct from the
  article's stylized formula examples.
- Equal prefix-group weighting in the original activity sensitivity averages
  DWA prevalence across represented five-character O&#42;NET-SOC prefixes.
  These are not official SOC minor groups. It is a taxonomy-weighting
  sensitivity, not an estimate of equal occupational-family influence.
- In this sensitivity, baseline and equal-prefix-group rarity ranks have
  Spearman correlation 0.5887, with two occupations in common among the top
  ten under both rankings. Slaughterers and Meat Packers remains the highest
  mean-IDF occupation, but its result is not evidence that the ranking is
  invariant to weighting.
- Independent reconstruction from the pinned bytes found that the baseline
  rarity leader, Slaughterers and Meat Packers, has 14 task statements, all
  Supplemental, which map to five distinct DWAs. Models has ten statements,
  one Core and nine Supplemental. Literal Core-only filtering retains 893
  occupations, not the baseline 923; it is not a common-support comparison.
- Of 425,503 occupation pairs, 371,633 (87.3397 percent) have no shared DWA.
  Near-ceiling mean pair distances therefore need a sparse-vocabulary
  reference, not an interpretation as measured occupational uniqueness.
- The archive receipt labels 30.3 as the May 2026 release. The task records
  span 2004-2025; a release label is not a uniform observation date.
- Generalized activity names come from `Content Model Reference`. The text
  hierarchy supplies GWA and IWA identifiers but not their names. Each linked
  DWA has one IWA parent, and each IWA has one GWA parent in this snapshot.
- Under a uniform occupation draw followed by a draw from that occupation's
  binary DWA shares, occupation-identifying information is 6.389725 bits at
  DWA grain, 3.954185 at IWA grain, and 1.665578 at GWA grain. The latter
  retains 26.0665 percent of the fine-label signal. This is a synthetic
  database-description experiment, not worker, semantic, or economic
  information. Parent losses sum to 4.724147 bits.
- Five occupation pairs have exactly identical detailed DWA sets in the
  snapshot. All share major-group prefix `25`, but three pairs cross
  five-character code-group prefixes; this does not imply that a whole major group shares one
  activity profile.
- The article's 200-draw DWA-to-GWA partition reference preserves each
  generalized group's number of DWA labels but randomizes their assignments.
  It does not preserve IWA nesting or semantic relatedness and is not an
  economic null model or inferential test. Observed GWA information is
  1.665578 bits; for seed 20261026, random-draw percentiles are 1.243709,
  1.264714, and 1.290753 bits at the 5th, 50th, and 95th percentiles. All 200
  draws are below the observed value; this finite comparison is descriptive.

Reproduction: `data-raw/occupational-distinctiveness.R` reads a local ZIP
without network access and writes the derived snapshot, coverage, resolution
diagnostics, and provenance under
`inst/extdata/occupational-distinctiveness/`.
The companion scripts `occupational-information.R` and `occupational-null.R`
take the same local ZIP argument and regenerate the information-loss,
equal-prefix-group sensitivity, randomized-partition, and finite-run
fixed-margin reference tables. Curveball trades preserve both matrix margins.
Twenty separately restarted runs of 20 times the occupation count in attempted
trades are a descriptive reference, not certified uniform samples or a
calibrated significance test.

Sources:

- https://www.onetcenter.org/dl_files/database/db_30_3_text.zip
- https://www.onetcenter.org/dictionary/30.3/excel/tasks_to_dwas.html
- https://www.onetcenter.org/dictionary/30.3/excel/gwas_to_iwas_to_dwas.html
- https://www.onetcenter.org/dictionary/30.3/excel/essential_skills.html
- https://www.onetcenter.org/dictionary/30.3/excel/training_and_experience.html
- https://www.onetcenter.org/dictionary/30.3/excel/training_and_experience_categories.html
- https://www.onetcenter.org/dictionary/30.3/excel/scales_reference.html
- https://arxiv.org/html/physics/0004057v1
- https://doi.org/10.1038/ncomms5114

## Skills and software extensions to the distinctiveness article

Verified on 2026-10-26 against the official release 30.3 data dictionary:

- `Essential Skills` reports 10 elements on two scales for 894 occupations.
  `Transferable Skills` reports 25 elements on two scales for 894 occupations.
  These published domain counts do not establish the intersection with the
  article's 923-occupation DWA universe or the coverage after suppression filters.
- Release 30.3 renamed `Technology Skills` to `Software Skills`. It also renamed
  `Example`, `Commodity Code`, and `Commodity Title` to `Workplace Example`,
  `Element ID`, and `Element Name`. The dictionary lists 31,821 example-link
  rows, not 31,821 distinct software products or occupations.
- Software examples are classified in the Content Model hierarchy based on
  UNSPSC version 260801. The software file provides occupation links and
  `Hot Technology` and `In Demand` flags, not Importance or Level ratings,
  employment counts, or numeric worker-use rates.
- `Hot Technology` concerns requirements frequently included across employer
  job postings. `In Demand` concerns requirements frequently included in
  postings for the particular occupation. Neither flag is a numeric prevalence
  weight.
- Importance and Level measure different aspects of a requirement. Level scale
  anchors are element-specific, so a shared numeric range does not establish
  identical substantive meaning across skill elements.
- Direct reconstruction from the pinned 30.3 text ZIP finds 923 occupations
  with 31,821 occupation-example links and 8,753 exact example strings.
  There are 8,908 global example-category pairs: 151 strings have multiple
  classifications across occupations, but none has multiple category rows
  within the same occupation. The 134 represented categories have 17 parents
  in the Content Model. Case and whitespace normalization changes no links or
  distinct strings in this snapshot.
- Microsoft Excel is listed for 859 of 923 covered occupations (93.1 percent).
  Of 8,753 exact example strings, 6,139 occur in one occupation only. These
  are database-link counts, not use rates. List lengths range from one to 430.
- Under a uniform occupation draw and uniform draw from each occupation's
  example-category pairs, fine-label information is 5.060966 bits, category
  information is 2.199011 bits, and family information is 0.947371 bits.
  The family retains 18.7192 percent of the fine-pair information.
  Dropping the category annotation from the fine label leaves 5.031090 bits;
  the annotation contributes 0.029876 bits. This is a synthetic description
  experiment, not a measure of worker knowledge or software adoption.

Reproduction: `data-raw/occupational-software.R` checks the same pinned ZIP
digest as the activity analysis and writes derived offline CSVs under
`inst/extdata/occupational-software/`.

### Employment and separate skills extension

- The pinned May 2025 national OEWS file has 830 detailed cells totaling
  155,481,840 jobs, versus the official all-occupations total of 155,495,730.
  The 13,890 residual is recorded but not explained or allocated. All twelve
  published combination codes have detailed rows. Seven also have broad rows
  with identical employment; including those broad rows would double count.
- The primary activity comparison uses `.00` SOC descriptions and complete
  cells only: 756 cells, 765 constituent descriptions, and 146,251,550 jobs,
  or 94.055 percent of the official total. Four partial-profile cells and
  70 cells with no required broad profile account for another 9,230,290 jobs.
  Specialty profiles do not silently replace missing broad descriptions.
- Within complete combination cells, the point estimate allocates employment
  equally across constituent SOC descriptions. Arbitrary-allocation bounds
  use those disjoint units. The all-code sensitivity includes overlapping
  parent/specialty descriptions and reports a profile envelope instead,
  not a sharp employment bound. Wider incomplete-cell envelopes are relative
  to returned-panel employment, not the official total.
- Official SOC minor groups use four-character prefixes here, distinct from
  the original five-character-prefix sensitivity. On the primary matched
  support, there are 94 represented minor groups and 109 five-character groups.
  Of the full archive's 2,083 DWAs, 2,034 have a primary matched link.
  Allocation bounds have zero width for 92.4 percent of those linked DWAs.
  Equal-description and employment-weighted DWA prevalence have Spearman
  correlation 0.657045. This changes the reference distribution, not work time.
- Raw BLS 2025 definitions do not include the historical `25-9041` part in
  `25-9045`. The package bridge's existing historical entry is recorded in
  the combination audit, not changed as part of the article extension.
- Each skills file has complete ratings for 894 occupations. Of the 923
  activity codes, 29 lack a complete skill profile and another 71 have at
  least one recommended-suppressed rating. The primary common support is
  therefore 823 codes, with 894 retained as an explicitly separate sensitivity.
  Accountants and Software Developers are among the suppressed-profile
  exclusions. Not Relevant flags do not themselves exclude a code.
- Essential and Transferable blocks retain separate Importance and Level
  scales. The archived ratings are eight-analyst means; thresholding a mean
  is not assigning respondents to a category. Precision fields and flags
  are audited without inventing joint uncertainty or a new suppression cutoff.
- Against mean absolute scale-range-normalized rating gaps, element-wise
  percentile ranks produce mean five-neighbor Jaccard overlaps of 0.445 to
  0.485. Single-neighbor sets are disjoint for about 48 to 55 percent of
  primary occupations. This is a descriptive sensitivity, not a significance
  result.
- Recomputed on those 823 codes with distances rounded to eight decimals,
  activity and software five-neighbor sets have median Jaccard overlap zero;
  55.772 percent share no neighbor. Software versus each skills block also
  has median five-neighbor overlap zero. Cutoff ties are retained.
  At k = 5, software sets contain more than five neighbors for 515 of the
  823 codes, which also affects neighbor-set overlap.

Reproduction: `data-raw/occupational-skills.R`,
`data-raw/occupational-employment.R`, and
`data-raw/occupational-cross-domain.R`. The derived tables, input receipts,
scope restrictions and decision ledger are under
`inst/extdata/occupational-extensions/`. Its README records remaining source
gaps: the OEWS ownership-code interpretation and Survey Methods PDF were not
verified. No ownership breakdown or sampling-error propagation is claimed.

Sources:

- https://www.onetcenter.org/dictionary/30.3/excel/essential_skills.html
- https://www.onetcenter.org/dictionary/30.3/excel/transferable_skills.html
- https://www.onetcenter.org/dictionary/30.3/excel/software_skills.html
- https://www.onetcenter.org/dictionary/30.3/excel/scales_reference.html
- https://www.onetcenter.org/dl_files/database/db_30_3_text.zip

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
