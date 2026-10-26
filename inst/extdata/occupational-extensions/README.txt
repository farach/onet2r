Derived public-data snapshot for employment-weighted activity prevalence and
rated-skills comparisons

Sources
O*NET 30.3 Database, U.S. Department of Labor, Employment and Training
Administration (USDOL/ETA).
https://www.onetcenter.org/dl_files/database/db_30_3_text.zip
License: Creative Commons Attribution 4.0 International.
https://creativecommons.org/licenses/by/4.0/
O*NET is a trademark of USDOL/ETA.

Occupational Employment and Wage Statistics (OEWS) May 2025 national
estimates and the May 2025 OEWS occupation definitions, U.S. Bureau of Labor
Statistics (BLS).
https://www.bls.gov/oes/special-requests/oesm25nat.zip
https://www.bls.gov/oes/occupation_definitions_m2025.xlsx
BLS states that what it publishes is in the public domain, except for
previously copyrighted photographs and illustrations, and asks users to cite
BLS as the source. https://www.bls.gov/opub/copyright-information.htm

onet2r has modified this information by calculating employment-weighted
activity prevalence under named allocation rules, allocation bounds, coverage
counts, rank sensitivities, rating distributions, threshold shares, absolute,
ordinal and composition distances between occupations, and nearest-neighbor
sets with ties retained.
Occupation titles, activity names and skill names are retained as source
labels.
USDOL/ETA and BLS have not approved, endorsed, or tested these modifications.

What these tables are
Descriptive tables for an article. They are not an exposure measure, and the
package exports no substantive measure from them. No table holds a p-value,
a significance test or a confidence interval. Employment bounds follow from
stated allocation rules and are not sampling intervals. Overlap figures
between neighbor sets describe how much two distance choices agree.
Decisions about what was done, left out or left open are in
decisions-ledger.csv.

Pinned inputs
The raw inputs are not bundled. The scripts read local paths, check the
SHA-256 of every input and never download.
db_30_3_text.zip, 13,222,549 bytes
  7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780
oesm25nat.zip, 279,525 bytes
  b5855a37f3e03e779f6fbf173d3bbc94aeeff33426aef32fa95ce6d025bab1af
occupation_definitions_m2025.xlsx, 93,634 bytes
  d927416c1165e2043e8626f380f3bcaaa2f98377bfa5ef5c08209e573d95bb24
BLS returns HTTP 403 to scripted clients such as curl, so obtain the two BLS
files through a browser. employment-provenance.csv records how each source
URL was checked.

Regenerate from the repository root with the current package installed:
Rscript data-raw/occupational-skills.R path/to/db_30_3_text.zip
Rscript data-raw/occupational-employment.R path/to/db_30_3_text.zip
path/to/oesm25nat.zip path/to/occupation_definitions_m2025.xlsx
The second command is one command on one line. On Windows ARM, set
CLI_NO_THREAD=true and remove LC_CTYPE in the same command that starts R.
No table holds a run timestamp, and a rerun reproduces every CSV byte for byte.
All CSV files use LF line endings.
The helpers are in inst/article-helpers (occupational-neighbors.R,
occupational-skills.R, occupational-employment.R). They are not exported.

EMPLOYMENT-WEIGHTED ACTIVITY PREVALENCE

Terms
cell  One OEWS detailed row. Cells are disjoint and each is counted once.
      The May 2025 national file has 830 detailed cells. Every row of the
      file is area 99 (U.S.), NAICS 000000 (cross-industry) and ownership
      code 1235. The meaning of that code was not checked against a BLS
      field dictionary, because none ships with the May 2025 files.
unit  One O*NET occupation description that shares its cell's employment.
rule  How units are defined inside a cell. The two rules differ in meaning.

Rule soc (primary, bound_type disjoint_allocation_bound)
One unit per BLS-defined SOC occupation in the cell, represented by its .00
O*NET-SOC profile. A combined OEWS code such as 13-1020 has one unit per
constituent SOC from the BLS definitions. The units in a cell are distinct SOC
occupations, so they are disjoint worker groups, and the bounds keep that
reading. They hold for any nonnegative split of the cell's employment among
its SOC occupations. A nested detailed code is never substituted for a SOC
whose .00 code has no activity profile. Those cells are counted as incomplete
and left out of the prevalence. The soc rule has 765 units in the 756 complete
cells.

Rule code (sensitivity, bound_type profile_envelope)
One unit per O*NET-SOC code in the cell. This includes nested detailed codes
whose descriptions overlap their .00 parent. It is synthetic profile
reweighting, not a disjoint worker allocation. Its minimum and maximum are a
profile-allocation sensitivity, not a bound over mutually exclusive worker
groups. A SOC with no O*NET-SOC code gets an unprofiled placeholder unit named
<soc>.-- and makes its cell incomplete. The code rule has 848 units in the same
756 complete cells.

Weighting schemes
Every scheme within a rule weights the same units, so the schemes differ only
in their weights.
equal_unit            Weight 1 per unit. This is the article's equal-
                      occupation weighting on this universe.
equal_minor_group     Equal total weight per true SOC minor group. A minor
                      group is the first four characters of the SOC code,
                      for example 11-1.
equal_prefix5_group   Equal total weight per five-character SOC prefix, for
                      example 11-10. The original article calls this
                      grouping a minor group. It is a finer grouping than a
                      SOC minor group.
employment            Each complete cell's employment is split equally among
                      its units. Only complete cells enter.
employment_two_stage  Code rule only. A cell's employment is split equally
                      across its SOC occupations and then equally across the
                      codes of each SOC. It is blank for the soc rule, where
                      it equals employment.
The summary also compares the article's published equal-code results over all
923 profiled codes (original_923) with equal_unit. The rank table carries the
full-923 rank and the rank inside the matched universe. That separates a
universe effect from a weighting effect.

Bounds
employment_lower and employment_upper bound the employment prevalence over
the complete cells. A cell counts toward the lower bound only if every unit
links the activity, and toward the upper bound if any unit does. Under rule
soc they are bounds over disjoint worker groups. Under rule code they are the
profile-allocation sensitivity described above and not a sharp bound. The
upper value is the union upper-allocation scenario. It is never a headline
estimate. The wide versions also let the employment of the incomplete
cells in the returned panel either link or not link the activity. The
equal_unit prevalence is a different estimand and can fall outside the
employment bounds. The summary reports how often, as a description only.

Cell status and coverage
complete    Every unit has an activity profile. Only these cells enter the
            prevalence. Both rules share the same 756 complete cells, which
            hold 146,251,550 workers.
partial     Some units have a profile and some do not.
no_profile  No unit has a profile.
unmatched   No O*NET-SOC code exists for any SOC in the cell. There are none.
incomplete  partial, no_profile and unmatched together. There are 74 cells
            with 9,230,290 workers.
Cells that lack a .00 profile are counted and are never filled with nested
specialties. The soc rule has 4 partial and 70 no_profile cells. The code rule
has 27 partial and 47 no_profile cells. Twenty-four cells hold an "All Other"
SOC occupation whose .00 code has no profile while nested detailed codes have
one. They hold 3,768,470 workers and are incomplete under both rules.
employment-coverage.csv reports every scope as a share of the official OEWS
total (155,495,730 workers) and as a share of the returned panel (the 830
detailed cells, 155,481,840 workers). The official total exceeds the detailed
rows by 13,890 workers. The files read for this snapshot do not explain the
difference, and the snapshot does not allocate it.
OEWS is an employer survey of nonfarm wage and salary workers. It excludes the
self-employed and the military. Cell employment is an estimate, and its
sampling error (emp_prse in employment-cell-inventory.csv) is not carried into
the prevalence or the bounds.

Combined OEWS codes
Twelve OEWS codes combine SOC occupations. The BLS definitions list 28
constituent SOC occupations for them. All twelve are detailed rows in the May
2025 national file, so the public weight-panel builder, which keeps detailed
rows, drops none. Seven of the twelve also appear as broad rows with the same
employment, 6,674,160 workers in all. Using every row would count that mass
twice, so only detailed rows are used. Eight combined cells are complete under
both rules. The four that are not are 25-9045, 47-4090, 51-2090 and 53-1047.
Each lacks a profiled .00 code for an "All Other" part.
The package's internal constituent list for 25-9045 includes 25-9041. It is not
in the BLS 2025 definition or in the 30.3 roster, and it has no effect on this
snapshot. The discrepancy is recorded in employment-combination-audit.csv and
is not edited here.

RATED SKILLS

Blocks
Four blocks: es_im, es_lv, ts_im and ts_lv. Domains are Essential (10
elements) and Transferable (25 elements). Scales are Importance (IM, 1 to 5)
and Level (LV, 0 to 7). Domains and scales are never pooled, and no table
combines the blocks into one score.

Primary distance
The mean over a block's elements of the absolute rating gap divided by the
scale range from the Scales Reference table. The range is 4 for Importance and
7 for Level. Distances lie in [0, 1]. They are computed exactly on whole
hundredths and rounded to 8 decimals. The distance assumes equal intervals on
each scale and treats a gap of one Level point the same on every element. It
is not row-normalized, so it compares rating levels and not rating shapes.

Primary universe
923 occupations have activity profiles. 894 of them are complete in all four
blocks, and 29 lack a skills profile. 823 of the 894 have no Recommend Suppress
flag in any block, and 71 have at least one. The 823 occupations are the
primary universe (in_common_universe in skills-common-universe.csv.gz). The
894 are a stated sensitivity (in_pool_complete). Two stricter pools are
disclosed as counts only. In the 894 pool, 93 occupations have no Not Relevant
rating and 84 have neither a Not Relevant rating nor a Recommend Suppress flag.
Recommend Suppress appears only on Level rows. skills-rating-audit.csv records
the standard-error range of flagged and unflagged rows. The primary universe
leaves out flagged occupations so that no recommended-suppressed rating is
published as a primary measurement.

Not Relevant and Level zero
Not Relevant (NR) ratings stay in the primary distance with their recorded
values. The audit counts them, and nr_floor rows in the sensitivity table set
Level to 0 on NR ratings as a declared check on Level blocks only. When an
analyst rates Importance as 1, Level is set to 0 (HumRRO, O*NET Analyst
Occupational Skills Ratings: Procedures Update, revised March 2018). A Level
mean of 0 is therefore not an unrated cell. The audit finds that every
Level-zero row has Importance at the floor and that no Importance-at-floor row
has a positive Level.
Ratings are means over eight analysts. An integer Importance threshold does not
mean that most analysts chose that category. skills-thresholds.csv labels
thresholds as scale_category_label (Importance 2, 3 and 4, with the category
names Somewhat Important, Important and Very Important), level_example_anchor
(Level 2, 4 and 6, with the element's own anchor text) or grid_point (no
published meaning). Importance thresholds stop at 4.5.

Nearest neighbors
Rows are directed. For each occupation and block, rank is 1 plus the number of
strictly closer occupations, so rank <= k is the nearest set at k with ties at
the cutoff retained. The table keeps every neighbor through rank 10. Ranks use
the exact integer sums of the absolute gaps in hundredths, so ties are exact.
The distance column is rounded to 8 decimals.

Sensitivity to the distance choice
skills-neighbor-sensitivity.csv reports mean set size, Jaccard overlap and the
share of identical or disjoint sets, by alternative, block and k. It reports
overlap only.
primary_tie_retention  Mean set size and the share of sets larger than k.
ordinal_percentile     Mean over elements of the absolute gap in percentile
                       rank. An occupation's percentile rank on an element
                       is its average rank within the primary universe
                       divided by the number of occupations. It drops the
                       equal-interval assumption and the comparison of Level
                       gaps across elements.
composition_secondary  Total variation between row-normalized profiles. It
                       discards each occupation's overall rating level.
nr_floor               Level blocks only. NR ratings are set to Level 0.
pool_894               The same absolute distance over the 894 pool, with the
                       823 occupations as queries. Neighbors can fall outside
                       the primary universe.
distance_spearman compares all pairwise distances, ranked after rounding to 8
decimals. It is descriptive only.

FILES
Columns are the CSV headers. This list gives the grain and the codes that need
a note.

Employment
employment-provenance.csv  One row per input: release, file name, source URL,
  SHA-256 and an access note.
employment-accounting.csv  Official and observed counts. status is match or
  observed_only. Sources are onet_taxonomy, oews_raw, oews_panel,
  bls_definitions, package_constituent_list, bridge, soc_rule_universe and
  code_rule_universe. The oews_raw rows hold the file scope, the group counts,
  the combined-code counts and the repeated broad rows.
employment-roster.csv.gz  One row per O*NET-SOC title (1,016). It gives the
  taxonomy class, the SOC minor group (4 characters), the 5-character prefix,
  the OEWS cell, the unit under each rule and the cell status under each rule.
employment-cell-inventory.csv  One row per OEWS cell (830). cell_type is
  direct or combination. It counts SOC occupations without a .00 profile and
  nested profiled codes the soc rule does not use, and gives the cell status
  under each rule.
employment-combination-audit.csv  One row per combined OEWS code (12). It
  compares BLS and package constituents and counts parts with a profiled .00.
employment-coverage.csv  One row per unit_rule and scope. Scopes are
  official_total, returned_panel, official_not_returned, matched, complete,
  incomplete, partial, no_profile, unmatched and
  cells_with_all_other_soc_with_nested_detail. The soc rule also has
  incomplete_with_unused_nested_profiles. Shares are of the official total and
  of the returned panel.
activity-employment-prevalence.csv.gz  One row per unit_rule and DWA (2,083 each).
  Prevalence under each weighting, the exact and wide bounds, and
  idf_equal_unit and idf_employment, which are blank where prevalence is 0.
activity-employment-summary.csv  Long format: unit_rule, bound_type,
  comparison, metric and value. It holds the agreement between schemes, rank
  correlations, top-10 and top-20 overlap, bound widths and the share of
  equal-weight prevalences outside the bounds. prevalence_spearman ranks the
  prevalence values rounded to 8 decimals. Values that differ only by floating
  point noise stay tied, and the result reproduces from
  activity-employment-prevalence.csv.gz.
activity-employment-rank-sensitivity.csv  Units in the top 10 (ties retained)
  of any series, 32 per rule. selected_reason names those series. mean_idf_*
  is the mean IDF of a unit's own activities under that weighting. rank_* is
  the rank by mean IDF, where 1 is the rarest unit and tied units share their
  minimum rank. A rank is blank where the unit is outside that series.
  rank_original_923 is the rank among all 923 codes and
  rank_original_923_in_universe is the rank inside the matched universe.

Skills
skills-provenance.csv  One row: release, source, pool sizes, element counts,
  scale ranges, k_max, distance rounding and the stated primary choices.
skills-rating-audit.csv  One row per block, computed on the 894 pool. Missing
  values, out-of-range values, rater counts, standard errors, flag counts and
  the Level-zero checks.
skills-common-universe.csv.gz  One row per DWA occupation (923). Completeness and
  flag counts per block, the pool memberships, in_common_universe and
  exclusion_reason (incomplete_skills_profile, recommend_suppress_flag or
  blank).
skills-nearest-neighbors.csv.gz  block, occupation, rank, neighbor and distance.
skills-neighbor-sensitivity.csv  Long format: alternative, block, k, metric
  and value.
skills-ordinal-neighbor-overlap.csv.gz  Per-occupation Jaccard overlap at k of
  1, 5 and 10 between the primary and the ordinal_percentile sets.
skills-rating-distributions.csv  Mean, spread and quantiles per element and
  block over the primary universe.
skills-thresholds.csv  Share of occupations at or above each threshold (>=),
  with the threshold basis. Level blocks also show the share with NR excluded
  and with NR set to Level 0. Those columns are blank on Importance blocks,
  which have no NR flag. The last two columns repeat the share over the 894
  pool.
skills-level-anchors.csv  The Level 2, 4 and 6 example anchors for every
  skill element.
skills-pair-gap-examples.csv  The nearest, median and farthest pair in each
  block (pair_kind) with the gap on every element. The mean of gap_normalized
  within a block equals the block distance.

Common-support comparisons
cross-domain-neighbors.csv.gz  Directed neighbors for the same 823 occupations
  in activities, software and the four skill blocks. Boundary ties through
  rank 10 are retained after rounding distances to eight decimal places.
cross-domain-neighbor-overlap.csv.gz  Per-query neighbor-set Jaccard for each
  block pair at k = 1, 5 and 10.
cross-domain-overlap-summary.csv  Mean, median and disjoint-set share for each
  block pair and k.
cross-domain-selected-neighbors.csv  Nearest sets for eligible selected examples.
cross-domain-provenance.csv  Common support, distance rules and rounding.
All .csv.gz tables use lossless gzip compression. Base R read.csv() reads
them directly. Compression changes no rows or values.

Ledger
decisions-ledger.csv  One row per decision with a status of implemented,
  implemented_sensitivity, deferred, delegated, open_gap, recorded_not_edited
  or rejected, a rationale and a trigger for revisiting it.

Open source gaps
BLS publishes no field-descriptions workbook with the May 2025 files, so the
column layout was checked against the Technical Notes, the occupation
definitions and the workbook headers. The OEWS Survey Methods document was not
read. The occupation definitions workbook is not linked from the OEWS tables,
structure or technical notes pages. Its direct URL returned the pinned bytes
in a browser.

Carry this attribution/license/modification notice with separate CSV copies.
