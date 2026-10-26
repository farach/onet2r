# onet2r 0.4.0 specification

This document fixes the contracts for the 0.4.0 work. It is intentionally
limited to plumbing: versioned O&#42;NET data, crosswalks, weights, validation,
aggregation, and provenance around a measure supplied by the user. The package
does not ship a substantive AI exposure score or task score.

## Design rules

- Existing exported functions remain additive and backwards compatible.
- O&#42;NET-SOC stays at its native 8-digit detail code in archive panels and
  taxonomy bridges. Six-digit SOC is a derived employment-join key.
- All user-facing outputs are tibbles with stable empty schemas.
- Network work is optional in examples and tests. Tests use bundled fixtures.
- Every aggregation records provenance sufficient to reproduce the number in
  an accessor-readable column, not only as a fragile attribute.
- Multi-year or multi-cell weight panels must be filtered before aggregation.

## Occupational distinctiveness article

`vignettes/occupational-distinctiveness.Rmd` is a research workflow, not a new
public measure API. Its article-only helpers in `inst/article-helpers/` use
explicit occupation universes, unique occupation-item keys, non-negative
weights, occupation document frequency, and within-occupation normalization.
Missing and all-zero occupation profiles fail explicitly rather than being
silently excluded. Total-variation distances describe composition, not
absolute rating levels.

The bundled public-archive snapshot has a source digest and a deterministic
local-input regeneration script in `data-raw/`. Article builds and tests
remain offline. Full-archive activity rankings are labeled separately from
stylized formula examples. Coarsening preserves activity mass and the
occupation universe so loss of distinctions is not confused with real
convergence or with refitted IDF weights.

The information budget fixes a uniform occupation prior and original DWA
profile mass. Its mixture is the mean normalized profile, not document
prevalence. Information and parent losses are in bits, with retained
percentages divided by fine-label information. Mapping gaps, duplicate
parents, and non-nested routes fail explicitly. The finite-run Curveball
reference preserves row and column totals and supplies no p-values or
confidence intervals. Rankings disclose Supplemental-task dependence,
inventory-size dependence, and the different support of Core-only analyses.
An equal-five-character-prefix TF-IDF sensitivity changes the reference
prevalence but not the occupation-DWA profiles. These prefix groups are not
official SOC minor groups. A separate 200-draw partition reference
preserves each generalized group's DWA count while randomizing DWA assignments;
it breaks IWA nesting and semantic groupings and is descriptive, not inferential.
Static figures have text alternatives and offline table counterparts.

The software companion uses exact workplace-example strings for binary
prevalence and profile comparison. Classification accounting uses a separate,
explicitly labeled example-category-pair vocabulary, because one string can
have different published classifications across occupations. In release 30.3
there are no within-occupation multi-route examples. Pair-to-category-to-family
aggregation conserves original occurrence mass. Exact-name information and
the extra category-annotation information are reported separately.
Alternative Jaccard distances and binary category presence are separate
specifications. Neighbor sets retain ties at the requested rank boundary.
Missing occupational profiles are excluded and counted, not filled with zero.
Neither the software nor the activity workflow estimates actual worker use.

Employment comparisons keep the OEWS estimate year, raw input receipts and
published cell inventory. The primary representation uses a SOC-level `.00`
profile rather than counting nested specialties as disjoint worker groups.
Complete, partial, absent-profile and unmatched cells are counted separately.
The primary point estimate uses complete cells. Its disjoint-unit bounds use
`bound_type = "disjoint_allocation_bound"`. The overlapping all-code sensitivity
uses `bound_type = "profile_envelope"`, not a sharp worker-allocation bound.
Unknown omitted employment is separate from within-cell allocation, and
neither is a sampling confidence interval.
Equal-code, equal-minor-group and employment-weighted comparisons use the
same support. Reweighting all available O&#42;NET codes is labeled a synthetic
profile sensitivity, not an observed employment split.

Essential and Transferable Skills retain separate Importance and Level
blocks. Primary cross-domain comparisons require complete ratings with no
Recommend Suppress flag. Recorded Not Relevant values remain visible; they
are not silently converted into observed zeros. Absolute rating gaps are
primary, with their interval/comparability assumptions disclosed and
element-wise percentile-rank and other sensitivities reported separately.
All domain neighbors are recomputed on the same eligible occupation universe,
with boundary ties retained. Neither native distances nor education and
experience categories are pooled into a composite score.

Large derived tables are losslessly compressed as `.csv.gz`; no raw inputs
are bundled. Article-only `snapshot_csv_path()` requires exactly one plain or
gzipped CSV at the requested path, rejecting ambiguous duplicate snapshots.
Generators use the shared writer and reproduce the compressed files
deterministically.

## Longitudinal panel contract

`onet_archive_read()` returns one row per archive observation with:

- release metadata: `release_version`, `release_date`, `soc_vintage`;
- occupation identifiers: `onet_soc_code`, `soc_code`, `title`;
- content identifiers: `domain`, `element_id`, `element_name`, `scale_id`;
- estimate fields: `data_value`, `n`, `standard_error`, `lower_ci_bound`,
  `upper_ci_bound`, `recommend_suppress`;
- source fields: `source_date`, `domain_source`.

`onet_soc_code` is the native key. `soc_code` exists only for OEWS, PUMS, and
other employment-weight joins.

`onet_archive_reference()` reads reference and lookup tables that have no
O&#42;NET-SOC column, such as `GWAs to IWAs to DWAs`, `Scales Reference`, and
`Task Categories`. It returns `release_version`, `release_date`, and the
published columns in snake case. Identifier and code columns stay character so
they join to `onet_archive_read()` output. Text archives keep repeated names out
of linking and rating files, so DWA titles, scale names, and task text come from
these tables or from `Task Statements`.

`onet_panel_reconcile()` compares adjacent releases. Its output includes
ordinary matched comparisons and coverage rows:

- `stale_carryforward`, `real_update`, `resampled_stable`, and
  `recode_or_recalc_flag` for matched rows;
- `transition_data` when either side has `Domain Source` containing
  `Transition`;
- `suppressed_change` when either side has `Recommend Suppress == "Y"`;
- `new` and `dropped` for occupation-content rows present on only one side.

Rows with `transition_data`, `suppressed_change`, `new`, or `dropped` are not
counted as safely comparable changes.

## Resurvey and content-change contract

Three verbs turn a Task Ratings `onet_panel()` into resurvey denominators and
content-change metrics. They ship verbs and denominators only; hazard
estimation, weighting, bounds, and figures live in downstream analysis, not in
the package.

`onet_resurvey_panel()` returns one row per occupation, item, and release. It
adds the survey clock (`occ_survey_date`, the newest survey `source_date` in
that release, excluding `Analyst - Transition`), `prev_survey_date`,
`prev_release_version`, `resurvey_event` (the clock advanced versus the
occupation's prior release and the incoming step is not a seam), `cycle_index`
(cumulative resurvey count within the occupation), `age_resolved` (years of
staleness a resurvey resolved), and the incoming seam flags `seam_in` and
`seam_type`.

`onet_condition_on_resurvey()` adds `selection_reason` (a factor with levels
`resurveyed`, `unrevisited`, `taxonomy_seam`, `suppressed`) and `at_risk`.
Precedence is `taxonomy_seam` then `suppressed` then `resurveyed` then
`unrevisited`, so structural exclusions win over the resurvey signal. The
v25.1 SOC carry-forward is always `taxonomy_seam`, never a resurvey.

`onet_content_change()` is the single source of content metrics. It returns one
row per occupation and release pair (adjacent pairs by default) with `n_from`,
`n_to`, `n_added`, `n_dropped`, `n_retained`, `jaccard` (set similarity),
`churn_rate` (`1 - jaccard`), `rating_delta_l2` (Euclidean norm of rating
change over retained items), and `cosine` (cosine similarity over the item
union with zero fill). By default, only pairs crossing the v25.1 SOC-2010 to
SOC-2018 taxonomy seam carry metrics flagged `seam`, `seam_type`, and
`safely_comparable == FALSE`; v21.0 is not a package-verified default seam.
Callers with channel-specific evidence can supply a custom `seams` table
(to `onet_content_change()` or `onet_resurvey_panel()`) to add seam dates such
as v21.0, but such overrides require external justification and are not
package defaults.

## Reference-SOC resolver contract

A reference-SOC resolver maps source codes onto a selected reference SOC
vintage. It returns:

- `source_taxonomy`, `source_year`, `reference_taxonomy`;
- `source_code`, `reference_soc_code`;
- `map_type`, `crosswalk_weight`, `crosswalk_path`.

O&#42;NET bridges use official O&#42;NET-SOC crosswalks at 8-digit detail. OEWS and
PUMS helpers map their source SOC vintage to the same reference before
downstream aggregation.

## Weight panel contract

OEWS and PUMS weights are normalized to one shape:

- `reference_soc_code`;
- `year`;
- optional cell columns such as geography or demographic group;
- `employment`;
- `weight_share`;
- `source`, `source_taxonomy`, `reference_taxonomy`;
- optional `employment_se` when replicate weights are available.

Raw PUMS microdata is never cached by the package. O&#42;NET archive ZIPs, O&#42;NET
crosswalk CSVs, OEWS source ZIPs, API responses, and aggregated weights may be
cached under `tools::R_user_dir("onet2r", "cache")`; `onet_cache_clear()` removes
the package-managed cache sections.

If BLS blocks automated OEWS ZIP downloads, `onet_oews()` may use a matching
browser-downloaded ZIP from the user's Downloads folder or from
`options(onet2r.oews_download_dir = ...)`. The lookup checks the exact ZIP name
and numbered browser copies before listing the folder, because a folder listing
can stop early on Windows. User-supplied ZIPs are validated before
they are copied into the package-managed OEWS cache and are never deleted by
validation.

`onet_measure_aggregate()` accepts exactly one weight period and one optional
cell at a time. If a panel contains more than one `year`, callers must pass
`year`. If a panel contains multiple non-standard cell columns, callers must
pass `cell` as a named list or vector. A filtered panel must contain at most one
row per `reference_soc_code`.

An aggregation `bridge` maps `from_onet_soc_code` to `reference_soc_code`, with
an optional `crosswalk_weight` that defaults to 1. `onet_crosswalk_bridge()`
output is also accepted, with `to_soc_code` as the reference SOC. Scores that
share a reference SOC are averaged by `crosswalk_weight`, and measure
occupations without a bridge row are reported and left out. A bridge
`crosswalk_path` column is recorded as the aggregate's crosswalk path.
`onet_oews_bridge()` builds a bridge for an OEWS panel from May 2021 or later.
An O&#42;NET occupation maps to its own SOC when the panel publishes that SOC.
Otherwise, if the occupation is part of one of the 12 combined codes in the BLS
May 2021 OEWS occupation definitions and that combined code is in the panel, it
maps to the combined code. Anything else keeps its own SOC as `"not_in_panel"`.
Membership comes from the BLS definitions, not from the panel rows, so an
occupation suppressed in a state, metropolitan, or industry panel is never
merged into a neighboring combined code. Panels with a year before 2021, or a
missing year, are rejected because the May 2019 and May 2020 hybrid releases
and older SOC releases use different combined codes.

`n_occupations` and `n_reference_soc` count the O&#42;NET occupations and
reference SOCs that contribute to an aggregate after the `year` and `cell`
filters.

## Measure object contract

A bring-your-own measure object is created from a user table and records:

- measure metadata: `measure_id`, `measure_name`, `source`;
- key type: `occupation`, `task`, or `dwa`;
- O&#42;NET release used to compute the measure;
- key column and score column;
- validated score table;
- coverage table, unmatched keys, and optional employment coverage.

The constructor validates keys against the requested O&#42;NET release universe.
It does not transform a user's substantive score except for type checks and the
requested aggregation.

## Aggregation contract

Task-level aggregation rolls task scores to occupation scores with task
relevance or importance weights from O&#42;NET task files. The default task
weight scale is `RT`, which is the Task Ratings scale id for Relevance of Task.
The user chooses Core-only or Core-plus-Supplemental task handling. Effective
task ratings must contain at most one row per occupation and task. When a
release column is present, one call accepts exactly one non-missing effective
release. `release_version` and `version` are coalesced row by row and must agree
where both are populated. Multi-vintage comparisons use a named list of
single-release task-rating tables with `onet_measure_sensitivity()`. Explicit
release columns take precedence over list names; a list name supplies release
provenance only when neither release column exists.

Content-change inputs must contain at most one row per occupation, item,
release, and scale after filtering. `onet_content_change()` rejects duplicate
keys rather than choosing the first row. With `scale = NULL`, item-scale
combinations remain separate throughout the comparison.

Occupation-level aggregation rolls occupation scores to population aggregates
using the shared weight-panel object. The output includes:

- aggregate value and total employment covered;
- coverage counts and shares;
- a `provenance` list-column readable with `onet_provenance()`.

`onet_provenance()` returns a tibble describing the measure id, weight source,
weight year, source and reference taxonomies, bridge use, and crosswalk path for
an aggregate or sensitivity table.

`onet_coverage()` returns coverage for a measure, aggregate, or decomposition
object. `onet_measure_coverage()` remains as a measure-specific compatibility
wrapper.

## Sensitivity and robustness diagnostic contract

`onet_measure_sensitivity()` runs the same user-supplied measure through a grid
of plumbing choices. For occupation-level measures, the grid is bridges by
weight panels. For task-level measures, the grid also includes task relevance
scale and supplemental-task choices. It returns one row per scenario with:

- scenario label;
- measure id and release or taxonomy metadata;
- weight source, weight year, source taxonomy, reference taxonomy;
- bridge use and crosswalk path;
- task scale and supplemental-task choice when relevant;
- aggregate value and employment coverage;
- baseline movement fields from `onet_robustness_diagnostic()`;
- a `provenance` list-column for each scenario.

The second argument is `weight_panels`, not a generic change table. Each entry
must have employment weight-panel columns including `reference_soc_code`,
`year`, `employment`, and `weight_share`. Output from
`onet_content_change()` is rejected because it describes task-content movement,
not employment weights. Multi-release task sensitivity uses named lists of
single-release `task_ratings` and matching `task_metadata`; explicit release
columns supply the output provenance. The result reports movement in scenario
aggregates. It does not return Spearman correlations, rank or quintile changes,
variance decompositions, or content-drift diagnostics.

`onet_robustness_diagnostic()` remains a formatter for already computed
scenario results and returns a tidy table with one row per choice set:

- O&#42;NET release or taxonomy vintage;
- weight source and year;
- crosswalk path;
- task handling choice;
- aggregate value;
- movement relative to the baseline;
- coverage and leakage fields.

## Decomposition contract

The within-versus-between decomposition consumes a reconciled panel, a measure,
and two weight panels. It returns within, between, interaction, total change,
and unclassifiable components. The within term only uses rows flagged
`safely_comparable`. Components plus the unclassifiable bucket must equal the
total aggregate change within numeric tolerance.

The decomposition output includes a `coverage` list-column readable with
`onet_coverage()`. Coverage reports common occupations, safely comparable
occupations, and arithmetic leakage.

## Fixture contract

Bundled fixtures under `inst/extdata/onet-mini/` are tiny and synthetic. They
must keep the source file names and column names used by official O&#42;NET text
archives, but their values are only for arithmetic, schema, and narrative tests.

- `db_24_3_text` represents a 2010 O&#42;NET-SOC vintage release.
- `db_25_1_text` represents the first 2019 O&#42;NET-SOC vintage release.
- `db_30_2_text` and `db_30_3_text` represent same-vintage 2019 releases.
- Task fixtures include `Task Statements` and `Task Ratings` rows with `RT`
  relevance values and `IM` importance values.
