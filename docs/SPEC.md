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

Downloaded source caches are reusable only with a receipt whose URL identity,
version, optional `as_of`, and digest agree with the request. Receiptless or
legacy-unverified files fail closed when any such provenance constraint is
present; `force = TRUE` replaces the source and receipt atomically. Fully
unconstrained legacy reuse may warn for compatibility, but it records only the
local path, current-byte digest, size, and modification time. Receipt and error
rendering removes URL credentials and query values.

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
union with zero fill). Pairs crossing the v21.0 or v25.1 seam carry metrics but
are flagged `seam`, `seam_type`, and `safely_comparable == FALSE`.

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
`options(onet2r.oews_download_dir = ...)`. User-supplied ZIPs are validated before
they are copied into the package-managed OEWS cache and are never deleted by
validation.

`onet_measure_aggregate()` accepts exactly one weight period and one optional
cell at a time. If a panel contains more than one `year`, callers must pass
`year`. If a panel contains multiple non-standard cell columns, callers must
pass `cell` as a named list or vector. A filtered panel must contain at most one
row per `reference_soc_code`.

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
