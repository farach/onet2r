# onet2r 0.3.0 specification

This document fixes the contracts for the 0.3.0 work. It is intentionally
limited to plumbing: versioned O&#42;NET data, crosswalks, weights, validation,
aggregation, and provenance around a measure supplied by the user. The package
does not ship a substantive AI exposure score or task score.

## Design rules

- Existing exported functions remain additive and backwards compatible.
- O&#42;NET-SOC stays at its native 8-digit detail code in archive panels and
  taxonomy bridges. Six-digit SOC is a derived employment-join key.
- All user-facing outputs are tibbles with stable empty schemas.
- Network work is optional in examples and tests. Tests use bundled fixtures.
- Every aggregation records provenance sufficient to reproduce the number.

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

Raw PUMS microdata is never cached by the package. Only aggregated weights may
be cached under `tools::R_user_dir("onet2r", "cache")`.

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
relevance or importance weights from O&#42;NET task files. The user chooses
Core-only or Core-plus-Supplementary task handling.

Occupation-level aggregation rolls occupation scores to population aggregates
using the shared weight-panel object. The output includes:

- aggregate value and total employment covered;
- coverage counts and shares;
- provenance columns or an attached provenance attribute.

## Robustness diagnostic contract

The diagnostic recomputes the same user measure under alternative plumbing
choices and returns a tidy table with one row per choice set:

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
