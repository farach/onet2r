# Create a Bring-Your-Own O\*NET Measure

Validates a user-supplied occupation, task, or DWA score table and
records coverage against an optional universe. The package does not
supply or alter the substantive score.

## Usage

``` r
onet_measure(
  data,
  key = NULL,
  score = NULL,
  key_type = c("occupation", "task", "dwa"),
  universe = NULL,
  measure_id = "user_measure",
  measure_name = measure_id,
  source = NA_character_,
  release_version = NA_character_,
  weight_panel = NULL,
  items = NULL,
  agg = NULL,
  item = "task_id",
  scale = "IM"
)
```

## Arguments

- data:

  A data frame. For the default path, the user-supplied measure table.
  For the `items`/`agg` path, a Task Ratings style panel with the `item`
  column, `data_value`, and (when `scale` is not `NULL`) `scale_id`.

- key:

  Name of the key column for the default path. Ignored on the
  `items`/`agg` path, where the measure is keyed on `item`.

- score:

  Name of the numeric score column for the default path. Ignored on the
  `items`/`agg` path, where the score is the `data_value` on `scale`.

- key_type:

  Measure grain: occupation, task, or DWA. Forced to `"task"` on the
  `items`/`agg` path.

- universe:

  Optional vector or data frame of valid keys.

- measure_id:

  Short identifier for the measure.

- measure_name:

  Human-readable measure name.

- source:

  Optional source label.

- release_version:

  Optional O\*NET release used to create the measure.

- weight_panel:

  Optional weight panel used to report employment coverage for
  occupation-level measures.

- items:

  Optional character vector of target task ids. Supplying it (or `agg`)
  selects the convenience path. Required when `agg = "targeted"`.

- agg:

  Aggregation mode for the convenience path. `"targeted"` restricts the
  panel to `items`; `"aggregate"` keeps every item. Defaults to
  `"targeted"` when `items` is supplied.

- item:

  Column identifying the content item on the `items`/`agg` path and used
  as the measure key. Defaults to `"task_id"`.

- scale:

  Scale id used to select one rating row per task on the `items`/`agg`
  path, for example `"IM"` for Importance. Use `NULL` to keep every row
  of `data`, which requires `item` to be unique on its own.

## Value

An `onet_measure` object with `data`, `coverage`, `unmatched`, and
`metadata` fields. The `items`/`agg` path returns a task-grain measure
(`key_type = "task"`) keyed on `item` and scored on the `scale` rating,
ready for
[`onet_task_to_occupation()`](https://farach.github.io/onet2r/reference/onet_task_to_occupation.md).

## Details

Two construction paths are available. The default path validates a table
of `key` and `score` columns you already built. The convenience path,
selected by passing `items` or `agg`, builds a task-grain measure in one
line from a Task Ratings style panel: it restricts the panel to the
caller's target `items` (when `agg = "targeted"`) or keeps every item
(when `agg = "aggregate"`), selects one rating row per task on `scale`
(default Importance, `"IM"`), and keys the result on `item` (default
`"task_id"`). The result is exactly the measure the default path returns
on that same subset, so it is a thin convenience wrapper, not a new
estimator. Roll it up to occupations with
[`onet_task_to_occupation()`](https://farach.github.io/onet2r/reference/onet_task_to_occupation.md)
and
[`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md).
The target items, not the package, carry the substantive judgement.

A single `items` vector expresses one target set, so the switch builds a
targeted composite such as a routine-task composite. A difference index
like routine-task intensity, which subtracts abstract and manual
composites from a routine composite, is composed separately from two or
more such measures.

## Examples

``` r
scores <- tibble::tibble(
  onet_soc_code = c("15-1252.00", "29-1141.00"),
  score = c(0.7, 0.2)
)
universe <- c("15-1252.00", "29-1141.00", "11-1011.00")
measure <- onet_measure(scores, "onet_soc_code", "score", universe = universe)
onet_measure_coverage(measure)
#> # A tibble: 1 × 6
#>   key_type   n_input n_universe n_matched coverage_share employment_coverage_s…¹
#>   <chr>        <int>      <int>     <int>          <dbl>                   <dbl>
#> 1 occupation       2          3         2          0.667                      NA
#> # ℹ abbreviated name: ¹​employment_coverage_share

# Convenience path: build a task-grain targeted measure from a panel in one
# line, ready to roll up with onet_task_to_occupation().
panel <- tibble::tibble(
  onet_soc_code = rep(c("15-1252.00", "29-1141.00"), each = 3),
  task_id = c("1", "2", "3", "4", "5", "6"),
  scale_id = "IM",
  data_value = c(4.5, 3.0, 2.0, 1.0, 4.0, 3.0)
)
targeted <- onet_measure(panel, items = c("1", "5"), agg = "targeted")
targeted$data
#> # A tibble: 2 × 6
#>   onet_soc_code task_id scale_id data_value measure_key measure_score
#>   <chr>         <chr>   <chr>         <dbl> <chr>               <dbl>
#> 1 15-1252.00    1       IM              4.5 1                     4.5
#> 2 29-1141.00    5       IM              4   5                     4  
```
