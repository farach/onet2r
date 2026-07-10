# Stress Test a User-Supplied Measure

Runs the same user-supplied measure through alternative weight panels,
bridges, and task-handling choices. The package does not create the
substantive measure. It only changes the plumbing around that measure.

## Usage

``` r
onet_measure_sensitivity(
  measure,
  weight_panels,
  bridges = list(no_bridge = NULL),
  task_ratings = NULL,
  task_metadata = NULL,
  include_supplemental = FALSE,
  weight_scale = "RT",
  year = NULL,
  cell = NULL,
  baseline = NULL
)
```

## Arguments

- measure:

  An `onet_measure` object.

- weight_panels:

  A weight-panel data frame or named list of weight-panel data frames.

- bridges:

  Optional bridge data frame, `NULL`, or named list of bridges.

- task_ratings:

  For task-level measures, a task-ratings data frame or named list of
  task-ratings data frames. Release provenance uses `release_version`,
  then row-wise `version` where `release_version` is missing. When
  neither column exists, an explicit list name is used.

- task_metadata:

  Optional task metadata data frame or named list matching
  `task_ratings`.

- include_supplemental:

  Logical vector. For task-level measures, controls whether Supplemental
  tasks are included.

- weight_scale:

  Character vector of task rating scale ids. Defaults to `"RT"`, the
  Task Ratings scale for Relevance of Task.

- year:

  Optional single year passed to
  [`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md).

- cell:

  Optional cell filter passed to
  [`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md).

- baseline:

  Optional scenario label used as the movement baseline.

## Value

A tibble with one row per scenario, aggregate results, movement fields,
and provenance list-column metadata.

## Examples

``` r
scores <- tibble::tibble(
  onet_soc_code = c("15-1252.00", "29-1141.00"),
  score = c(0.7, 0.2)
)
measure <- onet_measure(scores, "onet_soc_code", "score")
weights <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  year = 2024L,
  employment = c(100, 300),
  weight_share = c(0.25, 0.75),
  source = "fixture",
  source_taxonomy = "2018 SOC",
  reference_taxonomy = "2018 SOC"
)
onet_measure_sensitivity(measure, weights)
#> # A tibble: 1 × 20
#>   scenario  measure_id task_release soc_vintage weight_panel bridge weight_scale
#>   <chr>     <chr>      <chr>        <chr>       <chr>        <chr>  <chr>       
#> 1 measure_… user_meas… NA           NA          weights      no_br… NA          
#> # ℹ 13 more variables: include_supplemental <lgl>, aggregate <dbl>,
#> #   total_employment <dbl>, covered_employment <dbl>,
#> #   employment_coverage_share <dbl>, n_occupations <int>,
#> #   n_reference_soc <int>, coverage <list>, provenance <list>,
#> #   baseline_scenario <chr>, baseline_aggregate <dbl>, movement <dbl>,
#> #   movement_percent <dbl>
```
