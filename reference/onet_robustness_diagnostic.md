# Compare Aggregates Across Plumbing Choices

Compare Aggregates Across Plumbing Choices

## Usage

``` r
onet_robustness_diagnostic(
  results,
  baseline = NULL,
  scenario = "scenario",
  aggregate = "aggregate"
)
```

## Arguments

- results:

  A data frame with one row per scenario and an aggregate column.

- baseline:

  Scenario name used as the baseline. If `NULL`, the first row is the
  baseline.

- scenario:

  Scenario column.

- aggregate:

  Aggregate value column.

## Value

A tibble with baseline movement fields.

## Examples

``` r
scenarios <- tibble::tibble(
  scenario = c("baseline", "alternate_weights"),
  aggregate = c(0.40, 0.45)
)
onet_robustness_diagnostic(scenarios)
#> # A tibble: 2 × 6
#>   scenario          aggregate baseline_scenario baseline_aggregate movement
#>   <chr>                 <dbl> <chr>                          <dbl>    <dbl>
#> 1 baseline               0.4  baseline                         0.4     0   
#> 2 alternate_weights      0.45 baseline                         0.4     0.05
#> # ℹ 1 more variable: movement_percent <dbl>
```
