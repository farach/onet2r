# Create a Bring-Your-Own O\*NET Measure

Validates a user-supplied occupation, task, or DWA score table and
records coverage against an optional universe. The package does not
supply or alter the substantive score.

## Usage

``` r
onet_measure(
  data,
  key,
  score,
  key_type = c("occupation", "task", "dwa"),
  universe = NULL,
  measure_id = "user_measure",
  measure_name = measure_id,
  source = NA_character_,
  release_version = NA_character_,
  weight_panel = NULL
)
```

## Arguments

- data:

  A data frame containing the user-supplied measure.

- key:

  Name of the key column.

- score:

  Name of the numeric score column.

- key_type:

  Measure grain: occupation, task, or DWA.

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

## Value

An `onet_measure` object with `data`, `coverage`, `unmatched`, and
`metadata` fields.

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
```
