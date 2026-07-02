# Aggregate Occupation Measures with Employment Weights

Aggregates an occupation-level measure using a normalized weight panel.

## Usage

``` r
onet_measure_aggregate(
  measure,
  weight_panel,
  occupation_code = "onet_soc_code",
  score = "measure_score",
  bridge = NULL,
  measure_id = "user_measure",
  year = NULL,
  cell = NULL
)
```

## Arguments

- measure:

  An `onet_measure` object or a data frame of occupation scores.

- weight_panel:

  A weight panel from
  [`onet_weight_panel_oews()`](https://farach.github.io/onet2r/reference/onet_weight_panel_oews.md)
  or
  [`onet_weight_panel_pums()`](https://farach.github.io/onet2r/reference/onet_weight_panel_pums.md).

- occupation_code:

  Occupation code column when `measure` is a data frame.

- score:

  Score column when `measure` is a data frame.

- bridge:

  Optional bridge from O\*NET-SOC to `reference_soc_code`.

- measure_id:

  Identifier used when `measure` is a data frame.

- year:

  Optional single year used to filter a multi-year weight panel.

- cell:

  Optional named list or named vector used to filter a multi-cell weight
  panel, such as `list(state = "WA")`.

## Value

A one-row tibble with the aggregate, coverage fields, and list-column
metadata readable with
[`onet_provenance()`](https://farach.github.io/onet2r/reference/onet_provenance.md)
and
[`onet_coverage()`](https://farach.github.io/onet2r/reference/onet_coverage.md).

## Details

When multiple O\*NET detail occupations map to the same reference SOC,
`onet_measure_aggregate()` first averages those detail scores within the
SOC. Employment coverage is then counted once per reference SOC, so
coverage shares cannot exceed 100 percent because of detail-code
duplication. If more than 5 percent of filtered weight-panel employment
has no matching measure score, the function reports the largest
unmatched reference SOCs.

## Examples

``` r
measure <- onet_measure(
  tibble::tibble(onet_soc_code = c("15-1252.00", "29-1141.00"), score = c(0.7, 0.2)),
  key = "onet_soc_code",
  score = "score",
  measure_id = "stylized_score"
)
weights <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  year = 2024L,
  employment = c(100, 300),
  weight_share = c(0.25, 0.75),
  source = "fixture",
  source_taxonomy = "2018 SOC",
  reference_taxonomy = "2018 SOC"
)
onet_measure_aggregate(measure, weights)
#> # A tibble: 1 × 9
#>   measure_id     aggregate total_employment covered_employment
#>   <chr>              <dbl>            <dbl>              <dbl>
#> 1 stylized_score     0.325              400                400
#> # ℹ 5 more variables: employment_coverage_share <dbl>, n_occupations <int>,
#> #   n_reference_soc <int>, coverage <list>, provenance <list>
```
