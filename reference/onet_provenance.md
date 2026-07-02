# Return Aggregate Provenance

Returns the provenance recorded by an aggregate or sensitivity table.

## Usage

``` r
onet_provenance(x)
```

## Arguments

- x:

  An object returned by
  [`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md)
  or
  [`onet_measure_sensitivity()`](https://farach.github.io/onet2r/reference/onet_measure_sensitivity.md).

## Value

A tibble with provenance fields.

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
aggregate <- onet_measure_aggregate(measure, weights)
onet_provenance(aggregate)
#> # A tibble: 1 × 8
#>   measure_id     measure_release weight_source weight_year source_taxonomy
#>   <chr>          <chr>           <chr>               <int> <chr>          
#> 1 stylized_score NA              fixture              2024 2018 SOC       
#> # ℹ 3 more variables: reference_taxonomy <chr>, bridge_used <lgl>,
#> #   crosswalk_path <chr>
```
