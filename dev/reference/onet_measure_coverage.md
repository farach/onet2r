# Return Measure Coverage

Return Measure Coverage

## Usage

``` r
onet_measure_coverage(measure)
```

## Arguments

- measure:

  An object from
  [`onet_measure()`](https://farach.github.io/onet2r/dev/reference/onet_measure.md).

## Value

A tibble with key and optional employment coverage.

## Examples

``` r
measure <- onet_measure(
  tibble::tibble(onet_soc_code = "15-1252.00", score = 0.7),
  key = "onet_soc_code",
  score = "score",
  universe = c("15-1252.00", "29-1141.00")
)
onet_measure_coverage(measure)
#> # A tibble: 1 × 6
#>   key_type   n_input n_universe n_matched coverage_share employment_coverage_s…¹
#>   <chr>        <int>      <int>     <int>          <dbl>                   <dbl>
#> 1 occupation       1          2         1            0.5                      NA
#> # ℹ abbreviated name: ¹​employment_coverage_share
```
