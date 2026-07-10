# Return Object Coverage

Returns the coverage table recorded by an onet2r measure, aggregate, or
decomposition object. For sensitivity results, returns one coverage row
per scenario with scenario identifiers attached.

## Usage

``` r
onet_coverage(x)
```

## Arguments

- x:

  An onet2r object with coverage metadata.

## Value

A tibble with coverage fields.

## Examples

``` r
measure <- onet_measure(
  tibble::tibble(onet_soc_code = "15-1252.00", score = 0.7),
  key = "onet_soc_code",
  score = "score",
  universe = c("15-1252.00", "29-1141.00")
)
onet_coverage(measure)
#> # A tibble: 1 × 6
#>   key_type   n_input n_universe n_matched coverage_share employment_coverage_s…¹
#>   <chr>        <int>      <int>     <int>          <dbl>                   <dbl>
#> 1 occupation       1          2         1            0.5                      NA
#> # ℹ abbreviated name: ¹​employment_coverage_share
```
