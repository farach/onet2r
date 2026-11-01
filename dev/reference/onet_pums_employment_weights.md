# Create SOC Employment Weights from PUMS Microdata

Converts American Community Survey (ACS) PUMS microdata with occupation
codes into SOC-level employment weights. (CPS microdata uses different
weights and reference periods; see
[`onet_weight_panel_pums()`](https://farach.github.io/onet2r/dev/reference/onet_weight_panel_pums.md)
for the replicate-weight-aware successor when supplying appropriate
occupation and weight columns.) Use this after downloading PUMS data
with packages such as `tidycensus` or `ipumsr`.

## Usage

``` r
onet_pums_employment_weights(pums, socp = "SOCP", weight = "PWGTP")
```

## Arguments

- pums:

  A data frame containing individual-level occupation records.

- socp:

  Name of the occupation-code column. ACS PUMS commonly uses `"SOCP"`.

- weight:

  Optional name of the person-weight column. If `NULL`, each row
  receives weight 1. ACS PUMS commonly uses `"PWGTP"`.

## Value

A tibble with `soc_code`, `employment`, and `records`.

## Details

**\[deprecated\]**

## Lifecycle

This helper is soft-deprecated in favor of
[`onet_weight_panel_pums()`](https://farach.github.io/onet2r/dev/reference/onet_weight_panel_pums.md),
which keeps source and reference taxonomies explicit.

## Examples

``` r
pums <- tibble::tibble(
  SOCP = c("151252", "151252", "291141"),
  PWGTP = c(120, 80, 200)
)

suppressWarnings(onet_pums_employment_weights(pums))
#> # A tibble: 2 × 3
#>   soc_code employment records
#>   <chr>         <dbl>   <int>
#> 1 15-1252         200       2
#> 2 29-1141         200       1
```
