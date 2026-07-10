# Join O\*NET Occupations to National OEWS Estimates

Joins O\*NET occupation data to national OEWS employment and wage
estimates by converting detailed O\*NET-SOC codes such as `"15-1252.00"`
to SOC codes such as `"15-1252"`.

## Usage

``` r
onet_join_oews(
  occupations,
  oews = NULL,
  year = latest_oews_year(),
  by = "code",
  cache_dir = tools::R_user_dir("onet2r", "cache"),
  force = FALSE,
  quiet = TRUE
)
```

## Arguments

- occupations:

  A data frame containing an O\*NET occupation code column.

- oews:

  Optional OEWS tibble. If omitted,
  [`onet_oews_national()`](https://farach.github.io/onet2r/dev/reference/onet_oews_national.md)
  is called.

- year:

  Integer OEWS estimate year, used when `oews` is omitted.

- by:

  Name of the occupation code column in `occupations`.

- cache_dir:

  Directory used to cache the downloaded BLS ZIP file.

- force:

  Logical; if `TRUE`, re-download OEWS data when `oews` is omitted.

- quiet:

  Logical; if `FALSE`, show download progress.

## Value

A tibble containing `occupations` plus a `soc_code` column and matching
national OEWS employment and wage estimate columns.

## Details

**\[deprecated\]**

## Lifecycle

This helper is soft-deprecated in favor of
[`onet_weight_panel_oews()`](https://farach.github.io/onet2r/dev/reference/onet_weight_panel_oews.md)
plus
[`onet_measure_aggregate()`](https://farach.github.io/onet2r/dev/reference/onet_measure_aggregate.md)
for vintage-aware weighting.

## Examples

``` r
occupations <- tibble::tibble(
  code = c("15-1252.00", "29-1141.00"),
  title = c("Software Developers", "Registered Nurses")
)

oews <- tibble::tibble(
  occ_code = c("15-1252", "29-1141"),
  occ_title = c("Software Developers", "Registered Nurses"),
  tot_emp = c(1847900, 3175400),
  a_median = c(133080, 93070)
)

suppressWarnings(onet_join_oews(occupations, oews = oews))
#> # A tibble: 2 × 6
#>   code       title               soc_code occ_title           tot_emp a_median
#>   <chr>      <chr>               <chr>    <chr>                 <dbl>    <dbl>
#> 1 15-1252.00 Software Developers 15-1252  Software Developers 1847900   133080
#> 2 29-1141.00 Registered Nurses   29-1141  Registered Nurses   3175400    93070
```
