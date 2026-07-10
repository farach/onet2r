# Resolve Source Codes to a Reference SOC

Maps a source occupation code column onto a reference SOC vintage using
an optional crosswalk. If no crosswalk is supplied, source and reference
codes are treated as the same SOC grain.

## Usage

``` r
onet_reference_soc_resolve(
  data,
  code,
  source_taxonomy,
  reference_taxonomy = "2018 SOC",
  source_year = NA_integer_,
  crosswalk = NULL
)
```

## Arguments

- data:

  A data frame with source occupation codes.

- code:

  Source code column.

- source_taxonomy:

  Source taxonomy label.

- reference_taxonomy:

  Reference taxonomy label.

- source_year:

  Optional source year.

- crosswalk:

  Optional data frame with `source_code` and `reference_soc_code`, plus
  optional `crosswalk_weight` and `map_type`.

## Value

A tibble with source and reference SOC mapping fields.

## Examples

``` r
jobs <- tibble::tibble(occ_code = c("15-1252", "29-1141"))
onet_reference_soc_resolve(
  jobs,
  code = "occ_code",
  source_taxonomy = "2018 SOC"
)
#> # A tibble: 2 × 8
#>   source_taxonomy source_year reference_taxonomy source_code reference_soc_code
#>   <chr>                 <int> <chr>              <chr>       <chr>             
#> 1 2018 SOC                 NA 2018 SOC           15-1252     15-1252           
#> 2 2018 SOC                 NA 2018 SOC           29-1141     29-1141           
#> # ℹ 3 more variables: map_type <chr>, crosswalk_weight <dbl>,
#> #   crosswalk_path <chr>
```
