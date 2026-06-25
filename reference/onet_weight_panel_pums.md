# Create a PUMS Weight Panel

Create a PUMS Weight Panel

## Usage

``` r
onet_weight_panel_pums(
  pums,
  year,
  socp = "SOCP",
  weight = "PWGTP",
  group = NULL,
  replicate_weights = NULL,
  reference_taxonomy = "2018 SOC",
  source_taxonomy = NULL,
  crosswalk = NULL
)
```

## Arguments

- pums:

  PUMS microdata or already-filtered person records.

- year:

  ACS or CPS year.

- socp:

  Occupation column, commonly `SOCP`.

- weight:

  Person weight column, commonly `PWGTP`.

- group:

  Optional columns for demographic or geographic cells.

- replicate_weights:

  Optional replicate-weight columns.

- reference_taxonomy:

  Reference SOC label.

- source_taxonomy:

  Optional source taxonomy label.

- crosswalk:

  Optional source-to-reference crosswalk.

## Value

A normalized employment-weight panel.

## Examples

``` r
pums <- tibble::tibble(
  SOCP = c("151252", "151252", "291141"),
  PWGTP = c(50, 50, 300),
  state = c("WA", "WA", "WA")
)
onet_weight_panel_pums(pums, year = 2024, group = "state")
#> # A tibble: 2 × 8
#>   reference_soc_code state  year employment weight_share source source_taxonomy
#>   <chr>              <chr> <int>      <dbl>        <dbl> <chr>  <chr>          
#> 1 15-1252            WA     2024        100         0.25 PUMS   2018 SOC       
#> 2 29-1141            WA     2024        300         0.75 PUMS   2018 SOC       
#> # ℹ 1 more variable: reference_taxonomy <chr>
```
