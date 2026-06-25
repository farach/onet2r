# Create an OEWS Weight Panel

Create an OEWS Weight Panel

## Usage

``` r
onet_weight_panel_oews(
  oews,
  year,
  code = "occ_code",
  employment = "tot_emp",
  reference_taxonomy = "2018 SOC",
  source_taxonomy = NULL,
  crosswalk = NULL
)
```

## Arguments

- oews:

  OEWS data with occupation and employment columns.

- year:

  OEWS estimate year.

- code:

  OEWS occupation code column.

- employment:

  Employment column.

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
oews <- tibble::tibble(
  occ_code = c("15-1252", "29-1141"),
  tot_emp = c(100, 300)
)
onet_weight_panel_oews(oews, year = 2024)
#> # A tibble: 2 × 7
#>   reference_soc_code  year employment weight_share source source_taxonomy
#>   <chr>              <int>      <dbl>        <dbl> <chr>  <chr>          
#> 1 15-1252             2024        100         0.25 OEWS   2018 SOC       
#> 2 29-1141             2024        300         0.75 OEWS   2018 SOC       
#> # ℹ 1 more variable: reference_taxonomy <chr>
```
