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

  ACS PUMS microdata or already-filtered person records.

- year:

  ACS PUMS data year.

- socp:

  Occupation column, commonly `SOCP`. Do not pass Census `OCCP`; it is
  not an SOC field.

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

A normalized employment-weight panel. When `group` is supplied,
`weight_share` is computed within each group cell.

## Details

`onet_weight_panel_pums()` expects ACS PUMS records that have already
been filtered to the employment universe used by the analysis. For
employment weights, a common ACS starting point is employed civilians,
`ESR %in% c(1, 2)`, often restricted to age 16 or older. Use `SOCP` as
the occupation field. `OCCP` is a Census occupation recode, not an SOC
field.

ACS `SOCP` can include aggregate codes with trailing `X` characters.
Those rows cannot be matched directly to O\*NET or OEWS SOC codes, so
they are dropped with a warning unless the caller supplies a crosswalk
that maps them to valid reference SOC codes.

Replicate-weight standard errors use
`sqrt((4 / R) * sum((theta_r - theta)^2))`. For ACS PUMS, pass the full
80 `PWGTP1` through `PWGTP80` columns. Passing a smaller subset produces
a warning because the resulting standard error is not survey-valid.

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
