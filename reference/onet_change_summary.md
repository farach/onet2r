# Summarise O\*NET Panel Change Types

Counts reconciliation change types overall and by two-digit job family.

## Usage

``` r
onet_change_summary(reconciled, by = c("overall", "job_family"))
```

## Arguments

- reconciled:

  A tibble from
  [`onet_panel_reconcile()`](https://farach.github.io/onet2r/reference/onet_panel_reconcile.md).

- by:

  Summary level to return. `"overall"` returns one package-level row;
  `"job_family"` returns the overall row plus one row per two-digit SOC
  family.

## Value

A tibble with `job_family`, `change_type`, `safely_comparable`, `n`, and
`share`.

## Examples

``` r
reconciled <- tibble::tibble(
  to_soc_code = c("11-1011", "11-1011", "15-1252"),
  change_type = factor(c("real_update", "stale_carryforward", "real_update")),
  safely_comparable = c(TRUE, TRUE, TRUE)
)
onet_change_summary(reconciled)
#> # A tibble: 1 × 10
#>   summary_level job_family change_type n_pairs share_pairs mean_value_change
#>   <chr>         <chr>      <chr>         <int>       <dbl>             <dbl>
#> 1 overall       NA         real_update       3           1                NA
#> # ℹ 4 more variables: median_abs_value_change <dbl>,
#> #   share_safely_comparable <dbl>, share_method_break <dbl>,
#> #   share_crosswalk_uncertain <dbl>
```
