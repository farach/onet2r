# Condition an O\*NET Panel on Resurvey Events

Labels each row of an
[`onet_resurvey_panel()`](https://farach.github.io/onet2r/reference/onet_resurvey_panel.md)
frame with the reason it is or is not part of the at-risk set for change
estimation. This is the resurvey denominator: comparisons only carry
information about task change when the occupation was actually re-rated,
so estimation should restrict to the rows this function marks at risk.

## Usage

``` r
onet_condition_on_resurvey(resurvey_panel, at_risk_only = FALSE)
```

## Arguments

- resurvey_panel:

  A tibble from
  [`onet_resurvey_panel()`](https://farach.github.io/onet2r/reference/onet_resurvey_panel.md).

- at_risk_only:

  If `TRUE`, return only the resurveyed at-risk rows. The default
  returns every row with its label so the excluded denominator stays
  visible.

## Value

The input tibble with two added columns: `selection_reason`, a factor
with levels `resurveyed`, `unrevisited`, `taxonomy_seam`, and
`suppressed`, and `at_risk`, `TRUE` only when `selection_reason` is
`resurveyed`.

## Details

Labels are assigned by precedence so structural exclusions win over the
resurvey signal:

1.  `taxonomy_seam` when the incoming transition crosses a seam
    (`seam_in`) or the row is an Analyst - Transition carry-forward. The
    v25.1 SOC-2010 to SOC-2018 carry-forward is never treated as a
    resurvey.

2.  `suppressed` when `recommend_suppress` is `"Y"`.

3.  `resurveyed` when the occupation survey clock advanced.

4.  `unrevisited` otherwise, including a first appearance with no prior
    release to compare against.

## Examples

``` r
panel <- tibble::tibble(
  release_version = rep(c("22.1", "23.1"), each = 2),
  release_date = rep(as.Date(c("2017-10-01", "2018-11-01")), each = 2),
  soc_vintage = "2010",
  onet_soc_code = rep(c("15-1132.00", "29-1141.00"), 2),
  soc_code = rep(c("15-1132", "29-1141"), 2),
  task_id = rep(c("1001", "1002"), 2),
  scale_id = "IM",
  data_value = c(4.1, 4.6, 4.1, 4.8),
  source_date = as.Date(c("2016-07-01", "2016-07-01", "2016-07-01", "2018-07-01")),
  domain_source = "Incumbent"
)
rp <- onet_resurvey_panel(panel)
onet_condition_on_resurvey(rp)
#> # A tibble: 4 × 24
#>   onet_soc_code soc_code task_id title task  release_version release_date
#>   <chr>         <chr>    <chr>   <chr> <chr> <chr>           <date>      
#> 1 15-1132.00    15-1132  1001    NA    NA    22.1            2017-10-01  
#> 2 15-1132.00    15-1132  1001    NA    NA    23.1            2018-11-01  
#> 3 29-1141.00    29-1141  1002    NA    NA    22.1            2017-10-01  
#> 4 29-1141.00    29-1141  1002    NA    NA    23.1            2018-11-01  
#> # ℹ 17 more variables: soc_vintage <fct>, scale_id <chr>, data_value <dbl>,
#> #   source_date <date>, domain_source <chr>, survey_source <chr>,
#> #   recommend_suppress <chr>, occ_survey_date <date>, prev_survey_date <date>,
#> #   prev_release_version <chr>, resurvey_event <lgl>, cycle_index <int>,
#> #   age_resolved <dbl>, seam_in <lgl>, seam_type <chr>, selection_reason <fct>,
#> #   at_risk <lgl>
```
