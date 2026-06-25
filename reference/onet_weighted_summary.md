# Summarise O\*NET Values with Employment or Wage Weights

Computes employment-weighted summaries for O\*NET task, skill, ability,
or work-activity rows after joining occupation-level employment and wage
estimates. This is useful for moving from occupation-level O\*NET
outputs to labor-market-weighted measures.

## Usage

``` r
onet_weighted_summary(
  data,
  group,
  value,
  occupation_code = "code",
  oews = NULL,
  weight = "tot_emp",
  wage = "a_median"
)
```

## Arguments

- data:

  A data frame containing one row per occupation-value pair.

- group:

  Character vector of columns to group by, such as task or skill
  identifiers.

- value:

  Name of the numeric O\*NET value column to average.

- occupation_code:

  Name of the occupation code column in `data`.

- oews:

  Optional OEWS tibble. If supplied and `weight` is not already in
  `data`, it is joined with
  [`onet_join_oews()`](https://farach.github.io/onet2r/reference/onet_join_oews.md).

- weight:

  Name of the employment weight column. Defaults to `tot_emp`, the OEWS
  total-employment column.

- wage:

  Name of the wage column used for wage-weighted summaries.

## Value

A tibble with `group` columns plus `n_records`, `n_occupations`,
`total_weight`, `weighted_mean`, and `wage_weighted_mean`.

## Lifecycle

This helper is soft-deprecated for measure work. Prefer
[`onet_weight_panel_oews()`](https://farach.github.io/onet2r/reference/onet_weight_panel_oews.md),
[`onet_weight_panel_pums()`](https://farach.github.io/onet2r/reference/onet_weight_panel_pums.md),
and
[`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md)
for vintage-aware weighting.

## Examples

``` r
skills <- tibble::tibble(
  code = c("15-1252.00", "15-1252.00", "29-1141.00", "29-1141.00"),
  element_id = c("2.A.1.a", "2.A.1.b", "2.A.1.a", "2.A.1.b"),
  element_name = c(
    "Reading Comprehension", "Active Listening",
    "Reading Comprehension", "Active Listening"
  ),
  data_value = c(4.12, 4.00, 3.88, 4.25)
)

oews <- tibble::tibble(
  occ_code = c("15-1252", "29-1141"),
  tot_emp = c(1847900, 3175400),
  a_median = c(133080, 93070)
)

suppressWarnings(
  onet_weighted_summary(
    skills,
    group = c("element_id", "element_name"),
    value = "data_value",
    oews = oews
  )
)
#> # A tibble: 2 × 7
#>   element_id element_name     n_records n_occupations total_weight weighted_mean
#>   <chr>      <chr>                <int>         <int>        <dbl>         <dbl>
#> 1 2.A.1.a    Reading Compreh…         2             2      5023300          3.97
#> 2 2.A.1.b    Active Listening         2             2      5023300          4.16
#> # ℹ 1 more variable: wage_weighted_mean <dbl>
```
