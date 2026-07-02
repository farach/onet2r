# Reconcile O\*NET Panel Changes

Compares adjacent releases in a panel and classifies changes using the
value change by collection-date change truth table.

## Usage

``` r
onet_panel_reconcile(panel, bridge, weight = "equal")
```

## Arguments

- panel:

  A tibble from
  [`onet_panel()`](https://farach.github.io/onet2r/reference/onet_panel.md)
  or the same schema.

- bridge:

  A bridge from
  [`onet_crosswalk_bridge()`](https://farach.github.io/onet2r/reference/onet_crosswalk_bridge.md).

- weight:

  Weighting method. Only `"equal"` is implemented.

## Value

A tibble of adjacent-release comparisons with change and coverage flags.

## Examples

``` r
panel <- tibble::tibble(
  release_version = c("29.0", "30.0", "29.0", "30.0"),
  release_date = as.Date(c("2024-08-01", "2025-08-01", "2024-08-01", "2025-08-01")),
  soc_vintage = rep("2019", 4),
  domain = "Abilities",
  onet_soc_code = rep("11-1011.00", 4),
  soc_code = rep("11-1011", 4),
  element_id = c("1.A.1.a.1", "1.A.1.a.1", "1.A.1.a.2", "1.A.1.a.2"),
  element_name = c(
    "Oral Comprehension", "Oral Comprehension",
    "Written Comprehension", "Written Comprehension"
  ),
  scale_id = rep("IM", 4),
  data_value = c(4.5, 4.7, 4.2, 4.2),
  source_date = as.Date(c("2023-08-01", "2024-08-01", "2023-08-01", "2023-08-01")),
  domain_source = rep("Analyst", 4)
)
bridge <- tibble::tibble(
  from_vintage = "2019",
  to_vintage = "2019",
  from_soc_code = "11-1011",
  to_soc_code = "11-1011",
  map_type = "one_to_one",
  crosswalk_weight = 1
)
onet_panel_reconcile(panel, bridge)
#> # A tibble: 2 × 37
#>   from_release to_release from_release_date to_release_date from_onet_soc_code
#>   <chr>        <chr>      <date>            <date>          <chr>             
#> 1 29.0         30.0       2024-08-01        2025-08-01      11-1011.00        
#> 2 29.0         30.0       2024-08-01        2025-08-01      11-1011.00        
#> # ℹ 32 more variables: to_onet_soc_code <chr>, from_soc_code <chr>,
#> #   to_soc_code <chr>, soc_vintage_from <chr>, soc_vintage_to <chr>,
#> #   domain <chr>, element_id <chr>, element_name <chr>, scale_id <chr>,
#> #   from_value <dbl>, to_value <dbl>, value_change <dbl>,
#> #   value_percent_change <dbl>, from_source_date <date>, to_source_date <date>,
#> #   from_domain_source <chr>, to_domain_source <chr>,
#> #   from_recommend_suppress <chr>, to_recommend_suppress <chr>, …
```
