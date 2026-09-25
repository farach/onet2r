# Bridge O\*NET Occupations to OEWS Reference Codes

Builds a bridge from O\*NET-SOC codes to the reference SOC codes of an
OEWS weight panel, including the combined codes OEWS publishes in place
of some detailed SOC occupations. Pass the result to the `bridge`
argument of
[`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md)
or
[`onet_measure_sensitivity()`](https://farach.github.io/onet2r/reference/onet_measure_sensitivity.md).

## Usage

``` r
onet_oews_bridge(occupations, weight_panel, occupation_code = "onet_soc_code")
```

## Arguments

- occupations:

  O\*NET-SOC codes to bridge: a character vector, a data frame with an
  `occupation_code` column, or an occupation-level
  [`onet_measure()`](https://farach.github.io/onet2r/reference/onet_measure.md).
  Usually the occupation scores you are aggregating.

- weight_panel:

  A weight panel from
  [`onet_weight_panel_oews()`](https://farach.github.io/onet2r/reference/onet_weight_panel_oews.md)
  built from May 2021 or later OEWS estimates, or any data frame with
  `reference_soc_code` and `year` columns. National, state,
  metropolitan, and industry panels all work.

- occupation_code:

  Occupation code column when `occupations` is a data frame.

## Value

A tibble with one row per O\*NET-SOC code and columns
`from_onet_soc_code`, `from_soc_code`, `reference_soc_code`, `map_type`,
`crosswalk_weight`, and `crosswalk_path`. `map_type` is `"direct"` when
the occupation's SOC is in the panel, `"oews_combination"` when OEWS
publishes the occupation inside a combined code that is in the panel,
and `"not_in_panel"` otherwise. Rows other than `"oews_combination"`
keep the occupation's own SOC as `reference_soc_code`, so aggregation
treats them as it would without a bridge.
[`onet_provenance()`](https://farach.github.io/onet2r/reference/onet_provenance.md)
reports `crosswalk_path` for aggregates that use the bridge.

## Details

Beginning with the May 2021 estimates, OEWS publishes most detailed 2018
SOC occupations but combines some of them, either at the
broad-occupation level (for example `31-1120`, Home Health and Personal
Care Aides, which holds `31-1121` and `31-1122`) or as OEWS-specific
codes (for example `25-9045`, Teaching Assistants, Except
Postsecondary). Without a bridge, O\*NET occupations inside those codes
cannot match the panel, and their employment is left out of
`covered_employment`.

The bridge uses the 12 combined codes, and the SOC occupations each one
includes, listed in the BLS May 2021 OEWS occupation definitions. The
May 2023 through May 2025 national files publish the same 12 codes.
Because the list comes from those definitions rather than from the rows
of `weight_panel`, an occupation missing from a state, metropolitan, or
industry panel, for example because its estimate is suppressed, is never
merged into a neighboring combined code. It stays `"not_in_panel"`.

Occupations inside one combination are averaged with equal weight, the
same way
[`onet_measure_aggregate()`](https://farach.github.io/onet2r/reference/onet_measure_aggregate.md)
averages several O\*NET detail codes that share one SOC. OEWS publishes
no employment split among them.

May 2019 and May 2020 OEWS estimates use a hybrid of the 2010 and 2018
SOC with different combined codes, and earlier estimates use older SOC
versions, so `weight_panel` must contain only years from 2021 on. Build
a bridge by hand for earlier panels.

## Examples

``` r
weights <- tibble::tibble(
  reference_soc_code = c("29-1141", "31-1120"),
  year = 2024L,
  employment = c(3000, 4000),
  weight_share = c(3, 4) / 7,
  source = "OEWS",
  source_taxonomy = "2018 SOC",
  reference_taxonomy = "2018 SOC"
)
# Stylized scores for illustration only.
scores <- tibble::tibble(
  onet_soc_code = c("29-1141.00", "31-1121.00", "31-1122.00"),
  measure_score = c(0.2, 0.4, 0.6)
)
bridge <- onet_oews_bridge(scores, weights)
#> Mapped 2 O*NET occupations into 1 OEWS combination code: "31-1120".
bridge
#> # A tibble: 3 × 6
#>   from_onet_soc_code from_soc_code reference_soc_code map_type  crosswalk_weight
#>   <chr>              <chr>         <chr>              <chr>                <dbl>
#> 1 29-1141.00         29-1141       29-1141            direct                   1
#> 2 31-1121.00         31-1121       31-1120            oews_com…                1
#> 3 31-1122.00         31-1122       31-1120            oews_com…                1
#> # ℹ 1 more variable: crosswalk_path <chr>
onet_measure_aggregate(scores, weights, bridge = bridge, measure_id = "stylized")
#> # A tibble: 1 × 9
#>   measure_id aggregate total_employment covered_employment
#>   <chr>          <dbl>            <dbl>              <dbl>
#> 1 stylized       0.371             7000               7000
#> # ℹ 5 more variables: employment_coverage_share <dbl>, n_occupations <int>,
#> #   n_reference_soc <int>, coverage <list>, provenance <list>
```
