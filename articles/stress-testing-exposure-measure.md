# Stress-Testing a User-Supplied Exposure Measure

This article uses a stylized task score to show the sensitivity
workflow. The score is not a package-endorsed exposure measure. The
package’s job is to show how much the headline number moves when
non-substantive choices change.

## Build the Measure from Task Files

``` r
tasks <- onet_archive_read(
  "30.3",
  "Task Statements",
  path = archive_dir,
  release_date = "2026-05-01"
)
ratings <- onet_archive_read(
  "30.3",
  "Task Ratings",
  path = archive_dir,
  release_date = "2026-05-01"
)

task_scores <- tibble::tibble(
  task_id = c("1001", "1002", "2001"),
  score = c(0.80, 0.40, 0.20)
)

measure <- onet_measure(
  task_scores,
  key = "task_id",
  score = "score",
  key_type = "task",
  universe = tasks$task_id,
  measure_id = "stylized_exposure",
  release_version = "30.3"
)

onet_coverage(measure)
#> # A tibble: 1 × 6
#>   key_type n_input n_universe n_matched coverage_share employment_coverage_share
#>   <chr>      <int>      <int>     <int>          <dbl>                     <dbl>
#> 1 task           3          3         3              1                        NA
```

## Create Alternative Weight Panels

``` r
oews_weights <- onet_weight_panel_oews(
  onet_oews_national(2024, path = oews_path),
  year = 2024
)

pums <- tibble::tibble(
  SOCP = c("151252", "151252", "291141", "291141"),
  PWGTP = c(80, 120, 200, 80)
)
pums_weights <- onet_weight_panel_pums(pums, year = 2022)

oews_weights
#> # A tibble: 3 × 7
#>   reference_soc_code  year employment weight_share source source_taxonomy
#>   <chr>              <int>      <dbl>        <dbl> <chr>  <chr>          
#> 1 11-1011             2024     211230       0.0404 OEWS   2018 SOC       
#> 2 15-1252             2024    1847900       0.353  OEWS   2018 SOC       
#> 3 29-1141             2024    3175400       0.607  OEWS   2018 SOC       
#> # ℹ 1 more variable: reference_taxonomy <chr>
pums_weights
#> # A tibble: 2 × 7
#>   reference_soc_code  year employment weight_share source source_taxonomy
#>   <chr>              <int>      <dbl>        <dbl> <chr>  <chr>          
#> 1 15-1252             2022        200        0.417 PUMS   2018 SOC       
#> 2 29-1141             2022        280        0.583 PUMS   2018 SOC       
#> # ℹ 1 more variable: reference_taxonomy <chr>
```

## Run the Sensitivity Grid

``` r
sensitivity <- onet_measure_sensitivity(
  measure,
  weight_panels = list(oews = oews_weights, pums = pums_weights),
  task_ratings = ratings,
  task_metadata = tasks,
  include_supplemental = c(FALSE, TRUE)
)

sensitivity |>
  select(
    scenario,
    aggregate,
    employment_coverage_share,
    movement,
    movement_percent
  ) |>
  print(width = Inf)
#> # A tibble: 4 × 5
#>   scenario                                                    aggregate
#>   <chr>                                                           <dbl>
#> 1 RT_core / task_release / oews / no_bridge                       0.421
#> 2 RT_core / task_release / pums / no_bridge                       0.45 
#> 3 RT_core_plus_supplemental / task_release / oews / no_bridge     0.373
#> 4 RT_core_plus_supplemental / task_release / pums / no_bridge     0.396
#>   employment_coverage_share movement movement_percent
#>                       <dbl>    <dbl>            <dbl>
#> 1                     0.960   0                0     
#> 2                     1       0.0293           0.0696
#> 3                     0.960  -0.0473          -0.112 
#> 4                     1      -0.0243          -0.0577
```

``` r
barplot(
  height = setNames(sensitivity$aggregate, sensitivity$scenario),
  col = c("#0f766e", "#14b8a6", "#64748b", "#94a3b8"),
  border = NA,
  las = 2,
  ylab = "Aggregate score",
  main = "Sensitivity to Weight and Task Choices"
)
```

![Bar chart of stylized exposure aggregates across task and weight
choices.](stress-testing-exposure-measure_files/figure-html/sensitivity-chart-1.png)

## Inspect Provenance

``` r
onet_provenance(sensitivity)
#> # A tibble: 4 × 7
#>   measure_id        weight_source weight_year source_taxonomy reference_taxonomy
#>   <chr>             <chr>               <int> <chr>           <chr>             
#> 1 stylized_exposure OEWS                 2024 2018 SOC        2018 SOC          
#> 2 stylized_exposure PUMS                 2022 2018 SOC        2018 SOC          
#> 3 stylized_exposure OEWS                 2024 2018 SOC        2018 SOC          
#> 4 stylized_exposure PUMS                 2022 2018 SOC        2018 SOC          
#> # ℹ 2 more variables: bridge_used <lgl>, crosswalk_path <chr>
```

If the sign, rank, or interpretation of a result depends on one plumbing
choice, that belongs in the write-up. The function does not make the
result more true. It makes the fragility visible.
