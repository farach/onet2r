# Choosing Employment Weights: OEWS versus PUMS

Weights answer a population question. OEWS is usually the best choice
when the target is official occupation employment and wage context. PUMS
is useful when the target is a custom sample or demographic cell.

## Start with the Same O\*NET Score

``` r
abilities <- onet_archive_read(
  "30.3",
  "Abilities",
  path = archive_303,
  release_date = "2026-05-01"
)

score <- abilities |>
  filter(element_id == "1.A.1.a.1") |>
  transmute(onet_soc_code, measure_score = data_value)

score
#> # A tibble: 4 × 2
#>   onet_soc_code measure_score
#>   <chr>                 <dbl>
#> 1 15-1252.00             4.35
#> 2 29-1141.00             4.71
#> 3 11-1011.00             4.5 
#> 4 41-1011.00             4.15
```

## OEWS Weights

``` r
oews_weights <- onet_weight_panel_oews(
  onet_oews_national(2024, path = oews_path),
  year = 2024
)

oews_result <- onet_measure_aggregate(
  score,
  oews_weights,
  measure_id = "oral_comprehension_fixture"
)

oews_weights
#> # A tibble: 3 × 7
#>   reference_soc_code  year employment weight_share source source_taxonomy
#>   <chr>              <int>      <dbl>        <dbl> <chr>  <chr>          
#> 1 11-1011             2024     211230       0.0404 OEWS   2018 SOC       
#> 2 15-1252             2024    1847900       0.353  OEWS   2018 SOC       
#> 3 29-1141             2024    3175400       0.607  OEWS   2018 SOC       
#> # ℹ 1 more variable: reference_taxonomy <chr>
oews_result |>
  select(-coverage, -provenance) |>
  print(width = Inf)
#> # A tibble: 1 × 7
#>   measure_id                 aggregate total_employment covered_employment
#>   <chr>                          <dbl>            <dbl>              <dbl>
#> 1 oral_comprehension_fixture      4.57          5234530            5234530
#>   employment_coverage_share n_occupations n_reference_soc
#>                       <dbl>         <int>           <int>
#> 1                         1             4               4
```

## PUMS Weights

``` r
pums <- tibble::tibble(
  SOCP = c("151252", "151252", "291141", "291141", "111011", "111011"),
  PWGTP = c(120, 80, 90, 110, 10, 30),
  sex = c("F", "M", "F", "M", "F", "M")
)

pums_weights <- onet_weight_panel_pums(
  pums,
  year = 2022,
  group = "sex"
)

pums_f <- onet_measure_aggregate(
  score,
  pums_weights,
  measure_id = "oral_comprehension_fixture",
  cell = list(sex = "F")
)
pums_m <- onet_measure_aggregate(
  score,
  pums_weights,
  measure_id = "oral_comprehension_fixture",
  cell = list(sex = "M")
)

pums_weights
#> # A tibble: 6 × 8
#>   reference_soc_code sex    year employment weight_share source source_taxonomy
#>   <chr>              <chr> <int>      <dbl>        <dbl> <chr>  <chr>          
#> 1 11-1011            F      2022         10       0.0227 PUMS   2018 SOC       
#> 2 11-1011            M      2022         30       0.0682 PUMS   2018 SOC       
#> 3 15-1252            F      2022        120       0.273  PUMS   2018 SOC       
#> 4 15-1252            M      2022         80       0.182  PUMS   2018 SOC       
#> 5 29-1141            F      2022         90       0.205  PUMS   2018 SOC       
#> 6 29-1141            M      2022        110       0.25   PUMS   2018 SOC       
#> # ℹ 1 more variable: reference_taxonomy <chr>
```

## Compare the Answers

``` r
comparison <- tibble::tibble(
  population = c("OEWS national", "PUMS-style F", "PUMS-style M"),
  aggregate = c(oews_result$aggregate, pums_f$aggregate, pums_m$aggregate),
  coverage = c(
    oews_result$employment_coverage_share,
    pums_f$employment_coverage_share,
    pums_m$employment_coverage_share
  )
)

comparison
#> # A tibble: 3 × 3
#>   population    aggregate coverage
#>   <chr>             <dbl>    <dbl>
#> 1 OEWS national      4.57        1
#> 2 PUMS-style F       4.50        1
#> 3 PUMS-style M       4.55        1
```

``` r
barplot(
  height = setNames(comparison$aggregate, comparison$population),
  col = c("#0f766e", "#14b8a6", "#64748b"),
  border = NA,
  las = 2,
  ylab = "Aggregate score",
  main = "Weight Source Changes the Population"
)
```

![Bar chart comparing OEWS and PUMS-style weighted
aggregates.](choosing-employment-weights_files/figure-html/weights-chart-1.png)

If your claim is about the national labor market, OEWS is the natural
default. If your claim is about a subgroup, geography, or survey-defined
population, build a PUMS weight panel and state the cell explicitly.
