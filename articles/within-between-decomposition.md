# Within-Versus-Between Decomposition

Aggregate changes can come from two places. Occupations can change
internally, or employment can shift toward occupations that already had
different scores. With O\*NET, the within term needs an extra gate: only
rows that survive the comparability checks should be counted as safely
comparable within-occupation change.

## A Two-Occupation Example

``` r
from_scores <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  measure_score = c(1.0, 2.0),
  safely_comparable = c(TRUE, FALSE)
)
to_scores <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  measure_score = c(2.0, 2.5),
  safely_comparable = c(TRUE, FALSE)
)
from_weights <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  employment = c(100, 100)
)
to_weights <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  employment = c(150, 50)
)

from_scores
#> # A tibble: 2 × 3
#>   reference_soc_code measure_score safely_comparable
#>   <chr>                      <dbl> <lgl>            
#> 1 15-1252                        1 TRUE             
#> 2 29-1141                        2 FALSE
to_scores
#> # A tibble: 2 × 3
#>   reference_soc_code measure_score safely_comparable
#>   <chr>                      <dbl> <lgl>            
#> 1 15-1252                      2   TRUE             
#> 2 29-1141                      2.5 FALSE
```

The first occupation is safe for within-change attribution. The second
is not, so its within movement goes to the unclassifiable bucket.

``` r
decomp <- onet_decompose_change(
  from_scores,
  to_scores,
  from_weights,
  to_weights
)

decomp |>
  select(component, value) |>
  print(width = Inf)
#> # A tibble: 5 × 2
#>   component       value
#>   <chr>           <dbl>
#> 1 within          0.5  
#> 2 between        -0.25 
#> 3 interaction     0.125
#> 4 unclassifiable  0.25 
#> 5 total_change    0.625

onet_coverage(decomp)
#> # A tibble: 1 × 3
#>   n_common n_safely_comparable leakage
#>      <int>               <int>   <dbl>
#> 1        2                   1       0
```

``` r
components <- decomp |>
  filter(component != "total_change")

barplot(
  height = setNames(components$value, components$component),
  col = c("#0f766e", "#14b8a6", "#99f6e4", "#64748b"),
  border = NA,
  las = 2,
  ylab = "Contribution to change",
  main = "Where Did the Aggregate Change Come From?"
)
abline(h = 0, col = "#334155")
```

![Bar chart showing within, between, interaction, and unclassifiable
decomposition
components.](within-between-decomposition_files/figure-html/decomposition-chart-1.png)

The component rows other than `total_change` sum to the total change.

``` r
decomp |>
  summarise(
    component_sum = sum(value[component != "total_change"]),
    total_change = value[component == "total_change"],
    difference = component_sum - total_change
  )
#> # A tibble: 1 × 3
#>   component_sum total_change difference
#>           <dbl>        <dbl>      <dbl>
#> 1         0.625        0.625          0
```

## A Demographic Split

The same decomposition can be run after users prepare weight panels for
demographic cells. The package does not download raw PUMS data in
examples, but the fixture below follows the same reference-SOC, cell,
employment shape.

``` r
from_weights_by_sex <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141", "15-1252", "29-1141"),
  sex = c("F", "F", "M", "M"),
  employment = c(40, 70, 60, 30)
)
to_weights_by_sex <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141", "15-1252", "29-1141"),
  sex = c("F", "F", "M", "M"),
  employment = c(70, 50, 80, 0)
)

split_results <- lapply(unique(from_weights_by_sex$sex), function(cell) {
  result <- onet_decompose_change(
    from_scores,
    to_scores,
    from_weights_by_sex |> filter(sex == cell),
    to_weights_by_sex |> filter(sex == cell)
  )
  result$sex <- cell
  result
}) |>
  purrr::list_rbind()

split_results |>
  select(sex, component, value) |>
  print(n = Inf, width = Inf)
#> # A tibble: 10 × 3
#>    sex   component       value
#>    <chr> <chr>           <dbl>
#>  1 F     within          0.364
#>  2 F     between        -0.220
#>  3 F     interaction     0.110
#>  4 F     unclassifiable  0.318
#>  5 F     total_change    0.572
#>  6 M     within          0.667
#>  7 M     between        -0.333
#>  8 M     interaction     0.167
#>  9 M     unclassifiable  0.167
#> 10 M     total_change    0.667
```

Each cell has its own weight shift and its own unclassifiable bucket.
That is the part reviewers need to see when an aggregate result depends
on vintage bridges, source dates, or task-handling choices.
