# From Tasks to Occupations

Many user measures start at the task level: a researcher labels tasks, a
model scores task text, or a team codes task exposure manually.
Occupation-level analysis needs one score per occupation.
[`onet_task_to_occupation()`](https://farach.github.io/onet2r/reference/onet_task_to_occupation.md)
performs that mechanical rollup using O\*NET task ratings.

## Read Task Statements and Ratings

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

tasks |>
  select(onet_soc_code, task_id, task_type, task) |>
  print(width = Inf)
#> # A tibble: 3 × 4
#>   onet_soc_code task_id task_type   
#>   <chr>         <chr>   <chr>       
#> 1 15-1252.00    1001    Core        
#> 2 15-1252.00    1002    Supplemental
#> 3 29-1141.00    2001    Core        
#>   task                                         
#>   <chr>                                        
#> 1 Analyze user needs and software requirements.
#> 2 Prepare reports on software testing status.  
#> 3 Monitor patient health and record signs.

ratings |>
  select(onet_soc_code, task_id, scale_id, scale_name, data_value) |>
  head(8) |>
  print(width = Inf)
#> # A tibble: 5 × 5
#>   onet_soc_code task_id scale_id scale_name        data_value
#>   <chr>         <chr>   <fct>    <chr>                  <dbl>
#> 1 15-1252.00    1001    RT       Relevance of Task       95  
#> 2 15-1252.00    1001    IM       Importance               4.5
#> 3 15-1252.00    1002    RT       Relevance of Task       45  
#> 4 29-1141.00    2001    RT       Relevance of Task       98  
#> 5 29-1141.00    2001    IM       Importance               4.8
```

## Validate Task Scores

``` r
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
  measure_id = "stylized_task_score"
)

onet_coverage(measure)
#> # A tibble: 1 × 6
#>   key_type n_input n_universe n_matched coverage_share employment_coverage_share
#>   <chr>      <int>      <int>     <int>          <dbl>                     <dbl>
#> 1 task           3          3         3              1                        NA
```

## Roll Up with Relevance Weights

``` r
core_only <- onet_task_to_occupation(
  measure,
  task_ratings = ratings,
  task_metadata = tasks,
  weight_scale = "RT",
  include_supplemental = FALSE
)

core_plus_supplemental <- onet_task_to_occupation(
  measure,
  task_ratings = ratings,
  task_metadata = tasks,
  weight_scale = "RT",
  include_supplemental = TRUE
)

core_only
#> # A tibble: 2 × 5
#>   onet_soc_code n_tasks total_task_weight measure_score soc_code
#>   <chr>           <int>             <dbl>         <dbl> <chr>   
#> 1 15-1252.00          1                95           0.8 15-1252 
#> 2 29-1141.00          1                98           0.2 29-1141
core_plus_supplemental
#> # A tibble: 2 × 5
#>   onet_soc_code n_tasks total_task_weight measure_score soc_code
#>   <chr>           <int>             <dbl>         <dbl> <chr>   
#> 1 15-1252.00          2               140         0.671 15-1252 
#> 2 29-1141.00          1                98         0.2   29-1141
```

## Interpret the Plumbing Choice

``` r
comparison <- bind_rows(
  core_only |> mutate(rule = "Core tasks only"),
  core_plus_supplemental |> mutate(rule = "Core plus Supplemental")
) |>
  select(rule, onet_soc_code, n_tasks, total_task_weight, measure_score)

comparison |>
  print(width = Inf)
#> # A tibble: 4 × 5
#>   rule                   onet_soc_code n_tasks total_task_weight measure_score
#>   <chr>                  <chr>           <int>             <dbl>         <dbl>
#> 1 Core tasks only        15-1252.00          1                95         0.8  
#> 2 Core tasks only        29-1141.00          1                98         0.2  
#> 3 Core plus Supplemental 15-1252.00          2               140         0.671
#> 4 Core plus Supplemental 29-1141.00          1                98         0.2
```

``` r
labels <- paste(comparison$onet_soc_code, comparison$rule, sep = "\n")
barplot(
  height = setNames(comparison$measure_score, labels),
  col = rep(c("#0f766e", "#14b8a6"), each = 2),
  border = NA,
  las = 2,
  ylab = "Occupation score",
  main = "Task Handling Can Change Occupation Scores"
)
```

![Bar chart of occupation scores under core-only and
core-plus-supplemental task
rules.](from-tasks-to-occupations_files/figure-html/task-chart-1.png)

The task score did not change. The rollup rule changed. That distinction
matters because a transparent article should separate substantive
scoring from package plumbing.
