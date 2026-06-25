# Aggregate Task Scores to Occupations

Rolls a task-level measure up to occupations using O\*NET task
relevance, importance, or equal task weights.

## Usage

``` r
onet_task_to_occupation(
  measure,
  task_ratings,
  task_metadata = NULL,
  occupation_code = "onet_soc_code",
  task_id = "task_id",
  task_type = "task_type",
  scale_id = "scale_id",
  value = "data_value",
  weight_scale = "RT",
  include_supplemental = FALSE
)
```

## Arguments

- measure:

  An `onet_measure` object with `key_type = "task"`.

- task_ratings:

  A data frame containing occupation task ratings.

- task_metadata:

  Optional data frame containing task type by task id.

- occupation_code:

  Column with O\*NET-SOC codes in `task_ratings`.

- task_id:

  Column with task ids in `task_ratings` and `task_metadata`.

- task_type:

  Column with Core or Supplemental labels.

- scale_id:

  Column with O\*NET task rating scale ids.

- value:

  Column with rating values.

- weight_scale:

  Scale used for task weights. Relevance (`"RT"`) is the default.

- include_supplemental:

  If `TRUE`, include Supplemental tasks.

## Value

A tibble with occupation-level measure scores.

## Examples

``` r
task_measure <- onet_measure(
  tibble::tibble(task_id = c("1001", "1002"), score = c(0.8, 0.4)),
  key = "task_id",
  score = "score",
  key_type = "task"
)
task_ratings <- tibble::tibble(
  onet_soc_code = c("15-1252.00", "15-1252.00"),
  task_id = c("1001", "1002"),
  scale_id = "RT",
  data_value = c(80, 20),
  task_type = "Core"
)
onet_task_to_occupation(task_measure, task_ratings)
#> # A tibble: 1 × 5
#>   onet_soc_code n_tasks total_task_weight measure_score soc_code
#>   <chr>           <int>             <dbl>         <dbl> <chr>   
#> 1 15-1252.00          2               100          0.72 15-1252 
```
