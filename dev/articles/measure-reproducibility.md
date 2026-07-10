# Reproducible User Measures

O\*NET users often arrive with their own score: an exposure measure, a
task classification, a skill index, or a hand-coded construct. `onet2r`
does not try to decide which score is correct. It helps with the parts
around the score: checking keys, using versioned O\*NET files,
aggregating tasks to occupations, adding employment weights, and
recording enough provenance for someone else to reproduce the number.

This article uses the small archive fixtures shipped with the package.
The task score is stylized and should not be interpreted as a real
exposure measure. Every displayed table is produced by package
functions.

## Read Task Files from a Pinned Release

``` r
tasks <- onet_archive_read(
  "30.3",
  "Task Statements",
  path = archive_dir,
  release_date = "2026-05-01"
)
task_ratings <- onet_archive_read(
  "30.3",
  "Task Ratings",
  path = archive_dir,
  release_date = "2026-05-01"
)

tasks |>
  select(onet_soc_code, task_id, task_type, task) |>
  head() |>
  knitr::kable(digits = 3, align = "l")
```

| onet_soc_code | task_id | task_type    | task                                          |
|:--------------|:--------|:-------------|:----------------------------------------------|
| 15-1252.00    | 1001    | Core         | Analyze user needs and software requirements. |
| 15-1252.00    | 1002    | Supplemental | Prepare reports on software testing status.   |
| 29-1141.00    | 2001    | Core         | Monitor patient health and record signs.      |

`Task Statements` carries task ids and Core or Supplemental labels.
`Task Ratings` carries relevance and importance ratings used to
aggregate task-level scores.

A practical note on choosing between the Importance (1-5) and Level
(0-7) scales: they are empirically near-redundant. Handel ([Handel
2016](#ref-handel2016onet)) reports a mean within-descriptor correlation
of 0.92 across 130,249 ratings, with roughly 19% of correlations at 0.98
or above. A measure built from IM and one built from LV will usually
rank occupations almost identically, so treat an IM-vs-LV sensitivity
check as cheap insurance, not as two independent measures.

``` r
task_ratings |>
  select(onet_soc_code, task_id, scale_id, scale_name, data_value, recommend_suppress) |>
  head() |>
  knitr::kable(digits = 3, align = "l")
```

| onet_soc_code | task_id | scale_id | scale_name        | data_value | recommend_suppress |
|:--------------|:--------|:---------|:------------------|:-----------|:-------------------|
| 15-1252.00    | 1001    | RT       | Relevance of Task | 95.0       | N                  |
| 15-1252.00    | 1001    | IM       | Importance        | 4.5        | N                  |
| 15-1252.00    | 1002    | RT       | Relevance of Task | 45.0       | N                  |
| 29-1141.00    | 2001    | RT       | Relevance of Task | 98.0       | N                  |
| 29-1141.00    | 2001    | IM       | Importance        | 4.8        | N                  |

## Validate a User-Supplied Measure

Here the user brings a 3-task score. It could come from a model, a
survey, or manual coding. The constructor checks that the task ids match
the selected O\*NET release universe.

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
  measure_id = "stylized_task_score",
  measure_name = "Stylized task score",
  release_version = "30.3"
)

onet_coverage(measure) |>
  knitr::kable(digits = 3, align = "l")
```

| key_type | n_input | n_universe | n_matched | coverage_share | employment_coverage_share |
|:---------|:--------|:-----------|:----------|:---------------|:--------------------------|
| task     | 3       | 3          | 3         | 1              | NA                        |

The object also stores unmatched keys explicitly.

``` r
unmatched <- if (nrow(measure$unmatched) == 0) {
  tibble::tibble(status = "No unmatched task ids")
} else {
  measure$unmatched
}

unmatched |>
  knitr::kable(digits = 3, align = "l")
```

| status                |
|:----------------------|
| No unmatched task ids |

## Roll Tasks to Occupations

The next step is mechanical. We use O\*NET task relevance ratings as
weights and restrict the calculation to Core tasks.

``` r
occupation_scores <- onet_task_to_occupation(
  measure,
  task_ratings = task_ratings,
  task_metadata = tasks,
  weight_scale = "RT",
  include_supplemental = FALSE
)

occupation_scores |>
  select(onet_soc_code, soc_code, n_tasks, total_task_weight, measure_score) |>
  knitr::kable(digits = 3, align = "l")
```

| onet_soc_code | soc_code | n_tasks | total_task_weight | measure_score |
|:--------------|:---------|:--------|:------------------|:--------------|
| 15-1252.00    | 15-1252  | 1       | 95                | 0.8           |
| 29-1141.00    | 29-1141  | 1       | 98                | 0.2           |

Changing the Core-only rule is a plumbing choice, not a change to the
user’s score. It belongs in provenance and sensitivity checks.

``` r
occupation_scores_all <- onet_task_to_occupation(
  measure,
  task_ratings = task_ratings,
  task_metadata = tasks,
  weight_scale = "RT",
  include_supplemental = TRUE
)

occupation_scores_all |>
  select(onet_soc_code, soc_code, n_tasks, total_task_weight, measure_score) |>
  knitr::kable(digits = 3, align = "l")
```

| onet_soc_code | soc_code | n_tasks | total_task_weight | measure_score |
|:--------------|:---------|:--------|:------------------|:--------------|
| 15-1252.00    | 15-1252  | 2       | 140               | 0.671         |
| 29-1141.00    | 29-1141  | 1       | 98                | 0.200         |

## Add Employment Weights

OEWS files are SOC-level. The weight helper makes the source vintage
explicit and returns a single shape used downstream.

``` r
oews_sample <- onet_oews_national(
  path = onet2r_inst_path("extdata", "oews-national-sample.csv")
)

weights <- onet_weight_panel_oews(oews_sample, year = 2024)
#> Dropped 2 OEWS aggregate rows; keeping "detailed" occupations.

weights |>
  knitr::kable(digits = 3, align = "l")
```

| reference_soc_code | year | employment | weight_share | source | source_taxonomy | reference_taxonomy |
|:-------------------|:-----|:-----------|:-------------|:-------|:----------------|:-------------------|
| 11-1011            | 2024 | 211230     | 0.040        | OEWS   | 2018 SOC        | 2018 SOC           |
| 15-1252            | 2024 | 1847900    | 0.353        | OEWS   | 2018 SOC        | 2018 SOC           |
| 29-1141            | 2024 | 3175400    | 0.607        | OEWS   | 2018 SOC        | 2018 SOC           |

## Aggregate and Inspect Provenance

``` r
national <- onet_measure_aggregate(
  occupation_scores,
  weights,
  measure_id = "stylized_task_score"
)

national |>
  select(-coverage, -provenance) |>
  knitr::kable(digits = 3, align = "l")
```

| measure_id          | aggregate | total_employment | covered_employment | employment_coverage_share | n_occupations | n_reference_soc |
|:--------------------|:----------|:-----------------|:-------------------|:--------------------------|:--------------|:----------------|
| stylized_task_score | 0.421     | 5234530          | 5023300            | 0.96                      | 2             | 2               |

``` r

onet_provenance(national) |>
  knitr::kable(digits = 3, align = "l")
```

| measure_id          | measure_release | weight_source | weight_year | source_taxonomy | reference_taxonomy | bridge_used | crosswalk_path        |
|:--------------------|:----------------|:--------------|:------------|:----------------|:-------------------|:------------|:----------------------|
| stylized_task_score | 30.3            | OEWS          | 2024        | 2018 SOC        | 2018 SOC           | FALSE       | 2018 SOC -\> 2018 SOC |

## Check Sensitivity to Plumbing Choices

``` r
diagnostic <- onet_measure_sensitivity(
  measure,
  weight_panels = weights,
  task_ratings = task_ratings,
  task_metadata = tasks,
  include_supplemental = c(FALSE, TRUE)
)

diagnostic |>
  select(scenario, aggregate, employment_coverage_share, movement, movement_percent) |>
  knitr::kable(digits = 3, align = "l")
```

| scenario                                                       | aggregate | employment_coverage_share | movement | movement_percent |
|:---------------------------------------------------------------|:----------|:--------------------------|:---------|:-----------------|
| RT_core / task_release / weights / no_bridge                   | 0.421     | 0.96                      | 0.000    | 0.000            |
| RT_core_plus_supplemental / task_release / weights / no_bridge | 0.373     | 0.96                      | -0.047   | -0.112           |

The diagnostic will not tell you which task score is right. It reports
how far the headline aggregate moves when non-substantive plumbing
changes. The second argument is `weight_panels`; for multi-release task
comparisons, pass named lists of single-release frames through
`task_ratings` and matching `task_metadata`. A content-change table is
not an employment weight panel and is rejected. The returned `movement`
fields compare scenario aggregates with the baseline. They are not
Spearman, rank, quintile, variance, or content-drift diagnostics.

Handel, Michael J. 2016. “The O\*NET Content Model: Strengths and
Limitations.” *Journal for Labour Market Research* 49 (2): 157–76.
<https://doi.org/10.1007/s12651-016-0199-8>.
