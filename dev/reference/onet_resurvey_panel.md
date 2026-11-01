# Assemble a Task by Resurvey-Cycle Panel

Restructures a longitudinal O\*NET panel into the task by resurvey-cycle
frame used to study when occupations are actually re-rated. O\*NET
publishes an occupation survey `source_date` on each Task Ratings row
and rotates its incumbent-worker survey across only part of the taxonomy
each year, so a cell is an observation of change only when the
occupation was re-rated between two releases. This function exposes that
resurvey clock at the task level.

## Usage

``` r
onet_resurvey_panel(
  panel,
  scale = "IM",
  item = "task_id",
  survey_sources = c("Incumbent", "Occupational Expert"),
  min_importance = NULL,
  seams = NULL
)
```

## Arguments

- panel:

  A tibble from
  [`onet_panel()`](https://farach.github.io/onet2r/dev/reference/onet_panel.md)
  (a Task Ratings panel) or the same schema. Must contain
  `onet_soc_code`, `release_version`, `release_date`, `soc_vintage`,
  `source_date`, `domain_source`, `data_value`, and the `item` key
  column. `scale_id` is required when `scale` is not `NULL`.

- scale:

  Optional scale id to filter to a single rating, for example `"IM"` for
  Importance. Filtering keeps one row per occupation, item, and release.
  Use `NULL` to keep every scale.

- item:

  Column that identifies the content item within an occupation. Defaults
  to `"task_id"`.

- survey_sources:

  Character vector of `domain_source` values that count as an incumbent
  survey rotation. Defaults to Incumbent and Occupational Expert.
  Matching is case-insensitive and by substring.

- min_importance:

  Optional numeric floor applied to `data_value` (on the selected
  `scale`). Rows below the floor, or with missing `data_value`, are
  dropped. Use it to apply an Importance floor for any channel-specific
  artifact a caller has external evidence for, for example near a
  specific release boundary.

- seams:

  Optional data frame with `seam_type` and `seam_date` columns that
  overrides the default seam table returned by `onet_known_seams()`,
  which currently contains only the verified v25.1 SOC-2010 to SOC-2018
  taxonomy seam. Use it to supply channel-specific or source-specific
  seam dates, such as a v21.0 row, when a caller has external evidence
  that a comparison spanning that date needs seam treatment; v21.0 is
  not a package-verified default seam. A row is a date-based seam when
  its `seam_date` falls in the interval between a release and its prior
  release. Supply an empty table to disable date-based seams entirely.
  `NULL` keeps the default table, so Task Ratings output is unchanged.
  Cross-vintage SOC seams are always detected from `soc_vintage`
  regardless of this table.

## Value

A tibble with one row per occupation, item, and release. Alongside the
identifier and rating columns it carries the resurvey clock:
`survey_source` (survey, analyst, other, or unknown), `occ_survey_date`
(the occupation survey clock, the latest survey `source_date` in that
release), `prev_survey_date`, `prev_release_version`, `resurvey_event`
(the survey clock advanced versus the occupation's prior release within
the same taxonomy era), `cycle_index` (cumulative resurvey count),
`age_resolved` (years of staleness a resurvey resolved), and the
incoming seam flags `seam_in` and `seam_type`.

## Details

The occupation survey clock is the newest `source_date` among rows whose
`domain_source` is a survey source; Analyst - Transition carry-forward
rows are excluded from the clock. Resurvey events and cycle indices are
computed within each `soc_vintage` era so the v25.1 SOC seam never
registers as a resurvey. Pair this frame with
[`onet_condition_on_resurvey()`](https://farach.github.io/onet2r/dev/reference/onet_condition_on_resurvey.md)
to obtain the at-risk set and its selection reasons.

## Examples

``` r
panel <- tibble::tibble(
  release_version = rep(c("22.1", "23.1"), each = 2),
  release_date = rep(as.Date(c("2017-10-01", "2018-11-01")), each = 2),
  soc_vintage = "2010",
  onet_soc_code = rep(c("15-1132.00", "29-1141.00"), 2),
  soc_code = rep(c("15-1132", "29-1141"), 2),
  task_id = rep(c("1001", "1002"), 2),
  task = rep(c("Write code.", "Assess patients."), 2),
  scale_id = "IM",
  data_value = c(4.1, 4.6, 4.1, 4.8),
  source_date = as.Date(c("2016-07-01", "2016-07-01", "2016-07-01", "2018-07-01")),
  domain_source = "Incumbent"
)
onet_resurvey_panel(panel)
#> # A tibble: 4 × 22
#>   onet_soc_code soc_code task_id title task         release_version release_date
#>   <chr>         <chr>    <chr>   <chr> <chr>        <chr>           <date>      
#> 1 15-1132.00    15-1132  1001    NA    Write code.  22.1            2017-10-01  
#> 2 15-1132.00    15-1132  1001    NA    Write code.  23.1            2018-11-01  
#> 3 29-1141.00    29-1141  1002    NA    Assess pati… 22.1            2017-10-01  
#> 4 29-1141.00    29-1141  1002    NA    Assess pati… 23.1            2018-11-01  
#> # ℹ 15 more variables: soc_vintage <fct>, scale_id <chr>, data_value <dbl>,
#> #   source_date <date>, domain_source <chr>, survey_source <chr>,
#> #   recommend_suppress <chr>, occ_survey_date <date>, prev_survey_date <date>,
#> #   prev_release_version <chr>, resurvey_event <lgl>, cycle_index <int>,
#> #   age_resolved <dbl>, seam_in <lgl>, seam_type <chr>
```
