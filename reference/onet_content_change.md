# Measure O\*NET Content Change Between Releases

Computes task-set and rating content-change metrics for each occupation
between O\*NET releases. This is the single source of content metrics
for longitudinal work, so robustness measures cannot silently diverge.
It is seam-aware: comparisons that cross the v21.0 Task Relevance
retirement or the v25.1 SOC-2010 to SOC-2018 change are flagged so
taxonomy churn is not counted as content churn.

## Usage

``` r
onet_content_change(
  panel,
  scale = "IM",
  item = "task_id",
  min_importance = NULL,
  from = NULL,
  to = NULL,
  seams = NULL
)
```

## Arguments

- panel:

  A tibble from
  [`onet_panel()`](https://farach.github.io/onet2r/reference/onet_panel.md)
  (a Task Ratings panel) or the same schema. Must contain
  `onet_soc_code`, `release_version`, `release_date`, `soc_vintage`,
  `data_value`, and the `item` key column. `scale_id` is required when
  `scale` is not `NULL`.

- scale:

  Optional scale id used for the rating metrics and item membership, for
  example `"IM"` for Importance. Use `NULL` to keep every scale.

- item:

  Column that identifies the content item within an occupation. Defaults
  to `"task_id"`. Item membership drives the set metrics.

- min_importance:

  Optional numeric floor applied to `data_value` on the selected
  `scale`. Items below the floor, or with missing `data_value`, are
  dropped before set membership is computed. Use it to apply the
  Importance filter that removes the post-v21.0 Task Relevance artifact.

- from, to:

  Optional release versions selecting a single comparison. When both are
  supplied only that pair is returned; otherwise every adjacent release
  pair is compared in release-date order.

- seams:

  Optional data frame with `seam_type` and `seam_date` columns that
  overrides the default Task-Ratings seam table returned by
  `onet_known_seams()`. Use it for non-Task-Ratings inputs such as Work
  Activities, Work Context, or Abilities, where the v21.0 Task Relevance
  scale seam does not apply. Supply an empty table to disable date-based
  seams entirely. `NULL` keeps the default table, so Task Ratings output
  is unchanged. Cross-vintage SOC seams are always detected from
  `soc_vintage` regardless of this table.

## Value

A tibble with one row per occupation and release pair: `n_from`, `n_to`,
`n_added`, `n_dropped`, `n_retained`; `jaccard` (set similarity, the
retained share of the item union); `churn_rate` (`1 - jaccard`);
`rating_delta_l2` (the Euclidean norm of `data_value` changes over
retained items); `cosine` (cosine similarity of the `data_value` vectors
over the item union, missing items filled with zero); and the seam flags
`seam`, `seam_type`, and `safely_comparable`.

## Details

Set metrics compare the `item` keys an occupation carries in each
release. The filtered input must contain at most one row per occupation,
item, release, and scale. Duplicate keys are rejected rather than
resolved by row order. When `scale = NULL` and `scale_id` is present,
item-scale combinations define membership so ratings from different
scales are not joined to each other. `rating_delta_l2` uses only
retained items, so it measures rating drift among tasks that persist;
`cosine` uses the union with zero fill, so it responds to both
membership turnover and rating change. Seam-crossing pairs still receive
metrics but are marked `safely_comparable = FALSE`; include them only in
a clearly labeled seam-inclusive row. Occupations are matched on
`onet_soc_code`, so cross-taxonomy pairs generally share few codes and
should be bridged with
[`onet_crosswalk_bridge()`](https://farach.github.io/onet2r/reference/onet_crosswalk_bridge.md)
before interpretation.

## Examples

``` r
panel <- tibble::tibble(
  release_version = c(rep("22.1", 3), rep("23.1", 3)),
  release_date = c(rep(as.Date("2017-10-01"), 3), rep(as.Date("2018-11-01"), 3)),
  soc_vintage = "2010",
  onet_soc_code = "15-1132.00",
  task_id = c("1", "2", "3", "1", "2", "4"),
  scale_id = "IM",
  data_value = c(4.0, 3.5, 2.0, 4.5, 3.5, 3.0)
)
onet_content_change(panel)
#> # A tibble: 1 × 21
#>   onet_soc_code soc_code title from_release to_release from_release_date
#>   <chr>         <chr>    <chr> <chr>        <chr>      <date>           
#> 1 15-1132.00    15-1132  NA    22.1         23.1       2017-10-01       
#> # ℹ 15 more variables: to_release_date <date>, soc_vintage_from <fct>,
#> #   soc_vintage_to <fct>, n_from <int>, n_to <int>, n_added <int>,
#> #   n_dropped <int>, n_retained <int>, jaccard <dbl>, churn_rate <dbl>,
#> #   rating_delta_l2 <dbl>, cosine <dbl>, seam <lgl>, seam_type <chr>,
#> #   safely_comparable <lgl>
```
