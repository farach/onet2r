# Importing Published Exposure Scores

`onet2r` does not ship or endorse an exposure measure. When you want to
work with a published one, two adapters read the authors’
occupation-level files and turn them into the same task-grain
[`onet_measure()`](https://farach.github.io/onet2r/reference/onet_measure.md)
object that a hand-built score would produce:

- [`onet_import_eloundou()`](https://farach.github.io/onet2r/reference/onet_import_eloundou.md)
  reads the GPT exposure table from Eloundou et al.
  ([2023](#ref-eloundou2023gpts)), keyed on 8-digit O\*NET-SOC codes.
- [`onet_import_felten_aioe()`](https://farach.github.io/onet2r/reference/onet_import_felten_aioe.md)
  reads the AI Occupational Exposure (AIOE) scores from Felten, Raj, and
  Seamans ([2021](#ref-felten2021aioe)), keyed on 6-digit SOC codes.

Both are thin adapters. They select a score column, standardize the
occupation code, join it to the tasks of a panel you supply, and record
where the file came from. They do not rescale, average, or otherwise
transform the published values, and the package never bundles the files.

**Stylized values.** To keep this article offline, it writes small local
extracts with the published column names and made-up values. None of the
numbers below are the published exposure scores.

## Choose the Task Panel

The panel sets the grain of the result: every distinct occupation and
task pair in it receives a score. Here it comes from the relevance
ratings of a pinned release in the bundled fixtures.

``` r
tasks <- onet_archive_read(
  "30.3",
  "Task Statements",
  path = archive_303,
  release_date = "2026-05-01"
)
ratings <- onet_archive_read(
  "30.3",
  "Task Ratings",
  path = archive_303,
  release_date = "2026-05-01"
)

task_panel <- distinct(ratings, onet_soc_code, task_id)

task_panel |>
  knitr::kable(align = "l")
```

| onet_soc_code | task_id |
|:--------------|:--------|
| 15-1252.00    | 1001    |
| 15-1252.00    | 1002    |
| 29-1141.00    | 2001    |

## Import an Occupation-Level Score

``` r
eloundou_extract <- tempfile(fileext = ".csv")
utils::write.csv(
  data.frame(
    `O*NET-SOC Code` = c("15-1252.00", "29-1141.00", "11-1011.00"),
    human_rating_beta = c(0.60, 0.20, 0.40),
    dv_rating_beta = c(0.70, 0.30, 0.50),
    check.names = FALSE
  ),
  eloundou_extract,
  row.names = FALSE
)

exposure <- onet_import_eloundou(
  task_panel,
  path = eloundou_extract,
  score = "human_rating_beta",
  as_of = "stylized extract for this article",
  release_version = "30.3"
)

exposure$data |>
  select(onet_soc_code, task_id, measure_key, measure_score) |>
  knitr::kable(digits = 3, align = "l")
```

| onet_soc_code | task_id | measure_key | measure_score |
|:--------------|:--------|:------------|:--------------|
| 15-1252.00    | 1001    | 1001        | 0.6           |
| 15-1252.00    | 1002    | 1002        | 0.6           |
| 29-1141.00    | 2001    | 2001        | 0.2           |

Every task inherits its occupation’s published score. Both Software
Developers tasks get the same value, because the source is
occupation-level. This is the structurally blind construction that
task-aware measures are usually contrasted against, so treat it as a
benchmark rather than a task-level estimate. The published file offers
several definitions, such as `human_rating_beta` and the model-labeled
`dv_rating_beta`; `score` selects one, and choosing it is your decision.

## Check the Source Receipt

Each imported measure records a receipt with the file’s SHA-256 digest,
size, retrieval time, and any `as_of` label.

``` r
receipt <- exposure$metadata$source_receipt

tibble::tibble(
  field = c("provenance_status", "as_of", "file_size", "actual_sha256"),
  value = c(
    receipt$provenance_status,
    receipt$as_of,
    format(receipt$file_size),
    receipt$actual_sha256
  )
) |>
  knitr::kable(align = "l")
```

| field             | value                                                            |
|:------------------|:-----------------------------------------------------------------|
| provenance_status | recorded                                                         |
| as_of             | stylized extract for this article                                |
| file_size         | 117                                                              |
| actual_sha256     | 0b1da5799e5db23a95d892f494dfc59b7675cd43fef1f970e637d3cfcb863b91 |

Record the digest the first time you read a file, and pass it back as
`expected_sha256` in later runs. A changed file is rejected before it is
parsed.

``` r
recorded_digest <- receipt$actual_sha256

verified <- onet_import_eloundou(
  task_panel,
  path = eloundou_extract,
  expected_sha256 = recorded_digest,
  release_version = "30.3"
)
rejected <- tryCatch(
  onet_import_eloundou(
    task_panel,
    path = eloundou_extract,
    expected_sha256 = strrep("0", 64)
  ),
  error = conditionMessage
)

tibble::tibble(
  check = c("Recorded digest accepted", "Wrong digest rejected"),
  result = c(
    inherits(verified, "onet_measure"),
    grepl("digest mismatch", rejected, fixed = TRUE)
  )
) |>
  knitr::kable(align = "l")
```

| check                    | result |
|:-------------------------|:-------|
| Recorded digest accepted | TRUE   |
| Wrong digest rejected    | TRUE   |

## Roll Up and Weight

The imported object is an ordinary task-grain measure, so the usual
plumbing applies.

``` r
occupation_exposure <- onet_task_to_occupation(
  exposure,
  task_ratings = ratings,
  task_metadata = tasks
)

occupation_exposure |>
  select(onet_soc_code, n_tasks, total_task_weight, measure_score) |>
  knitr::kable(digits = 3, align = "l")
```

| onet_soc_code | n_tasks | total_task_weight | measure_score |
|:--------------|:--------|:------------------|:--------------|
| 15-1252.00    | 1       | 95                | 0.6           |
| 29-1141.00    | 1       | 98                | 0.2           |

``` r

weights <- onet_weight_panel_oews(
  onet_oews_national(2024, path = oews_path),
  year = 2024
)
#> Dropped 2 OEWS aggregate rows; keeping "detailed" occupations.

national <- onet_measure_aggregate(occupation_exposure, weights)

national |>
  select(-coverage, -provenance) |>
  knitr::kable(digits = 3, align = "l")
```

| measure_id            | aggregate | total_employment | covered_employment | employment_coverage_share | n_occupations | n_reference_soc |
|:----------------------|:----------|:-----------------|:-------------------|:--------------------------|:--------------|:----------------|
| eloundou_gpt_exposure | 0.347     | 5234530          | 5023300            | 0.96                      | 2             | 2               |

Because every task of an occupation carries the same score, the rollup
returns the published occupation score for each occupation with rated
core tasks. Chief Executives have a score in the extract but no tasks in
the fixture panel, so their employment stays in the denominator as
unmatched, and coverage is below 1.

## Compare Definitions and Sources

``` r
model_exposure <- onet_import_eloundou(
  task_panel,
  path = eloundou_extract,
  score = "dv_rating_beta",
  measure_id = "eloundou_dv_beta",
  release_version = "30.3"
)

felten_extract <- tempfile(fileext = ".csv")
utils::write.csv(
  data.frame(
    `SOC Code` = c("15-1252", "29-1141", "11-1011"),
    AIOE = c(1.10, -0.30, 0.80),
    check.names = FALSE
  ),
  felten_extract,
  row.names = FALSE
)
aioe <- onet_import_felten_aioe(
  task_panel,
  path = felten_extract,
  release_version = "30.3"
)

aggregate_measure <- function(measure) {
  measure |>
    onet_task_to_occupation(task_ratings = ratings, task_metadata = tasks) |>
    onet_measure_aggregate(weights)
}

list(
  `Human-rated beta` = exposure,
  `Model-rated beta` = model_exposure,
  AIOE = aioe
) |>
  purrr::map(aggregate_measure) |>
  purrr::list_rbind(names_to = "source") |>
  select(source, measure_id, aggregate, employment_coverage_share) |>
  knitr::kable(digits = 3, align = "l")
```

| source           | measure_id            | aggregate | employment_coverage_share |
|:-----------------|:----------------------|:----------|:--------------------------|
| Human-rated beta | eloundou_gpt_exposure | 0.347     | 0.96                      |
| Model-rated beta | eloundou_dv_beta      | 0.447     | 0.96                      |
| AIOE             | felten_aioe           | 0.215     | 0.96                      |

The AIOE adapter joins on the 6-digit SOC code, so every O\*NET detail
code under one SOC inherits the same score. The two sources are on
different scales: the beta scores are shares of exposed tasks, while
AIOE is a standardized index. Compare orderings across sources, not
levels.

## Occupations without a Score

Tasks whose occupation has no score in the file are dropped with a
warning rather than scored as zero.

``` r
partial_panel <- tibble::tibble(
  onet_soc_code = c("15-1252.00", "51-2092.00"),
  task_id = c("1001", "9001")
)

partial <- onet_import_eloundou(partial_panel, path = eloundou_extract)
#> Warning: Dropped 1 task with no matching exposure score.
#> ℹ 1 occupation had no score in the import file.

partial$data |>
  select(onet_soc_code, task_id, measure_score) |>
  knitr::kable(digits = 3, align = "l")
```

| onet_soc_code | task_id | measure_score |
|:--------------|:--------|:--------------|
| 15-1252.00    | 1001    | 0.6           |

## Reading the Published Files

With no `path`, each adapter downloads the authors’ file from a URL
pinned to a specific commit and caches it in the `reference` section of
the package cache. Pinning the digest and an `as_of` label makes a rerun
fail loudly if the source ever changes.

``` r
exposure <- onet_import_eloundou(
  task_panel,
  as_of = "GPTs-are-GPTs commit 0471612",
  expected_sha256 = "<digest recorded on first download>"
)

aioe <- onet_import_felten_aioe(task_panel)

# Remove cached reference downloads.
onet_cache_clear(what = "reference")
```

The Eloundou et al. table is distributed under the MIT License. The AIOE
workbook is provided for research use. Cite the source papers when you
use either score.

## References

Eloundou, Tyna, Sam Manning, Pamela Mishkin, and Daniel Rock. 2023.
“GPTs Are GPTs: An Early Look at the Labor Market Impact Potential of
Large Language Models.” arXiv:2303.10130.
<https://arxiv.org/abs/2303.10130>.

Felten, Edward, Manav Raj, and Robert Seamans. 2021. “Occupational,
Industry, and Geographic Exposure to Artificial Intelligence: A Novel
Dataset and Its Potential Uses.” *Strategic Management Journal* 42 (12):
2195–2217. <https://doi.org/10.1002/smj.3286>.
