# Import Eloundou et al. (2023) GPT-Exposure Scores as a Task-Grain Measure

Reads the occupation-level GPT-exposure table released with Eloundou,
Manning, Mishkin, and Rock (2023), "GPTs are GPTs: An Early Look at the
Labor Market Impact Potential of Large Language Models", and broadcasts
it onto the tasks of a Task Ratings style `panel`, returning a
task-grain
[`onet_measure()`](https://farach.github.io/onet2r/reference/onet_measure.md)
keyed on `(occupation, task)`. Every task inherits its occupation's
published exposure. This is a thin adapter: it selects the score column,
standardizes the O\*NET-SOC code, joins to the panel, and records
provenance. It does not rescale, average, or otherwise transform the
published exposure values.

## Usage

``` r
onet_import_eloundou(
  panel,
  path = NULL,
  url = onet_eloundou_url,
  score = "human_rating_beta",
  key = NULL,
  sheet = NULL,
  occupation_code = "onet_soc_code",
  task_id = "task_id",
  measure_id = "eloundou_gpt_exposure",
  measure_name = "Eloundou et al. (2023) GPT exposure",
  force = FALSE,
  ...
)
```

## Arguments

- panel:

  A Task Ratings style panel with an occupation column
  (`occupation_code`, default `"onet_soc_code"`) and a task column
  (`task_id`, default `"task_id"`). Its distinct occupation-task pairs
  set the grain of the returned measure.

- path:

  Optional path to a local copy of the exposure file. A comma or tab
  separated file or an Excel workbook is accepted. When supplied, no
  download is attempted.

- url:

  Download URL used when `path` is `NULL`. Defaults to the pinned
  occupation-level `occ_level.csv` in the authors' public repository.

- score:

  Name of the exposure column to use as the measure score. The published
  `occ_level.csv` carries `human_rating_alpha`, `human_rating_beta`,
  `human_rating_gamma` (human-labeled exposure at the alpha, beta, and
  gamma definitions) and the `dv_rating_*` model-labeled counterparts.
  Defaults to `"human_rating_beta"`.

- key:

  Optional name of the O\*NET-SOC code column in the exposure file. When
  `NULL`, common column names such as `"O*NET-SOC Code"` are detected
  automatically.

- sheet:

  Optional worksheet name or index used when reading an Excel workbook.

- occupation_code, task_id:

  Names of the occupation and task columns in `panel`.

- measure_id, measure_name:

  Identifiers recorded on the returned measure.

- force:

  Logical; re-download even when a cached copy exists.

- ...:

  Additional arguments passed to
  [`onet_measure()`](https://farach.github.io/onet2r/reference/onet_measure.md),
  such as `universe` or `weight_panel`.

## Value

A task-grain `onet_measure` object (`key_type = "task"`) keyed on
`task_id` and scored on the selected exposure column, with the
occupation code retained. It is ready for
[`onet_task_to_occupation()`](https://farach.github.io/onet2r/reference/onet_task_to_occupation.md).

## Details

The published scores are occupation-level; broadcasting them to every
task of an occupation is the structurally blind aggregate construction
the source paper contrasts against task-aware measures. Tasks whose
occupation has no published score are dropped with a warning. Pass a
single-release panel so each task id is unique.

The exposure data are distributed by OpenAI under the MIT License.
onet2r never bundles or ships the file; you must supply `path` or
download it from `url`. Downloads are cached under
`tools::R_user_dir("onet2r", "cache")` in the `reference` section and
can be cleared with `onet_cache_clear(what = "reference")`. Cite the
source paper when you use the scores.

The three exposure definitions follow the paper: alpha counts tasks
exposed by direct model access, beta adds tasks reachable with
complementary software built on the model, and gamma is the broadest
definition. Choosing among them is a substantive decision left to the
caller; the adapter does not endorse a definition.

## Examples

``` r
# Offline: broadcast a small local extract onto a tiny panel.
extract <- tempfile(fileext = ".csv")
utils::write.csv(
  data.frame(
    `O*NET-SOC Code` = c("15-1252.00", "29-1141.00"),
    Title = c("Software Developers", "Registered Nurses"),
    human_rating_beta = c(0.63, 0.14),
    check.names = FALSE
  ),
  extract,
  row.names = FALSE
)
panel <- tibble::tibble(
  onet_soc_code = rep(c("15-1252.00", "29-1141.00"), each = 2),
  task_id = c("1", "2", "3", "4")
)
measure <- onet_import_eloundou(panel, path = extract)
measure$data
#> # A tibble: 4 × 5
#>   onet_soc_code task_id human_rating_beta measure_key measure_score
#>   <chr>         <chr>   <chr>             <chr>               <dbl>
#> 1 15-1252.00    1       0.63              1                    0.63
#> 2 15-1252.00    2       0.63              2                    0.63
#> 3 29-1141.00    3       0.14              3                    0.14
#> 4 29-1141.00    4       0.14              4                    0.14

# Online: download the published occupation-level table.
if (interactive()) {
  exposure <- onet_import_eloundou(panel)
  head(exposure$data)
}
```
