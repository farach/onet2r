# Import the Felten, Raj, and Seamans AIOE Scores as a Task-Grain Measure

Reads the occupation-level AI Occupational Exposure (AIOE) scores from
Felten, Raj, and Seamans (2021), "Occupational, Industry, and Geographic
Exposure to Artificial Intelligence", and broadcasts them onto the tasks
of a Task Ratings style `panel`, returning a task-grain
[`onet_measure()`](https://farach.github.io/onet2r/dev/reference/onet_measure.md)
keyed on `(occupation, task)`. Every task inherits its occupation's
published AIOE score. This is a thin adapter: it selects the score
column, standardizes the SOC code, joins to the panel, and records
provenance without transforming the published values.

## Usage

``` r
onet_import_felten_aioe(
  panel,
  path = NULL,
  url = onet_felten_aioe_url,
  score = "AIOE",
  key = NULL,
  sheet = "Appendix A",
  occupation_code = "onet_soc_code",
  task_id = "task_id",
  measure_id = "felten_aioe",
  measure_name = "Felten, Raj, and Seamans (2021) AIOE",
  force = FALSE,
  ...,
  expected_sha256 = NULL,
  as_of = NULL
)
```

## Arguments

- panel:

  A Task Ratings style panel with an occupation column
  (`occupation_code`, default `"onet_soc_code"`) and a task column
  (`task_id`, default `"task_id"`). Its distinct occupation-task pairs
  set the grain of the returned measure.

- path:

  Optional path to a local copy of the AIOE workbook or a comma or tab
  separated export. When supplied, no download is attempted.

- url:

  Download URL used when `path` is `NULL`. Defaults to the pinned
  `AIOE_DataAppendix.xlsx` in the authors' public repository.

- score:

  Name of the exposure column to use as the measure score. Defaults to
  `"AIOE"`.

- key:

  Optional name of the SOC code column in the workbook. When `NULL`,
  common column names such as `"SOC Code"` are detected automatically.

- sheet:

  Worksheet holding the occupation scores. Defaults to `"Appendix A"`,
  the occupation sheet of the published workbook.

- occupation_code, task_id:

  Names of the occupation and task columns in `panel`.

- measure_id, measure_name:

  Identifiers recorded on the returned measure.

- force:

  Logical; re-download even when a cached copy exists.

- ...:

  Additional arguments passed to
  [`onet_measure()`](https://farach.github.io/onet2r/dev/reference/onet_measure.md),
  such as `universe` or `weight_panel`.

- expected_sha256:

  Optional expected SHA-256 digest for the local or downloaded source.
  It is verified before parsing and on cache reuse.

- as_of:

  Optional source date or label recorded in provenance. Cached downloads
  with different `as_of` metadata are not silently reused.

## Value

A task-grain `onet_measure` object (`key_type = "task"`) keyed on
`task_id` and scored on the selected AIOE column, with the occupation
code retained. It is ready for
[`onet_task_to_occupation()`](https://farach.github.io/onet2r/dev/reference/onet_task_to_occupation.md).

## Details

AIOE scores are indexed by 6-digit SOC code, not by 8-digit O\*NET-SOC
code. The adapter derives a 6-digit SOC code from the panel's occupation
code and joins on it, then broadcasts each occupation's score to its
tasks. Tasks whose occupation has no published score are dropped with a
warning. Pass a single-release panel so each task id is unique.

onet2r never bundles or ships the workbook; you must supply `path` or
download it from `url`. Downloads are cached under
`tools::R_user_dir("onet2r", "cache")` in the `reference` section and
can be cleared with `onet_cache_clear(what = "reference")`. The AIOE
workbook is provided for research use; cite Felten, Raj, and Seamans
(2021) when you use the scores. The returned measure includes
`metadata$source_receipt` with the source URL or local path, retrieval
time, digest, file size, source commit when inferable from a pinned
GitHub raw URL, and version or `as_of` metadata. A cached download
without a receipt cannot satisfy the requested URL provenance. Use
`force = TRUE` to replace legacy cached bytes.

## Examples

``` r
# Offline: broadcast a small local extract onto a tiny panel.
extract <- tempfile(fileext = ".csv")
utils::write.csv(
  data.frame(
    `SOC Code` = c("15-1252", "29-1141"),
    `Occupation Title` = c("Software Developers", "Registered Nurses"),
    AIOE = c(1.08, -0.32),
    check.names = FALSE
  ),
  extract,
  row.names = FALSE
)
panel <- tibble::tibble(
  onet_soc_code = rep(c("15-1252.00", "29-1141.00"), each = 2),
  task_id = c("1", "2", "3", "4")
)
measure <- onet_import_felten_aioe(panel, path = extract)
measure$data
#> # A tibble: 4 × 6
#>   onet_soc_code task_id soc_code AIOE  measure_key measure_score
#>   <chr>         <chr>   <chr>    <chr> <chr>               <dbl>
#> 1 15-1252.00    1       15-1252  1.08  1                    1.08
#> 2 15-1252.00    2       15-1252  1.08  2                    1.08
#> 3 29-1141.00    3       29-1141  -0.32 3                   -0.32
#> 4 29-1141.00    4       29-1141  -0.32 4                   -0.32

# Online: download the published AIOE workbook.
if (interactive()) {
  aioe <- onet_import_felten_aioe(panel)
  head(aioe$data)
}
```
