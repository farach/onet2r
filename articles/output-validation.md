# Validating onet2r Outputs

The package test suite checks implementation details with mocked
responses and small deterministic files. For real projects, it is also
useful to inspect output shape: Are expected columns present? Are rows
non-empty where they should be? Are list endpoints returning the fields
users need?

`onet2r` includes a practical validation script that maintainers can run
from the source tree with `Rscript inst/examples/validate-outputs.R`.
From an installed package, source the file returned by:

``` r
onet2r_inst_path("examples", "validate-outputs.R")
#> [1] "../inst/examples/validate-outputs.R"
```

## What the Script Checks

The validation script always checks deterministic local outputs:

- reading bundled OEWS sample data;
- reading a local O\*NET archive-format table;
- creating a reference-SOC OEWS weight panel;
- aggregating an occupation score with coverage and provenance;
- configuring cache and rate-limit settings.

When `ONET_API_KEY` is available, it also performs live smoke checks
across the main exported O\*NET functions. Each result is inspected for
expected type, minimum row count, and important columns.

## Example: Local Validation without an API Key

The local validation below uses package functions rather than copied
output.

``` r
oews <- onet_oews_national(2024, path = sample_oews)
weights <- onet_weight_panel_oews(oews, year = 2024)
#> Dropped 2 OEWS aggregate rows; keeping "detailed" occupations.

abilities <- onet_archive_read(
  "30.3",
  "Abilities",
  path = archive_303,
  release_date = "2026-05-01"
)

oral_scores <- abilities |>
  filter(element_id == "1.A.1.a.1") |>
  transmute(onet_soc_code, measure_score = data_value)

aggregate <- onet_measure_aggregate(
  oral_scores,
  weights,
  measure_id = "oral_comprehension_fixture"
)

tibble::tibble(
  check = c(
    "OEWS output is a tibble",
    "archive output has source dates",
    "weight panel has reference SOC codes",
    "aggregate has provenance",
    "aggregate has coverage"
  ),
  passed = c(
    inherits(oews, "tbl_df"),
    all(c("source_date", "data_value") %in% names(abilities)),
    all(c("reference_soc_code", "employment") %in% names(weights)),
    nrow(onet_provenance(aggregate)) == 1,
    nrow(onet_coverage(aggregate)) == 1
  )
) |>
  onet_kable()
```

| check                                | passed |
|:-------------------------------------|:-------|
| OEWS output is a tibble              | TRUE   |
| archive output has source dates      | TRUE   |
| weight panel has reference SOC codes | TRUE   |
| aggregate has provenance             | TRUE   |
| aggregate has coverage               | TRUE   |

``` r

weights |>
  head(5) |>
  onet_kable()
```

| reference_soc_code | year | employment | weight_share | source | source_taxonomy | reference_taxonomy |
|:-------------------|:-----|:-----------|:-------------|:-------|:----------------|:-------------------|
| 11-1011            | 2024 | 211230     | 0.040        | OEWS   | 2018 SOC        | 2018 SOC           |
| 15-1252            | 2024 | 1847900    | 0.353        | OEWS   | 2018 SOC        | 2018 SOC           |
| 29-1141            | 2024 | 3175400    | 0.607        | OEWS   | 2018 SOC        | 2018 SOC           |

``` r

aggregate |>
  select(-coverage, -provenance) |>
  onet_kable()
```

| measure_id                 | aggregate | total_employment | covered_employment | employment_coverage_share | n_occupations | n_reference_soc |
|:---------------------------|:----------|:-----------------|:-------------------|:--------------------------|:--------------|:----------------|
| oral_comprehension_fixture | 4.574     | 5234530          | 5234530            | 1                         | 4             | 4               |

## Example: Live Validation Status

Live validation only runs when a user supplies an API key. The site
still executes this chunk and reports whether live checks are available
in the current environment.

``` r
tibble::tibble(
  check = "ONET_API_KEY available for live API validation",
  passed = nzchar(Sys.getenv("ONET_API_KEY"))
) |>
  onet_kable()
```

| check                                          | passed |
|:-----------------------------------------------|:-------|
| ONET_API_KEY available for live API validation | FALSE  |

This does not replace unit tests. It gives maintainers and users a
repeatable way to confirm that live API outputs still match the shapes
used in examples, vignettes, and downstream analyses.
