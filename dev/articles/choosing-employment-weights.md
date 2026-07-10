# Choosing Employment Weights: OEWS versus PUMS

Weights answer a population question. OEWS is usually the best choice
when the target is official occupation employment and wage context. PUMS
is useful when the target is a custom sample or demographic cell.

## Start with the Same O\*NET Score

``` r
abilities <- onet_archive_read(
  "30.3",
  "Abilities",
  path = archive_303,
  release_date = "2026-05-01"
)

score <- abilities |>
  filter(element_id == "1.A.1.a.1") |>
  transmute(onet_soc_code, measure_score = data_value)

score |>
  knitr::kable(digits = 3, align = "l")
```

| onet_soc_code | measure_score |
|:--------------|:--------------|
| 15-1252.00    | 4.35          |
| 29-1141.00    | 4.71          |
| 11-1011.00    | 4.50          |
| 41-1011.00    | 4.15          |

## OEWS Weights

``` r
oews_weights <- onet_weight_panel_oews(
  onet_oews_national(2023, path = oews_path),
  year = 2023
)
#> Dropped 2 OEWS aggregate rows; keeping "detailed" occupations.

oews_result <- onet_measure_aggregate(
  score,
  oews_weights,
  measure_id = "oral_comprehension_fixture"
)

oews_weights |>
  knitr::kable(digits = 3, align = "l")
```

| reference_soc_code | year | employment | weight_share | source | source_taxonomy | reference_taxonomy |
|:-------------------|:-----|:-----------|:-------------|:-------|:----------------|:-------------------|
| 11-1011            | 2023 | 211230     | 0.040        | OEWS   | 2018 SOC        | 2018 SOC           |
| 15-1252            | 2023 | 1847900    | 0.353        | OEWS   | 2018 SOC        | 2018 SOC           |
| 29-1141            | 2023 | 3175400    | 0.607        | OEWS   | 2018 SOC        | 2018 SOC           |

``` r
oews_result |>
  select(-coverage, -provenance) |>
  knitr::kable(digits = 3, align = "l")
```

| measure_id                 | aggregate | total_employment | covered_employment | employment_coverage_share | n_occupations | n_reference_soc |
|:---------------------------|:----------|:-----------------|:-------------------|:--------------------------|:--------------|:----------------|
| oral_comprehension_fixture | 4.574     | 5234530          | 5234530            | 1                         | 4             | 4               |

## PUMS Weights

``` r
pums <- tibble::tibble(
  SOCP = c("151252", "151252", "291141", "291141", "111011", "111011"),
  PWGTP = c(120, 80, 90, 110, 10, 30),
  ESR = c("1", "1", "1", "1", "2", "2"),
  sex = c("F", "M", "F", "M", "F", "M")
)

pums_weights <- onet_weight_panel_pums(
  pums,
  year = 2022,
  group = "sex"
)

pums_f <- onet_measure_aggregate(
  score,
  pums_weights,
  measure_id = "oral_comprehension_fixture",
  cell = list(sex = "F")
)
pums_m <- onet_measure_aggregate(
  score,
  pums_weights,
  measure_id = "oral_comprehension_fixture",
  cell = list(sex = "M")
)

pums_weights |>
  knitr::kable(digits = 3, align = "l")
```

| reference_soc_code | sex | year | employment | weight_share | source | source_taxonomy | reference_taxonomy |
|:-------------------|:----|:-----|:-----------|:-------------|:-------|:----------------|:-------------------|
| 11-1011            | F   | 2022 | 10         | 0.045        | PUMS   | 2018 SOC        | 2018 SOC           |
| 11-1011            | M   | 2022 | 30         | 0.136        | PUMS   | 2018 SOC        | 2018 SOC           |
| 15-1252            | F   | 2022 | 120        | 0.545        | PUMS   | 2018 SOC        | 2018 SOC           |
| 15-1252            | M   | 2022 | 80         | 0.364        | PUMS   | 2018 SOC        | 2018 SOC           |
| 29-1141            | F   | 2022 | 90         | 0.409        | PUMS   | 2018 SOC        | 2018 SOC           |
| 29-1141            | M   | 2022 | 110        | 0.500        | PUMS   | 2018 SOC        | 2018 SOC           |

Raw ACS PUMS needs a universe filter before this step. Employment
weights usually start with employed civilians, `ESR %in% c(1, 2)`, and
age 16 or older. Use `SOCP`; `OCCP` is a Census occupation recode, not a
six-digit SOC code.

``` r
pums <- tidycensus::get_pums(
  variables = c("SOCP", "PWGTP", "ESR", "AGEP"),
  year = 2023,
  survey = "acs1"
) |>
  filter(ESR %in% c("1", "2"), AGEP >= 16)
```

## Compare the Answers

``` r
comparison <- tibble::tibble(
  population = c("OEWS national", "PUMS-style F", "PUMS-style M"),
  aggregate = c(oews_result$aggregate, pums_f$aggregate, pums_m$aggregate),
  coverage = c(
    oews_result$employment_coverage_share,
    pums_f$employment_coverage_share,
    pums_m$employment_coverage_share
  )
)

comparison |>
  knitr::kable(digits = 3, align = "l")
```

| population    | aggregate | coverage |
|:--------------|:----------|:---------|
| OEWS national | 4.574     | 1        |
| PUMS-style F  | 4.504     | 1        |
| PUMS-style M  | 4.550     | 1        |

If your claim is about the national labor market, OEWS is the natural
default. If your claim is about a subgroup, geography, or survey-defined
population, build a PUMS weight panel and state the cell explicitly.
