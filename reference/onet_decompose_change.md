# Decompose Aggregate Change into Within and Between Components

Decomposes aggregate change between two periods into within-occupation,
between-occupation, interaction, and unclassifiable components. The
within term is restricted to rows marked comparable.

## Usage

``` r
onet_decompose_change(
  from_scores,
  to_scores,
  from_weights,
  to_weights,
  occupation_code = "reference_soc_code",
  score = "measure_score",
  weight = "employment",
  comparable = "safely_comparable"
)
```

## Arguments

- from_scores:

  Initial-period occupation scores.

- to_scores:

  Final-period occupation scores.

- from_weights:

  Initial-period weights.

- to_weights:

  Final-period weights.

- occupation_code:

  Join column for scores and weights.

- score:

  Score column in score tables.

- weight:

  Weight column in weight tables.

- comparable:

  Optional logical column in `to_scores` or `from_scores` marking rows
  safe for within-change attribution.

## Value

A tibble with decomposition components and coverage list-column metadata
readable with
[`onet_coverage()`](https://farach.github.io/onet2r/reference/onet_coverage.md).

## Details

When the caller has reconciled a panel with stale carry-forward values,
this decomposition treats those carried values as observed values. The
resulting contribution is arithmetic, not proof that the underlying
source measure was refreshed in that release.

## Examples

``` r
from_scores <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  measure_score = c(0.6, 0.3),
  safely_comparable = c(TRUE, TRUE)
)
to_scores <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  measure_score = c(0.7, 0.2),
  safely_comparable = c(TRUE, FALSE)
)
from_weights <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  employment = c(100, 300)
)
to_weights <- tibble::tibble(
  reference_soc_code = c("15-1252", "29-1141"),
  employment = c(150, 250)
)
onet_decompose_change(from_scores, to_scores, from_weights, to_weights)
#> # A tibble: 5 × 3
#>   component        value coverage        
#>   <chr>            <dbl> <list>          
#> 1 within          0.025  <tibble [1 × 3]>
#> 2 between         0.0375 <tibble [1 × 3]>
#> 3 interaction     0.025  <tibble [1 × 3]>
#> 4 unclassifiable -0.075  <tibble [1 × 3]>
#> 5 total_change    0.0125 <tibble [1 × 3]>
```
