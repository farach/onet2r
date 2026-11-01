# Reading OpenAI Signals Work-Activity Shares

[OpenAI
Signals](https://openai.com/index/chatgpt-expanding-world-of-work/)
classifies consumer ChatGPT work messages into O\*NET Intermediate Work
Activities (IWAs). These data describe activities people bring to
ChatGPT. They do not measure the occupation of each user, successful
task completion, or how much AI changes a job.

[`onet_openai_iwa_read()`](https://farach.github.io/onet2r/reference/onet_openai_iwa_read.md)
reads the country-quarter IWA CSV you already have. It does not download
data, choose an exposure score, or allocate usage to occupations. This
development feature is intended for a future CRAN update.

## An offline example with declared expectations

**Every share below is invented.** The fixture is a format
demonstration, not a finding about U.S. or British ChatGPT use.

``` r
path <- system.file("extdata", "openai-iwa-synthetic.csv", package = "onet2r")
usage <- onet_openai_iwa_read(path, as_of = "invented teaching example")
stopifnot(
  nrow(usage) == 9L,
  n_distinct(usage$country_code) == 2L,
  n_distinct(usage$quarter) == 2L,
  sum(usage$is_other_iwa) == 3L,
  sum(usage$share_of_messages == 0) == 1L,
  all(is.na(usage$iwa_id[usage$is_other_iwa]))
)
knitr::kable(usage |>
  select(quarter, country_code, iwa_category, share_of_messages))
```

| quarter    | country_code | iwa_category  | share_of_messages |
|:-----------|:-------------|:--------------|------------------:|
| 2025-01-01 | US           | 4.A.1.a.1.I01 |           0.50000 |
| 2025-01-01 | US           | 4.A.1.a.1.I02 |           0.30000 |
| 2025-01-01 | US           | Other IWA     |           0.20000 |
| 2025-04-01 | US           | 4.A.1.a.1.I01 |           0.40000 |
| 2025-04-01 | US           | 4.A.1.a.1.I02 |           0.40000 |
| 2025-04-01 | US           | Other IWA     |           0.19999 |
| 2025-01-01 | GB           | 4.A.1.a.1.I01 |           0.60000 |
| 2025-01-01 | GB           | 4.A.1.a.1.I02 |           0.00000 |
| 2025-01-01 | GB           | Other IWA     |           0.40000 |

The expected output has **nine rows**, **two countries**, **two
quarters**, **three `Other IWA` rows**, and **one reported zero**.
Britain has no second quarter in this invented file. That missing group
is not evidence of zero use.

## Keep the denominator and the published precision

The source defines each share as the fraction of classified work-related
messages in a particular country and quarter assigned to an IWA
category. It is not a share of all ChatGPT messages or a share of
workers.

``` r
totals <- usage |>
  summarise(
    reported_categories = n(),
    published_share_sum = sum(share_of_messages),
    .by = c(country_code, quarter)
  )
stopifnot(
  nrow(totals) == 3L,
  all(totals$reported_categories == 3L),
  isTRUE(all.equal(
    totals$published_share_sum[
      totals$country_code == "US" & totals$quarter == "2025-04-01"
    ], 0.99999
  ))
)
knitr::kable(totals, digits = 5)
```

| country_code | quarter    | reported_categories | published_share_sum |
|:-------------|:-----------|--------------------:|--------------------:|
| US           | 2025-01-01 |                   3 |             1.00000 |
| US           | 2025-04-01 |                   3 |             0.99999 |
| GB           | 2025-01-01 |                   3 |             1.00000 |

The invented second-quarter U.S. sum is **0.99999**, not exactly one.
The reader preserves it. The actual release normalizes after adding
differential-privacy noise, then rounds IWA shares to five decimal
places. Neither small discrepancies nor reported zeros justify
reconstructing unobserved messages. Partial groups can also have lower
sums, so totals alone do not prove completeness.

## Separate named activities from the residual category

`iwa_category` preserves the source label. `iwa_id` contains a joinable
identifier only for a named activity, while `is_other_iwa` marks the
residual.

``` r
other <- usage |> filter(is_other_iwa)
stopifnot(nrow(other) == 3L, all(is.na(other$iwa_id)))
knitr::kable(other |> select(country_code, quarter, iwa_category, share_of_messages))
```

| country_code | quarter    | iwa_category | share_of_messages |
|:-------------|:-----------|:-------------|------------------:|
| US           | 2025-01-01 | Other IWA    |           0.20000 |
| US           | 2025-04-01 | Other IWA    |           0.19999 |
| GB           | 2025-01-01 | Other IWA    |           0.40000 |

To look up names, use an explicitly selected O\*NET archive’s
`GWAs to IWAs to DWAs` reference table, read through
[`onet_archive_reference()`](https://farach.github.io/onet2r/reference/onet_archive_reference.md).
Its `iwa_element_id` is the matching activity key. First make a unique
IWA-name lookup and check unmatched source IDs. Do not join directly to
the full reference table without accounting for its multiple DWA rows
per IWA.

IWAs are shared across occupations. A named IWA match tells you which
jobs list an activity, not which workers sent the messages. The same
usage share must not be interpreted as an occupation-specific adoption
rate simply because it can be joined to several occupations.

## Use your downloaded release

Extract only
`public_release_csv/share_of_work_related_messages_by_onet_iwa_country_quarter.csv`
from the Signals ZIP, then pass its local path to
[`onet_openai_iwa_read()`](https://farach.github.io/onet2r/reference/onet_openai_iwa_read.md).
An optional `expected_sha256` pins the extracted CSV, not the ZIP.
`source_url` and `as_of` record metadata you supply. The reader’s
timestamp means local read time, not the original download time.

The README version 3.0 supplied with the November 2026 download
specifies CC BY 4.0. Cite OpenAI Signals, preserve attribution, and
identify the snapshot. The package does not bundle real usage rows.

The inspected release has 164 named IWAs plus `Other IWA` across 94
countries, with unequal country coverage over seven quarters. Consumer
messages from adult accounts are sampled. Enterprise and Codex use are
excluded, as are deleted accounts and users opting out of training.
Those coverage boundaries matter when comparing the data with employment
statistics.

## How this differs from TaskNet and exposure studies

TaskNet supplies an alternative inventory of occupational statements.
Signals supplies a distribution of classified consumer work messages.
Published exposure studies supply estimates under their own definitions.
These are different research objects. onet2r records their keys, values,
and provenance without treating one as a substitute for another.
