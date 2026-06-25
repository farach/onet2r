# Download OEWS Estimates

Downloads Occupational Employment and Wage Statistics (OEWS) estimates
from the U.S. Bureau of Labor Statistics and returns them as a tibble.
OEWS estimates include employment counts and wage percentiles by
Standard Occupational Classification (SOC) code.

## Usage

``` r
onet_oews(
  type = c("national", "state", "metro", "industry"),
  year = latest_oews_year(),
  path = NULL,
  cache_dir = tools::R_user_dir("onet2r", "cache"),
  force = FALSE,
  quiet = TRUE
)
```

## Arguments

- type:

  OEWS file type: `"national"`, `"state"`, `"metro"`, or `"industry"`.

- year:

  Integer estimate year. Defaults to the latest year expected to be
  available from the BLS public OEWS downloads.

- path:

  Optional path to a manually downloaded OEWS ZIP, CSV, TXT, DAT, or
  Excel file. If supplied, no download is attempted.

- cache_dir:

  Directory used to cache the downloaded BLS ZIP file.

- force:

  Logical; if `TRUE`, re-download even when a cached file exists.

- quiet:

  Logical; if `FALSE`, show download progress.

## Value

A tibble of OEWS estimates with `year`, `oews_type`, and snake_case OEWS
columns.

## Examples

``` r
if (FALSE) { # interactive()
oews <- onet_oews("national", 2024)
head(oews)
}
```
