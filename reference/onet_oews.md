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

  Integer estimate year. Defaults to the most recent May estimates
  assumed available (previous calendar year from April onward, two years
  back before April). If BLS has not yet published that release, the
  download fails - pass the prior year explicitly.

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
columns. Special OEWS markers are preserved as indicator columns when
present: `#` as top-coded, `*` as wage suppressed, `**` as employment
suppressed, and `~` as less than 0.5 percent.

## Details

### Wage-field semantics

OEWS publishes both hourly (`h_*`) and annual (`a_*`) wage fields. Some
occupations are annual-only (`annual = TRUE`; e.g., teachers) or
hourly-only (`hourly = TRUE`; e.g., actors) - their missing counterpart
fields are structural, not suppressed. Suppression and top-coding are
flagged separately (see the `*_topcoded` and suppression indicator
columns). `*_prse` columns are percent relative standard errors of the
corresponding estimates.

### BLS download blocking

BLS may reject automated ZIP downloads with HTTP 403 even when the same
URL downloads in a browser. `onet_oews()` handles that case in three
steps: first it checks the package cache, then it looks for the matching
ZIP in your Downloads folder, then in interactive sessions it opens the
official BLS URL in your browser and waits for the downloaded file to
appear. Set `options(onet2r.oews_download_dir = "path/to/downloads")` to
search a different manual-download folder. Set
`options(onet2r.oews_browser_fallback = FALSE)` to disable opening a
browser. Set `options(onet2r.oews_browser_wait = 120)` to control how
many seconds `onet_oews()` waits for a browser download. You can always
pass a ZIP path directly with `path`; this is the stable fallback if BLS
changes OEWS URLs or file names.

## Examples

``` r
if (FALSE) { # interactive()
oews <- onet_oews("national", 2024)
head(oews)
}
```
