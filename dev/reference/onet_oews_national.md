# Download National OEWS Estimates

Downloads national OEWS employment and wage estimates.

## Usage

``` r
onet_oews_national(
  year = latest_oews_year(),
  path = NULL,
  cache_dir = tools::R_user_dir("onet2r", "cache"),
  force = FALSE,
  quiet = TRUE
)
```

## Arguments

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

A tibble of national OEWS estimates with snake_case columns.

## Examples

``` r
if (FALSE) { # interactive()
onet_oews_national(2024)
}
```
