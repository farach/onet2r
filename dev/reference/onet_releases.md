# List O\*NET Archive Releases

Parses the O\*NET database releases archive and returns downloadable
text archives. The release archive states release months, not days, so
`release_date` uses the first day of each stated month.

## Usage

``` r
onet_releases(refresh = FALSE)
```

## Arguments

- refresh:

  Set `TRUE` to re-scrape the releases page instead of using the
  per-session cache.

## Value

A tibble with release metadata and text archive URLs.

## Details

Text archives (`format = "text"`) exist for releases 20.1 (October 2015)
onward. Earlier releases (5.0-20.0) are published as legacy ZIPs
(`format = "legacy_zip"`); onet2r can download them, but their internal
file layouts vary and parsing is verified only for 20.1+. The O\*NET
production archive begins at 5.0 (April 2003), the first release of the
Data Collection Program - no survey-based data exists before that.

## Examples

``` r
if (interactive()) {
  onet_releases()
}
```
