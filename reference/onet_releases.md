# List O\*NET Archive Releases

Parses the O\*NET database releases archive and returns downloadable
text archives. The release archive states release months, not days, so
`release_date` uses the first day of each stated month.

## Usage

``` r
onet_releases()
```

## Value

A tibble with release metadata and text archive URLs.

## Examples

``` r
if (interactive()) {
  onet_releases()
}
```
