# Clear Cached O\*NET API Responses

Deletes cached O\*NET API responses and downloaded source archives.

## Usage

``` r
onet_cache_clear(
  cache_dir = getOption("onet2r.cache_dir", tools::R_user_dir("onet2r", "cache")),
  what = c("api", "archives", "crosswalks", "oews", "reference", "all")
)
```

## Arguments

- cache_dir:

  Directory containing cached API responses.

- what:

  Cache section to clear. Use `"api"` for Web Services responses,
  `"archives"` for O\*NET database ZIPs, `"crosswalks"` for O\*NET
  bridge CSVs, `"oews"` for BLS OEWS ZIPs, `"reference"` for reference
  workbooks, or `"all"` for every section.

## Value

Invisibly returns the cache directory path.

## Examples

``` r
tmp <- tempfile()
onet_cache_use(cache_dir = tmp)
onet_cache_clear(cache_dir = tmp)
```
