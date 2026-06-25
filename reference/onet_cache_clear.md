# Clear Cached O\*NET API Responses

Deletes cached O\*NET API responses created by
[`onet_cache_use()`](https://farach.github.io/onet2r/reference/onet_cache_use.md).

## Usage

``` r
onet_cache_clear(
  cache_dir = getOption("onet2r.cache_dir", tools::R_user_dir("onet2r", "cache"))
)
```

## Arguments

- cache_dir:

  Directory containing cached API responses.

## Value

Invisibly returns the cache directory path.

## Examples

``` r
tmp <- tempfile()
onet_cache_use(cache_dir = tmp)
onet_cache_clear(cache_dir = tmp)
```
