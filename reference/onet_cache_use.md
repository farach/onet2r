# Configure O\*NET API Response Caching

Enables or disables local caching for O\*NET API responses. Caching is
off by default because API results may change over time, but it is
useful when developing analyses that repeatedly request the same
endpoints.

## Usage

``` r
onet_cache_use(
  enabled = TRUE,
  cache_dir = tools::R_user_dir("onet2r", "cache")
)
```

## Arguments

- enabled:

  Logical; enable or disable response caching.

- cache_dir:

  Directory where cached API responses should be stored.

## Value

Invisibly returns a list with the active cache settings.

## Details

Cached Web Services responses are reused until the caller disables
caching or clears them with
[`onet_cache_clear()`](https://farach.github.io/onet2r/reference/onet_cache_clear.md).
The package does not attach a time-to-live, so enable caching only when
stale O\*NET API responses are acceptable for the analysis.

## Examples

``` r
onet_cache_use(enabled = FALSE)
```
