# Download an O\*NET Archive

Downloads and caches one O\*NET text archive. Existing non-empty cached
files are reused.

## Usage

``` r
onet_archive_download(version, dir = onet_cache_dir(), force = FALSE)
```

## Arguments

- version:

  O\*NET database version, for example `"30.3"`.

- dir:

  Cache directory.

- force:

  Logical; if `TRUE`, re-download even when a cached archive exists.

## Value

The path to the cached ZIP file.

## Details

Downloaded archives are cached under
`tools::R_user_dir("onet2r", "cache")` in the `archives` section. Use
`onet_cache_clear(what = "archives")` or
`onet_cache_clear(what = "all")` to remove them.

## Examples

``` r
if (interactive()) {
  path <- onet_archive_download("30.3")
  basename(path)
}
```
