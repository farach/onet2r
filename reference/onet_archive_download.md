# Download an O\*NET Archive

Downloads and caches one O\*NET text archive. Existing non-empty cached
files are reused.

## Usage

``` r
onet_archive_download(version, dir = onet_cache_dir())
```

## Arguments

- version:

  O\*NET database version, for example `"30.3"`.

- dir:

  Cache directory.

## Value

The path to the cached ZIP file.

## Examples

``` r
if (interactive()) {
  path <- onet_archive_download("30.3")
  basename(path)
}
```
