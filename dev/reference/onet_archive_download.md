# Download an O\*NET Archive

Downloads and caches one O\*NET text archive. Existing non-empty cached
files are reused only when their recorded provenance and digest still
match.

## Usage

``` r
onet_archive_download(
  version,
  dir = onet_cache_dir(),
  force = FALSE,
  expected_sha256 = NULL,
  as_of = NULL
)
```

## Arguments

- version:

  O\*NET database version, for example `"30.3"`.

- dir:

  Cache directory.

- force:

  Logical; if `TRUE`, re-download even when a cached archive exists.

- expected_sha256:

  Optional expected SHA-256 digest. The digest is checked before the
  archive is parsed and whenever a cached file is reused.

- as_of:

  Optional source date or label to record in the receipt. A cached
  archive with different `as_of` metadata is not reused.

## Value

The path to the cached ZIP file.

## Details

Downloaded archives are cached under
`tools::R_user_dir("onet2r", "cache")` in the `archives` section. Use
`onet_cache_clear(what = "archives")` or
`onet_cache_clear(what = "all")` to remove them. An RDS receipt beside
each archive records its URL, retrieval time, SHA-256 digest, size,
version, and optional `as_of` metadata. The archive URLs are not assumed
to be immutable; supply `expected_sha256` when an independently verified
digest is available. A cached archive without a receipt cannot satisfy
the requested URL or version provenance. Use `force = TRUE` to replace
legacy cached bytes. The returned path names the shared cache entry,
which a later `force = TRUE` call may replace. Use
[`onet_archive_read()`](https://farach.github.io/onet2r/dev/reference/onet_archive_read.md)
to parse a verified private snapshot rather than reopening the shared
path.

## Examples

``` r
if (interactive()) {
  path <- onet_archive_download("30.3")
  basename(path)
}
```
