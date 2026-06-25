# Read an O\*NET Archive Table

Reads a descriptor-like table from a cached O\*NET text archive and
normalizes it to the longitudinal panel schema.

## Usage

``` r
onet_archive_read(version, table, path = NULL, release_date = NULL)
```

## Arguments

- version:

  O\*NET database version, for example `"30.3"`.

- table:

  Archive table name, for example `"Abilities"` or `"Work Activities"`.

- path:

  Optional path to a local text archive ZIP file or extracted archive
  directory. If supplied, no download is attempted.

- release_date:

  Optional release date for a local archive. Downloaded archives use the
  release date from
  [`onet_releases()`](https://farach.github.io/onet2r/reference/onet_releases.md).

## Value

A tibble in the longitudinal panel schema.

## Examples

``` r
if (interactive()) {
  abilities <- onet_archive_read("30.3", "Abilities")
  head(abilities)
}
```
