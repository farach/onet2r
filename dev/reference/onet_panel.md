# Assemble an O\*NET Longitudinal Panel

Reads the same archive table across releases and row-binds the
normalized descriptor rows.

## Usage

``` r
onet_panel(
  table_or_element,
  versions,
  scale = NULL,
  archives = NULL,
  release_dates = NULL
)
```

## Arguments

- table_or_element:

  Archive table name in the first implementation.

- versions:

  Character vector of O\*NET database versions.

- scale:

  Optional scale id filter.

- archives:

  Optional local archive ZIP files or extracted archive directories. Use
  a named character vector keyed by version, or an unnamed vector the
  same length and order as `versions`.

- release_dates:

  Optional release dates for local archives. Use a named vector keyed by
  version, or an unnamed vector the same length and order as `versions`.

## Value

A tibble in the longitudinal panel schema.

## Examples

``` r
if (interactive()) {
  panel <- onet_panel("Abilities", versions = c("30.2", "30.3"), scale = "IM")
  head(panel)
}
```
