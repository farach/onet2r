# Get O\*NET Technology Skills (details)

Flattens the `category -> example` and optionally `example_more`
structure.

## Usage

``` r
onet_technology_skills(code, start = 1, end = 20, include_more = FALSE)
```

## Arguments

- code:

  An O\*NET-SOC occupation code (e.g., "15-1252.00").

- start:

  Integer specifying the first category to return (default 1).

- end:

  Integer specifying the last category to return (default 20).

- include_more:

  Logical; include `example_more` items (default FALSE).

## Value

A tibble with one row per technology example, including category
metadata.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_technology_skills("15-1252.00", end = 3)
}
```
