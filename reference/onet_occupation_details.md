# Get O\*NET Occupation Details Index

Retrieves the details index for a specific occupation. The response
includes links to available detailed sections from the occupation
overview.

## Usage

``` r
onet_occupation_details(code)
```

## Arguments

- code:

  An O\*NET-SOC occupation code (e.g., "15-1252.00").

## Value

A tibble with columns `title` and `href`, one row per available details
section. Zero rows when the occupation has no details sections.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_occupation_details("15-1252.00")
}
```
