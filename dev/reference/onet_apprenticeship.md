# Get O\*NET Apprenticeship Opportunities (details)

Get O\*NET Apprenticeship Opportunities (details)

## Usage

``` r
onet_apprenticeship(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code (e.g., "15-1252.00").

- start:

  Integer specifying the first result to return (default 1).

- end:

  Integer specifying the last result to return (default 20).

## Value

A tibble with one row per apprenticeship example title.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_apprenticeship("15-1252.00", end = 5)
}
```
