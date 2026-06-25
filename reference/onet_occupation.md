# Get O\*NET Occupation Overview

Retrieves overview information for a specific occupation.

## Usage

``` r
onet_occupation(code)
```

## Arguments

- code:

  An O\*NET-SOC occupation code (e.g., "15-1252.00").

## Value

A list containing the occupation overview data.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_occupation("15-1252.00")
}
```
