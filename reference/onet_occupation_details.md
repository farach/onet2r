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

A list containing the available occupation details sections.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_occupation_details("15-1252.00")
}
```
