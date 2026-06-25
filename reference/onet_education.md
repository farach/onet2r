# Get O\*NET Education (details)

Get O\*NET Education (details)

## Usage

``` r
onet_education(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code (e.g., "15-1252.00").

- start:

  Integer specifying the first result to return (default 1).

- end:

  Integer specifying the last result to return (default 20).

## Value

A tibble with columns:

- code:

  Education category code

- title:

  Education level

- percentage_of_respondents:

  Percent of respondents reporting this level

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_education("15-1252.00")
}
```
