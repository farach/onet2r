# Get O\*NET Occupation Interests (details)

Get O\*NET Occupation Interests (details)

## Usage

``` r
onet_interests(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of interest elements.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_interests("15-1252.00", end = 5)
}
```
