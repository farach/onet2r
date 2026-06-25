# Get O\*NET Occupation Knowledge (details)

Get O\*NET Occupation Knowledge (details)

## Usage

``` r
onet_knowledge(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of knowledge elements.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_knowledge("15-1252.00", end = 5)
}
```
