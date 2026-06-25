# Get O\*NET Related Occupations (details)

Get O\*NET Related Occupations (details)

## Usage

``` r
onet_related_occupations(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of related occupations.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_related_occupations("15-1252.00", end = 5)
}
```
