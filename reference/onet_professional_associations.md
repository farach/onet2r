# Get O\*NET Professional Associations (details)

Get O\*NET Professional Associations (details)

## Usage

``` r
onet_professional_associations(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of professional associations.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_professional_associations("15-1252.00", end = 5)
}
```
