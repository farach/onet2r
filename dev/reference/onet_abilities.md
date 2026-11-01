# Get O\*NET Occupation Abilities (details)

Get O\*NET Occupation Abilities (details)

## Usage

``` r
onet_abilities(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of ability elements.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_abilities("15-1252.00", end = 5)
}
```
