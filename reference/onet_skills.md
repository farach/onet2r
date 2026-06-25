# Get O\*NET Occupation Skills (details)

Get O\*NET Occupation Skills (details)

## Usage

``` r
onet_skills(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of skills.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_skills("15-1252.00", end = 5)
}
```
