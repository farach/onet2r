# Get O\*NET Occupation Work Activities (details)

Get O\*NET Occupation Work Activities (details)

## Usage

``` r
onet_work_activities(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of work activities.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_work_activities("15-1252.00", end = 5)
}
```
