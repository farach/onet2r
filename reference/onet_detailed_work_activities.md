# Get O\*NET Detailed Work Activities (details)

Note: this endpoint returns items under `activity` with fields like
id/title/related.

## Usage

``` r
onet_detailed_work_activities(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of detailed work activities.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_detailed_work_activities("15-1252.00", end = 5)
}
```
