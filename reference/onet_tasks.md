# Get O\*NET Occupation Tasks (details)

Get O\*NET Occupation Tasks (details)

## Usage

``` r
onet_tasks(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of tasks.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_tasks("15-1252.00", end = 5)
}
```
