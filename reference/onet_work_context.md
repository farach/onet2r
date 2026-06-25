# Get O\*NET Occupation Work Context (details)

Get O\*NET Occupation Work Context (details)

## Usage

``` r
onet_work_context(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of work context elements.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_work_context("15-1252.00", end = 5)
}
```
