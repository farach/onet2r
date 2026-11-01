# Get O\*NET Hot Technologies

Retrieves hot technologies for a specific occupation from the hot
technology endpoint.

## Usage

``` r
onet_hot_technology(code, start = 1, end = 20)

onet_technology(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code (e.g., "15-1252.00").

- start:

  Integer specifying the first result to return (default 1).

- end:

  Integer specifying the last result to return (default 20).

## Value

A tibble with columns:

- title:

  Technology name

- href:

  Technology details link

- hot_technology:

  Logical indicating hot technology

- in_demand:

  Logical indicating in-demand

- percentage:

  Percent of postings/mentions as defined by API

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_hot_technology("15-1252.00", end = 5)
}
```
