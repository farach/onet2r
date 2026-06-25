# List a Page of O\*NET Occupations

Retrieves a single page of occupations in the O\*NET database.

## Usage

``` r
onet_occupations(start = 1, end = 1000)
```

## Arguments

- start:

  Integer specifying the first result to return (default 1).

- end:

  Integer specifying the last result to return (default 1000).

## Value

A tibble with columns:

- code:

  O\*NET-SOC occupation code

- title:

  Occupation title

## Details

Use
[`onet_occupations_all()`](https://farach.github.io/onet2r/reference/onet_occupations_all.md)
to automatically paginate through the full occupation list.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_occupations(start = 1, end = 5)
}
```
