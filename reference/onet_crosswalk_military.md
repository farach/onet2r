# Military to Civilian Occupation Crosswalk

Searches for civilian occupations that match military job titles or
codes.

## Usage

``` r
onet_crosswalk_military(keyword, start = 1, end = 20)
```

## Arguments

- keyword:

  A character string containing a military job title or code.

- start:

  Integer specifying the first result to return (default 1).

- end:

  Integer specifying the last result to return (default 20).

## Value

A tibble with matching civilian occupations:

- code:

  O\*NET-SOC occupation code

- title:

  Civilian occupation title

## Details

The O\*NET military crosswalk is a keyword search over military titles
and codes. It can return zero rows for valid-looking military codes or
titles when the keyword does not match the Web Services index.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_crosswalk_military("infantry")

onet_crosswalk_military("11B")
}
```
