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

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
# Search by military job title
onet_crosswalk_military("infantry")

# Search by military code
onet_crosswalk_military("11B")
}
```
