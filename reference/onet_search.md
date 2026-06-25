# Search O\*NET Occupations

Searches O\*NET occupations by keyword or O\*NET-SOC code.

## Usage

``` r
onet_search(keyword, start = 1, end = 20)
```

## Arguments

- keyword:

  A character string containing the search term or O\*NET-SOC code.

- start:

  Integer specifying the first result to return (default 1).

- end:

  Integer specifying the last result to return (default 20).

## Value

A tibble with columns:

- code:

  O\*NET-SOC occupation code

- title:

  Occupation title

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
# Search by keyword
onet_search("software developer")

# Search by SOC code
onet_search("15-1252")
}
```
