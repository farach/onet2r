# List All O\*NET Occupations with Auto-Pagination

Retrieves all occupations in the O\*NET database by automatically
paginating through the results.

## Usage

``` r
onet_occupations_all(page_size = 2000, show_progress = TRUE)
```

## Arguments

- page_size:

  Integer specifying how many rows to fetch per request (default 2000,
  which is the API maximum).

- show_progress:

  Logical indicating whether to show progress messages for pagination
  (default TRUE).

## Value

A tibble with columns:

- code:

  O\*NET-SOC occupation code

- title:

  Occupation title

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_occupations_all(show_progress = FALSE)
}
```
