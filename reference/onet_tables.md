# List O\*NET Database Tables

Retrieves a list of available database tables in O\*NET.

## Usage

``` r
onet_tables()
```

## Value

A tibble with columns:

- id:

  Table identifier

- title:

  Table title/description

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
tables <- onet_tables()
head(tables)
}
```
