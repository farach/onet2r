# Get O\*NET Table Column Information

Retrieves metadata about columns in a specific database table.

## Usage

``` r
onet_table_info(table_id)
```

## Arguments

- table_id:

  Character string specifying the table identifier (e.g.,
  "occupation_data").

## Value

A tibble with column metadata including:

- name:

  Column identifier

- title:

  Column title

- type:

  Column data type

- format:

  Column format

- optional:

  Whether the column is optional

- description:

  Column description

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
info <- onet_table_info("occupation_data")
}
```
