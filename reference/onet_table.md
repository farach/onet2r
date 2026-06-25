# Get O\*NET Table Data

Retrieves all rows from a database table, automatically paginating
through results.

## Usage

``` r
onet_table(table_id, page_size = 2000, show_progress = TRUE)
```

## Arguments

- table_id:

  Character string specifying the table identifier (e.g.,
  "occupation_data").

- page_size:

  Integer specifying how many rows to fetch per request (default 2000,
  which is the API maximum).

- show_progress:

  Logical indicating whether to show progress messages for pagination
  (default TRUE).

## Value

A tibble containing all rows from the table.

## Details

This function automatically handles pagination to retrieve all rows from
large tables. For very large tables, this may take some time and make
multiple API requests. Progress messages can be disabled by setting
`show_progress = FALSE`.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
# Get occupation data without progress messages.
occ_data <- onet_table("occupation_data", show_progress = FALSE)
}
```
