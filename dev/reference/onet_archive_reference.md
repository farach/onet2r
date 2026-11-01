# Read an O\*NET Archive Reference Table

Reads a table from an O\*NET text archive with its published columns.
Use it for reference and lookup tables that have no O\*NET-SOC code
column, such as `"GWAs to IWAs to DWAs"`, `"Scales Reference"`,
`"Task Categories"`, and `"Content Model Reference"`, which
[`onet_archive_read()`](https://farach.github.io/onet2r/dev/reference/onet_archive_read.md)
does not read. It also returns columns that the longitudinal panel
schema does not keep, such as `job_zone` in `"Job Zones"`.

## Usage

``` r
onet_archive_reference(version, table, path = NULL, release_date = NULL)
```

## Arguments

- version:

  O\*NET database version, for example `"30.3"`.

- table:

  Archive table name, for example `"GWAs to IWAs to DWAs"` or
  `"Scales Reference"`.

- path:

  Optional path to a local text archive ZIP file or extracted archive
  directory. If supplied, no download is attempted.

- release_date:

  Optional release date for a local archive. Downloaded archives use the
  release date from
  [`onet_releases()`](https://farach.github.io/onet2r/dev/reference/onet_releases.md).

## Value

A tibble with `release_version`, `release_date`, and the table's
published columns renamed to snake case, with `O*NET` written as `onet`.
Columns whose names end in `_id` or `_code` stay character so they join
to
[`onet_archive_read()`](https://farach.github.io/onet2r/dev/reference/onet_archive_read.md)
output. Other columns are converted with
[`utils::type.convert()`](https://rdrr.io/r/utils/type.convert.html).

## Details

Recent tab-delimited text archives (30.2 through 31.0 were checked) keep
repeated names out of linking and rating files. `Tasks to DWAs.txt` and
`Task Ratings.txt` carry codes but not the names found in the Excel
files, so
[`onet_archive_read()`](https://farach.github.io/onet2r/dev/reference/onet_archive_read.md)
returns `NA` for fields such as `dwa_element_name`, `scale_name`, and
`title`. Occupation titles and task text can be joined back from
`Occupation Data` and `Task Statements`, which
[`onet_archive_read()`](https://farach.github.io/onet2r/dev/reference/onet_archive_read.md)
reads. Scale names are only in `Scales Reference.txt`, and DWA titles
only in `GWAs to IWAs to DWAs.txt` (release 30.3 onward) or
`DWA Reference.txt` (earlier releases). Read those with
`onet_archive_reference()`. For example:

    dwa_titles <- onet_archive_reference("31.0", "GWAs to IWAs to DWAs", path = zip)
    tasks_to_dwas <- onet_archive_read("31.0", "Tasks to DWAs", path = zip) |>
      dplyr::select(-"dwa_element_name") |>
      dplyr::left_join(
        dplyr::distinct(dwa_titles, dwa_element_id, dwa_element_name),
        by = "dwa_element_id",
        relationship = "many-to-one"
      )

## Examples

``` r
archive_dir <- system.file(
  "extdata",
  "onet-mini",
  "db_30_3_text",
  package = "onet2r"
)
onet_archive_reference(
  "30.3",
  "GWAs to IWAs to DWAs",
  path = archive_dir,
  release_date = "2026-05-01"
)
#> # A tibble: 3 × 8
#>   release_version release_date gwa_element_id gwa_element_name    iwa_element_id
#>   <chr>           <date>       <chr>          <chr>               <chr>         
#> 1 30.3            2026-05-01   4.A.2          Information Input   4.A.2.a.1.a   
#> 2 30.3            2026-05-01   4.A.4          Communicating and … 4.A.4.a.4.a   
#> 3 30.3            2026-05-01   4.A.2          Information Input   4.A.2.a.3.a   
#> # ℹ 3 more variables: iwa_element_name <chr>, dwa_element_id <chr>,
#> #   dwa_element_name <chr>
```
