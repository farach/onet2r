# Get All O\*NET Occupation Work Activities (details)

Retrieves all work activity rows for a single occupation by
automatically paginating through the endpoint.

## Usage

``` r
onet_work_activities_all(code, page_size = 2000, show_progress = TRUE)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- page_size:

  Integer specifying how many rows to fetch per request (default 2000,
  which is the API maximum).

- show_progress:

  Logical indicating whether to show progress messages for pagination
  (default TRUE).

## Value

A tibble of work activities.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_work_activities_all("15-1252.00", show_progress = FALSE)
}
```
