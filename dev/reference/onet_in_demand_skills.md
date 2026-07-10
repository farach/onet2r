# Get O\*NET In-Demand Skills (details)

Retrieves in-demand technology records for an occupation. The O\*NET
details index advertises an `in_demand` link for some occupations, but
the endpoint may return 404; this helper uses the working hot-technology
endpoint and filters to rows where `in_demand` is `TRUE`.

## Usage

``` r
onet_in_demand_skills(code, start = 1, end = 20)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

- start:

  Integer specifying the first result to return.

- end:

  Integer specifying the last result to return.

## Value

A tibble of in-demand skills records.

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_in_demand_skills("15-1252.00", end = 5)
}
```
