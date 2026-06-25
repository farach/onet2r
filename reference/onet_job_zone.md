# Get O\*NET Job Zone (details)

Note: this endpoint returns a non-paged object with keys like: code,
title, education, related_experience, job_training, job_zone_examples,
svp_range

## Usage

``` r
onet_job_zone(code)
```

## Arguments

- code:

  An O\*NET-SOC occupation code.

## Value

A list (faithful to API response).

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_job_zone("15-1252.00")
}
```
