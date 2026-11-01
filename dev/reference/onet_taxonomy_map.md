# Map O\*NET-SOC Codes Between Taxonomy Versions

Converts occupation codes between the active O\*NET-SOC taxonomy and the
2010 SOC taxonomy.

## Usage

``` r
onet_taxonomy_map(code, from = c("active", "2010"), to = NULL)
```

## Arguments

- code:

  An occupation code to convert.

- from:

  Source taxonomy: "active" (current O\*NET-SOC) or "2010" (2010 SOC).

- to:

  Target taxonomy: "active" or "2010".

## Value

A tibble with mapped occupation codes:

- code:

  Mapped occupation code in target taxonomy

- title:

  Occupation title

## Examples

``` r
if (FALSE) { # interactive() && nzchar(Sys.getenv("ONET_API_KEY"))
onet_taxonomy_map("15-1252.00", from = "active", to = "2010")

onet_taxonomy_map("15-1131.00", from = "2010", to = "active")
}
```
