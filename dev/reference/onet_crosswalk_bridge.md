# Build an O\*NET-SOC Crosswalk Bridge

Builds an adjacent or chained O\*NET-SOC bridge between taxonomy
vintages. Bridges keep the native 8-digit O\*NET-SOC code and include a
derived 6-digit SOC code for employment joins. Equal weights are used
for transparent split apportionment.

## Usage

``` r
onet_crosswalk_bridge(
  from_vintage,
  to_vintage,
  weight = c("equal", "employment")
)
```

## Arguments

- from_vintage:

  Source taxonomy vintage.

- to_vintage:

  Target taxonomy vintage.

- weight:

  Weighting method. Only `"equal"` is implemented.

## Value

A tibble with source and target O\*NET-SOC codes, derived SOC codes,
vintages, map type, and equal split weights.

## Examples

``` r
if (interactive()) {
  onet_crosswalk_bridge("2010", "2019")
}
```
