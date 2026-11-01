# Get the O\*NET Longitudinal Data Updates Record

Downloads and parses the official O\*NET Longitudinal Data Updates
workbook, which records which occupations were re-rated in each data
collection stream. Use it as ground truth alongside the change
classification from
[`onet_panel_reconcile()`](https://farach.github.io/onet2r/dev/reference/onet_panel_reconcile.md):
a `real_update` row for an occupation the official record says was not
re-rated deserves scrutiny, and vice versa.

## Usage

``` r
onet_data_updates(path = NULL, force = FALSE)
```

## Arguments

- path:

  Optional path to a local copy of the workbook. If supplied, no
  download is attempted.

- force:

  Logical; re-download even when a cached copy exists.

## Value

A tibble with `data_update_type`, `onet_soc_code`, `title`, and the
normalized columns published in the workbook, such as
`number_of_updates_as_of_08_2025`, `current_date`, `current_database`,
and previous update date/database columns.

## Details

The current workbook contains separate sheets for Incumbent or OE
updates, Abilities Analyst updates, and Skills Analyst updates.
`onet_data_updates()` combines those sheets and preserves the sheet
label in `data_update_type`.

O\*NET restates historical update counts against the current O\*NET-SOC
2019 taxonomy, so per-cycle counts are not comparable across taxonomy
versions without bridging; see
[`onet_crosswalk_bridge()`](https://farach.github.io/onet2r/dev/reference/onet_crosswalk_bridge.md).

The file is cached under `tools::R_user_dir("onet2r", "cache")` in the
`reference` section; clear it with
`onet_cache_clear(what = "reference")`.

## Examples

``` r
if (interactive()) {
  updates <- onet_data_updates()
  head(updates)
}
```
