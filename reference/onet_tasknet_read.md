# Read a Local Stanford TaskNet Inventory

Reads a manually acquired TaskNet CSV without downloading or caching it.
Study identifiers and ratings remain separate from official O\*NET
archive identifiers and estimates. Real TaskNet data are not bundled.

## Usage

``` r
onet_tasknet_read(
  path,
  source_url = NULL,
  as_of = NULL,
  expected_sha256 = NULL
)
```

## Arguments

- path:

  Path to an existing local UTF-8 CSV file.

- source_url:

  Optional public HTTP(S) source URL, supplied by the caller.

- as_of:

  Optional caller-supplied snapshot label.

- expected_sha256:

  Optional expected SHA-256 digest of the input bytes.

## Value

A tibble with `study_task_id`, `inventory`, `region`, `title`,
`onet_soc_code`, `task`, raw relationship columns, numeric task ratings,
and character research-score fields. Additional source columns remain
character. Known optional columns are present even when absent from the
file. Provenance columns are `source_path`, `source_url`,
`source_sha256`, `source_read_at`, `source_as_of`, and integer
`source_record`. The full source receipt is also retained as an
attribute.

## Details

Required columns are `id`, `inventory`, `region`, `occupation`, `soc`,
`statement`, `refines`, `refined_by`, `overlaps`, `relevance`,
`importance`, and `frequency`. Empty rating cells become `NA_real_`;
nonempty invalid numbers are errors. Relevance and share ratings use
0–1, importance 1–5, frequency 1–7, and duration and training 1–9.
Fractional summaries are retained without interpreting categories as
time shares or exposure.

The reader validates all relationship endpoints against the complete
file. `refines` and `refined_by` must be reciprocal, as must `overlaps`.
Endpoints must share an occupation code and belong to opposite
inventories. TaskNet rows may be `new` or `overlap`; comparison rows may
be `missing` or `overlap`. These labels describe inventory comparisons,
not time change.

The source `id` is a study ID even on O\*NET comparison rows. No
official task ID, release, or Core/Supplemental status is inferred.
Research scores are preserved as character source fields, not endorsed
or selected.

The digest identifies exactly the bytes parsed. Source URL and `as_of`
are caller assertions, not independently verified metadata. Read time is
not download time. Provenance describes the source bytes, not later
edits.
[`onet_tasknet_relations()`](https://farach.github.io/onet2r/reference/onet_tasknet_relations.md)
requires the unmodified reader result; construct links before filtering
either table. Header-only files return typed empties. Confirm the
separate CSV's redistribution terms before sharing real rows.

## Examples

``` r
path <- system.file("extdata", "tasknet-synthetic.csv", package = "onet2r")
tasks <- onet_tasknet_read(path, as_of = "synthetic example")
tasks[c("study_task_id", "inventory", "importance")]
#> # A tibble: 9 × 3
#>   study_task_id inventory importance
#>   <chr>         <chr>          <dbl>
#> 1 S-FIX-01      TaskNet            4
#> 2 S-FIX-02      TaskNet            5
#> 3 S-FIX-03      TaskNet            3
#> 4 S-FIX-04      TaskNet            2
#> 5 O-FIX-01      O*NET              4
#> 6 O-FIX-02      O*NET              3
#> 7 O-FIX-03      O*NET              2
#> 8 S-AUX-01      TaskNet            4
#> 9 O-AUX-01      O*NET              4
onet_tasknet_relations(tasks)
#> # A tibble: 5 × 13
#>   from_study_task_id to_study_task_id relation_type onet_soc_code from_inventory
#>   <chr>              <chr>            <chr>         <chr>         <chr>         
#> 1 S-FIX-01           O-FIX-01         refines       15-1252.00    TaskNet       
#> 2 S-FIX-02           O-FIX-01         refines       15-1252.00    TaskNet       
#> 3 S-FIX-02           O-FIX-02         refines       15-1252.00    TaskNet       
#> 4 S-FIX-03           O-FIX-02         overlaps      15-1252.00    TaskNet       
#> 5 S-AUX-01           O-AUX-01         overlaps      31-1122.00    TaskNet       
#> # ℹ 8 more variables: to_inventory <chr>, from_source_record <int>,
#> #   to_source_record <int>, source_path <chr>, source_url <chr>,
#> #   source_sha256 <chr>, source_read_at <chr>, source_as_of <chr>
```
