# Expand Explicit TaskNet Inventory Relationships

Converts declared refinements and overlaps into unique
TaskNet-to-comparison edges. Reciprocal declarations are counted once.
No matching, transitive closure, weighting, or exposure estimation is
performed.

## Usage

``` r
onet_tasknet_relations(tasks)
```

## Arguments

- tasks:

  The unmodified tibble returned by
  [`onet_tasknet_read()`](https://farach.github.io/onet2r/dev/reference/onet_tasknet_read.md).

## Value

A tibble with `from_study_task_id`, `to_study_task_id`, `relation_type`
(`"refines"` or `"overlaps"`), `onet_soc_code`, `from_inventory`,
`to_inventory`, and the reader's provenance columns. The `from` endpoint
is always TaskNet. Refinement means that this statement refines the
comparison statement at `to`. Empty inputs and relation-free inventories
have the same typed output schema. Distinct relationship types on the
same pair are retained as separate edges, so edge counts need not equal
counts of distinct statement pairs.

## Details

Build relationships before filtering or editing the reader output. A
content signature rejects modified data rather than presenting derived
declarations as relationships from the original source bytes. This is an
integrity guard, not a security or authenticity signature.

## Examples

``` r
path <- system.file("extdata", "tasknet-synthetic.csv", package = "onet2r")
onet_tasknet_relations(onet_tasknet_read(path))
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
