# Longitudinal O\*NET Archive Analysis

Read
[`vignette("longitudinal-onet-background", package = "onet2r")`](https://farach.github.io/onet2r/articles/longitudinal-onet-background.md)
before interpreting changes. O\*NET was not designed as a longitudinal
panel, so the practical job is not only to compute differences. It is to
separate real descriptor updates from carryforwards, re-surveys with
stable values, recodes, transition rows, and taxonomy seams.

This walkthrough uses packaged archive-format text files so every
displayed table comes from `onet2r` functions while the vignette remains
CRAN-safe and does not require network access.

## A Practical Question

Suppose you want to know whether selected cognitive ability ratings
changed between O\*NET releases for occupations that matter in workforce
planning: software developers, registered nurses, chief executives, and
retail supervisors. A naive `new - old` difference is not enough,
because unchanged values may be carryforwards and changed values may
reflect a recode rather than a fresh survey update.

## Read One Archive Table

[`onet_archive_read()`](https://farach.github.io/onet2r/reference/onet_archive_read.md)
reads an O\*NET text archive table and normalizes it into one panel
schema.

``` r
abilities_303 <- onet_archive_read(
  "30.3",
  "Abilities",
  path = example_archive("30.3"),
  release_date = "2026-05-01"
)

abilities_303 |>
  select(
    release_version,
    onet_soc_code,
    soc_code,
    element_id,
    element_name,
    data_value,
    source_date,
    domain_source
  ) |>
  head(8) |>
  print(width = Inf)
#> # A tibble: 7 × 8
#>   release_version onet_soc_code soc_code element_id element_name       
#>   <chr>           <chr>         <chr>    <chr>      <chr>              
#> 1 30.3            15-1252.00    15-1252  1.A.1.a.1  Oral Comprehension 
#> 2 30.3            15-1252.00    15-1252  1.A.1.b.1  Problem Sensitivity
#> 3 30.3            29-1141.00    29-1141  1.A.1.a.1  Oral Comprehension 
#> 4 30.3            29-1141.00    29-1141  1.A.1.b.1  Problem Sensitivity
#> 5 30.3            11-1011.00    11-1011  1.A.1.a.1  Oral Comprehension 
#> 6 30.3            11-1011.00    11-1011  1.A.1.b.1  Problem Sensitivity
#> 7 30.3            41-1011.00    41-1011  1.A.1.a.1  Oral Comprehension 
#>   data_value source_date domain_source
#>        <dbl> <date>      <fct>        
#> 1       4.35 2025-07-01  Analyst      
#> 2       4.5  2024-07-01  Analyst      
#> 3       4.71 2025-08-01  Incumbent    
#> 4       4.9  2024-08-01  Incumbent    
#> 5       4.5  2025-07-01  Incumbent    
#> 6       4.22 2024-07-01  Analyst      
#> 7       4.15 2025-06-01  Analyst
```

The output has one row per occupation, element, scale, and release. The
important longitudinal fields are `release_version`, `soc_vintage`,
`onet_soc_code`, `data_value`, `source_date`, and `domain_source`.

## Assemble a Same-Vintage Panel

[`onet_panel()`](https://farach.github.io/onet2r/reference/onet_panel.md)
applies the same reader across releases and row-binds the normalized
outputs.

``` r
same_vintage_archives <- c(
  `30.2` = example_archive("30.2"),
  `30.3` = example_archive("30.3")
)
same_vintage_dates <- c(`30.2` = "2026-02-01", `30.3` = "2026-05-01")

panel <- onet_panel(
  "Abilities",
  versions = c("30.2", "30.3"),
  scale = "IM",
  archives = same_vintage_archives,
  release_dates = same_vintage_dates
)

panel |>
  arrange(onet_soc_code, element_id, release_version) |>
  select(
    release_version,
    onet_soc_code,
    element_name,
    data_value,
    source_date,
    domain_source
  ) |>
  head(10) |>
  print(width = Inf)
#> # A tibble: 10 × 6
#>   release_version onet_soc_code element_name        data_value source_date
#>   <chr>           <chr>         <chr>                    <dbl> <date>     
#> 1 30.2            11-1011.00    Oral Comprehension        4.38 2024-07-01 
#> 2 30.3            11-1011.00    Oral Comprehension        4.5  2025-07-01 
#> 3 30.2            11-1011.00    Problem Sensitivity       4.22 2024-07-01 
#> 4 30.3            11-1011.00    Problem Sensitivity       4.22 2024-07-01 
#>   domain_source
#>   <fct>        
#> 1 Analyst      
#> 2 Incumbent    
#> 3 Analyst      
#> 4 Analyst      
#> # ℹ 6 more rows
```

This is the table you would save as your audit trail before doing any
modeling. It records the exact versions, values, and source dates used
in the comparison.

## Reconcile Adjacent Releases

[`onet_panel_reconcile()`](https://farach.github.io/onet2r/reference/onet_panel_reconcile.md)
compares adjacent releases and classifies each matched
occupation-element-scale pair.

``` r
changes <- onet_panel_reconcile(panel, onet_crosswalk_bridge("2019", "2019"))

changes |>
  select(
    from_soc_code,
    to_soc_code,
    element_name,
    from_value,
    to_value,
    value_change,
    from_source_date,
    to_source_date,
    change_type,
    method_break,
    safely_comparable
  ) |>
  arrange(desc(abs(value_change))) |>
  head(10) |>
  print(width = Inf)
#> # A tibble: 7 × 11
#>   from_soc_code to_soc_code element_name        from_value to_value value_change
#>   <chr>         <chr>       <chr>                    <dbl>    <dbl>        <dbl>
#> 1 29-1141       29-1141     Problem Sensitivity       4.6      4.9         0.300
#> 2 15-1252       15-1252     Oral Comprehension        4.12     4.35        0.230
#> 3 41-1011       41-1011     Oral Comprehension        4        4.15        0.150
#> 4 11-1011       11-1011     Oral Comprehension        4.38     4.5         0.120
#> 5 15-1252       15-1252     Problem Sensitivity       4.5      4.5         0    
#> 6 29-1141       29-1141     Oral Comprehension        4.71     4.71        0    
#> 7 11-1011       11-1011     Problem Sensitivity       4.22     4.22        0    
#>   from_source_date to_source_date change_type           method_break
#>   <date>           <date>         <fct>                 <lgl>       
#> 1 2024-08-01       2024-08-01     recode_or_recalc_flag FALSE       
#> 2 2024-07-01       2025-07-01     real_update           FALSE       
#> 3 2024-06-01       2025-06-01     real_update           FALSE       
#> 4 2024-07-01       2025-07-01     real_update           TRUE        
#> 5 2024-07-01       2024-07-01     stale_carryforward    FALSE       
#> 6 2024-08-01       2025-08-01     resampled_stable      FALSE       
#> 7 2024-07-01       2024-07-01     stale_carryforward    FALSE       
#>   safely_comparable
#>   <lgl>            
#> 1 FALSE            
#> 2 TRUE             
#> 3 TRUE             
#> 4 FALSE            
#> 5 TRUE             
#> 6 TRUE             
#> 7 TRUE
```

Read `change_type` before interpreting `value_change`.

| Value changed? | Source date changed? | Classification          | Interpretation                                                        |
|----------------|----------------------|-------------------------|-----------------------------------------------------------------------|
| no             | no                   | `stale_carryforward`    | The release likely carried forward the prior value.                   |
| yes            | yes                  | `real_update`           | The value changed with a new source date.                             |
| no             | yes                  | `resampled_stable`      | The occupation appears updated, but the score stayed stable.          |
| yes            | no                   | `recode_or_recalc_flag` | Treat cautiously because the value changed without a new source date. |

``` r
changes |>
  filter(value_changed) |>
  mutate(abs_change = abs(value_change)) |>
  arrange(safely_comparable, desc(abs_change)) |>
  select(
    to_soc_code,
    element_name,
    from_value,
    to_value,
    value_change,
    change_type,
    method_break,
    safely_comparable
  ) |>
  print(width = Inf)
#> # A tibble: 4 × 8
#>   to_soc_code element_name        from_value to_value value_change
#>   <chr>       <chr>                    <dbl>    <dbl>        <dbl>
#> 1 29-1141     Problem Sensitivity       4.6      4.9         0.300
#> 2 11-1011     Oral Comprehension        4.38     4.5         0.120
#> 3 15-1252     Oral Comprehension        4.12     4.35        0.230
#> 4 41-1011     Oral Comprehension        4        4.15        0.150
#>   change_type           method_break safely_comparable
#>   <fct>                 <lgl>        <lgl>            
#> 1 recode_or_recalc_flag FALSE        FALSE            
#> 2 real_update           TRUE         FALSE            
#> 3 real_update           FALSE        TRUE             
#> 4 real_update           FALSE        TRUE
```

## Cross a Taxonomy Seam

Same-vintage comparisons are the easy case. The next example uses the
bundled 2010-vintage and 2019-vintage fixtures. The bridge is
intentionally tiny, but it shows the real problem: a 2010 occupation can
split into more than one 2019 occupation.

``` r
cross_archives <- c(
  `24.3` = example_archive("24.3"),
  `25.1` = example_archive("25.1")
)
cross_dates <- c(`24.3` = "2020-08-01", `25.1` = "2020-11-01")

cross_panel <- onet_panel(
  "Abilities",
  versions = c("24.3", "25.1"),
  scale = "IM",
  archives = cross_archives,
  release_dates = cross_dates
)

bridge_2010_2019 <- tibble::tibble(
  from_vintage = "2010",
  to_vintage = "2019",
  from_onet_soc_code = c("15-1132.00", "15-1132.00", "29-1141.00"),
  to_onet_soc_code = c("15-1252.00", "15-1253.00", "29-1141.00"),
  map_type = c("split", "split", "one_to_one"),
  crosswalk_weight = c(0.5, 0.5, 1)
)

cross_changes <- onet_panel_reconcile(cross_panel, bridge_2010_2019)

cross_changes |>
  select(
    from_onet_soc_code,
    to_onet_soc_code,
    element_name,
    change_type,
    crosswalk_uncertain,
    transition_data,
    safely_comparable
  ) |>
  print(width = Inf)
#> # A tibble: 5 × 7
#>   from_onet_soc_code to_onet_soc_code element_name        change_type    
#>   <chr>              <chr>            <chr>               <fct>          
#> 1 15-1132.00         15-1252.00       Oral Comprehension  transition_data
#> 2 15-1132.00         15-1253.00       Oral Comprehension  transition_data
#> 3 29-1141.00         29-1141.00       Oral Comprehension  real_update    
#> 4 15-1132.00         15-1252.00       Problem Sensitivity dropped        
#> 5 15-1132.00         15-1253.00       Problem Sensitivity dropped        
#>   crosswalk_uncertain transition_data safely_comparable
#>   <lgl>               <lgl>           <lgl>            
#> 1 TRUE                TRUE            FALSE            
#> 2 TRUE                TRUE            FALSE            
#> 3 FALSE               FALSE           TRUE             
#> 4 TRUE                FALSE           FALSE            
#> 5 TRUE                FALSE           FALSE
```

``` r
summary_counts <- changes |>
  count(change_type, name = "rows") |>
  arrange(desc(rows))

barplot(
  height = setNames(summary_counts$rows, summary_counts$change_type),
  col = "#0f766e",
  border = NA,
  las = 2,
  ylab = "Rows",
  main = "Change Classification Comes Before Interpretation"
)
```

![Bar chart of same-vintage change
classifications.](longitudinal-archives_files/figure-html/summary-chart-1.png)

## Summarize the Panel

[`onet_change_summary()`](https://farach.github.io/onet2r/reference/onet_change_summary.md)
gives a compact audit of the reconciliation result. The job-family view
is useful for spotting whether one part of the SOC taxonomy is driving
the apparent change.

``` r
onet_change_summary(changes, by = "job_family") |>
  print(width = Inf)
#> # A tibble: 5 × 10
#>   summary_level job_family change_type        n_pairs share_pairs
#>   <chr>         <chr>      <chr>                <int>       <dbl>
#> 1 overall       NA         real_update              7       1    
#> 2 job_family    11         stale_carryforward       2       0.286
#> 3 job_family    15         stale_carryforward       2       0.286
#> 4 job_family    29         resampled_stable         2       0.286
#> 5 job_family    41         real_update              1       0.143
#>   mean_value_change median_abs_value_change share_safely_comparable
#>               <dbl>                   <dbl>                   <dbl>
#> 1            0.114                   0.120                    0.714
#> 2            0.0600                  0.0600                   0.5  
#> 3            0.115                   0.115                    1    
#> 4            0.150                   0.150                    0.5  
#> 5            0.150                   0.150                    1    
#>   share_method_break share_crosswalk_uncertain
#>                <dbl>                     <dbl>
#> 1              0.143                         0
#> 2              0.5                           0
#> 3              0                             0
#> 4              0                             0
#> 5              0                             0
```

## Recommended Workflow for Real Archive Work

1.  Use
    [`onet_releases()`](https://farach.github.io/onet2r/reference/onet_releases.md)
    to identify release versions and O\*NET-SOC vintages.
2.  Build a panel with
    [`onet_panel()`](https://farach.github.io/onet2r/reference/onet_panel.md)
    for one domain at a time, such as `"Abilities"`, `"Skills"`, or
    `"Work Activities"`.
3.  Build a bridge with
    [`onet_crosswalk_bridge()`](https://farach.github.io/onet2r/reference/onet_crosswalk_bridge.md)
    when releases use different O\*NET-SOC vintages.
4.  Reconcile with
    [`onet_panel_reconcile()`](https://farach.github.io/onet2r/reference/onet_panel_reconcile.md).
5.  Filter, weight, or model only after checking `change_type`,
    `method_break`, `crosswalk_uncertain`, and `safely_comparable`.
