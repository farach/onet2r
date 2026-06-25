# Getting Started with onet2r

`onet2r` helps you move from occupation search to analysis-ready O\*NET
tables, archived O\*NET releases, and BLS OEWS wage and employment
context. Current O\*NET Web Services calls require a free API key from
the [O\*NET developer
portal](https://services.onetcenter.org/developer/). Store it in
`.Renviron` as `ONET_API_KEY=your-api-key-here` so scripts do not
contain secrets.

The live Web Services examples are opt-in for package builds because
CRAN and CI should not depend on an external API. Everything else in
this article uses local files through actual `onet2r` functions.

``` r
tibble::tibble(
  setting = c("ONET_API_KEY configured", "live vignette API calls enabled"),
  value = c(has_onet_key, run_live)
)
#> # A tibble: 2 × 2
#>   setting                         value
#>   <chr>                           <lgl>
#> 1 ONET_API_KEY configured         FALSE
#> 2 live vignette API calls enabled FALSE
```

## Search the Current O\*NET API

When live calls are enabled, the first step is usually
[`onet_search()`](https://farach.github.io/onet2r/reference/onet_search.md),
followed by a detail endpoint such as
[`onet_skills()`](https://farach.github.io/onet2r/reference/onet_skills.md)
or
[`onet_abilities()`](https://farach.github.io/onet2r/reference/onet_abilities.md).

``` r
if (run_live) {
  onet_search("software developer", start = 1, end = 5)
} else {
  tibble::tibble(
    live_api_example = "skipped",
    reason = "Set ONET_API_KEY and ONET2R_RUN_LIVE_VIGNETTES=true to run onet_search()."
  )
}
#> # A tibble: 1 × 2
#>   live_api_example reason                                                       
#>   <chr>            <chr>                                                        
#> 1 skipped          Set ONET_API_KEY and ONET2R_RUN_LIVE_VIGNETTES=true to run o…
```

``` r
if (run_live) {
  onet_skills("15-1252.00", start = 1, end = 5)
} else {
  tibble::tibble(
    live_api_example = "skipped",
    reason = "Set ONET_API_KEY and ONET2R_RUN_LIVE_VIGNETTES=true to run onet_skills()."
  )
}
#> # A tibble: 1 × 2
#>   live_api_example reason                                                       
#>   <chr>            <chr>                                                        
#> 1 skipped          Set ONET_API_KEY and ONET2R_RUN_LIVE_VIGNETTES=true to run o…
```

## Read an Archived O\*NET Table

The Web Services API serves the current release. Historical work uses
the downloadable archive tables.
[`onet_archive_read()`](https://farach.github.io/onet2r/reference/onet_archive_read.md)
normalizes those archive files into tibbles with stable columns.

``` r
abilities <- onet_archive_read(
  "30.3",
  "Abilities",
  path = archive_303,
  release_date = "2026-05-01"
)

abilities |>
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

## Add Labor-Market Context

BLS OEWS estimates add employment and wage scale to O\*NET occupation
rows. The modern weighting path creates an explicit reference-SOC weight
panel.

``` r
oews <- onet_oews_national(2024, path = sample_oews)
weights <- onet_weight_panel_oews(oews, year = 2024)

weights |>
  print(width = Inf)
#> # A tibble: 3 × 7
#>   reference_soc_code  year employment weight_share source source_taxonomy
#>   <chr>              <int>      <dbl>        <dbl> <chr>  <chr>          
#> 1 11-1011             2024     211230       0.0404 OEWS   2018 SOC       
#> 2 15-1252             2024    1847900       0.353  OEWS   2018 SOC       
#> 3 29-1141             2024    3175400       0.607  OEWS   2018 SOC       
#>   reference_taxonomy
#>   <chr>             
#> 1 2018 SOC          
#> 2 2018 SOC          
#> 3 2018 SOC
```

To answer a concrete question, take one ability, treat the O\*NET values
as the user-supplied occupation score, and aggregate with OEWS
employment.

``` r
oral_scores <- abilities |>
  filter(element_id == "1.A.1.a.1") |>
  transmute(onet_soc_code, measure_score = data_value)

oral_aggregate <- onet_measure_aggregate(
  oral_scores,
  weights,
  measure_id = "oral_comprehension_fixture"
)

oral_aggregate |>
  select(-coverage, -provenance) |>
  print(width = Inf)
#> # A tibble: 1 × 7
#>   measure_id                 aggregate total_employment covered_employment
#>   <chr>                          <dbl>            <dbl>              <dbl>
#> 1 oral_comprehension_fixture      4.57          5234530            5234530
#>   employment_coverage_share n_occupations n_reference_soc
#>                       <dbl>         <int>           <int>
#> 1                         1             4               4

onet_provenance(oral_aggregate)
#> # A tibble: 1 × 7
#>   measure_id        weight_source weight_year source_taxonomy reference_taxonomy
#>   <chr>             <chr>               <int> <chr>           <chr>             
#> 1 oral_comprehensi… OEWS                 2024 2018 SOC        2018 SOC          
#> # ℹ 2 more variables: bridge_used <lgl>, crosswalk_path <chr>
```

## Compare Two Archive Releases

For historical analysis, build a panel, reconcile adjacent releases, and
inspect comparability flags before interpreting changes.

``` r
panel <- onet_panel(
  "Abilities",
  versions = c("30.2", "30.3"),
  scale = "IM",
  archives = c(`30.2` = archive_302, `30.3` = archive_303),
  release_dates = c(`30.2` = "2026-02-01", `30.3` = "2026-05-01")
)

changes <- onet_panel_reconcile(
  panel,
  bridge = onet_crosswalk_bridge("2019", "2019")
)

changes |>
  select(
    to_soc_code,
    element_name,
    from_value,
    to_value,
    value_change,
    change_type,
    safely_comparable
  ) |>
  arrange(desc(abs(value_change))) |>
  head(8) |>
  print(width = Inf)
#> # A tibble: 7 × 7
#>   to_soc_code element_name        from_value to_value value_change
#>   <chr>       <chr>                    <dbl>    <dbl>        <dbl>
#> 1 29-1141     Problem Sensitivity       4.6      4.9         0.300
#> 2 15-1252     Oral Comprehension        4.12     4.35        0.230
#> 3 41-1011     Oral Comprehension        4        4.15        0.150
#> 4 11-1011     Oral Comprehension        4.38     4.5         0.120
#> 5 15-1252     Problem Sensitivity       4.5      4.5         0    
#> 6 29-1141     Oral Comprehension        4.71     4.71        0    
#> 7 11-1011     Problem Sensitivity       4.22     4.22        0    
#>   change_type           safely_comparable
#>   <fct>                 <lgl>            
#> 1 recode_or_recalc_flag FALSE            
#> 2 real_update           TRUE             
#> 3 real_update           TRUE             
#> 4 real_update           FALSE            
#> 5 stale_carryforward    TRUE             
#> 6 resampled_stable      TRUE             
#> 7 stale_carryforward    TRUE
```

``` r
plot_data <- changes |>
  count(change_type, name = "rows") |>
  arrange(desc(rows))

barplot(
  height = setNames(plot_data$rows, plot_data$change_type),
  col = "#0f766e",
  border = NA,
  las = 2,
  ylab = "Rows",
  main = "What Kind of Change Was Observed?"
)
```

![Bar chart of change classifications in the getting started
example.](getting-started_files/figure-html/getting-started-chart-1.png)

## Next Steps

- Read
  [`vignette("longitudinal-onet-background", package = "onet2r")`](https://farach.github.io/onet2r/articles/longitudinal-onet-background.md)
  before interpreting cross-release changes.
- Read
  [`vignette("longitudinal-archives", package = "onet2r")`](https://farach.github.io/onet2r/articles/longitudinal-archives.md)
  to assemble and reconcile archive panels.
- Read
  [`vignette("oews-wage-context", package = "onet2r")`](https://farach.github.io/onet2r/articles/oews-wage-context.md)
  to choose and apply employment weights.
- Read
  [`vignette("stress-testing-exposure-measure", package = "onet2r")`](https://farach.github.io/onet2r/articles/stress-testing-exposure-measure.md)
  to stress test a user-supplied measure.
