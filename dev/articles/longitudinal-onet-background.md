# Why Longitudinal O\*NET Analysis Is Hard

O\*NET is one of the most useful public sources for occupation-level
skills, tasks, abilities, work activities, and work contexts. It is also
easy to misuse as a time series. The most important fact is simple:
O\*NET was not built as a longitudinal panel. The Web Services API
serves the current release, while version-over-version analysis has to
be reconstructed from the downloadable release archives ([National
Center for O\*NET Development, n.d.b](#ref-onet_archive)).

That reconstruction is why `onet2r` includes archive, bridge, and
reconciliation helpers. They do not make the historical data
automatically causal or comparable. They make the assumptions visible.

## The 3 Problems Users Must Handle

### 1. The API Is Current-Release First

Most API workflows ask “what does this occupation look like now?”
Longitudinal work asks “what changed between release A and release B?”
Those are different questions. For panels, use archive releases rather
than the current-release API.

``` r
panel <- onet_panel(
  "Abilities",
  versions = c("30.2", "30.3"),
  scale = "IM",
  archives = c(`30.2` = archive_302, `30.3` = archive_303),
  release_dates = c(`30.2` = "2026-02-01", `30.3` = "2026-05-01")
)

panel |>
  count(release_version, release_date, soc_vintage, domain) |>
  knitr::kable(digits = 3, align = "l")
```

| release_version | release_date | soc_vintage | domain    | n   |
|:----------------|:-------------|:------------|:----------|:----|
| 30.2            | 2026-02-01   | 2019        | Abilities | 7   |
| 30.3            | 2026-05-01   | 2019        | Abilities | 7   |

The table above is produced by
[`onet_panel()`](https://farach.github.io/onet2r/dev/reference/onet_panel.md).
In a live workflow, the same call can download archive ZIP files by
version. In a package vignette, local archive paths keep the output
reproducible.

### 2. Occupation Codes Change across Taxonomies

O\*NET-SOC vintages do not line up one-to-one forever. Occupations
split, merge, appear, and disappear. Hosseinioun et al.
([2025](#ref-hosseinioun2025skill)) handle this by composing adjacent
official O\*NET-SOC crosswalks, which is the same strategy used by
[`onet_crosswalk_bridge()`](https://farach.github.io/onet2r/dev/reference/onet_crosswalk_bridge.md).

The synthetic fixture below spans the 2010-to-2019 O\*NET-SOC seam. One
2010 software occupation maps to 2 2019 occupations, so the bridge is
uncertain for within-occupation comparison.

``` r
cross_panel <- onet_panel(
  "Abilities",
  versions = c("24.3", "25.1"),
  scale = "IM",
  archives = c(`24.3` = archive_243, `25.1` = archive_251),
  release_dates = c(`24.3` = "2020-08-01", `25.1` = "2020-11-01")
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
  knitr::kable(digits = 3, align = "l")
```

| from_onet_soc_code | to_onet_soc_code | element_name        | change_type     | crosswalk_uncertain | transition_data | safely_comparable |
|:-------------------|:-----------------|:--------------------|:----------------|:--------------------|:----------------|:------------------|
| 15-1132.00         | 15-1252.00       | Oral Comprehension  | transition_data | TRUE                | TRUE            | FALSE             |
| 15-1132.00         | 15-1253.00       | Oral Comprehension  | transition_data | TRUE                | TRUE            | FALSE             |
| 29-1141.00         | 29-1141.00       | Oral Comprehension  | real_update     | FALSE               | FALSE           | TRUE              |
| 15-1132.00         | 15-1252.00       | Problem Sensitivity | dropped         | TRUE                | FALSE           | FALSE             |
| 15-1132.00         | 15-1253.00       | Problem Sensitivity | dropped         | TRUE                | FALSE           | FALSE             |

### 3. A Release-to-Release “Change” May Not Be a True Content Change

Many O\*NET archive tables include a source date and domain source.
These fields matter because a value can appear unchanged simply because
the occupation was not re-surveyed, or a value can change without a
source-date change because of a recode or recalculation. Handel
([2016](#ref-handel2016onet)) is a useful entry point for the broader
measurement and comparability cautions.

``` r
changes <- onet_panel_reconcile(
  panel,
  bridge = onet_crosswalk_bridge("2019", "2019")
)

changes |>
  distinct(value_changed, date_changed, change_type, safely_comparable) |>
  arrange(change_type) |>
  knitr::kable(digits = 3, align = "l")
```

| value_changed | date_changed | change_type           | safely_comparable |
|:--------------|:-------------|:----------------------|:------------------|
| FALSE         | FALSE        | stale_carryforward    | TRUE              |
| TRUE          | TRUE         | real_update           | TRUE              |
| TRUE          | TRUE         | real_update           | FALSE             |
| FALSE         | TRUE         | resampled_stable      | TRUE              |
| TRUE          | FALSE        | recode_or_recalc_flag | FALSE             |

``` r
cross_changes |>
  mutate(comparability = if_else(safely_comparable, "Safe", "Not safe")) |>
  count(comparability, name = "rows") |>
  knitr::kable(digits = 3, align = "l")
```

| comparability | rows |
|:--------------|:-----|
| Not safe      | 4    |
| Safe          | 1    |

Those rows are not a hand-built truth table. They are the observed
classifications from the example archive panels. The patterns are the
minimum checks users should make before interpreting any cross-release
difference.

### Re-ratings are not independent

Two facts sharpen the carry-forward caution. First, the Skills domain
moved from incumbent to trained-analyst ratings around 2008, a
documented comparability break ([Handel 2016](#ref-handel2016onet)).
Second, for occupations rated a second time, O\*NET’s analyst procedures
explicitly provide prior ratings to the analysts ([Fleisher and
Tsacoumis 2012](#ref-onet_aoskills_update)). Modern release-over-release
changes in analyst-rated domains are therefore not independent
re-measurements; treat small movements as measurement persistence, not
signal. The official [Longitudinal Data Updates
record](https://www.onetcenter.org/dataUpdates.html) (see
[`onet_data_updates()`](https://farach.github.io/onet2r/dev/reference/onet_data_updates.md))
tells you which occupations were actually re-rated in each cycle.

## How This Differs from the Standard Task Approach

The canonical task-framework literature often avoids O\*NET’s
version-over-version variation. Instead, it holds task scores fixed and
gets time variation from changing occupational employment shares
([Autor, Levy, and Murnane 2003](#ref-autor2003skill); [Autor and Dorn
2013](#ref-autordorn2013growth); [Autor 2013](#ref-autor2013task)). That
approach is standard because it is cleaner for many labor-market
questions. `onet2r` enables a complementary route: directly comparing
archived O\*NET content after marking taxonomy and survey-timing
problems.

Direct within-occupation change work is closer to what the longitudinal
module does. Consoli et al. ([2023](#ref-consoli2023routinization))
combine DOT and O\*NET to study within-occupation task change over a
long period. Chen et al. ([2026](#ref-chen2026ai)) report a narrower
O\*NET version comparison as one stylized fact inside a paper whose main
data source is online vacancies.

## Official Resources to Check

- The O\*NET Database Releases Archive is the source for release ZIP
  files ([National Center for O\*NET Development,
  n.d.b](#ref-onet_archive)).
- The O\*NET Data Dictionary documents table structure, date fields,
  source fields, and historical content changes ([National Center for
  O\*NET Development, n.d.a](#ref-onet_dictionary)).
- The O\*NET-SOC taxonomy files provide the official adjacent crosswalks
  used by the bridge layer ([National Center for O\*NET Development,
  n.d.c](#ref-onet_crosswalks)).
- BLS provides a methodological overview for mapping Employment
  Projections and O\*NET data, including split, merge, and imputation
  issues that motivate future employment-weighted bridge options ([U.S.
  Bureau of Labor Statistics 2021](#ref-bls2021mapping)).

## Adjacent Tools to Verify before Citing

Several tools map O\*NET to other classification systems rather than
across O\*NET’s own versions. They may be useful, but they are
intentionally not in `inst/REFERENCES.bib` until verified to citation
standard:

- Institute for Structural Research occupational task crosswalk files.
- `occupationcross`, reported as an R package for occupational
  reclassification.
- The Dais NOC-to-O\*NET crosswalk for Canada.
- European Commission ESCO-O\*NET crosswalks.

## References

Autor, David H. 2013. “The ‘Task Approach’ to Labor Markets: An
Overview.” *Journal for Labour Market Research* 46 (3): 185–99.
<https://doi.org/10.1007/s12651-013-0128-z>.

Autor, David H., and David Dorn. 2013. “The Growth of Low-Skill Service
Jobs and the Polarization of the US Labor Market.” *American Economic
Review* 103 (5): 1553–97. <https://doi.org/10.1257/aer.103.5.1553>.

Autor, David H., Frank Levy, and Richard J. Murnane. 2003. “The Skill
Content of Recent Technological Change: An Empirical Exploration.” *The
Quarterly Journal of Economics* 118 (4): 1279–1333.
<https://doi.org/10.1162/003355303322552801>.

Chen, Hangyu, Yongming Sun, and Yiming Yuan. 2026. “Artificial
Intelligence and Skills: Evidence from Contrastive Learning in Online
Job Vacancies.” arXiv:2601.03558; SSRN 5924344.
<https://doi.org/10.2139/ssrn.5924344>.

Consoli, Davide, Giovanni Marin, Francesco Rentocchini, and Francesco
Vona. 2023. “Routinization, Within-Occupation Task Changes and Long-Run
Employment Dynamics.” *Research Policy* 52 (1): 104658.
<https://doi.org/10.1016/j.respol.2022.104658>.

Fleisher, Matthew S., and Suzanne Tsacoumis. 2012. “O\*NET Analyst
Occupational Skills Ratings: Procedures Update.” Human Resources
Research Organization.
<https://www.onetcenter.org/dl_files/AOSkills_ProcUpdate.pdf>.

Handel, Michael J. 2016. “The O\*NET Content Model: Strengths and
Limitations.” *Journal for Labour Market Research* 49 (2): 157–76.
<https://doi.org/10.1007/s12651-016-0199-8>.

Hosseinioun, Moh, Frank Neffke, Letian Zhang, and Hyejin Youn. 2025.
“Skill Dependencies Uncover Nested Human Capital.” *Nature Human
Behaviour* 9 (4): 673–87. <https://doi.org/10.1038/s41562-024-02093-2>.

National Center for O\*NET Development. n.d.a. “O\*NET Data Dictionary.”
O\*NET Resource Center.
<https://www.onetcenter.org/dictionary/30.3/excel/>.

———. n.d.b. “O\*NET Database Releases Archive.” O\*NET Resource Center.
<https://www.onetcenter.org/db_releases.html>.

———. n.d.c. “O\*NET-SOC Taxonomy and Crosswalk Files.” O\*NET Resource
Center. <https://www.onetcenter.org/taxonomy.html>.

U.S. Bureau of Labor Statistics. 2021. “Mapping Employment Projections
and O\*NET Data: A Methodological Overview.” *Monthly Labor Review*.
