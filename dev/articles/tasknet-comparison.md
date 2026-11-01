# Comparing TaskNet and O\*NET Task Inventories

Stanford’s
[TaskNet](https://digitaleconomy.stanford.edu/project/tasknet/) elicits
workers’ accounts of their jobs and combines them into occupational task
statements. Its public pilot compares 202 TaskNet statements with 118
O\*NET statements across six occupations. The [Stanford publication
abstract](https://digitaleconomy.stanford.edu/publication/tasknet-building-occupational-task-inventories-from-worker-interviews-at-scale/)
identifies that comparison release as 31.0; the CSV itself has no
release version field. It is an alternative inventory, not an official
O\*NET release or a representative inventory of the entire workforce.

[`onet_tasknet_read()`](https://farach.github.io/onet2r/dev/reference/onet_tasknet_read.md)
reads a CSV you already have. It does not download, cache, match
statements by similarity, or estimate exposure.
[`onet_tasknet_relations()`](https://farach.github.io/onet2r/dev/reference/onet_tasknet_relations.md)
expands the supplied comparison links without inventing allocation
weights.

## Start with a synthetic file

**Every statement and rating in this example is stylized.** The bundled
CSV is invented to demonstrate the format. Neither its task statements
nor its numerical results are findings about real occupations.

``` r
path <- system.file("extdata", "tasknet-synthetic.csv", package = "onet2r")
tasks <- onet_tasknet_read(path, as_of = "synthetic teaching fixture")

counts <- tasks |> count(inventory, name = "statements")
stopifnot(
  nrow(tasks) == 9L,
  sum(tasks$inventory == "TaskNet") == 5L,
  sum(tasks$inventory == "O*NET") == 4L,
  n_distinct(tasks$onet_soc_code) == 2L
)
knitr::kable(counts)
```

| inventory | statements |
|:----------|-----------:|
| O\*NET    |          4 |
| TaskNet   |          5 |

The expected inventory counts are **5 and 4**, with nine unique study
IDs. `study_task_id` identifies a record within the study file. Even an
O\*NET-labelled comparison row has a study ID, not an official archive
`task_id`. The `onet_soc_code` column retains eight-digit occupational
detail. Do not join study IDs directly to official archive Task IDs.

## Expand explicit relationships

``` r
links <- onet_tasknet_relations(tasks)
stopifnot(
  nrow(links) == 5L,
  sum(links$relation_type == "refines") == 3L,
  sum(links$relation_type == "overlaps") == 2L
)
links |>
  select(from_study_task_id, to_study_task_id, relation_type) |>
  knitr::kable()
```

| from_study_task_id | to_study_task_id | relation_type |
|:-------------------|:-----------------|:--------------|
| S-FIX-01           | O-FIX-01         | refines       |
| S-FIX-02           | O-FIX-01         | refines       |
| S-FIX-02           | O-FIX-02         | refines       |
| S-FIX-03           | O-FIX-02         | overlaps      |
| S-AUX-01           | O-AUX-01         | overlaps      |

All edges point from TaskNet to the comparison inventory. A `refined_by`
declaration on a comparison row is the inverse of `refines` on a TaskNet
row. Reciprocal declarations describe one edge, not two. An overlap is
stored in the same canonical direction for convenience, not because it
is causal. Different relation types on one pair remain separate edges.
Count distinct endpoint pairs explicitly if that, rather than typed
edges, is the denominator.

There are **three refinement edges but only two refining TaskNet
statements** in this example. One statement can refine several others,
and several statements can refine the same one. Expanding links and then
averaging joined ratings would give some statements extra weight.

``` r
refining <- links |>
  filter(relation_type == "refines") |>
  distinct(from_study_task_id)
stopifnot(nrow(refining) == 2L)
```

Create links from the complete, unchanged reader result before selecting
an inventory or occupation subset. The relation helper rejects edited or
subset input, even when a subset would contain every endpoint it needs.
Missing targets, nonreciprocal declarations, and region labels that
disagree with links are errors, not cases the reader silently repairs.

## Describe differences without inventing time change

``` r
summary <- tasks |>
  summarise(
    statements = n(),
    mean_importance = mean(importance),
    .by = inventory
  )
stopifnot(
  isTRUE(all.equal(
    summary$mean_importance[summary$inventory == "TaskNet"], 3.6
  )),
  isTRUE(all.equal(
    summary$mean_importance[summary$inventory == "O*NET"], 3.25
  ))
)
knitr::kable(summary, digits = 2)
```

| inventory | statements | mean_importance |
|:----------|-----------:|----------------:|
| TaskNet   |          5 |            3.60 |
| O\*NET    |          4 |            3.25 |

``` r

unmatched <- tasks |>
  filter(region %in% c("new", "missing")) |>
  select(inventory, region, study_task_id, task)
stopifnot(nrow(unmatched) == 2L)
knitr::kable(unmatched)
```

| inventory | region  | study_task_id | task                                    |
|:----------|:--------|:--------------|:----------------------------------------|
| TaskNet   | new     | S-FIX-04      | Stylized: document a fixture exception. |
| O\*NET    | missing | O-FIX-03      | Stylized: archive a fixture record.     |

The expected means are **3.60 and 3.25** for two different stylized
statement sets. This is not a matched comparison, a significance test,
or a change over time. Inventory construction, statement granularity,
and rating samples can all affect such summaries. On real data, disclose
missing-rating counts and denominators rather than silently dropping
observations. Duration, training, articulability, verifiability, and
top-worker gap are TaskNet-only fields in the inspected public snapshot;
all 118 comparison records have missing values for each. Those fields
cannot support a two-inventory rating comparison.

The source’s `new` and `missing` labels describe matches between
inventories. They do not date the emergence or disappearance of work.
TaskNet relevance is not a time share, and published frequency and
duration ratings do not automatically supply time-allocation weights.

## Read a manually acquired public file

Obtain the CSV through the [Stanford project
page](https://digitaleconomy.stanford.edu/project/tasknet/). Keep the
original file and record its actual acquisition time. The following code
is not executed during builds:

``` r
tasks <- onet_tasknet_read(
  "tasknet-data.csv",
  source_url = paste0(
    "https://digitaleconomy.stanford.edu/",
    "app/uploads/2026/10/tasknet-data.csv"
  ),
  as_of = "your recorded source snapshot"
)
links <- onet_tasknet_relations(tasks)
```

The reader records the SHA-256 of the bytes it parses. A source URL or
snapshot label supplied by the caller is a provenance assertion, not
independently verified publication metadata. Reading a local file does
not establish when it was downloaded. A digest identifies bytes, not a
license or a guarantee of scientific validity.

Do not bundle or redistribute the real CSV until its terms are
confirmed. The [TaskNet system
repository](https://github.com/jennjwang/tasknet-system) has its own
licensing statements and synthetic validation inputs. Those do not
establish redistribution rights for Stanford’s separate public CSV.

## Keep downstream measures explicit

[`onet_measure()`](https://farach.github.io/onet2r/dev/reference/onet_measure.md)
can validate user-supplied scores keyed to `study_task_id`. Do not
manufacture Core/Supplemental labels to pass TaskNet data through
[`onet_task_to_occupation()`](https://farach.github.io/onet2r/dev/reference/onet_task_to_occupation.md),
whose contract expects O\*NET task metadata. No new aggregation method
or exposure score is introduced here.

The CSV’s research-score columns are retained as source fields, not
endorsed or selected as defaults. Official archive readers and
longitudinal reconciliation remain separate from this cross-inventory
comparison.

Source: Wang, Haupt, Robinson, Lewis, Brynjolfsson, and Yang (2026),
[*TaskNet: Building Occupational Task Inventories from Worker Interviews
at Scale*](https://doi.org/10.2139/ssrn.7670419). Definitions above are
limited to the accessible Stanford project documentation and public CSV.
