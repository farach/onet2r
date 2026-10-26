# What Makes Occupations Different?

Two occupations can share reading, communication, and problem solving
yet differ in what they produce or the knowledge they need. I want to
understand those differences without making every shared requirement
disappear into a broad label.

I start with a simpler question. Which recorded activities appear in
many occupations, and which appear in only a few? TF-IDF gives uncommon
links more weight. For a binary activity list, an occupation’s summary
is the mean log inverse prevalence of its activities.

Rarity alone leaves a lot unanswered. Two occupations might share the
same unusual activities and be each other’s closest neighbor. A common
skill could distinguish them through its required level. I therefore
compare profiles as well as rarity, and check what changes when labels,
occupation weights or distance rules change.

The information-loss budget asks how much a listed activity tells us
about which occupation it came from, and how much of that information
remains after grouping the labels. It is a calculation about
descriptions. Economic value, worker time and effects of AI need
evidence these records do not contain.

The article uses a **derived snapshot of the full public O\*NET 30.3
archive** and clearly labeled **stylized examples** to explain the
formulas. Article builds use bundled tables and do not access the
network. The helpers are article code, not exported package functions or
an endorsed substantive score. Rankings describe the covered O\*NET
occupations under the specified choices, not all jobs or workers in the
United States.

## First choose a shared unit of work

Treat an occupation as a document and a shared activity as a term. Do
not start by treating occupation-specific Task IDs as a universal task
vocabulary. Count their actual cross-occupation overlap first. If almost
every ID belongs to only one occupation, almost every ID receives the
same rarity weight.

O\*NET provides a more useful bridge: **Tasks to DWAs** links task
statements to Detailed Work Activities. The **GWAs to IWAs to DWAs**
reference supplies the hierarchy from detailed to intermediate to
generalized activities. These links provide a published shared
vocabulary without requiring an embedding model or treating different
wording as different work.

The next example uses the bundled miniature fixture, with two
occupations. It demonstrates the join only; the full-archive results
begin below.

``` r
archive_dir <- onet2r_inst_path("extdata", "onet-mini", "db_30_3_text")
tasks <- onet_archive_read(
  "30.3", "Task Statements", path = archive_dir,
  release_date = "2026-05-01"
)
links <- onet_archive_read(
  "30.3", "Tasks to DWAs", path = archive_dir,
  release_date = "2026-05-01"
)
hierarchy <- onet_archive_reference(
  "30.3", "GWAs to IWAs to DWAs", path = archive_dir,
  release_date = "2026-05-01"
)

task_overlap <- tasks |>
  distinct(onet_soc_code, task_id) |>
  count(task_id, name = "n_occupations")
task_overlap |>
  count(n_occupations, name = "n_task_ids") |>
  knitr::kable()
```

| n_occupations | n_task_ids |
|--------------:|-----------:|
|             1 |          3 |

``` r

dwa_names <- hierarchy |>
  distinct(dwa_element_id, dwa_element_name)
activity_links <- links |>
  select(onet_soc_code, task_id, dwa_element_id) |>
  distinct() |>
  left_join(dwa_names, by = "dwa_element_id", relationship = "many-to-one")
stopifnot(
  !anyNA(activity_links$dwa_element_id),
  !anyNA(activity_links$dwa_element_name)
)
activity_links |>
  knitr::kable()
```

| onet_soc_code | task_id | dwa_element_id | dwa_element_name                              |
|:--------------|:--------|:---------------|:----------------------------------------------|
| 15-1252.00    | 1001    | 4.A.2.a.1.a.1  | Analyze data to inform operational decisions. |
| 15-1252.00    | 1002    | 4.A.4.a.4.a.1  | Prepare technical reports.                    |
| 29-1141.00    | 2001    | 4.A.2.a.3.a.1  | Monitor health or safety conditions.          |

Use the reference join even if the miniature fixture already includes
names. Real text archives omit repeated labels from the link files. For
a full archive, join occupation titles from `Occupation Data` and task
text from `Task Statements`, using occupation plus Task ID for the
latter.

Before scoring, report the number of occupations with task statements,
with links, and with usable ratings, plus the share of tasks that have a
DWA link. Inspect unmapped tasks and suppressed ratings. **An unobserved
profile is not a zero-work occupation.** The helpers below reject
missing or all-zero occupation profiles rather than silently shrink the
declared universe.

## Define frequency, prevalence, and distinctiveness separately

Let $x_{od}$ be a non-negative activity weight for occupation $o$ and
activity $d$, and let $N$ be the number of occupations in a fixed
universe. The baseline uses binary presence: $x_{od} = 1$ if at least
one retained task links to the activity, and zero otherwise.

$$df_{d} = \sum\limits_{o}\mathbf{1}\left( x_{od} > 0 \right)$$$$prevalence_{d} = df_{d}/N$$$$tf_{od} = \frac{x_{od}}{\sum\limits_{j}x_{oj}}$$$$idf_{d} = \log\left( N/df_{d} \right)$$$$tfidf_{od} = tf_{od}\, idf_{d}$$

Here TF means **within-occupation profile share**, not work time. Count
each occupation once in document frequency, regardless of how many of
its tasks map to the activity. The unsmoothed IDF deliberately assigns
zero to a universally present activity. A convention such as
$1 + \log\left( (N + 1)/\left( df_{d} + 1 \right) \right)$ would not.

For an occupation-level rarity summary, use
$D_{o} = \sum_{d}tfidf_{od}$. It is the average log inverse prevalence
of that occupation’s activity profile. Retain the item-level
contributions so the score remains interpretable.

## What the full archive shows

The snapshot covers **923 occupations** with 18796 task statements and
2083 linked DWAs. All 18796 task statements have at least one DWA link.
The occupation taxonomy contains 1016 entries, so 93 entries are outside
this task-based analytic universe. We do not give them zero scores.
There are 0 Task IDs shared across occupations, which is why shared
activities are a more useful baseline vocabulary.

The baseline retains any linked task, including Supplemental tasks,
counts each occupation-DWA link once, and gives each occupation equal
weight. Task ratings, employment weights, and AI-exposure measures are
excluded.

Four properties of this archive limit what the rankings can mean.

- **Presence is cheap.** One linked task, even a Supplemental one, makes
  an occupation-DWA pair present. Occupations have 4 to 40 task
  statements and 5 to 41 DWAs. Slaughterers and Meat Packers has 14 task
  statements, all Supplemental and none Core. Models has ten, one Core
  and nine Supplemental. Restricting the data literally to Core tasks
  would shrink the universe to 893 occupations and make Models a one-DWA
  rarity leader. That is a different universe, not a robustness pass on
  this ranking.
- **Measurement dates differ.** Task vintages span 2004 to 2025 inside
  the May 2026 archive. The release is not a uniform 2026 measurement.
- **Rankings are summaries of database labels.** They describe one
  published archive. They are not estimates of a population quantity, so
  they come with no confidence intervals and no sampling interpretation.
- **Code granularity matters.** The baseline gives every O\*NET-SOC code
  equal weight. A code group with more detailed codes therefore
  contributes more total weight to activity prevalence than a group with
  fewer codes. The results are code-weighted comparisons, not equally
  weighted comparisons of occupational families.

### Most common and least common detailed activities

``` r
observed_prevalence |>
  slice_max(n_occupations, n = 8, with_ties = TRUE) |>
  select(dwa_element_name, n_occupations, prevalence) |>
  knitr::kable(digits = 3, col.names = c(
    "Detailed work activity", "Occupations", "Share of covered occupations"
  ))
```

| Detailed work activity                                                                           | Occupations | Share of covered occupations |
|:-------------------------------------------------------------------------------------------------|------------:|-----------------------------:|
| Record operational or production data.                                                           |          91 |                        0.099 |
| Order materials, supplies, or equipment.                                                         |          71 |                        0.077 |
| Maintain inventories of materials, equipment, or products.                                       |          61 |                        0.066 |
| Serve on institutional or departmental committees.                                               |          58 |                        0.063 |
| Record patient medical histories.                                                                |          57 |                        0.062 |
| Supervise employees.                                                                             |          57 |                        0.062 |
| Attend training sessions or professional meetings to develop or maintain professional knowledge. |          56 |                        0.061 |
| Evaluate student work.                                                                           |          55 |                        0.060 |

![Horizontal bar chart of the eight DWAs linked to the most occupations.
The largest is linked to 91 of 923 occupations, about 9.9 percent. The
table above gives the exact
values.](occupational-distinctiveness_files/figure-html/figure-common-1.png)

The eight most widely linked detailed work activities, with the number
and share of the 923 covered occupations.

The most widely linked DWA is **Record operational or production data.**
It appears in 91 of 923 covered occupations, or 9.9 percent. “Most
common” is therefore relative: these are quite specific activity
descriptions, not a complete inventory of general human capabilities.

At the other end, **57 DWAs occur in only one covered occupation**. They
tie on document frequency and IDF. The next table shows eight of these
tied activities in identifier order, not eight uniquely lowest-ranked
activities.

``` r
observed_prevalence |>
  filter(n_occupations == 1L) |>
  arrange(item) |>
  head(8) |>
  select(dwa_element_name, n_occupations) |>
  knitr::kable(col.names = c("Example singleton DWA", "Occupations"))
```

| Example singleton DWA                                      | Occupations |
|:-----------------------------------------------------------|------------:|
| Review audio or video recordings.                          |           1 |
| Research product safety.                                   |           1 |
| Monitor biofuel production operations.                     |           1 |
| Monitor conditions at energy-producing landfills.          |           1 |
| Monitor recreational facility operations.                  |           1 |
| Time vehicle speed or traffic-control equipment operation. |           1 |
| Monitor external factors impacting operations.             |           1 |
| Label production materials.                                |           1 |

### Occupations with the rarest activity profiles

``` r
observed_occupations |>
  slice_min(rarity_rank, n = 8, with_ties = TRUE) |>
  select(rarity_rank, title, n_activities, mean_idf, nearest_distance) |>
  knitr::kable(digits = 3, col.names = c(
    "Rank", "Occupation", "DWAs", "Mean IDF", "Nearest profile distance"
  ))
```

| Rank | Occupation                                                     | DWAs | Mean IDF | Nearest profile distance |
|-----:|:---------------------------------------------------------------|-----:|---------:|-------------------------:|
|    1 | Slaughterers and Meat Packers                                  |    5 |    5.972 |                    0.625 |
|    2 | Fishing and Hunting Workers                                    |   20 |    5.569 |                    0.696 |
|    3 | Judges, Magistrate Judges, and Magistrates                     |   13 |    5.403 |                    0.357 |
|    4 | Agricultural Equipment Operators                               |   15 |    5.374 |                    0.696 |
|    5 | Janitors and Cleaners, Except Maids and Housekeeping Cleaners  |   18 |    5.363 |                    0.500 |
|    6 | Biofuels/Biodiesel Technology and Product Development Managers |    9 |    5.358 |                    0.692 |
|    7 | Administrative Law Judges, Adjudicators, and Hearing Officers  |   14 |    5.353 |                    0.235 |
|    8 | Forest and Conservation Workers                                |   16 |    5.344 |                    0.625 |

**Slaughterers and Meat Packers ranks first on this particular rarity
summary**, with 5 linked activities, the smallest profile size in the
archive, a minimum shared by six occupations. Its fourteen statements
are all Supplemental, so its rank rests on labels attached to non-Core
tasks. It is not necessarily the most isolated occupation, the most
specialized occupation under another vocabulary, or the occupation with
the largest entry barriers.

Profile size matters. With binary profiles, the score is a mean over
$n_{o}$ activities, so small profiles have more variable scores. In this
archive, profile size and the rarity score have a Spearman correlation
of -0.29, and profile size and nearest-neighbor distance have -0.34.
Read the rankings with the DWA counts beside them, as in the table
above.

These observations are a descriptive starting point. A publishable
substantive claim should survive Core-only filtering, alternative
task-link weights, minimum-coverage restrictions, and broader activity
groupings. The snapshot does not claim those robustness checks have all
been passed. Snapshot rarity scores and distances are rounded to 12
decimal places before ranking or choosing neighbors so numerical noise
does not break ties.

### Sensitivity to occupational-code granularity

The baseline gives each of the 923 detailed O\*NET-SOC codes equal
weight. As a sensitivity, this calculation first finds each DWA’s
prevalence within each five-character code-prefix group, then averages
those prevalences with each represented prefix group receiving equal
weight:

$${\bar{p}}_{d} = \frac{1}{G}\sum\limits_{g = 1}^{G}\frac{{\text{codes in}\mspace{6mu}}g{\mspace{6mu}\text{linked to}\mspace{6mu}}d}{{\text{covered codes in}\mspace{6mu}}g}.$$$${IDF}_{d}^{*} = \log\left( 1/{\bar{p}}_{d} \right).$$

The within-occupation TF remains normalized as before. This changes the
reference prevalence, not the occupational activity profiles. The two
rankings have a Spearman correlation of 0.59, and 2 occupations ranked
in the top ten (including ties) overlap.

![Scatter plot comparing each of 923 occupations' rarity rank under
equal code weights with its rank when five-character code-prefix groups
receive equal weight. The diagonal indicates unchanged rank; a table
below lists occupations with the largest
shifts.](occupational-distinctiveness_files/figure-html/figure-group-weighting-1.png)

Occupation rarity ranks under equal detailed-code weights versus equal
five-character code-group weights. Points away from the diagonal change
rank when the prevalence denominator changes.

| Occupation (code prefix)                                                                | Code-weighted rank | Group-weighted rank | Rank change |
|:----------------------------------------------------------------------------------------|-------------------:|--------------------:|------------:|
| First-Line Supervisors of Farming, Fishing, and Forestry Workers (45-10)                |                 24 |                 734 |        -710 |
| Pesticide Handlers, Sprayers, and Applicators, Vegetation (37-30)                       |                 19 |                 674 |        -655 |
| First-Line Supervisors of Landscaping, Lawn Service, and Groundskeeping Workers (37-10) |                 16 |                 653 |        -637 |
| Fishing and Hunting Workers (45-30)                                                     |                  2 |                 637 |        -635 |
| First-Line Supervisors of Food Preparation and Serving Workers (35-10)                  |                143 |                 774 |        -631 |
| Chefs and Head Cooks (35-10)                                                            |                146 |                 769 |        -623 |
| First-Line Supervisors of Housekeeping and Janitorial Workers (37-10)                   |                 21 |                 636 |        -615 |
| Hosts and Hostesses, Restaurant, Lounge, and Coffee Shop (35-90)                        |                181 |                 785 |        -604 |

These prefix groups are not the official SOC minor groups. The
employment extension below also uses the actual minor-group definition
and reports the two grouping choices separately.

This sensitivity gives each represented prefix group equal influence on
DWA prevalence; it does not make the group definitions substantively
neutral. Different taxonomies or broader grouping levels could produce
different rankings. The baseline remains the primary result because it
is the declared equal-code comparison, not because it is invariant to
taxonomy granularity. Positive rank changes indicate a better numeric
rank under the group-weighted sensitivity; negative values indicate a
worse rank.

### Skeptical check: profile size

| DWAs in profile | Occupations | Mean rarity score | Mean nearest distance |
|:----------------|------------:|------------------:|----------------------:|
| 5 to 10         |          56 |             4.642 |                 0.678 |
| 11 to 20        |         465 |             4.470 |                 0.591 |
| 21 to 30        |         340 |             4.280 |                 0.508 |
| 31 or more      |          62 |             4.100 |                 0.458 |

![Scatter plot of 923 occupations with number of linked detailed work
activities on the horizontal axis and rarity score on the vertical axis.
The rarity score tends to be lower for larger profiles, with a Spearman
correlation of about minus 0.29, and the scores spread more widely at
small profile
sizes.](occupational-distinctiveness_files/figure-html/figure-inventory-size-1.png)

Real O\*NET 30.3: rarity score against number of linked DWAs, with the
rarity leader labeled.

## Work through the score on a stylized example

``` r
activity_labels <- tibble::tribble(
  ~item, ~activity, ~broad_group,
  "communicate", "Exchange information", "Interaction",
  "analyze", "Analyze evidence", "Information",
  "test", "Test a system", "Information",
  "report", "Prepare a technical report", "Information",
  "care", "Provide direct care", "Practical work",
  "diagnose", "Diagnose a condition", "Information",
  "safety", "Check safety conditions", "Practical work",
  "repair", "Repair equipment", "Practical work",
  "teach", "Teach a procedure", "Interaction"
)
activity_sets <- list(
  A = c("communicate", "analyze", "test"),
  B = c("communicate", "analyze", "report"),
  C = c("communicate", "care", "diagnose"),
  D = c("communicate", "care", "safety"),
  E = c("communicate", "repair", "safety"),
  F = c("communicate", "teach", "report")
)
universe <- names(activity_sets)
activity_profiles <- bind_rows(lapply(universe, function(code) {
  tibble::tibble(occupation = code, item = activity_sets[[code]], weight = 1)
}))
scores <- profile_tfidf(activity_profiles, universe)

prevalence_table <- scores |>
  distinct(item, n_occupations, prevalence, idf) |>
  left_join(activity_labels, by = "item", relationship = "one-to-one") |>
  arrange(desc(prevalence), activity)
prevalence_table |>
  select(activity, n_occupations, prevalence, idf) |>
  knitr::kable(digits = 3)
```

| activity                   | n_occupations | prevalence |   idf |
|:---------------------------|--------------:|-----------:|------:|
| Exchange information       |             6 |      1.000 | 0.000 |
| Analyze evidence           |             2 |      0.333 | 1.099 |
| Check safety conditions    |             2 |      0.333 | 1.099 |
| Prepare a technical report |             2 |      0.333 | 1.099 |
| Provide direct care        |             2 |      0.333 | 1.099 |
| Diagnose a condition       |             1 |      0.167 | 1.792 |
| Repair equipment           |             1 |      0.167 | 1.792 |
| Teach a procedure          |             1 |      0.167 | 1.792 |
| Test a system              |             1 |      0.167 | 1.792 |

“Exchange information” appears in all six stylized occupations and
receives zero IDF. “Test a system” appears in one and receives
$\log(6)$. Neither statement establishes that testing is more important
than communication.

``` r
rarity <- scores |>
  summarise(mean_idf = sum(tf_idf), .by = occupation) |>
  mutate(rarity_rank = min_rank(desc(mean_idf))) |>
  arrange(rarity_rank, occupation)
rarity |>
  knitr::kable(digits = 3)
```

| occupation | mean_idf | rarity_rank |
|:-----------|---------:|------------:|
| A          |    0.963 |           1 |
| C          |    0.963 |           1 |
| E          |    0.963 |           1 |
| F          |    0.963 |           1 |
| B          |    0.732 |           5 |
| D          |    0.732 |           5 |

``` r

scores |>
  left_join(activity_labels, by = "item", relationship = "many-to-one") |>
  filter(occupation == "A") |>
  arrange(desc(tf_idf), activity) |>
  select(occupation, activity, share, idf, tf_idf) |>
  knitr::kable(digits = 3)
```

| occupation | activity             | share |   idf | tf_idf |
|:-----------|:---------------------|------:|------:|-------:|
| A          | Test a system        | 0.333 | 1.792 |  0.597 |
| A          | Analyze evidence     | 0.333 | 1.099 |  0.366 |
| A          | Exchange information | 0.333 | 0.000 |  0.000 |

Keep ties rather than declaring an arbitrary winner. A national ranking
and a ranking within healthcare answer different questions because the
denominator and IDF change. If the question is whether two groups
differ, calculate their scores with the **same vocabulary and reference
IDF**, not separately fitted IDFs that place each group on its own
scale.

### Alternative weights need their own interpretation

Binary presence avoids rewarding occupations merely because they have
more task statements. As sensitivity checks, use distinct linked-task
counts or task Importance/Relevance weights. A task can map to several
DWAs. For a count- or rating-weighted profile, split its mass over its
$k$ retained links as $w_{t}/k$, then sum by occupation and DWA.
Otherwise heavily mapped tasks receive more total weight just because
they have more links.

Decide whether to retain Supplemental tasks before calculating $k$.
Audit missing ratings instead of dropping them through an inner join.
Relevance, Importance, and frequency-category responses are different
quantities. Multiplying them does not produce an observed share of
working hours.

## Rare activities and unusual occupations are not the same

To identify unusual *combinations*, compare whole profiles. Here the
distance between two normalized profiles is total variation:

$$TV(o,p) = \frac{1}{2}\sum\limits_{d}\left| tf_{od} - tf_{pd} \right|.$$

It ranges from zero for identical compositions to one for disjoint
profiles. Using the original shares preserves ubiquitous activities that
TF-IDF would erase. Cosine similarity is another defensible choice, but
a profile containing only universal activities has a zero TF-IDF vector
and no defined TF-IDF cosine.

``` r
activity_distances <- profile_distances(activity_profiles, universe)
directed_distances <- bind_rows(
  activity_distances |>
    transmute(occupation = occupation_a, neighbor = occupation_b, distance),
  activity_distances |>
    transmute(occupation = occupation_b, neighbor = occupation_a, distance)
)
nearest <- directed_distances |>
  group_by(occupation) |>
  slice_min(distance, with_ties = TRUE) |>
  ungroup()
nearest |>
  left_join(rarity, by = "occupation", relationship = "many-to-one") |>
  arrange(desc(distance), occupation, neighbor) |>
  knitr::kable(digits = 3)
```

| occupation | neighbor | distance | mean_idf | rarity_rank |
|:-----------|:---------|---------:|---------:|------------:|
| A          | B        |    0.333 |    0.963 |           1 |
| B          | A        |    0.333 |    0.732 |           5 |
| B          | F        |    0.333 |    0.732 |           5 |
| C          | D        |    0.333 |    0.963 |           1 |
| D          | C        |    0.333 |    0.732 |           5 |
| D          | E        |    0.333 |    0.732 |           5 |
| E          | D        |    0.333 |    0.963 |           1 |
| F          | B        |    0.333 |    0.963 |           1 |

The nearest-neighbor distance measures isolation: how far an occupation
sits from its closest profile. Do not add the average distance to all
occupations as a second atypicality score. With about two thousand
mostly sparse activities, almost every pair of profiles is nearly
disjoint, so each occupation’s average distance is close to one and
tracks the rarity score (a Spearman correlation of about 0.88 in this
archive). Inspect the nearest neighbors and activity contributions
rather than relying on one rank. Neither rarity nor distance establishes
that workers can readily move between two occupations.

For binary profiles of sizes $a$ and $b$ that share $k$ activities,
total variation has the closed form $TV = 1 - k/\max(a,b)$. It depends
on the overlap count and the larger profile size, and it ignores how
rare the shared activities are. That is why rarity and isolation can
disagree, and why isolation carries a profile-size dependence of its
own: a small profile is close to another only if that profile is also
small and shares most of its activities; full containment in a larger
profile still leaves a large distance. The example above is
**stylized**; the table below uses the real archive.

For comparison, the most isolated full-archive profiles under this same
binary-presence, unweighted total-variation rule are:

``` r
observed_occupations |>
  slice_max(nearest_distance, n = 8, with_ties = TRUE) |>
  arrange(desc(nearest_distance), occupation) |>
  select(title, n_activities, nearest_distance, rarity_rank) |>
  knitr::kable(digits = 3, col.names = c(
    "Occupation", "DWAs", "Nearest distance", "Rarity rank"
  ))
```

| Occupation                        | DWAs | Nearest distance | Rarity rank |
|:----------------------------------|-----:|-----------------:|------------:|
| Models                            |    8 |            0.875 |          23 |
| Park Naturalists                  |   28 |            0.857 |         203 |
| Digital Forensics Analysts        |   20 |            0.846 |         226 |
| Lighting Technicians              |   19 |            0.842 |         204 |
| Traffic Technicians               |   19 |            0.842 |         217 |
| Insurance Appraisers, Auto Damage |    8 |            0.833 |         205 |
| Actuaries                         |   10 |            0.833 |         424 |
| Disc Jockeys, Except Radio        |   17 |            0.824 |         148 |
| Aircraft Service Attendants       |   15 |            0.824 |         429 |

![Scatter plot of 923 occupations. Rarity score is on the horizontal
axis and nearest-neighbor distance on the vertical axis. The two are
only weakly related, with a Spearman correlation of about 0.36. The
tables above and below give exact
values.](occupational-distinctiveness_files/figure-html/figure-rarity-isolation-1.png)

Real O\*NET 30.3 occupations: rarity score against nearest-neighbor
total-variation distance, with the rarity leader and the most isolated
profile labeled.

| Labeled occupation            | DWAs | Mean IDF | Nearest distance | Rarity rank |
|:------------------------------|-----:|---------:|-----------------:|------------:|
| Slaughterers and Meat Packers |    5 |    5.972 |            0.625 |           1 |
| Models                        |    8 |    5.160 |            0.875 |          23 |

The rank correlation between mean IDF and nearest-profile distance is
0.36. The different ordering is useful evidence that item rarity and
isolation answer different questions. Distances are descriptive
distances between published activity profiles, not probabilities of
successful job transitions.

The snapshot also contains 5 pairs with exactly the same detailed
activity set. All five share O\*NET-SOC major-group prefix 25, but 3
pairs cross five-character code-group prefixes. Detailed codes can
therefore distinguish labels even when their linked DWA inventories are
identical; this small cluster does not show that all occupations within
a major group share the same work.

| Occupation A                                                   | Occupation B                                                                         | Five-character prefixes (A / B) |
|:---------------------------------------------------------------|:-------------------------------------------------------------------------------------|:--------------------------------|
| 25-2023.00: Career/Technical Education Teachers, Middle School | 25-2031.00: Secondary School Teachers, Except Special and Career/Technical Education | 25-20 / 25-20                   |
| 25-1067.00: Sociology Teachers, Postsecondary                  | 25-1113.00: Social Work Teachers, Postsecondary                                      | 25-10 / 25-11                   |
| 25-1063.00: Economics Teachers, Postsecondary                  | 25-1065.00: Political Science Teachers, Postsecondary                                | 25-10 / 25-10                   |
| 25-1063.00: Economics Teachers, Postsecondary                  | 25-1122.00: Communications Teachers, Postsecondary                                   | 25-10 / 25-11                   |
| 25-1065.00: Political Science Teachers, Postsecondary          | 25-1122.00: Communications Teachers, Postsecondary                                   | 25-10 / 25-11                   |

## What changes when larger occupations count more?

An activity can appear in few occupation codes but still be attached to
a large amount of employment. To examine that, I pair the 30.3
descriptions with the May 2025 national OEWS estimates. This changes the
reference population. It does not tell us how often workers perform the
activity.

The mapping needs more care than matching the first six digits. OEWS
publishes some occupations as combined cells. O\*NET also has broad
`.00` descriptions alongside narrower specialties. Those descriptions
cannot all be treated as separate groups of workers.

The primary comparison therefore uses one `.00` description per
constituent SOC occupation. A cell enters the point estimate only if
every expected description is available. I do not fill a missing broad
description with a specialty profile. For a combined cell, the point
estimate divides its employment equally among its constituent SOC
descriptions. That split is an assumption, not a published employment
breakdown.

| Scope                 | Published cells | Employment | Share of official total |
|:----------------------|----------------:|-----------:|------------------------:|
| official_total        |              NA |  155495730 |                  1.0000 |
| returned_panel        |             830 |  155481840 |                  0.9999 |
| official_not_returned |              NA |      13890 |                  0.0001 |
| complete              |             756 |  146251550 |                  0.9406 |
| partial               |               4 |    3477400 |                  0.0224 |
| no_profile            |              70 |    5752890 |                  0.0370 |

The complete cells cover 146.25 million jobs, or 94.1 percent of the
release’s official total. Another 9.23 million are in cells with
incomplete descriptions. Separately, the sum of the 830 returned
detailed cells falls 13,890 below the official total. I leave that
difference unexplained rather than allocate it to an activity.

OEWS covers nonfarm wage-and-salary employment, not all workers. In
particular, these estimates exclude the self-employed and the military.
The results below are conditional on the complete cells, not estimates
of activity use across the entire workforce.

For a binary activity link, employment-weighted prevalence and the share
of allocated employment attached to a linked description are the same
calculation. If $E_{s}$ is a cell’s employment, $a_{os}$ its assumed
within-cell allocation, and $z_{od}$ a description’s activity link:

$$P_{d} = \frac{\sum\limits_{s}E_{s}\sum\limits_{o}a_{os}z_{od}}{\sum\limits_{s}E_{s}}.$$

The table compares this with equal-description prevalence on the same
765-description support. Official SOC minor groups receive equal weight
in another sensitivity. The older five-character-prefix grouping is kept
separately.

![Paired points compare equal-description prevalence with
employment-allocated prevalence for eight activities. The connecting
lines show how the choice of weights changes prevalence; the table below
supplies both values and the allocation
bounds.](occupational-distinctiveness_files/figure-html/employment-prevalence-1.png)

The eight activities with the largest employment-allocated prevalence in
the complete-cell comparison. Both series use the same SOC-description
support.

| Activity                                                                     | Equal descriptions (%) | Employment allocation (%) | Allocation lower (%) | Allocation upper (%) |
|:-----------------------------------------------------------------------------|-----------------------:|--------------------------:|---------------------:|---------------------:|
| Order materials, supplies, or equipment.                                     |                   8.76 |                     17.90 |                17.90 |                17.90 |
| Calculate costs of goods or services.                                        |                   3.27 |                     13.26 |                13.26 |                13.26 |
| Answer telephones to direct calls or provide information.                    |                   3.27 |                     11.09 |                11.09 |                11.09 |
| Prepare documentation for contracts, transactions, or regulatory compliance. |                   3.40 |                     10.69 |                10.69 |                10.69 |
| Record operational or production data.                                       |                  10.72 |                     10.48 |                10.39 |                10.56 |
| Collect deposits, payments or fees.                                          |                   3.40 |                     10.41 |                10.41 |                10.41 |
| Greet customers, patrons, or visitors.                                       |                   2.75 |                     10.35 |                10.34 |                10.37 |
| Process customer bills or payments.                                          |                   1.44 |                      9.63 |                 9.63 |                 9.63 |

Across activities, the equal-description and employment-weighted
prevalence orders have a Spearman correlation of about 0.66. Recomputing
IDF from the employment prevalence changes the rarity order too. It
answers how unusual a link is against this allocated employment
distribution. Averaging the original rarity scores with employment
weights would be a different operation.

For each complete cell, I also let its employment fall anywhere among
the constituent SOC descriptions. The smallest and largest possible link
shares give the allocation bounds above. Among the 2,034 DWAs linked to
these descriptions, 92.4 percent have zero-width bounds. This says that
most links are unaffected by the remaining within-cell split. It does
not settle the missing cells: the wider bounds in the bundled table let
each incomplete cell’s link be either zero or one, relative to
returned-panel employment.

These are allocation bounds, not confidence intervals. They do not
include OEWS sampling error or turn recorded links into worker behavior.
A separate all-code sensitivity reweights overlapping `.00` and
specialty descriptions. Its min/max results are labeled a **profile
envelope**, not a sharp bound over disjoint groups of workers.

The raw-file audit also matters. All twelve published combination codes
already have detailed rows in this year’s file. Seven appear again as
broad rows with the same employment. Adding those broad rows would
double count them; the existing detailed-row filter is appropriate for
these inputs.

## Skills, knowledge, experience, and working conditions

Use a separate representation for each domain:

| Domain                          | Useful representation                                                   | What not to do                                                          |
|:--------------------------------|:------------------------------------------------------------------------|:------------------------------------------------------------------------|
| Detailed work activities        | Binary presence or an explicit task-link weighting rule                 | Count occupation-specific wording as shared activity without checking   |
| Software examples               | Deduplicated exact-name links, with separate classification-level views | Treat listed examples or posting flags as worker-use rates              |
| Skills, knowledge, abilities    | One declared Importance or Level scale per element                      | Treat every recorded positive rating as a meaningful presence indicator |
| Related experience and training | Full response distribution within each element and scale                | Treat category codes as years or pool unrelated category systems        |
| Work context                    | Scale-aware ratings or category distributions                           | Mix all contexts and scales as interchangeable units                    |
| Education and Job Zone          | Separate entry-requirement descriptors                                  | Interpret a summary category as the full requirement profile            |

In release 30.3 the dictionary lists **Essential Skills**,
**Transferable Skills**, **Education**, and **Training and Experience**
separately. Do not assume an older filename such as `Skills` or
`Education, Training, and Experience` applies to this release.

Many rated descriptors appear for almost every occupation. Binary IDF
then collapses toward zero even when required *levels* differ
substantially. A threshold such as Importance above a chosen cutoff
creates a prevalence measure, but the cutoff is a research choice that
needs sensitivity checks. Keep raw levels and their element-specific
anchors alongside any normalized profile. Normalizing ratings removes
overall intensity and treats rating values as comparable non-negative
weights, an assumption rather than a property of an ordinal scale.

For experience, O\*NET’s category percentages describe respondents’
answers about requirements. They are **not** the distribution of actual
tenure among workers. Related Work Experience (`RW`), On-Site or
In-Plant Training (`PT`), and On-The-Job Training (`OJ`) each require
their own category lookup.

Total variation ignores the ordering of experience bins. If distance in
required duration matters, use an ordered-distribution distance with
justified category locations and explicit treatment of open-ended
categories. Do not assign arbitrary midpoints and call the result
measured years of experience.

We keep these domains separate rather than average them into one score.
Giving each block an equal weight would still be a judgment about what
matters, and putting all the columns in one vector would give longer
questionnaires more influence. Neither choice follows from the data.

Normalizing ratings can also remove a difference we care about. Two
profiles with ratings `(2, 2)` and `(4, 4)` have identical shares even
though every rating differs. The skills extension therefore keeps
absolute rating gaps. Education, experience, knowledge and work context
remain separate possible extensions. This article does not combine their
category systems or treat an education requirement as a measure of the
work being done.

### What the actual skills ratings show

The 30.3 files contain ten Essential Skills and 25 Transferable Skills,
each on Importance and Level scales. I analyze those four blocks
separately. Importance runs from 1 to 5; Level runs from 0 to 7 and has
element-specific anchors. The numerical ranges do not make a one-point
change in Science the same substantive change as a one-point change in
Speaking.

Of the 923 activity codes, 894 have complete skill profiles. Removing
the 71 with at least one Recommend Suppress flag leaves 823 for the
primary comparison. This is a meaningful restriction: Accountants and
Auditors and Software Developers are among the excluded codes. The
software article’s 923-code results are therefore not this
common-support comparison.

Not Relevant is handled separately. I retain the recorded values rather
than turn the flag into a zero. The sensitivity tables show what changes
when flagged Level values are placed at the scale floor, and when all
894 complete profiles are retained.

![Each of ten Essential Skills has a median point and a line from the
10th to 90th percentile of Importance ratings across 823 occupations.
The table below gives those values on the 1-to-5
scale.](occupational-distinctiveness_files/figure-html/skill-distributions-1.png)

Essential Skills Importance distributions across the 823 primary
occupation profiles. The lines describe the spread across occupations,
not uncertainty in an estimated mean.

| Essential skill       | 10th percentile | Median | 90th percentile |
|:----------------------|----------------:|-------:|----------------:|
| Reading Comprehension |            2.75 |   3.62 |            4.12 |
| Active Listening      |            3.00 |   3.75 |            4.00 |
| Writing               |            2.25 |   3.12 |            4.00 |
| Speaking              |            2.88 |   3.62 |            4.12 |
| Mathematics           |            2.00 |   2.50 |            3.25 |
| Science               |            1.00 |   1.75 |            3.35 |
| Critical Thinking     |            2.88 |   3.62 |            4.00 |
| Active Learning       |            2.38 |   3.12 |            3.88 |
| Learning Strategies   |            2.00 |   2.75 |            3.62 |
| Monitoring            |            2.88 |   3.12 |            3.88 |

Every retained rating is an eight-analyst mean in these files. A
threshold on that mean is not a count of analysts choosing a response
category. For Reading Comprehension, changing the threshold from 2 to 4
changes how widespread the stated requirement looks:

| Importance mean at least | Scale label at threshold | Share of primary codes (%) |
|-------------------------:|:-------------------------|---------------------------:|
|                        2 | Somewhat Important       |                       99.9 |
|                        3 | Important                |                       80.8 |
|                        4 | Very Important           |                       30.0 |

For profile comparisons, the primary distance is the mean absolute
element-level rating gap, divided by four for Importance or seven for
Level. Unlike row-normalized shares, this preserves overall rating
differences. It assumes comparable numerical gaps across elements within
a block.

The largest Essential-Level pair gap in this primary universe is between
Physicists and Slaughterers and Meat Packers. The three largest element
gaps make the comparison less abstract:

| Essential skill       | Physicists: Level | Slaughterers: Level |  Gap |
|:----------------------|------------------:|--------------------:|-----:|
| Science               |              5.75 |                0.00 | 5.75 |
| Mathematics           |              5.50 |                0.75 | 4.75 |
| Reading Comprehension |              5.88 |                1.75 | 4.13 |

For context, the Reading Comprehension Level anchors run from reading
step-by-step form instructions at 2 to reading a scientific journal
article about surgical procedures at 6. Those examples help interpret a
rating; they do not make every element’s scale interchangeable.

I also rank occupations within each skill element and compare percentile
profiles. That drops the equal-interval assumption about the original
ratings, although averaging percentile gaps still imposes a geometry.
The single nearest neighbor often changes:

| Skill block              | One-neighbor overlap | Five-neighbor overlap | Ten-neighbor overlap |
|:-------------------------|---------------------:|----------------------:|---------------------:|
| Essential: Importance    |                 0.49 |                  0.46 |                 0.50 |
| Essential: Level         |                 0.47 |                  0.45 |                 0.48 |
| Transferable: Importance |                 0.45 |                  0.44 |                 0.48 |
| Transferable: Level      |                 0.50 |                  0.48 |                 0.51 |

About 48 to 55 percent of occupations have disjoint single-neighbor sets
under the absolute-gap and percentile comparisons. At five neighbors,
fewer than one percent do. The broad distance ordering is much more
stable than the identity of one nearest occupation. That is a reason to
show neighborhoods and element gaps rather than present one match as
definitive. These are descriptive comparisons, without significance
claims or joint confidence intervals assembled from marginal standard
errors.

## Do activities, skills and software point to the same neighbors?

On the same 823 codes, I recompute activity and exact-name software
distances and compare them with each skill block. All six use the same
candidate occupations. Boundary ties are retained, after rounding
distances to eight decimal places. A five-neighbor set can therefore
contain more than five codes.

For software, 515 of the 823 codes have more than five neighbors at this
cutoff. These larger tied sets also affect the overlap comparison.

Agreement is low. At five neighbors, 55.8 percent of activity/software
comparisons share no neighbor at all. The median neighbor-set overlap is
zero. It is also zero between software and each skill block. This does
not establish that one domain is better. They record different aspects
of occupations, and the result also depends on each domain’s distance
rule.

![A six-by-six matrix compares activities, software, and four skill
blocks. Median five-neighbor overlap is zero for activities versus
software and for software versus every skill block. Essential Importance
versus Level is 0.11; Transferable Importance versus Level is 0.25. The
following table gives all block pairs at one, five, and ten
neighbors.](occupational-distinctiveness_files/figure-html/cross-domain-agreement-1.png)

Median neighbor-set Jaccard overlap on the common 823-code universe,
with five nearest neighbors and boundary ties. Diagonal entries compare
a block with itself.

| Block A                  | Block B                  | One neighbor | Five neighbors | Ten neighbors |
|:-------------------------|:-------------------------|-------------:|---------------:|--------------:|
| Activities               | Software                 |            0 |           0.00 |          0.04 |
| Activities               | Essential: Importance    |            0 |           0.00 |          0.05 |
| Activities               | Essential: Level         |            0 |           0.00 |          0.05 |
| Activities               | Transferable: Importance |            0 |           0.00 |          0.05 |
| Activities               | Transferable: Level      |            0 |           0.00 |          0.05 |
| Software                 | Essential: Importance    |            0 |           0.00 |          0.02 |
| Software                 | Essential: Level         |            0 |           0.00 |          0.02 |
| Software                 | Transferable: Importance |            0 |           0.00 |          0.03 |
| Software                 | Transferable: Level      |            0 |           0.00 |          0.03 |
| Essential: Importance    | Essential: Level         |            0 |           0.11 |          0.18 |
| Essential: Importance    | Transferable: Importance |            0 |           0.00 |          0.05 |
| Essential: Importance    | Transferable: Level      |            0 |           0.00 |          0.05 |
| Essential: Level         | Transferable: Importance |            0 |           0.00 |          0.05 |
| Essential: Level         | Transferable: Level      |            0 |           0.00 |          0.05 |
| Transferable: Importance | Transferable: Level      |            0 |           0.25 |          0.33 |

For Commercial and Industrial Designers, activities point to Fashion
Designers while software points to Graphic Designers. Essential-Level
ratings instead point to Landscape Architects. The nursing example also
changes with the domain:

| Occupation                          | Block                    | Nearest descriptions, including ties                    |
|:------------------------------------|:-------------------------|:--------------------------------------------------------|
| Commercial and Industrial Designers | Activities               | Fashion Designers                                       |
| Registered Nurses                   | Activities               | Acute Care Nurses; General Internal Medicine Physicians |
| Commercial and Industrial Designers | Essential: Importance    | Cytotechnologists                                       |
| Registered Nurses                   | Essential: Importance    | Exercise Physiologists                                  |
| Commercial and Industrial Designers | Essential: Level         | Landscape Architects                                    |
| Registered Nurses                   | Essential: Level         | Critical Care Nurses                                    |
| Commercial and Industrial Designers | Software                 | Graphic Designers                                       |
| Registered Nurses                   | Software                 | Patient Representatives                                 |
| Commercial and Industrial Designers | Transferable: Importance | Human Factors Engineers and Ergonomists                 |
| Registered Nurses                   | Transferable: Importance | Nurse Midwives                                          |
| Commercial and Industrial Designers | Transferable: Level      | Human Factors Engineers and Ergonomists                 |
| Registered Nurses                   | Transferable: Level      | Acute Care Nurses                                       |

These are neighbors among published descriptions on a restricted
support. The absence of Accountants or Software Developers from this
table follows the suppression rule, not a claim that their skills or
software cannot be studied. The 894-code skill sensitivity is kept
separately. No raw activity, software or rating distance is treated as a
common unit, and none of these neighbors establishes an easy worker
transition.

## When do differences disappear?

### Coarser labels can mechanically erase differences

Collapse the stylized activities into their invented broad groups,
holding the universe and activity mass fixed. In a real archive, use the
published DWA-to-IWA-to-GWA hierarchy instead of these demonstration
labels.

``` r
broad_profiles <- activity_profiles |>
  left_join(activity_labels, by = "item", relationship = "many-to-one") |>
  summarise(weight = sum(weight), .by = c(occupation, broad_group)) |>
  rename(item = broad_group)
coarse_distances <- profile_distances(broad_profiles, universe) |>
  rename(broad_distance = distance)
resolution_comparison <- activity_distances |>
  rename(detailed_distance = distance) |>
  left_join(
    coarse_distances, by = c("occupation_a", "occupation_b"),
    relationship = "one-to-one"
  ) |>
  mutate(distance_lost = detailed_distance - broad_distance) |>
  arrange(desc(distance_lost), occupation_a, occupation_b)
resolution_comparison |>
  head(8) |>
  knitr::kable(digits = 3)
```

| occupation_a | occupation_b | detailed_distance | broad_distance | distance_lost |
|:-------------|:-------------|------------------:|---------------:|--------------:|
| A            | B            |             0.333 |          0.000 |         0.333 |
| A            | C            |             0.667 |          0.333 |         0.333 |
| A            | F            |             0.667 |          0.333 |         0.333 |
| B            | C            |             0.667 |          0.333 |         0.333 |
| C            | E            |             0.667 |          0.333 |         0.333 |
| C            | F            |             0.667 |          0.333 |         0.333 |
| D            | E            |             0.333 |          0.000 |         0.333 |
| A            | D            |             0.667 |          0.667 |         0.000 |

``` r

pair_ab <- resolution_comparison |>
  filter(occupation_a == "A", occupation_b == "B")
stopifnot(
  abs(pair_ab$detailed_distance - 1 / 3) < 1e-12,
  pair_ab$broad_distance == 0,
  all(resolution_comparison$distance_lost >= -1e-12)
)
```

In the stylized example, A and B differ in testing versus reporting, but
both have the same broad information/interaction mixture. Their distance
disappears **because the representation discards that distinction**, not
because their work has changed. With mass-preserving grouping, total
variation cannot increase. That is a mathematical property of grouping,
so a drop in distance is guaranteed and is not itself a finding.
Re-binarizing the broad groups or re-estimating IDF changes additional
assumptions, so assess those changes separately.

The same mass-preserving coarsening can be run on the full archive. The
next table uses the published hierarchy, holds the occupation universe
fixed, and does not refit IDF or re-binarize parent activities. Read it
with care: at the detailed grain, 371,633 of 425,503 occupation pairs
(87.3 percent) share no DWA, and the mean pairwise distance is 0.987. A
statistic that starts this close to its ceiling has little room to move,
and by itself it does not show how distinctive occupations are. The null
reference below makes that concrete.

``` r
observed_resolution |>
  mutate(grain = c("Detailed activities", "Intermediate activities",
                   "Generalized activities")) |>
  knitr::kable(digits = 3)
```

| grain                   | n_pairs | mean_distance | median_distance | n_indistinguishable_pairs | mean_distance_lost |
|:------------------------|--------:|--------------:|----------------:|--------------------------:|-------------------:|
| Detailed activities     |  425503 |         0.987 |           1.000 |                         5 |              0.000 |
| Intermediate activities |  425503 |         0.925 |           0.952 |                         9 |              0.061 |
| Generalized activities  |  425503 |         0.716 |           0.724 |                         9 |              0.270 |

The reduction measures what grouping activity labels discards from a
near-saturated statistic. It is not evidence that workers’ actual jobs
have converged over time.

### An information-loss budget for coarsening

The information-loss budget asks how much information about occupation
identity survives when activity labels are grouped. This borrows the
relevance-target idea from the information bottleneck of Tishby,
Pereira, and Bialek: judge a compression by how much information it
retains about a target variable. We do not fit the bottleneck algorithm.
The hierarchy is fixed in advance by O\*NET.

Draw an occupation $O$ uniformly from the $N$ covered occupations. Then
draw a detailed activity $D$ from that occupation’s profile shares
$p_{od}$. The mixture is the mean profile, **not** the
document-frequency ratio:

$$q_{d} = \frac{1}{N}\sum\limits_{o}p_{od}$$$$I(O;D) = \frac{1}{N}\sum\limits_{o}\sum\limits_{d}p_{od}\log_{2}\frac{p_{od}}{q_{d}}.$$

$I(O;D)$ is the mean Kullback-Leibler divergence, in bits, between each
occupation’s profile and the mean profile. It lies between zero and
$\log_{2}N$. Aggregating the original mass through the fixed
DWA-to-IWA-to-GWA hierarchy gives $I\left( O;\text{IWA} \right)$ and
$I\left( O;\text{GWA} \right)$. By the data-processing inequality,

$$I(O;D) \geq I\left( O;\text{IWA} \right) \geq I\left( O;\text{GWA} \right).$$

The decrease is guaranteed, so the informative quantities are its size,
how it splits across hierarchy groups, and the retained percentage
relative to the detailed bits. Losses by parent group add up exactly to
the total bits discarded between the detailed and generalized grains.

**This is accounting, not a new metric.** When every occupation has the
same number $n$ of binary activities, $p_{od} = 1/n$ and
$q_{d} = df_{d}/(nN)$, so each occupation’s divergence in bits equals
its mean IDF divided by $\ln 2$. Unequal profile sizes break that
equality. The new contribution is the bookkeeping of what each grouping
level discards. It is not a new uniqueness score and it is not a
validated correction for profile size.

| Grain              | Labels with mass | Information (bits) | Lost from DWA (bits) | Retained (% of DWA bits) |
|:-------------------|-----------------:|-------------------:|---------------------:|-------------------------:|
| Detailed (DWA)     |             2083 |               6.39 |                 0.00 |                   100.00 |
| Intermediate (IWA) |              332 |               3.95 |                 2.44 |                    61.88 |
| Generalized (GWA)  |               41 |               1.67 |                 4.72 |                    26.07 |

Detailed activities carry 6.39 bits of the at most 9.85 bits that could
identify one of 923 occupations. Intermediate groups retain 3.95 bits
(61.9 percent of the detailed total) and generalized groups retain 1.67
bits (26.1 percent), a loss of 4.72 bits. The retained percentages use
the detailed bits as the denominator.

![Bar chart of information in bits at the detailed, intermediate, and
generalized activity grains: about 6.39, 3.95, and 1.67 bits, which is
100, 61.9, and 26.1 percent of the detailed bits. The table above gives
the exact
values.](occupational-distinctiveness_files/figure-html/figure-information-1.png)

Real O\*NET 30.3 information budget: bits of occupation identity carried
by activity labels at each grain, with the percentage of detailed bits
retained.

![Bar chart of discarded bits for the eight generalized work activity
groups with the largest losses. The group losses across all 41 groups
sum to about 4.72 bits discarded between the detailed and generalized
grains. The table below gives the eight largest groups; the bundled
information-loss CSV contains all
41.](occupational-distinctiveness_files/figure-html/figure-loss-1.png)

Real O\*NET 30.3: eight generalized-activity groups with the largest
bits discarded when detailed activities are grouped. Large losses can
reflect large group mass.

| Generalized activity group                             | DWAs | Share of mean profile | Lost bits | Lost bits per unit of mass |
|:-------------------------------------------------------|-----:|----------------------:|----------:|---------------------------:|
| Handling and Moving Objects                            |  269 |                0.1176 |    0.6191 |                     5.2665 |
| Documenting/Recording Information                      |  113 |                0.0785 |    0.4096 |                     5.2147 |
| Performing General Physical Activities                 |  126 |                0.0636 |    0.3107 |                     4.8839 |
| Getting Information                                    |  111 |                0.0502 |    0.2768 |                     5.5126 |
| Guiding, Directing, and Motivating Subordinates        |  108 |                0.0517 |    0.2684 |                     5.1916 |
| Inspecting Equipment, Structures, or Materials         |  107 |                0.0457 |    0.2519 |                     5.5071 |
| Thinking Creatively                                    |  123 |                0.0426 |    0.2324 |                     5.4514 |
| Communicating with Supervisors, Peers, or Subordinates |   67 |                0.0384 |    0.1988 |                     5.1815 |

The figure and table show the eight largest groups. The bundled
`information-loss.csv` contains results for all 41 groups.

A group’s weighted loss rises with its share of the mean profile
(`mixture_mass`), so a large group can top the table simply because it
is large. The conditional loss, which divides by that mass, shows the
loss per unit of mass. The budget is not classification accuracy, and it
does not measure semantic richness, difficulty, AI exposure, or whether
jobs are converging. It describes how much a given set of labels can say
about occupation identity under this sampling model.

### Does the published GWA grouping discard more than size alone?

This diagnostic randomly assigns the 2,083 DWA labels to 41 groups while
preserving the observed number of DWA labels in each group. The DWA-to-
occupation profiles stay fixed. The random assignments deliberately
break the published semantic groupings and IWA nesting, so this is a
size-preserving partition reference, not an economic null model or an
inferential test.

![Histogram of information about occupation identity under 200 random
DWA group assignments that preserve the observed group sizes. A teal
vertical line marks the observed O\*NET GWA
grouping.](occupational-distinctiveness_files/figure-html/figure-random-hierarchy-1.png)

Observed generalized-activity grouping compared with 200 random
assignments that preserve the number of detailed activities per group.

The observed generalized grouping carries 1.67 bits. The 5th, 50th, and
95th percentiles of the random-partition reference are 1.24, 1.26, 1.29
bits. 200 of the 200 assignments have information no greater than the
observed grouping. This comparison isolates group size under the
specified uniform assignment scheme; it does not establish that the
O\*NET hierarchy is uniquely meaningful or that occupations form natural
families.

### Is the distance more than a fixed-margin shuffle would give?

A fixed-margin reference helps judge the near-ceiling numbers above. The
Curveball algorithm of Strona and colleagues swaps activities between
two occupations while preserving **each occupation’s number of DWAs and
each DWA’s frequency exactly**. The reference here uses twenty
separately restarted chains, each starting from the observed activity
sets. Each chain attempts $20N$ trades, and attempted no-op trades
count. All chains are distinct pseudorandom draws under one seed,
20261025. The resulting sets are aggregated through the same fixed
hierarchy. Draw 0 is the observed archive, and draws 1 to 20 are the
reference draws.

| Source          | Grain | Mean pair min | Mean pair max | Median nearest min | Median nearest max | Draws |
|:----------------|:------|--------------:|--------------:|-------------------:|-------------------:|------:|
| Observed        | DWA   |        0.9865 |        0.9865 |             0.5909 |             0.5909 |     1 |
| Reference draws | DWA   |        0.9862 |        0.9863 |             0.8571 |             0.8621 |    20 |
| Observed        | IWA   |        0.9253 |        0.9253 |             0.4731 |             0.4731 |     1 |
| Reference draws | IWA   |        0.9128 |        0.9145 |             0.6899 |             0.6957 |    20 |
| Observed        | GWA   |        0.7161 |        0.7161 |             0.2863 |             0.2863 |     1 |
| Reference draws | GWA   |        0.6154 |        0.6246 |             0.3333 |             0.3457 |    20 |

![Chart comparing observed values with twenty fixed-margin reference
draws at each grain. The mean pairwise distance at the detailed grain is
about 0.987 observed and about 0.986 in the reference draws, a small
absolute difference with the observed value slightly above all twenty
endpoints. At the generalized grain the observed mean pairwise distance
is higher (0.716 against 0.615 to 0.625) while the median
nearest-neighbor distance is lower. The median nearest-neighbor distance
at the detailed grain is about 0.59 observed against about 0.86 in the
reference draws. The table above gives the exact minimum and maximum
values.](occupational-distinctiveness_files/figure-html/figure-null-1.png)

Real O\*NET 30.3 against fixed-margin Curveball reference draws:
pairwise and nearest-neighbor distance at each grain.

At the detailed grain, the observed mean pairwise distance is 0.9865,
compared with 0.9862 to 0.9863 across the twenty reference draws. This
absolute difference is small. At the generalized grain, the observed
mean pairwise distance is 0.7161, compared with 0.6154 to 0.6246. The
observed median nearest distance at the detailed grain is 0.5909,
compared with 0.8571 to 0.8621 across the reference draws. In this
finite reference, observed mean pairwise distances are above, and median
nearest distances below, the corresponding reference ranges at each
grain. This is a descriptive comparison, not an inferential test. The
twenty finite endpoints are not certified as uniform draws, and the
minimum-to-maximum range is not a confidence interval. The shuffled sets
are synthetic activity bundles, not economic counterfactuals. Because
mixing is not certified, the gap is not a proof of a statistical
difference. It also does not prove that the archive has, or lacks,
economic structure.

### Similarity can also increase without work converging

| Change in analysis                           | Why a gap might shrink                                   | Diagnostic                                                      |
|:---------------------------------------------|:---------------------------------------------------------|:----------------------------------------------------------------|
| Replace detailed tasks with broad activities | Distinct tasks acquire the same label                    | Hold weights fixed and compare DWA, IWA, and GWA distances      |
| Keep only common skills                      | Shared descriptors crowd out specialized knowledge       | Report domain-specific distances and dropped-feature coverage   |
| Normalize each profile                       | Absolute requirement levels disappear                    | Retain raw scale-level gaps                                     |
| Average occupations into industries          | Different occupational mixtures yield similar means      | Separate within-occupation profiles from employment composition |
| Change the reference universe or refit IDF   | Rarity changes even if no activity changes               | Score both samples with the same reference IDF                  |
| Drop missing or suppressed observations      | The most distinctive observations may leave the sample   | Show exclusions and common-support results                      |
| Compare adjacent database releases           | Copied records can look stable without fresh measurement | Use source dates, resurvey indicators, and taxonomy seam flags  |

For industry comparisons, “common across occupations” is not “common
among workers.” Use employment weights to calculate a separate
worker-weighted prevalence, and show coverage and the SOC bridge. Do not
describe linked task counts or Relevance ratings as worker time shares.
An industry-level result also needs an occupation-by-industry employment
panel, not just national occupational employment.

For changes over time, freeze a baseline IDF to measure changes in
occupational profiles on a common scale. Refit IDF separately as a
sensitivity analysis of changing rarity. Keep taxonomy and vocabulary
mappings visible. See [Resurvey Cycles and Content
Change](https://farach.github.io/onet2r/articles/resurvey-and-content-change.md)
and [Change
Decomposition](https://farach.github.io/onet2r/articles/within-between-decomposition.md).
A descriptive convergence result does not identify whether technology,
training, regulation, or organizational change caused it. Those
explanations need additional outcomes, timing, and an identification
strategy.

## Run the analysis on a local archive

The following is deliberately **not evaluated** during article builds.
Download the public 30.3 text archive separately and point
`archive_path` at the ZIP or its extracted directory. No API key is
required. Keep the source receipt and digest with the analysis. The
tables used here are not the entire published occupation taxonomy, so
define the analytic universe from documented coverage.

``` r
archive_path <- "path/to/db_30_3_text.zip"
full_tasks <- onet_archive_read(
  "30.3", "Task Statements", path = archive_path
)
full_links <- onet_archive_read(
  "30.3", "Tasks to DWAs", path = archive_path
)
full_hierarchy <- onet_archive_reference(
  "30.3", "GWAs to IWAs to DWAs", path = archive_path
)
occupation_names <- onet_archive_read(
  "30.3", "Occupation Data", path = archive_path
) |>
  distinct(onet_soc_code, title)
full_names <- full_hierarchy |>
  distinct(dwa_element_id, dwa_element_name)

# Baseline: any linked task, including Supplemental tasks.
# Optional Core-only sensitivity: uncomment this filter and replace `full_links`
# with `core_links` below. This shrinks the occupation universe, so recalculate
# prevalence and all rankings within the reduced universe.
# core_task_keys <- full_tasks |>
#   filter(task_type == "Core") |>
#   distinct(onet_soc_code, task_id)
# core_links <- full_links |>
#   semi_join(core_task_keys, by = c("onet_soc_code", "task_id"))
full_profiles <- full_links |>
  distinct(onet_soc_code, dwa_element_id) |>
  transmute(occupation = onet_soc_code, item = dwa_element_id, weight = 1)
full_universe <- sort(unique(full_profiles$occupation))
task_coverage <- full_tasks |>
  distinct(onet_soc_code, task_id) |>
  left_join(
    full_links |>
      distinct(onet_soc_code, task_id) |>
      mutate(has_dwa = TRUE),
    by = c("onet_soc_code", "task_id"),
    relationship = "one-to-one"
  ) |>
  summarise(
    n_tasks = n(), linked_tasks = sum(!is.na(has_dwa)),
    .by = onet_soc_code
  )
print(task_coverage)
print(anti_join(
  full_tasks |> distinct(onet_soc_code),
  full_profiles |> distinct(onet_soc_code = occupation),
  by = "onet_soc_code"
))
full_scores <- profile_tfidf(full_profiles, full_universe)
full_scores |>
  distinct(item, n_occupations, prevalence, idf) |>
  left_join(
    full_names, by = c("item" = "dwa_element_id"),
    relationship = "one-to-one"
  ) |>
  arrange(desc(prevalence), item) |>
  print(n = 20)
full_scores |>
  summarise(mean_idf = sum(tf_idf), .by = occupation) |>
  left_join(
    occupation_names, by = c("occupation" = "onet_soc_code"),
    relationship = "one-to-one"
  ) |>
  arrange(desc(mean_idf), occupation) |>
  print(n = 20)

experience <- onet_archive_read(
  "30.3", "Training and Experience", path = archive_path
)
experience_categories <- onet_archive_reference(
  "30.3", "Training and Experience Categories", path = archive_path
)
# Audit missing/suppressed values and category completeness before normalization.
experience |>
  filter(scale_id == "RW") |>
  select(onet_soc_code, element_id, scale_id, category, data_value,
         recommend_suppress) |>
  left_join(
    experience_categories |>
      select(element_id, scale_id, category, category_description),
    by = c("element_id", "scale_id", "category"),
    relationship = "many-to-one"
  )
```

For a production analysis, retain a manifest with release, source
digest, taxonomy, inclusion rule, mapping grain, weight scale,
suppression treatment, occupation universe, and IDF reference. Check
coverage separately for each domain, then use common support for
cross-domain comparisons. In rating tables, absence means missing unless
you have evidence it means zero.

The full-archive ranking above is conditional on its vocabulary,
comparison universe, and weighting decisions. Skills and software have
separate empirical results; education, experience and knowledge do not
acquire a national ranking from these comparisons.

### Reproduce the bundled snapshot

The snapshot was generated on 2026-10-25 from
<https://www.onetcenter.org/dl_files/database/db_30_3_text.zip>. The
archive SHA-256 is:

    #> 7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780

From a source checkout with the current package installed, run these
three commands, each with the same local ZIP argument:

    Rscript data-raw/occupational-distinctiveness.R path/to/db_30_3_text.zip
    Rscript data-raw/occupational-information.R path/to/db_30_3_text.zip
    Rscript data-raw/occupational-null.R path/to/db_30_3_text.zip

They regenerate the derived CSVs under
`inst/extdata/occupational-distinctiveness/`. The generators record
coverage and provenance, check task-link integrity and mass-preserving
coarsening, and check the archive digest. The `task-inventory.csv` file
carries task-vintage and inventory metadata. The raw archive is not
bundled. Source date and identifier changes across releases still
require separate review.

For the employment, skills and common-support extensions, run:

    Rscript data-raw\occupational-skills.R path\to\db_30_3_text.zip
    Rscript data-raw\occupational-employment.R path\to\db_30_3_text.zip path\to\oesm25nat.zip path\to\occupation_definitions_m2025.xlsx
    Rscript data-raw\occupational-cross-domain.R path\to\db_30_3_text.zip

The employment script checks all three input digests. The national ZIP
comes from the [BLS OEWS tables
page](https://www.bls.gov/oes/tables.htm); the [May 2025 definitions
workbook](https://www.bls.gov/oes/occupation_definitions_m2025.xlsx)
provides the combined-cell constituents. BLS may reject scripted
downloads. The input receipts record how the pinned bytes were
retrieved. The extension README and decision ledger retain source gaps,
including the unverified ownership-code interpretation and unread Survey
Methods PDF. Neither an ownership comparison nor propagation of OEWS
sampling error is claimed.

The largest derived tables use lossless gzip compression (`.csv.gz`).
Base R reads them with
[`read.csv()`](https://rdrr.io/r/utils/read.table.html); compression
changes storage, not the rows or the analysis.

## Sources and scope

This article includes information derived from the [O\*NET 30.3
Database](https://www.onetcenter.org/db_releases.html) by the U.S.
Department of Labor, Employment and Training Administration (USDOL/ETA),
used under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
O\*NET is a trademark of USDOL/ETA. onet2r has modified the information
by calculating the frequencies, scores, distances, information-loss
accounting, fixed-margin randomizations, and coverage summaries
described above. USDOL/ETA has not approved, endorsed, or tested these
modifications.

The data representations above follow the O\*NET Resource Center’s
[Tasks to DWAs
dictionary](https://www.onetcenter.org/dictionary/30.3/excel/tasks_to_dwas.html),
[activity
hierarchy](https://www.onetcenter.org/dictionary/30.3/excel/gwas_to_iwas_to_dwas.html),
[Essential Skills
dictionary](https://www.onetcenter.org/dictionary/30.3/excel/essential_skills.html),
[Transferable Skills
dictionary](https://www.onetcenter.org/dictionary/30.3/excel/transferable_skills.html),
[scale
definitions](https://www.onetcenter.org/dictionary/30.3/excel/scales_reference.html),
[Training and Experience
dictionary](https://www.onetcenter.org/dictionary/30.3/excel/training_and_experience.html),
and [experience category
definitions](https://www.onetcenter.org/dictionary/30.3/excel/training_and_experience_categories.html).
Verified schema facts are recorded in `docs/DATA_NOTES.md`.

TF-IDF is adapted from information retrieval. For the underlying
distinction between term frequency and document frequency, see Manning,
Raghavan, and Schuetze, *Introduction to Information Retrieval*, section
6.2, [Term frequency and
weighting](https://nlp.stanford.edu/IR-book/html/htmledition/term-frequency-and-weighting-1.html).
The occupation-level summary, distances, and weighting rules here are
explicit analytical choices, not measures validated by that reference.

The information-loss budget borrows the relevance-target idea from
Tishby, Pereira, and Bialek, [The information bottleneck
method](https://arxiv.org/html/physics/0004057v1). We do not fit the
bottleneck algorithm, and that reference does not validate the budget as
a measure of occupational distinctiveness. The fixed-margin reference
uses the Curveball algorithm: Strona et al. (2014), “A fast and unbiased
procedure to randomize ecological binary matrices with fixed row and
column totals,” *Nature Communications* 5:4114,
[doi:10.1038/ncomms5114](https://doi.org/10.1038/ncomms5114).
