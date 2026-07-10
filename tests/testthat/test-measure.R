test_that("onet_measure validates keys and reports coverage", {
  scores <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    score = c(0.7, 0.2)
  )
  weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141", "11-1011"),
    year = 2024L,
    employment = c(100, 300, 100),
    weight_share = c(0.2, 0.6, 0.2),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )

  measure <- onet_measure(
    scores,
    key = "onet_soc_code",
    score = "score",
    universe = c("15-1252.00", "29-1141.00", "11-1011.00"),
    measure_id = "example_exposure",
    weight_panel = weights
  )

  coverage <- onet_measure_coverage(measure)
  expect_s3_class(measure, "onet_measure")
  expect_equal(coverage$n_input, 2L)
  expect_equal(coverage$n_universe, 3L)
  expect_equal(coverage$coverage_share, 2 / 3)
  expect_equal(coverage$employment_coverage_share, 0.8)
})

test_that("onet_measure rejects duplicate measure keys", {
  scores <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "15-1252.00"),
    score = c(0.7, 0.8)
  )

  expect_error(
    onet_measure(scores, "onet_soc_code", "score"),
    "duplicate keys"
  )
})

test_that("onet_task_to_occupation rolls task measures to occupations", {
  archive_dir <- system.file(
    "extdata",
    "onet-mini",
    "db_30_3_text",
    package = "onet2r"
  )
  tasks <- onet_archive_read(
    "30.3",
    "Task Statements",
    path = archive_dir,
    release_date = "2026-05-01"
  )
  ratings <- onet_archive_read(
    "30.3",
    "Task Ratings",
    path = archive_dir,
    release_date = "2026-05-01"
  )
  task_scores <- tibble::tibble(
    task_id = c("1001", "1002", "2001"),
    score = c(0.8, 0.4, 0.2)
  )
  measure <- onet_measure(
    task_scores,
    key = "task_id",
    score = "score",
    key_type = "task",
    universe = tasks$task_id,
    measure_id = "task_score",
    release_version = "30.3"
  )

  result <- onet_task_to_occupation(
    measure,
    task_ratings = ratings,
    task_metadata = tasks,
    include_supplemental = FALSE
  )

  expect_equal(result$onet_soc_code, c("15-1252.00", "29-1141.00"))
  expect_equal(result$measure_score, c(0.8, 0.2))
  expect_equal(result$n_tasks, c(1L, 1L))
  expect_equal(result$measure_id, c("task_score", "task_score"))
  expect_equal(result$measure_release, c("30.3", "30.3"))
})

test_that("onet_task_to_occupation preserves valid single-release output", {
  measure <- onet_measure(
    tibble::tibble(task_key = c("1", "2"), score = c(0.1, 0.9)),
    key = "task_key",
    score = "score",
    key_type = "task"
  )
  ratings <- tibble::tibble(
    occupation = c("15-1252.00", "15-1252.00"),
    task_key = c("1", "2"),
    rating_scale = "RT",
    rating_value = c(90, 10),
    type = "Core"
  )

  expected <- onet_task_to_occupation(
    measure,
    ratings,
    occupation_code = "occupation",
    task_id = "task_key",
    task_type = "type",
    scale_id = "rating_scale",
    value = "rating_value"
  )
  ratings$version <- "30.3"

  expect_identical(
    onet_task_to_occupation(
      measure,
      ratings,
      occupation_code = "occupation",
      task_id = "task_key",
      task_type = "type",
      scale_id = "rating_scale",
      value = "rating_value"
    ),
    expected
  )
})

test_that("onet_task_to_occupation rejects multiple releases", {
  measure <- onet_measure(
    tibble::tibble(task_id = "1", score = 0.1),
    key = "task_id",
    score = "score",
    key_type = "task"
  )
  ratings <- tibble::tibble(
    onet_soc_code = "15-1252.00",
    task_id = "1",
    scale_id = "RT",
    data_value = c(90, 10),
    task_type = "Core",
    release_version = c("29.0", "30.0")
  )

  expect_snapshot(
    error = TRUE,
    onet_task_to_occupation(measure, ratings)
  )
})

test_that("onet_task_to_occupation rejects duplicate rows within a release", {
  measure <- onet_measure(
    tibble::tibble(task_id = "1", score = 0.1),
    key = "task_id",
    score = "score",
    key_type = "task"
  )
  ratings <- tibble::tibble(
    onet_soc_code = "15-1252.00",
    task_id = "1",
    scale_id = "RT",
    data_value = c(90, 90),
    task_type = "Core",
    release_version = "30.0"
  )

  expect_snapshot(
    error = TRUE,
    onet_task_to_occupation(measure, ratings)
  )
})

test_that("onet_task_to_occupation rejects unsupported task scales", {
  measure <- onet_measure(
    tibble::tibble(task_id = "1001", score = 0.8),
    key = "task_id",
    score = "score",
    key_type = "task"
  )
  ratings <- tibble::tibble(
    onet_soc_code = "15-1252.00",
    task_id = "1001",
    scale_id = "FT",
    category = 1L,
    data_value = 80,
    task_type = "Core"
  )

  expect_error(
    onet_task_to_occupation(measure, ratings, weight_scale = "FT"),
    "RT"
  )
})

test_that("onet_measure_aggregate computes weighted aggregates with provenance", {
  scores <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    score = c(0.7, 0.2)
  )
  weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    year = 2024L,
    employment = c(100, 300),
    weight_share = c(0.25, 0.75),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
  measure <- onet_measure(scores, "onet_soc_code", "score", measure_id = "m1")

  result <- onet_measure_aggregate(measure, weights)

  expect_s3_class(result, "onet_aggregate")
  expect_equal(result$aggregate, 0.325)
  expect_equal(result$covered_employment, 400)
  expect_equal(onet_provenance(result)$measure_id, "m1")
  expect_equal(onet_coverage(result)$covered_employment, 400)
})

test_that("onet_measure_aggregate reports largest unmatched weight-panel SOCs", {
  measure <- onet_measure(
    tibble::tibble(onet_soc_code = "11-1011.00", score = 0.5),
    key = "onet_soc_code",
    score = "score"
  )
  weights <- tibble::tibble(
    reference_soc_code = c("11-1011", "55-1011"),
    year = 2024L,
    employment = c(100, 900),
    weight_share = c(0.1, 0.9),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )

  expect_message(
    onet_measure_aggregate(measure, weights),
    "55-1011"
  )
})

test_that("onet_measure_aggregate collapses detail codes before weighting", {
  scores <- tibble::tibble(
    onet_soc_code = c("15-1299.01", "15-1299.02", "29-1141.00"),
    score = c(0, 1, 0.2)
  )
  weights <- tibble::tibble(
    reference_soc_code = c("15-1299", "29-1141"),
    year = 2024L,
    employment = c(100, 300),
    weight_share = c(0.25, 0.75),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
  measure <- onet_measure(
    scores,
    "onet_soc_code",
    "score",
    measure_id = "detail_test",
    release_version = "30.3"
  )

  result <- onet_measure_aggregate(measure, weights)

  expect_equal(result$aggregate, 0.275)
  expect_equal(result$covered_employment, 400)
  expect_equal(result$employment_coverage_share, 1)
  expect_equal(onet_provenance(result)$measure_release, "30.3")
})

test_that("onet_measure_aggregate keeps provenance from rollup data frames", {
  scores <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    measure_score = c(0.7, 0.2),
    measure_id = c("task_score", "task_score"),
    measure_release = c("30.3", "30.3")
  )
  weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    year = 2024L,
    employment = c(100, 300),
    weight_share = c(0.25, 0.75),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )

  result <- onet_measure_aggregate(scores, weights)

  expect_equal(result$measure_id, "task_score")
  expect_equal(onet_provenance(result)$measure_release, "30.3")
})

test_that("onet_measure_aggregate requires a single weight period and cell", {
  scores <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    score = c(0.7, 0.2)
  )
  weights <- tibble::tibble(
    reference_soc_code = rep(c("15-1252", "29-1141"), 2),
    year = c(2024L, 2024L, 2025L, 2025L),
    state = rep("US", 4),
    employment = c(100, 300, 120, 280),
    weight_share = c(0.25, 0.75, 0.3, 0.7),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
  measure <- onet_measure(scores, "onet_soc_code", "score", measure_id = "m1")

  expect_error(onet_measure_aggregate(measure, weights), "exactly one year")
  result <- onet_measure_aggregate(measure, weights, year = 2024, cell = list(state = "US"))
  expect_equal(result$aggregate, 0.325)

  multi_cell_weights <- tibble::tibble(
    reference_soc_code = rep(c("15-1252", "29-1141"), 2),
    year = 2024L,
    state = rep(c("US", "WA"), each = 2),
    employment = c(100, 300, 40, 60),
    weight_share = c(0.2, 0.6, 0.08, 0.12),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
  expect_error(
    onet_measure_aggregate(measure, multi_cell_weights),
    "exactly one cell"
  )
  cell_result <- onet_measure_aggregate(
    measure,
    multi_cell_weights,
    cell = list(state = "WA")
  )
  expect_equal(cell_result$aggregate, 0.4)

  missing_year_weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141", "11-1011"),
    year = c(2024L, 2024L, NA_integer_),
    state = "US",
    employment = c(100, 300, 1000),
    weight_share = c(0.071, 0.214, 0.715),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
  expect_error(
    onet_measure_aggregate(measure, missing_year_weights, cell = list(state = "US")),
    "missing years"
  )
  filtered <- onet_measure_aggregate(
    measure,
    missing_year_weights,
    year = 2024,
    cell = list(state = "US")
  )
  expect_equal(filtered$covered_employment, 400)
})

test_that("onet_measure_sensitivity compares occupation plumbing scenarios", {
  scores <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    score = c(0.7, 0.2)
  )
  measure <- onet_measure(scores, "onet_soc_code", "score", measure_id = "m1")
  base_weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    year = 2024L,
    employment = c(100, 300),
    weight_share = c(0.25, 0.75),
    source = "OEWS",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
  even_weights <- base_weights
  even_weights$employment <- c(200, 200)
  even_weights$weight_share <- c(0.5, 0.5)
  even_weights$source <- "PUMS"

  result <- onet_measure_sensitivity(
    measure,
    weight_panels = list(oews = base_weights, pums = even_weights)
  )

  expect_s3_class(result, "onet_sensitivity")
  expect_equal(result$aggregate, c(0.325, 0.45))
  expect_equal(result$movement, c(0, 0.125))
  expect_equal(onet_provenance(result)$weight_source, c("OEWS", "PUMS"))

  coverage <- onet_coverage(result)
  expect_equal(nrow(coverage), 2L)
  expect_equal(coverage$scenario, result$scenario)
  expect_equal(coverage$covered_employment, c(400, 400))
})

test_that("onet_measure_sensitivity compares task handling choices", {
  archive_dir <- system.file(
    "extdata",
    "onet-mini",
    "db_30_3_text",
    package = "onet2r"
  )
  tasks <- onet_archive_read(
    "30.3",
    "Task Statements",
    path = archive_dir,
    release_date = "2026-05-01"
  )
  ratings <- onet_archive_read(
    "30.3",
    "Task Ratings",
    path = archive_dir,
    release_date = "2026-05-01"
  )
  task_scores <- tibble::tibble(
    task_id = c("1001", "1002", "2001"),
    score = c(0.8, 0.4, 0.2)
  )
  measure <- onet_measure(
    task_scores,
    key = "task_id",
    score = "score",
    key_type = "task",
    universe = tasks$task_id,
    measure_id = "stylized_task_score"
  )
  weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    year = 2024L,
    employment = c(100, 300),
    weight_share = c(0.25, 0.75),
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )

  result <- onet_measure_sensitivity(
    measure,
    weight_panels = weights,
    task_ratings = ratings,
    task_metadata = tasks,
    include_supplemental = c(FALSE, TRUE)
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result$include_supplemental, c(FALSE, TRUE))
  expect_equal(result$task_release, c("30.3", "30.3"))
  expect_true(all(result$employment_coverage_share == 1))
})

test_that("onet_measure_sensitivity accepts named single-release task ratings", {
  measure <- onet_measure(
    tibble::tibble(task_id = "1", score = 0.4),
    key = "task_id",
    score = "score",
    key_type = "task"
  )
  ratings <- tibble::tibble(
    onet_soc_code = "15-1252.00",
    task_id = "1",
    scale_id = "RT",
    data_value = 100,
    task_type = "Core"
  )
  ratings_29 <- dplyr::mutate(ratings, release_version = "29.0")
  ratings_30 <- dplyr::mutate(ratings, release_version = "30.0")
  weights <- tibble::tibble(
    reference_soc_code = "15-1252",
    year = 2024L,
    employment = 100,
    weight_share = 1,
    source = "fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )

  result <- onet_measure_sensitivity(
    measure,
    weight_panels = weights,
    task_ratings = list(`29.0` = ratings_29, `30.0` = ratings_30)
  )

  expect_equal(nrow(result), 2L)
  expect_equal(result$task_release, c("29.0", "30.0"))
  expect_equal(result$aggregate, c(0.4, 0.4))
})

test_that("onet_robustness_diagnostic compares scenarios to a baseline", {
  results <- tibble::tibble(
    scenario = c("baseline", "alt_weights", "alt_release"),
    aggregate = c(0.4, 0.45, 0.35)
  )

  diagnostic <- onet_robustness_diagnostic(results)

  expect_equal(diagnostic$baseline_aggregate, c(0.4, 0.4, 0.4))
  expect_equal(diagnostic$movement, c(0, 0.05, -0.05), tolerance = 1e-8)
})

# ---------------------------------------------------------------------------
# onet_measure(items =, agg =) convenience switch
# ---------------------------------------------------------------------------

# Two occupations, three tasks each, all on the IM scale.
targeted_panel <- function() {
  tibble::tibble(
    onet_soc_code = rep(c("15-1252.00", "29-1141.00"), each = 3),
    task_id = c("1", "2", "3", "4", "5", "6"),
    scale_id = "IM",
    data_value = c(4.5, 3.0, 2.0, 1.0, 4.0, 3.0)
  )
}

# The same panel with RT weights and task types added, so a targeted measure
# built from it can also be rolled up with onet_task_to_occupation().
feed_targeted_panel <- function() {
  base <- tibble::tibble(
    onet_soc_code = rep(c("15-1252.00", "29-1141.00"), each = 3),
    task_id = c("1", "2", "3", "4", "5", "6"),
    task_type = "Core"
  )
  dplyr::bind_rows(
    dplyr::mutate(base, scale_id = "IM", data_value = c(4.5, 3.0, 2.0, 1.0, 4.0, 3.0)),
    dplyr::mutate(base, scale_id = "RT", data_value = c(80, 60, 40, 50, 70, 30))
  )
}

test_that("onet_measure builds a targeted task measure from a panel", {
  measure <- onet_measure(
    targeted_panel(),
    items = c("1", "5"),
    agg = "targeted"
  )

  expect_s3_class(measure, "onet_measure")
  expect_equal(measure$metadata$key_type, "task")
  expect_equal(measure$metadata$score, "data_value")

  # Task grain, restricted to the requested items, scored on the IM value.
  expect_setequal(measure$data$measure_key, c("1", "5"))
  task1 <- measure$data[measure$data$measure_key == "1", ]
  task5 <- measure$data[measure$data$measure_key == "5", ]
  expect_equal(task1$measure_score, 4.5)
  expect_equal(task5$measure_score, 4.0)
  # Occupation code rides along for the downstream rollup.
  expect_true("onet_soc_code" %in% names(measure$data))
})

test_that("onet_measure targeted path equals the canonical longhand", {
  panel <- targeted_panel()
  items <- c("1", "5")

  sugar <- onet_measure(panel, items = items, agg = "targeted")
  filtered <- panel[panel$scale_id == "IM" & panel$task_id %in% items, , drop = FALSE]
  canonical <- onet_measure(
    filtered,
    key = "task_id",
    score = "data_value",
    key_type = "task"
  )

  # The switch is exactly the default path on the filtered item subset.
  expect_equal(sugar, canonical)
})

test_that("onet_measure aggregate path keeps every item at task grain", {
  measure <- onet_measure(targeted_panel(), agg = "aggregate")

  expect_equal(measure$metadata$key_type, "task")
  expect_setequal(measure$data$measure_key, as.character(1:6))
  task1 <- measure$data[measure$data$measure_key == "1", ]
  expect_equal(task1$measure_score, 4.5)
})

test_that("onet_measure aggregate path equals the canonical longhand over all items", {
  panel <- targeted_panel()

  sugar <- onet_measure(panel, agg = "aggregate")
  filtered <- panel[panel$scale_id == "IM", , drop = FALSE]
  canonical <- onet_measure(
    filtered,
    key = "task_id",
    score = "data_value",
    key_type = "task"
  )

  expect_equal(sugar, canonical)
})

test_that("onet_measure aggregate path rejects an item restriction", {
  expect_error(
    onet_measure(targeted_panel(), items = c("1", "5"), agg = "aggregate"),
    "aggregate"
  )
})

test_that("onet_measure items path defaults agg to targeted", {
  explicit <- onet_measure(targeted_panel(), items = c("1", "5"), agg = "targeted")
  implied <- onet_measure(targeted_panel(), items = c("1", "5"))
  expect_equal(implied, explicit)
})

test_that("onet_measure targeted output feeds onet_task_to_occupation", {
  panel <- feed_targeted_panel()
  e_tgt <- onet_measure(panel, items = c("1", "5"), agg = "targeted")
  expect_equal(e_tgt$metadata$key_type, "task")

  occ <- onet_task_to_occupation(e_tgt, panel)
  expect_s3_class(occ, "tbl_df")
  expect_setequal(occ$onet_soc_code, c("15-1252.00", "29-1141.00"))
  # Each occupation contributes only its single targeted task, so the
  # relevance-weighted score is that task's importance value.
  soft <- occ[occ$onet_soc_code == "15-1252.00", ]
  expect_equal(soft$measure_score, 4.5)
})

test_that("onet_measure items path filters to the requested scale", {
  panel <- dplyr::bind_rows(
    targeted_panel(),
    tibble::tibble(
      onet_soc_code = "15-1252.00",
      task_id = "1",
      scale_id = "RL",
      data_value = 99
    )
  )
  measure <- onet_measure(panel, items = c("1", "5"), scale = "IM")
  task1 <- measure$data[measure$data$measure_key == "1", ]
  # The out-of-scale RL row is ignored, so task 1 keeps its IM value.
  expect_equal(task1$measure_score, 4.5)
})

test_that("onet_measure targeted path warns on items absent from the panel", {
  expect_warning(
    measure <- onet_measure(targeted_panel(), items = c("1", "999")),
    "not found"
  )
  expect_setequal(measure$data$measure_key, "1")
})

test_that("onet_measure items path rejects an unknown aggregation", {
  expect_error(
    onet_measure(targeted_panel(), items = c("1", "5"), agg = "mean"),
    "agg"
  )
})

test_that("onet_measure targeted path requires a non-empty item set", {
  expect_error(
    onet_measure(targeted_panel(), items = character()),
    "items"
  )
  expect_error(
    onet_measure(targeted_panel(), items = c("1", NA)),
    "items"
  )
})

test_that("onet_measure items path rejects duplicate task keys", {
  panel <- dplyr::bind_rows(
    dplyr::mutate(targeted_panel(), release_version = "25.1"),
    dplyr::mutate(targeted_panel(), release_version = "26.1")
  )
  # A multi-release panel repeats task ids on the scale, which the underlying
  # default path rejects; the switch inherits that guarantee unchanged.
  expect_error(
    onet_measure(panel, items = c("1", "5")),
    "duplicate"
  )
})

test_that("onet_measure items path errors when the scale has no rows", {
  expect_error(
    onet_measure(targeted_panel(), items = c("1", "5"), scale = "PT"),
    "No rows remain"
  )
})

test_that("onet_measure key/score path is unchanged by the new arguments", {
  scores <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    score = c(0.7, 0.2)
  )
  measure <- onet_measure(scores, key = "onet_soc_code", score = "score")
  expect_equal(measure$data$measure_score, c(0.7, 0.2))
  expect_equal(measure$metadata$score, "score")
})
