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
    universe = tasks$task_id
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

test_that("onet_robustness_diagnostic compares scenarios to a baseline", {
  results <- tibble::tibble(
    scenario = c("baseline", "alt_weights", "alt_release"),
    aggregate = c(0.4, 0.45, 0.35)
  )

  diagnostic <- onet_robustness_diagnostic(results)

  expect_equal(diagnostic$baseline_aggregate, c(0.4, 0.4, 0.4))
  expect_equal(diagnostic$movement, c(0, 0.05, -0.05), tolerance = 1e-8)
})
