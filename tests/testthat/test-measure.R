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

  expect_equal(result$aggregate, 0.325)
  expect_equal(result$covered_employment, 400)
  expect_equal(attr(result, "provenance")$measure_id, "m1")
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
