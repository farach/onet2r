showcase_release <- function(version, date, values, source_dates) {
  tibble::tibble(
    release_version = version,
    release_date = as.Date(date),
    soc_vintage = "2019",
    onet_soc_code = rep(c("15-1252.00", "29-1141.00"), each = 2),
    soc_code = rep(c("15-1252", "29-1141"), each = 2),
    title = rep(c("Software Developers", "Registered Nurses"), each = 2),
    task_id = as.character(1:4),
    task = c("Design systems", "Review code", "Assess patients", "Document care"),
    scale_id = "IM",
    data_value = values,
    source_date = as.Date(source_dates),
    domain_source = "Incumbent",
    recommend_suppress = "N"
  )
}

showcase_inputs <- function() {
  ratings_302 <- showcase_release(
    "30.2",
    "2026-02-01",
    c(4, 2, 1, 3),
    rep("2025-01-01", 4)
  )
  ratings_303 <- showcase_release(
    "30.3",
    "2026-05-01",
    c(2, 4, 3, 1),
    c("2026-03-01", "2026-03-01", "2025-01-01", "2025-01-01")
  )
  metadata <- tibble::tibble(
    task_id = as.character(1:4),
    task_type = "Core"
  )
  weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    year = 2025L,
    employment = c(100, 300),
    weight_share = c(0.25, 0.75),
    source = "synthetic showcase fixture",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
  list(
    ratings_302 = ratings_302,
    ratings_303 = ratings_303,
    metadata = metadata,
    weights = weights
  )
}

test_that("public APIs implement the installed showcase contract", {
  inputs <- showcase_inputs()
  panel <- dplyr::bind_rows(inputs$ratings_302, inputs$ratings_303)

  resurvey <- onet_resurvey_panel(panel)
  conditioned <- onet_condition_on_resurvey(resurvey)
  content <- onet_content_change(panel)

  expect_s3_class(resurvey, "onet_resurvey_panel")
  expect_equal(nrow(resurvey), 8L)
  expect_equal(
    conditioned$at_risk[
      conditioned$release_version == "30.3" &
        conditioned$onet_soc_code == "15-1252.00"
    ],
    c(TRUE, TRUE)
  )
  expect_equal(
    conditioned$selection_reason[
      conditioned$release_version == "30.3" &
        conditioned$onet_soc_code == "29-1141.00"
    ],
    factor(
      c("unrevisited", "unrevisited"),
      levels = levels(conditioned$selection_reason)
    )
  )
  expect_s3_class(content, "tbl_df")
  expect_equal(unique(content$from_release), "30.2")
  expect_equal(unique(content$to_release), "30.3")
  expect_equal(content$n_retained, c(2L, 2L))
  expect_equal(content$n_added, c(0L, 0L))
  expect_equal(content$n_dropped, c(0L, 0L))
  expect_equal(content$rating_delta_l2, c(sqrt(8), sqrt(8)))

  eloundou_path <- tempfile(fileext = ".csv")
  aioe_path <- tempfile(fileext = ".csv")
  withr::defer(unlink(c(eloundou_path, aioe_path)))
  utils::write.csv(
    data.frame(
      `O*NET-SOC Code` = c("15-1252.00", "29-1141.00"),
      human_rating_beta = c(0.6, 0.2),
      check.names = FALSE
    ),
    eloundou_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      `SOC Code` = c("15-1252", "29-1141"),
      AIOE = c(1.1, -0.3),
      check.names = FALSE
    ),
    aioe_path,
    row.names = FALSE
  )

  task_pairs <- inputs$ratings_302[c("onet_soc_code", "task_id")]
  eloundou <- onet_import_eloundou(task_pairs, path = eloundou_path)
  aioe <- onet_import_felten_aioe(task_pairs, path = aioe_path)

  expect_s3_class(eloundou, "onet_measure")
  expect_s3_class(aioe, "onet_measure")
  expect_equal(eloundou$metadata$key_type, "task")
  expect_equal(aioe$metadata$key_type, "task")
  expect_equal(eloundou$data$measure_score, c(0.6, 0.6, 0.2, 0.2))
  expect_equal(aioe$data$measure_score, c(1.1, 1.1, -0.3, -0.3))

  targeted <- onet_measure(
    inputs$ratings_302,
    items = c("1", "3"),
    agg = "targeted",
    release_version = "30.2",
    measure_id = "stylized_targeted"
  )
  aggregate_measure <- onet_measure(
    inputs$ratings_302,
    agg = "aggregate",
    release_version = "30.2",
    measure_id = "stylized_aggregate"
  )

  expect_s3_class(targeted, "onet_measure")
  expect_s3_class(aggregate_measure, "onet_measure")
  expect_equal(targeted$metadata$key_type, "task")
  expect_setequal(targeted$data$measure_key, c("1", "3"))
  expect_equal(nrow(aggregate_measure$data), 4L)

  occupation <- onet_task_to_occupation(
    aggregate_measure,
    task_ratings = inputs$ratings_302,
    task_metadata = inputs$metadata,
    weight_scale = "IM"
  )
  national <- onet_measure_aggregate(occupation, inputs$weights)

  expect_s3_class(occupation, "tbl_df")
  expect_equal(occupation$measure_release, rep("30.2", 2))
  expect_equal(occupation$measure_score, c(10 / 3, 2.5))
  expect_s3_class(national, "onet_aggregate")
  expect_equal(national$aggregate, 65 / 24)

  sensitivity <- onet_measure_sensitivity(
    aggregate_measure,
    weight_panels = list(national = inputs$weights),
    task_ratings = list(
      release_302 = inputs$ratings_302,
      release_303 = inputs$ratings_303
    ),
    task_metadata = list(
      release_302 = inputs$metadata,
      release_303 = inputs$metadata
    ),
    weight_scale = "IM"
  )

  expect_s3_class(sensitivity, "onet_sensitivity")
  expect_equal(sensitivity$task_release, c("30.2", "30.3"))
  expect_equal(sensitivity$aggregate, c(65 / 24, 43 / 24))
  expect_equal(sensitivity$movement, c(0, -11 / 12))
  expect_named(
    sensitivity,
    c(
      "scenario", "measure_id", "task_release", "soc_vintage",
      "weight_panel", "bridge", "weight_scale", "include_supplemental",
      "aggregate", "total_employment", "covered_employment",
      "employment_coverage_share", "n_occupations", "n_reference_soc",
      "coverage", "provenance", "baseline_scenario", "baseline_aggregate",
      "movement", "movement_percent"
    )
  )

  expect_error(
    onet_measure_sensitivity(
      aggregate_measure,
      weight_panels = content,
      task_ratings = list(release_302 = inputs$ratings_302),
      task_metadata = list(release_302 = inputs$metadata),
      weight_scale = "IM"
    ),
    "employment weight panels, not content-change output"
  )
})

test_that("grain guards fail for duplicate orderings and mixed releases", {
  inputs <- showcase_inputs()
  panel <- dplyr::bind_rows(inputs$ratings_302, inputs$ratings_303)
  duplicate <- dplyr::bind_rows(panel, panel[1, ])

  for (order in list(seq_len(nrow(duplicate)), rev(seq_len(nrow(duplicate))))) {
    expect_error(
      onet_content_change(duplicate[order, ]),
      "at most one row per occupation, item, release, and scale"
    )
  }

  measure <- onet_measure(
    inputs$ratings_302,
    agg = "aggregate",
    release_version = "30.2"
  )
  expect_error(
    onet_task_to_occupation(
      measure,
      task_ratings = panel,
      task_metadata = inputs$metadata,
      weight_scale = "IM"
    ),
    "exactly one non-missing release"
  )
})
