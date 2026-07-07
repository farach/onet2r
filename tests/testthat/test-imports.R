# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# A minimal occupation-level extract in the published Eloundou layout.
write_eloundou_extract <- function(path, sep = ",") {
  df <- data.frame(
    `O*NET-SOC Code` = c("15-1252.00", "29-1141.00", "11-1011.00"),
    Title = c("Software Developers", "Registered Nurses", "Chief Executives"),
    human_rating_alpha = c(0.30, 0.05, 0.10),
    human_rating_beta = c(0.63, 0.14, 0.25),
    dv_rating_beta = c(0.70, 0.20, 0.30),
    check.names = FALSE
  )
  utils::write.table(
    df, path,
    sep = sep, row.names = FALSE, quote = FALSE
  )
  path
}

# A minimal occupation-level extract in the published AIOE layout.
write_aioe_extract <- function(path, sep = ",") {
  df <- data.frame(
    `SOC Code` = c("15-1252", "29-1141", "11-1011"),
    `Occupation Title` = c("Software Developers", "Registered Nurses", "Chief Executives"),
    AIOE = c(1.08, -0.32, 0.44),
    check.names = FALSE
  )
  utils::write.table(
    df, path,
    sep = sep, row.names = FALSE, quote = FALSE
  )
  path
}

# A tiny Task Ratings style panel: two tasks per occupation, RT + Core so it can
# also be fed straight into onet_task_to_occupation().
make_task_panel <- function() {
  tibble::tibble(
    onet_soc_code = rep(c("15-1252.00", "29-1141.00", "11-1011.00"), each = 2),
    task_id = as.character(1:6),
    scale_id = "RT",
    data_value = c(80, 20, 70, 30, 60, 40),
    task_type = "Core"
  )
}

# ---------------------------------------------------------------------------
# onet_import_eloundou()
# ---------------------------------------------------------------------------

test_that("onet_import_eloundou broadcasts occupation scores to task grain", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  panel <- make_task_panel()

  measure <- onet_import_eloundou(panel, path = extract)

  expect_s3_class(measure, "onet_measure")
  expect_equal(measure$metadata$key_type, "task")
  expect_equal(measure$metadata$measure_id, "eloundou_gpt_exposure")
  expect_match(measure$metadata$source, "Eloundou")
  expect_match(measure$metadata$source, "MIT License")
  # Keyed on task id, one row per distinct task in the panel.
  expect_setequal(measure$data$measure_key, as.character(1:6))
  # Every task inherits its occupation's published beta exposure.
  soft_tasks <- measure$data[measure$data$onet_soc_code == "15-1252.00", ]
  expect_equal(unique(soft_tasks$measure_score), 0.63)
  expect_setequal(soft_tasks$measure_key, c("1", "2"))
  # Occupation code is retained alongside the task key.
  expect_true(all(c("onet_soc_code", "task_id") %in% names(measure$data)))
})

test_that("onet_import_eloundou selects an alternate score column", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  measure <- onet_import_eloundou(
    make_task_panel(),
    path = extract,
    score = "dv_rating_beta"
  )
  soft <- measure$data[measure$data$onet_soc_code == "15-1252.00", ]
  expect_equal(unique(soft$measure_score), 0.70)
  expect_equal(measure$metadata$score, "dv_rating_beta")
})

test_that("onet_import_eloundou errors clearly on a missing score column", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  expect_error(
    onet_import_eloundou(make_task_panel(), path = extract, score = "not_a_column"),
    "not_a_column"
  )
})

test_that("onet_import_eloundou detects the key column despite punctuation", {
  df <- data.frame(
    `ONET SOC Code` = c("15-1252.00", "29-1141.00"),
    human_rating_beta = c(0.63, 0.14),
    check.names = FALSE
  )
  extract <- tempfile(fileext = ".csv")
  on.exit(unlink(extract), add = TRUE)
  utils::write.table(df, extract, sep = ",", row.names = FALSE, quote = FALSE)

  panel <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    task_id = c("1", "2")
  )
  measure <- onet_import_eloundou(panel, path = extract)
  expect_setequal(measure$data$measure_key, c("1", "2"))
})

test_that("onet_import_eloundou honors an explicit key argument", {
  df <- data.frame(
    my_code = c("15-1252.00", "29-1141.00"),
    human_rating_beta = c(0.63, 0.14),
    check.names = FALSE
  )
  extract <- tempfile(fileext = ".csv")
  on.exit(unlink(extract), add = TRUE)
  utils::write.table(df, extract, sep = ",", row.names = FALSE, quote = FALSE)

  panel <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    task_id = c("1", "2")
  )
  measure <- onet_import_eloundou(panel, path = extract, key = "my_code")
  expect_setequal(measure$data$measure_key, c("1", "2"))
})

test_that("onet_import_eloundou reads a tab separated extract", {
  extract <- write_eloundou_extract(tempfile(fileext = ".tsv"), sep = "\t")
  on.exit(unlink(extract), add = TRUE)

  measure <- onet_import_eloundou(make_task_panel(), path = extract)
  expect_equal(nrow(measure$data), 6L)
})

test_that("onet_import_eloundou drops tasks whose occupation lacks a score", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  panel <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "15-1252.00", "99-9999.00"),
    task_id = c("1", "2", "3")
  )
  expect_warning(
    measure <- onet_import_eloundou(panel, path = extract),
    "no matching exposure score"
  )
  expect_setequal(measure$data$measure_key, c("1", "2"))
})

test_that("onet_import_eloundou output feeds onet_task_to_occupation", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  panel <- make_task_panel()

  measure <- onet_import_eloundou(panel, path = extract)
  occ <- onet_task_to_occupation(measure, panel)

  expect_s3_class(occ, "tbl_df")
  expect_setequal(
    occ$onet_soc_code,
    c("15-1252.00", "29-1141.00", "11-1011.00")
  )
  # Broadcast means every task shares its occupation's score, so the
  # relevance-weighted occupation score equals that constant.
  soft <- occ[occ$onet_soc_code == "15-1252.00", ]
  expect_equal(soft$measure_score, 0.63)
})

test_that("onet_import_eloundou requires the panel task columns", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  bad_panel <- tibble::tibble(onet_soc_code = c("15-1252.00", "29-1141.00"))
  expect_error(
    onet_import_eloundou(bad_panel, path = extract),
    "task_id"
  )
})

# ---------------------------------------------------------------------------
# onet_import_felten_aioe()
# ---------------------------------------------------------------------------

test_that("onet_import_felten_aioe broadcasts AIOE scores to task grain", {
  extract <- write_aioe_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  panel <- make_task_panel()

  measure <- onet_import_felten_aioe(panel, path = extract)

  expect_s3_class(measure, "onet_measure")
  expect_equal(measure$metadata$key_type, "task")
  expect_equal(measure$metadata$measure_id, "felten_aioe")
  expect_match(measure$metadata$source, "Felten")
  # Keyed on task id, broadcast from the 6-digit SOC join.
  expect_setequal(measure$data$measure_key, as.character(1:6))
  soft <- measure$data[measure$data$onet_soc_code == "15-1252.00", ]
  expect_equal(unique(soft$measure_score), 1.08)
  # Both the 8-digit occupation code and the derived 6-digit SOC are retained.
  expect_true(all(c("onet_soc_code", "soc_code", "task_id") %in% names(measure$data)))
})

test_that("onet_import_felten_aioe errors clearly on a missing score column", {
  extract <- write_aioe_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  expect_error(
    onet_import_felten_aioe(make_task_panel(), path = extract, score = "not_a_column"),
    "not_a_column"
  )
})

test_that("onet_import_felten_aioe reads an Excel workbook sheet", {
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  extract <- tempfile(fileext = ".xlsx")
  on.exit(unlink(extract), add = TRUE)
  writexl::write_xlsx(
    list(`Appendix A` = data.frame(
      `SOC Code` = c("15-1252", "29-1141"),
      `Occupation Title` = c("Software Developers", "Registered Nurses"),
      AIOE = c(1.08, -0.32),
      check.names = FALSE
    )),
    extract
  )

  panel <- tibble::tibble(
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    task_id = c("1", "2")
  )
  measure <- onet_import_felten_aioe(panel, path = extract, sheet = "Appendix A")
  expect_setequal(measure$data$measure_key, c("1", "2"))
  soft <- measure$data[measure$data$onet_soc_code == "15-1252.00", ]
  expect_equal(soft$measure_score, 1.08)
})

test_that("onet_import_felten_aioe output feeds onet_task_to_occupation", {
  extract <- write_aioe_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  panel <- make_task_panel()

  measure <- onet_import_felten_aioe(panel, path = extract)
  occ <- onet_task_to_occupation(measure, panel)

  expect_s3_class(occ, "tbl_df")
  soft <- occ[occ$onet_soc_code == "15-1252.00", ]
  expect_equal(soft$measure_score, 1.08)
})

# ---------------------------------------------------------------------------
# Shared adapter validation
# ---------------------------------------------------------------------------

test_that("import adapters require a path or url and never bundle data", {
  panel <- make_task_panel()
  expect_error(
    onet_import_eloundou(panel, path = NULL, url = NULL),
    "never bundles"
  )
  expect_error(
    onet_import_felten_aioe(panel, path = NULL, url = NULL),
    "never bundles"
  )
})

test_that("import adapters reject a nonexistent path", {
  missing <- file.path(tempdir(), "does-not-exist-onet2r.csv")
  expect_error(
    onet_import_eloundou(make_task_panel(), path = missing),
    "does not exist"
  )
})

test_that("import adapters validate the force flag", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  expect_error(
    onet_import_eloundou(make_task_panel(), path = extract, force = "yes"),
    "force"
  )
})
