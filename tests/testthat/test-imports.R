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

test_that("onet_import_eloundou verifies and attaches a local source receipt", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  digest <- onet2r:::onet_sha256(extract)

  measure <- onet_import_eloundou(
    make_task_panel(),
    path = extract,
    expected_sha256 = digest,
    as_of = as.Date("2023-03-01")
  )
  receipt <- measure$metadata$source_receipt

  expect_named(
    receipt,
    c(
      "source_url", "source_path", "source_commit", "retrieved_at",
      "expected_sha256", "actual_sha256", "file_size", "version", "as_of"
    )
  )
  expect_equal(receipt$source_url, NA_character_)
  expect_equal(receipt$source_path, normalizePath(extract, winslash = "\\"))
  expect_equal(receipt$expected_sha256, digest)
  expect_equal(receipt$actual_sha256, digest)
  expect_equal(receipt$file_size, unname(file.info(extract)$size))
  expect_equal(receipt$as_of, "2023-03-01")
  expect_s3_class(receipt$retrieved_at, "POSIXct")
  expect_named(measure, c("data", "coverage", "unmatched", "metadata"))
  expect_named(
    measure$data,
    c(
      "onet_soc_code", "task_id", "human_rating_beta",
      "measure_key", "measure_score"
    )
  )
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

test_that("onet_import_felten_aioe verifies and attaches a local source receipt", {
  extract <- write_aioe_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  digest <- onet2r:::onet_sha256(extract)

  measure <- onet_import_felten_aioe(
    make_task_panel(),
    path = extract,
    expected_sha256 = digest,
    as_of = "2021"
  )
  receipt <- measure$metadata$source_receipt

  expect_equal(receipt$source_path, normalizePath(extract, winslash = "\\"))
  expect_equal(receipt$actual_sha256, digest)
  expect_equal(receipt$as_of, "2021")
  expect_named(measure, c("data", "coverage", "unmatched", "metadata"))
  expect_named(
    measure$data,
    c(
      "onet_soc_code", "task_id", "soc_code", "AIOE",
      "measure_key", "measure_score"
    )
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

test_that("import adapters reject local digest mismatches before parsing", {
  bad <- tempfile(fileext = ".csv")
  on.exit(unlink(bad), add = TRUE)
  writeLines("not,a,valid,adapter,file", bad)
  wrong_digest <- strrep("0", 64)

  expect_error(
    onet_import_eloundou(
      make_task_panel(),
      path = bad,
      expected_sha256 = wrong_digest
    ),
    "SHA-256 digest mismatch"
  )
  expect_error(
    onet_import_felten_aioe(
      make_task_panel(),
      path = bad,
      expected_sha256 = wrong_digest
    ),
    "SHA-256 digest mismatch"
  )
})

test_that("downloaded adapter sources reuse verified receipts without network", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir)
  dest <- file.path(reference_dir, basename(onet2r:::onet_eloundou_url))
  file.copy(extract, dest)
  digest <- onet2r:::onet_sha256(dest)

  local_mocked_bindings(
    onet_cache_dir = function() cache_dir,
    .package = "onet2r"
  )

  measure <- onet_import_eloundou(
    make_task_panel(),
    expected_sha256 = digest,
    as_of = "2023-03"
  )
  receipt <- measure$metadata$source_receipt

  expect_equal(receipt$source_url, onet2r:::onet_eloundou_url)
  expect_equal(
    receipt$source_commit,
    "0471612fef3cc22b74fb884d27bff9dbd3770582"
  )
  expect_equal(receipt$version, receipt$source_commit)
  expect_equal(receipt$actual_sha256, digest)
  expect_equal(receipt$as_of, "2023-03")
  expect_equal(file.exists(paste0(dest, ".receipt.rds")), TRUE)

  expect_error(
    onet_import_eloundou(
      make_task_panel(),
      url = "https://example.invalid/occ_level.csv",
      expected_sha256 = digest,
      as_of = "2023-03"
    ),
    "provenance does not match.*force = TRUE"
  )
})

test_that("legacy adapter caches do not claim unverified source provenance", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir)
  dest <- file.path(reference_dir, basename(onet2r:::onet_eloundou_url))
  file.copy(extract, dest)

  local_mocked_bindings(
    onet_cache_dir = function() cache_dir,
    .package = "onet2r"
  )

  measure <- onet_import_eloundou(make_task_panel())
  receipt <- measure$metadata$source_receipt

  expect_true(is.na(receipt$source_url))
  expect_true(is.na(receipt$source_commit))
  expect_true(is.na(receipt$version))
  expect_true(is.na(receipt$as_of))
  expect_equal(receipt$actual_sha256, onet2r:::onet_sha256(dest))
})
