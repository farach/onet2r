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
      "source_url", "source_url_sha256", "source_path", "source_commit",
      "retrieved_at",
      "expected_sha256", "actual_sha256", "file_size", "version", "as_of",
      "provenance_status"
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
  bad <- file.path(withr::local_tempdir(), "bad.csv")
  writeLines("not,a,valid,adapter,file", bad)
  wrong_digest <- strrep("0", 64)
  snapshot_paths <- character()
  original_copy <- onet2r:::onet_copy_cache_snapshot
  parsed <- FALSE

  local_mocked_bindings(
    onet_copy_cache_snapshot = function(from, to) {
      snapshot_paths <<- c(snapshot_paths, to)
      original_copy(from, to)
    },
    read_import_table = function(...) {
      parsed <<- TRUE
      stop("digest mismatch reached the parser")
    },
    .package = "onet2r"
  )
  conditions <- lapply(
    list(onet_import_eloundou, onet_import_felten_aioe),
    function(import) {
      tryCatch(
        import(
          make_task_panel(),
          path = bad,
          expected_sha256 = wrong_digest
        ),
        error = identity
      )
    }
  )

  for (condition in conditions) {
    expect_s3_class(condition, "onet2r_digest_mismatch")
    expect_match(conditionMessage(condition), "SHA-256 digest mismatch")
  }
  expect_identical(parsed, FALSE)
  expect_length(snapshot_paths, 2L)
  expect_identical(file.exists(snapshot_paths), rep(FALSE, 2L))
})

test_that("local adapters parse the exact bytes recorded by their receipt", {
  extract <- file.path(withr::local_tempdir(), "source.csv")
  write_eloundou_extract(extract)
  verified_digest <- onet2r:::onet_sha256(extract)
  original_reader <- onet2r:::read_import_table
  snapshot_path <- NULL
  parsed_digest <- NULL

  local_mocked_bindings(
    read_import_table = function(file, sheet = NULL) {
      snapshot_path <<- file
      parsed_digest <<- onet2r:::onet_sha256(file)
      utils::write.csv(
        data.frame(
          `O*NET-SOC Code` = c(
            "15-1252.00",
            "29-1141.00",
            "11-1011.00"
          ),
          human_rating_beta = c(0.99, 0.98, 0.97),
          check.names = FALSE
        ),
        extract,
        row.names = FALSE
      )
      original_reader(file, sheet)
    },
    .package = "onet2r"
  )

  measure <- onet_import_eloundou(
    make_task_panel(),
    path = extract,
    expected_sha256 = verified_digest,
    as_of = "2023-03"
  )
  receipt <- measure$metadata$source_receipt
  software <- measure$data$measure_score[
    measure$data$onet_soc_code == "15-1252.00"
  ]

  expect_equal(unique(software), 0.63)
  expect_identical(receipt$actual_sha256, verified_digest)
  expect_identical(receipt$expected_sha256, verified_digest)
  expect_identical(
    receipt$source_path,
    normalizePath(extract, winslash = "\\")
  )
  expect_identical(parsed_digest, verified_digest)
  expect_identical(onet2r:::onet_sha256(extract) == verified_digest, FALSE)
  expect_identical(file.exists(snapshot_path), FALSE)
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
  receipt <- onet2r:::onet_source_receipt(
    dest,
    source_url = onet2r:::onet_eloundou_url,
    expected_sha256 = digest,
    version = onet2r:::onet_source_commit(onet2r:::onet_eloundou_url),
    as_of = "2023-03"
  )
  saveRDS(receipt, paste0(dest, ".receipt.rds"))

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

test_that("adapter parsing consumes bytes from its verified cache snapshot", {
  source_a <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(source_a), add = TRUE)
  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir)
  url <- onet2r:::onet_eloundou_url
  dest <- file.path(reference_dir, basename(url))
  file.copy(source_a, dest)
  receipt_a <- onet2r:::onet_source_receipt(
    dest,
    source_url = url,
    version = onet2r:::onet_source_commit(url)
  )
  saveRDS(receipt_a, paste0(dest, ".receipt.rds"))
  source_b <- tempfile("adapter-b-", tmpdir = reference_dir, fileext = ".csv")
  utils::write.csv(
    data.frame(
      `O*NET-SOC Code` = c("15-1252.00", "29-1141.00", "11-1011.00"),
      human_rating_beta = c(0.99, 0.98, 0.97),
      check.names = FALSE
    ),
    source_b,
    row.names = FALSE
  )
  original_reader <- onet2r:::read_import_table
  snapshot_path <- NULL
  replaced <- FALSE

  local_mocked_bindings(
    onet_cache_dir = function() cache_dir,
    read_import_table = function(file, sheet = NULL) {
      snapshot_path <<- file
      if (!replaced) {
        receipt_b <- onet2r:::onet_source_receipt(
          source_b,
          source_url = url,
          version = onet2r:::onet_source_commit(url)
        )
        onet2r:::onet_atomic_commit_source(source_b, dest, receipt_b)
        replaced <<- TRUE
      }
      original_reader(file, sheet)
    },
    .package = "onet2r"
  )

  measure <- onet_import_eloundou(make_task_panel())

  software <- measure$data$measure_score[
    measure$data$onet_soc_code == "15-1252.00"
  ]
  expect_equal(unique(software), 0.63)
  expect_equal(
    measure$metadata$source_receipt$actual_sha256,
    receipt_a$actual_sha256
  )
  expect_equal(
    onet2r:::onet_sha256(dest),
    readRDS(paste0(dest, ".receipt.rds"))$actual_sha256
  )
  expect_equal(file.exists(snapshot_path), FALSE)
})

test_that("Felten parsing consumes bytes from its verified workbook snapshot", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  write_workbook <- function(path, score) {
    writexl::write_xlsx(
      list(`Appendix A` = data.frame(
        `SOC Code` = c("15-1252", "29-1141", "11-1011"),
        `Occupation Title` = c(
          "Software Developers",
          "Registered Nurses",
          "Chief Executives"
        ),
        AIOE = c(score, -0.32, 0.44),
        check.names = FALSE
      )),
      path
    )
    path
  }

  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir)
  url <- onet2r:::onet_felten_aioe_url
  dest <- file.path(reference_dir, basename(url))
  source_a <- write_workbook(tempfile(fileext = ".xlsx"), 1.08)
  file.copy(source_a, dest)
  receipt_a <- onet2r:::onet_source_receipt(
    dest,
    source_url = url,
    version = onet2r:::onet_source_commit(url)
  )
  saveRDS(receipt_a, paste0(dest, ".receipt.rds"))
  source_b <- write_workbook(
    tempfile("felten-b-", tmpdir = reference_dir, fileext = ".xlsx"),
    9.99
  )
  digest_b <- onet2r:::onet_sha256(source_b)
  original_reader <- onet2r:::read_import_table
  snapshot_path <- NULL
  replaced <- FALSE

  local_mocked_bindings(
    onet_cache_dir = function() cache_dir,
    read_import_table = function(file, sheet = NULL) {
      snapshot_path <<- file
      if (!replaced) {
        receipt_b <- onet2r:::onet_source_receipt(
          source_b,
          source_url = url,
          version = onet2r:::onet_source_commit(url)
        )
        onet2r:::onet_atomic_commit_source(source_b, dest, receipt_b)
        replaced <<- TRUE
      }
      original_reader(file, sheet)
    },
    .package = "onet2r"
  )

  measure <- onet_import_felten_aioe(make_task_panel())
  software <- measure$data$measure_score[
    measure$data$onet_soc_code == "15-1252.00"
  ]

  expect_equal(unique(software), 1.08)
  expect_equal(
    measure$metadata$source_receipt$actual_sha256,
    receipt_a$actual_sha256
  )
  expect_equal(onet2r:::onet_sha256(dest), digest_b)
  expect_equal(file.exists(snapshot_path), FALSE)
  expect_equal(dir.exists(paste0(dest, ".lock")), FALSE)
})

test_that("adapter snapshots are removed when parsing errors", {
  source <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(source), add = TRUE)
  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir)
  url <- onet2r:::onet_eloundou_url
  dest <- file.path(reference_dir, basename(url))
  file.copy(source, dest)
  saveRDS(
    onet2r:::onet_source_receipt(
      dest,
      source_url = url,
      version = onet2r:::onet_source_commit(url)
    ),
    paste0(dest, ".receipt.rds")
  )
  snapshot_path <- NULL

  local_mocked_bindings(
    onet_cache_dir = function() cache_dir,
    read_import_table = function(file, sheet = NULL) {
      snapshot_path <<- file
      stop("injected adapter parse failure")
    },
    .package = "onet2r"
  )

  expect_error(
    onet_import_eloundou(make_task_panel()),
    "injected adapter parse failure"
  )
  expect_equal(file.exists(snapshot_path), FALSE)
})

test_that("adapter cache basename collisions fail closed for legacy bytes", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir)
  url_one <- "https://one.example.invalid/occ_level.csv"
  url_two <- "https://two.example.invalid/occ_level.csv"
  dest <- file.path(reference_dir, basename(url_one))
  file.copy(extract, dest)

  local_mocked_bindings(
    onet_cache_dir = function() cache_dir,
    .package = "onet2r"
  )

  expect_error(
    onet_import_eloundou(make_task_panel(), url = url_one),
    "no trustworthy provenance receipt.*source_url.*force = TRUE",
    class = "onet2r_unverified_legacy_cache"
  )
  expect_error(
    onet_import_eloundou(make_task_panel(), url = url_two),
    "no trustworthy provenance receipt.*source_url.*force = TRUE",
    class = "onet2r_unverified_legacy_cache"
  )
  expect_equal(readLines(dest), readLines(extract))
  expect_equal(file.exists(paste0(dest, ".receipt.rds")), FALSE)
})

test_that("adapter cache distinguishes credential-scoped URLs with one basename", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir)
  url_one <- "https://example.invalid/occ_level.csv?token=first-secret"
  url_two <- "https://example.invalid/occ_level.csv?token=second-secret"
  dest <- file.path(reference_dir, "occ_level.csv")
  file.copy(extract, dest)
  receipt <- onet2r:::onet_source_receipt(dest, source_url = url_one)
  saveRDS(receipt, paste0(dest, ".receipt.rds"))

  local_mocked_bindings(
    onet_cache_dir = function() cache_dir,
    .package = "onet2r"
  )

  condition <- tryCatch(
    onet_import_eloundou(make_task_panel(), url = url_two),
    error = identity
  )
  message <- conditionMessage(condition)
  expect_match(message, "provenance does not match.*source_url.*force = TRUE")
  expect_no_match(message, "first-secret|second-secret")
  expect_equal(readLines(dest), readLines(extract))
})

test_that("adapter force redownload replaces legacy bytes and records provenance", {
  source_dir <- withr::local_tempdir()
  source <- write_eloundou_extract(file.path(source_dir, "occ_level.csv"))
  source_url <- paste0(
    "file:///",
    sub("^/", "", normalizePath(source, winslash = "/"))
  )
  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir)
  dest <- file.path(reference_dir, basename(source))
  writeLines("legacy,bytes", dest)
  original_reader <- onet2r:::read_import_table
  original_copy <- onet2r:::onet_copy_cache_snapshot
  snapshot_path <- NULL
  snapshot_copied_under_lock <- FALSE

  local_mocked_bindings(
    onet_cache_dir = function() cache_dir,
    onet_copy_cache_snapshot = function(from, to) {
      snapshot_copied_under_lock <<- dir.exists(paste0(dest, ".lock"))
      original_copy(from, to)
    },
    read_import_table = function(file, sheet = NULL) {
      snapshot_path <<- file
      original_reader(file, sheet)
    },
    .package = "onet2r"
  )

  measure <- onet_import_eloundou(
    make_task_panel(),
    url = source_url,
    force = TRUE,
    as_of = "2023-03"
  )
  receipt <- measure$metadata$source_receipt

  expect_equal(receipt$source_url, source_url)
  expect_equal(receipt$as_of, "2023-03")
  expect_equal(receipt$provenance_status, "recorded")
  expect_equal(readLines(dest), readLines(source))
  expect_equal(snapshot_copied_under_lock, TRUE)
  expect_equal(file.exists(snapshot_path), FALSE)
})
