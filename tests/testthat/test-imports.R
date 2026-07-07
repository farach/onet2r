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

# ---------------------------------------------------------------------------
# onet_import_eloundou()
# ---------------------------------------------------------------------------

test_that("onet_import_eloundou reads a local extract as an occupation measure", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  measure <- onet_import_eloundou(path = extract)

  expect_s3_class(measure, "onet_measure")
  expect_equal(measure$metadata$key_type, "occupation")
  expect_equal(measure$metadata$measure_id, "eloundou_gpt_exposure")
  expect_match(measure$metadata$source, "Eloundou")
  expect_match(measure$metadata$source, "MIT License")
  # 8-digit O*NET-SOC keys, published beta score preserved untransformed.
  expect_setequal(
    measure$data$measure_key,
    c("15-1252.00", "29-1141.00", "11-1011.00")
  )
  soft <- measure$data[measure$data$measure_key == "15-1252.00", ]
  expect_equal(soft$measure_score, 0.63)
  expect_true("onet_soc_code" %in% names(measure$data))
})

test_that("onet_import_eloundou selects an alternate score column", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  measure <- onet_import_eloundou(path = extract, score = "dv_rating_beta")
  soft <- measure$data[measure$data$measure_key == "15-1252.00", ]
  expect_equal(soft$measure_score, 0.70)
  expect_equal(measure$metadata$score, "dv_rating_beta")
})

test_that("onet_import_eloundou errors clearly on a missing score column", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  expect_error(
    onet_import_eloundou(path = extract, score = "not_a_column"),
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

  measure <- onet_import_eloundou(path = extract)
  expect_setequal(measure$data$measure_key, c("15-1252.00", "29-1141.00"))
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

  measure <- onet_import_eloundou(path = extract, key = "my_code")
  expect_setequal(measure$data$measure_key, c("15-1252.00", "29-1141.00"))
})

test_that("onet_import_eloundou reads a tab separated extract", {
  extract <- write_eloundou_extract(tempfile(fileext = ".tsv"), sep = "\t")
  on.exit(unlink(extract), add = TRUE)

  measure <- onet_import_eloundou(path = extract)
  expect_equal(nrow(measure$data), 3L)
})

# ---------------------------------------------------------------------------
# onet_import_felten_aioe()
# ---------------------------------------------------------------------------

test_that("onet_import_felten_aioe reads a local extract as an occupation measure", {
  extract <- write_aioe_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  measure <- onet_import_felten_aioe(path = extract)

  expect_s3_class(measure, "onet_measure")
  expect_equal(measure$metadata$key_type, "occupation")
  expect_equal(measure$metadata$measure_id, "felten_aioe")
  expect_match(measure$metadata$source, "Felten")
  # 6-digit SOC keys, published AIOE score preserved untransformed.
  expect_setequal(
    measure$data$measure_key,
    c("15-1252", "29-1141", "11-1011")
  )
  soft <- measure$data[measure$data$measure_key == "15-1252", ]
  expect_equal(soft$measure_score, 1.08)
  expect_true("soc_code" %in% names(measure$data))
})

test_that("onet_import_felten_aioe errors clearly on a missing score column", {
  extract <- write_aioe_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)

  expect_error(
    onet_import_felten_aioe(path = extract, score = "not_a_column"),
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

  measure <- onet_import_felten_aioe(path = extract, sheet = "Appendix A")
  expect_setequal(measure$data$measure_key, c("15-1252", "29-1141"))
  soft <- measure$data[measure$data$measure_key == "15-1252", ]
  expect_equal(soft$measure_score, 1.08)
})

# ---------------------------------------------------------------------------
# Shared adapter validation
# ---------------------------------------------------------------------------

test_that("import adapters require a path or url and never bundle data", {
  expect_error(
    onet_import_eloundou(path = NULL, url = NULL),
    "never bundles"
  )
  expect_error(
    onet_import_felten_aioe(path = NULL, url = NULL),
    "never bundles"
  )
})

test_that("import adapters reject a nonexistent path", {
  missing <- file.path(tempdir(), "does-not-exist-onet2r.csv")
  expect_error(
    onet_import_eloundou(path = missing),
    "does not exist"
  )
})

test_that("import adapters validate the force flag", {
  extract <- write_eloundou_extract(tempfile(fileext = ".csv"))
  on.exit(unlink(extract), add = TRUE)
  expect_error(
    onet_import_eloundou(path = extract, force = "yes"),
    "force"
  )
})
