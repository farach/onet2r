test_that("standardize_soc_code removes O*NET detail suffix", {
  result <- onet2r:::standardize_soc_code(
    c("15-1252.00", "29-1141", "11-1011.01", "151252")
  )

  expect_equal(result, c("15-1252", "29-1141", "11-1011", "15-1252"))
})

test_that("clean_oews_data normalizes names and numeric columns", {
  raw <- tibble::tibble(
    OCC_CODE = "15-1252",
    OCC_TITLE = "Software Developers",
    TOT_EMP = "1,847,900",
    A_MEDIAN = "$133,080",
    H_MEAN = "*"
  )

  result <- onet2r:::clean_oews_data(raw)

  expect_named(
    result,
    c("occ_code", "occ_title", "tot_emp", "a_median", "h_mean", "h_mean_wage_suppressed")
  )
  expect_equal(result$tot_emp, 1847900)
  expect_equal(result$a_median, 133080)
  expect_equal(result$h_mean, NA_real_)
  expect_equal(result$h_mean_wage_suppressed, TRUE)
})

test_that("clean_oews_data preserves OEWS special value semantics", {
  raw <- tibble::tibble(
    OCC_CODE = c("11-1011", "29-1141"),
    TOT_EMP = c("**", "300"),
    A_MEDIAN = c("#", "$93,070"),
    PCT_TOTAL = c("~", "1.0")
  )

  result <- onet2r:::clean_oews_data(raw)

  expect_equal(result$tot_emp, c(NA_real_, 300))
  expect_equal(result$a_median, c(NA_real_, 93070))
  expect_equal(result$pct_total, c(NA_real_, 1))
  expect_equal(result$tot_emp_employment_suppressed, c(TRUE, FALSE))
  expect_equal(result$a_median_topcoded, c(TRUE, FALSE))
  expect_equal(result$pct_total_less_than_half, c(TRUE, FALSE))
})

test_that("annual/hourly flags are coerced to logical", {
  raw <- tibble::tibble(
    OCC_CODE = c("25-2021", "27-2011"),
    O_GROUP = "detailed",
    TOT_EMP = c("1000", "500"),
    A_MEDIAN = c("65000", "*"),
    H_MEDIAN = c("*", "25.10"),
    ANNUAL = c("TRUE", ""),
    HOURLY = c("", "TRUE")
  )

  out <- onet2r:::clean_oews_data(raw)

  expect_type(out$annual, "logical")
  expect_type(out$hourly, "logical")
  expect_equal(out$annual, c(TRUE, FALSE))
  expect_equal(out$hourly, c(FALSE, TRUE))
})

test_that("oews_url uses current BLS file slugs", {
  expect_match(
    onet2r:::oews_url("national", 2023),
    "special-requests/oesm23nat\\.zip$"
  )
  expect_match(onet2r:::oews_url("metro", 2023), "oesm23ma\\.zip$")
  expect_match(onet2r:::oews_url("industry", 2023), "oesm23in4\\.zip$")
})

test_that("download_oews_file uses the package HTTP client", {
  cache_dir <- withr::local_tempdir()
  calls <- new.env(parent = emptyenv())

  local_mocked_bindings(
    download_oews_file_http = function(url, destfile, quiet) {
      calls$url <- url
      calls$destfile <- destfile
      calls$quiet <- quiet
      writeBin(charToRaw("zip"), destfile)
      invisible(destfile)
    },
    validate_oews_zip = function(path) {
      expect_identical(file.exists(path), TRUE)
      invisible(path)
    },
    .package = "onet2r"
  )

  result <- onet2r:::download_oews_file(
    type = "national",
    year = 2025,
    cache_dir = cache_dir,
    quiet = FALSE
  )

  expect_equal(calls$url, onet2r:::oews_url("national", 2025))
  expect_identical(calls$quiet, FALSE)
  expect_match(result, "oesm25nat\\.zip$")
  expect_identical(file.exists(result), TRUE)
  expect_identical(file.exists(calls$destfile), FALSE)
})

test_that("download_oews_file reuses a valid cached file", {
  cache_dir <- file.path(withr::local_tempdir(), "oews")
  dir.create(cache_dir, recursive = TRUE)
  cached_file <- file.path(cache_dir, "oesm25nat.zip")
  writeBin(charToRaw("zip"), cached_file)
  called <- FALSE

  local_mocked_bindings(
    download_oews_file_http = function(...) {
      called <<- TRUE
      stop("download should not be called")
    },
    validate_oews_zip = function(path) {
      expect_equal(normalizePath(path), normalizePath(cached_file))
      invisible(path)
    },
    .package = "onet2r"
  )

  result <- onet2r:::download_oews_file(
    type = "national",
    year = 2025,
    cache_dir = dirname(cache_dir)
  )

  expect_equal(normalizePath(result), normalizePath(cached_file))
  expect_identical(called, FALSE)
})

test_that("download_oews_file tries alternate official BLS URLs", {
  cache_dir <- withr::local_tempdir()
  calls <- character()

  local_mocked_bindings(
    download_oews_file_http = function(url, destfile, quiet) {
      calls <<- c(calls, url)
      if (length(calls) == 1) {
        cli::cli_abort("blocked")
      }
      writeBin(charToRaw("zip"), destfile)
      invisible(destfile)
    },
    validate_oews_zip = function(path) {
      expect_identical(file.exists(path), TRUE)
      invisible(path)
    },
    .package = "onet2r"
  )

  result <- onet2r:::download_oews_file(
    type = "state",
    year = 2025,
    cache_dir = cache_dir
  )

  expect_equal(length(calls), 2)
  expect_match(calls[[1]], "special-requests/oesm25st\\.zip$")
  expect_match(calls[[2]], "special\\.requests/oesm25st\\.zip$")
  expect_match(result, "oesm25st\\.zip$")
})

test_that("onet_oews reads local extracted files", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "OCC_CODE,OCC_TITLE,TOT_EMP,A_MEDIAN",
      "15-1252,Software Developers,\"1,847,900\",\"$133,080\""
    ),
    path
  )

  result <- onet_oews("national", year = 2024, path = path)

  expect_named(
    result,
    c("year", "oews_type", "occ_code", "occ_title", "tot_emp", "a_median")
  )
  expect_equal(result$year, 2024L)
  expect_equal(result$oews_type, "national")
  expect_equal(result$tot_emp, 1847900)
  expect_equal(result$a_median, 133080)
})

test_that("OEWS wrappers request the expected file types", {
  local_mocked_bindings(
    download_oews_file = function(type, year, cache_dir, force, quiet) {
      expect_in(type, c("national", "state", "metro", "industry"))
      "fake.csv"
    },
    read_oews_file = function(path) {
      expect_equal(path, "fake.csv")
      tibble::tibble(occ_code = "15-1252")
    },
    .package = "onet2r"
  )

  expect_equal(onet_oews_national(2024)$oews_type, "national")
  expect_equal(onet_oews_state(2024)$oews_type, "state")
  expect_equal(onet_oews_metro(2024)$oews_type, "metro")
  expect_equal(onet_oews_industry(2024)$oews_type, "industry")
})

test_that("onet_join_oews joins O*NET occupations to OEWS estimates", {
  occupations <- tibble::tibble(
    code = c("15-1252.00", "29-1141.00", "99-9999.00"),
    title = c("Software Developers", "Registered Nurses", "Missing")
  )
  oews <- tibble::tibble(
    occ_code = c("15-1252", "29-1141"),
    occ_title = c("Software Developers", "Registered Nurses"),
    tot_emp = c(1847900, 3175400),
    a_median = c(133080, 93070)
  )

  expect_warning(
    result <- onet_join_oews(occupations, oews = oews),
    class = "lifecycle_warning_deprecated"
  )

  expect_equal(result$soc_code, c("15-1252", "29-1141", "99-9999"))
  expect_equal(result$tot_emp, c(1847900, 3175400, NA))
  expect_equal(result$a_median, c(133080, 93070, NA))
})

test_that("onet_oews_national downloads, reads, and annotates year", {
  local_mocked_bindings(
    download_oews_file = function(type, year, cache_dir, force, quiet) {
      expect_equal(type, "national")
      expect_equal(year, 2024)
      expect_equal(force, FALSE)
      "fake.csv"
    },
    read_oews_file = function(path) {
      expect_equal(path, "fake.csv")
      tibble::tibble(
        occ_code = "15-1252",
        occ_title = "Software Developers",
        tot_emp = 1847900
      )
    },
    .package = "onet2r"
  )

  result <- onet_oews_national(year = 2024)

  expect_equal(names(result)[[1]], "year")
  expect_equal(names(result)[[2]], "oews_type")
  expect_equal(result$year, 2024L)
  expect_equal(result$oews_type, "national")
  expect_equal(result$occ_code, "15-1252")
})

test_that("onet_oews_national accepts a manually downloaded path", {
  path <- tempfile(fileext = ".zip")
  file.create(path)
  on.exit(unlink(path), add = TRUE)

  local_mocked_bindings(
    read_oews_file = function(path) {
      tibble::tibble(occ_code = "29-1141")
    },
    .package = "onet2r"
  )

  result <- onet_oews_national(year = 2024, path = path)

  expect_equal(result$year, 2024L)
  expect_equal(result$occ_code, "29-1141")
})
