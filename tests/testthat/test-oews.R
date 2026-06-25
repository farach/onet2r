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
    c("occ_code", "occ_title", "tot_emp", "a_median", "h_mean")
  )
  expect_equal(result$tot_emp, 1847900)
  expect_equal(result$a_median, 133080)
  expect_equal(result$h_mean, NA_real_)
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
