test_that("onet_request builds URL correctly with path only", {
  # This test verifies that onet_request can handle path-only endpoints
  # without query parameters (which was causing errors before)

  # Mock the API key
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  Sys.setenv(ONET_API_KEY = "test-key")

  # Build a request with only path
  req <- onet2r:::onet_request("database")

  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "https://api-v2.onetcenter.org/database")
})

test_that("onet_request builds URL correctly with path segments", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  Sys.setenv(ONET_API_KEY = "test-key")

  # Build a request with path segments
  req <- onet2r:::onet_request("online/occupations", .path_segments = c("15-1252.00", "summary"))

  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "https://api-v2.onetcenter.org/online/occupations/15-1252.00/summary")
})

test_that("onet_request builds URL correctly with query parameters", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  Sys.setenv(ONET_API_KEY = "test-key")

  # Build a request with query parameters
  req <- onet2r:::onet_request("online/search", .query = list(keyword = "software", start = 1, end = 20))

  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "https://api-v2.onetcenter.org/online/search")
  # The query params should be in the URL
  expect_match(req$url, "keyword=software")
  expect_match(req$url, "start=1")
  expect_match(req$url, "end=20")
})

test_that("onet_request builds URL correctly with both path segments and query parameters", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  Sys.setenv(ONET_API_KEY = "test-key")

  # Build a request with both path segments and query parameters
  req <- onet2r:::onet_request("database/rows", .path_segments = "skills", .query = list(start = 1, end = 100))

  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "https://api-v2.onetcenter.org/database/rows/skills")
  expect_match(req$url, "start=1")
  expect_match(req$url, "end=100")
})

test_that("onet_request redacts the API key header", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  test_key <- "test-api-key-123"
  Sys.setenv(ONET_API_KEY = test_key)

  req <- onet2r:::onet_request("database")
  headers <- paste(capture.output(str(req$headers)), collapse = "\n")

  expect_match(headers, "X-API-Key: <REDACTED>", fixed = TRUE)
  expect_no_match(headers, test_key, fixed = TRUE)
})

test_that("cache and rate-limit configuration return expected settings", {
  cache_dir <- withr::local_tempdir()

  settings <- onet_cache_use(enabled = TRUE, cache_dir = cache_dir)
  expect_equal(settings$enabled, TRUE)
  expect_equal(settings$cache_dir, cache_dir)
  expect_equal(getOption("onet2r.cache_enabled"), TRUE)

  expect_equal(onet_rate_limit(0.01), 0.01)
  expect_equal(getOption("onet2r.request_delay"), 0.01)

  onet_cache_clear(cache_dir = cache_dir)
  onet_cache_use(enabled = FALSE, cache_dir = cache_dir)
  onet_rate_limit(0)
})

test_that("onet_cache_clear removes reference cache files", {
  cache_dir <- withr::local_tempdir()
  dir.create(file.path(cache_dir, "reference"), recursive = TRUE)
  writeLines("cached", file.path(cache_dir, "reference", "updates.xlsx"))

  onet_cache_clear(cache_dir = cache_dir, what = "reference")

  expect_equal(file.exists(file.path(cache_dir, "reference", "updates.xlsx")), FALSE)
})

test_that("onet_cache_file uses request URL and cache options", {
  cache_dir <- withr::local_tempdir()
  onet_cache_use(enabled = TRUE, cache_dir = cache_dir)
  on.exit(onet_cache_use(enabled = FALSE, cache_dir = cache_dir), add = TRUE)

  req <- list(url = "https://api-v2.onetcenter.org/online/search?keyword=data")
  path <- onet2r:::onet_cache_file(req)

  expect_match(path, cache_dir, fixed = TRUE)
  expect_match(path, "\\.rds$")
})

test_that("onet_perform aborts on a corrupt API cache without requesting", {
  cache_dir <- withr::local_tempdir()
  onet_cache_use(enabled = TRUE, cache_dir = cache_dir)
  on.exit(onet_cache_use(enabled = FALSE, cache_dir = cache_dir), add = TRUE)

  req <- list(url = "https://api-v2.onetcenter.org/about")
  cache_file <- onet2r:::onet_cache_file(req)
  dir.create(dirname(cache_file), recursive = TRUE)
  writeBin(charToRaw("not an RDS file"), cache_file)

  expect_error(
    onet2r:::onet_perform(req),
    "Cached O\\*NET API response is corrupt.*onet_cache_clear",
    class = "onet2r_corrupt_cache"
  )
})

test_that("atomic cache writes replace files and remove temporary files", {
  cache_dir <- withr::local_tempdir()
  dest <- file.path(cache_dir, "value.rds")
  saveRDS("old", dest)

  expect_error(
    onet2r:::onet_atomic_write(dest, function(tmp) {
      saveRDS("incomplete", tmp)
      stop("writer failed")
    }),
    "writer failed"
  )
  expect_equal(readRDS(dest), "old")

  onet2r:::onet_atomic_save_rds("new", dest)

  expect_equal(readRDS(dest), "new")

  rename_calls <- 0L
  local_mocked_bindings(
    onet_file_rename = function(from, to) {
      rename_calls <<- rename_calls + 1L
      if (rename_calls %in% c(1L, 3L)) {
        return(FALSE)
      }
      base::file.rename(from, to)
    },
    .package = "onet2r"
  )
  expect_error(
    onet2r:::onet_atomic_save_rds("replacement", dest),
    "previous file was preserved"
  )

  expect_equal(readRDS(dest), "new")
  expect_equal(list.files(cache_dir, all.files = TRUE, no.. = TRUE), "value.rds")
})

test_that("source and receipt replacement rolls back as one cache pair", {
  cache_dir <- withr::local_tempdir()
  dest <- file.path(cache_dir, "source.csv")
  receipt_dest <- paste0(dest, ".receipt.rds")
  writeLines("old", dest)
  saveRDS(list(actual_sha256 = "old"), receipt_dest)
  tmp <- tempfile("source-", tmpdir = cache_dir)
  writeLines("new", tmp)

  local_mocked_bindings(
    onet_file_rename = function(from, to) {
      if (
        identical(to, receipt_dest) &&
          grepl("-write-", basename(from), fixed = TRUE)
      ) {
        return(FALSE)
      }
      base::file.rename(from, to)
    },
    .package = "onet2r"
  )

  expect_error(
    onet2r:::onet_atomic_commit_source(
      tmp,
      dest,
      list(actual_sha256 = "new")
    ),
    "previous pair was preserved"
  )
  expect_equal(readLines(dest), "old")
  expect_equal(readRDS(receipt_dest)$actual_sha256, "old")
  expect_equal(
    list.files(cache_dir, all.files = TRUE, no.. = TRUE),
    c("source.csv", "source.csv.receipt.rds")
  )
})

test_that("cache pair backup failure restores the first preserved file", {
  cache_dir <- withr::local_tempdir()
  dest <- file.path(cache_dir, "source.csv")
  receipt_dest <- paste0(dest, ".receipt.rds")
  writeLines("old", dest)
  saveRDS(list(actual_sha256 = "old"), receipt_dest)
  tmp <- tempfile("source-", tmpdir = cache_dir)
  writeLines("new", tmp)

  local_mocked_bindings(
    onet_file_rename = function(from, to) {
      if (identical(from, receipt_dest)) {
        return(FALSE)
      }
      if (grepl("-backup-", basename(from), fixed = TRUE)) {
        return(FALSE)
      }
      base::file.rename(from, to)
    },
    .package = "onet2r"
  )

  expect_error(
    onet2r:::onet_atomic_commit_source(
      tmp,
      dest,
      list(actual_sha256 = "new")
    ),
    "Failed to preserve the existing cache source and receipt"
  )
  expect_equal(readLines(dest), "old")
  expect_equal(readRDS(receipt_dest)$actual_sha256, "old")
  expect_equal(
    list.files(cache_dir, all.files = TRUE, no.. = TRUE),
    c(basename(tmp), "source.csv", "source.csv.receipt.rds")
  )
})
