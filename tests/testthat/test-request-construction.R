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

test_that("onet_cache_clear validates and forwards its wait timeout", {
  cache_dir <- withr::local_tempdir()
  calls <- list()
  local_mocked_bindings(
    onet_clear_cache_section = function(section, timeout) {
      calls[[length(calls) + 1L]] <<- list(
        section = section,
        timeout = timeout
      )
      invisible(section)
    },
    .package = "onet2r"
  )

  onet_cache_clear(
    cache_dir = cache_dir,
    what = "reference",
    timeout = 42
  )
  expect_length(calls, 1L)
  expect_identical(calls[[1]]$timeout, 42)
  expect_identical(
    calls[[1]]$section,
    file.path(cache_dir, "reference")
  )

  condition <- tryCatch(
    onet_cache_clear(
      cache_dir = cache_dir,
      what = "reference",
      timeout = -1
    ),
    error = identity
  )
  expect_s3_class(condition, "onet2r_invalid_cache_timeout")
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

test_that("receiptless cache constraints fail closed without writing provenance", {
  cache_dir <- withr::local_tempdir()
  path <- file.path(cache_dir, "legacy.csv")
  writeLines("legacy", path)
  digest <- onet2r:::onet_sha256(path)

  constrained_calls <- list(
    source_url = function() {
      onet2r:::onet_cached_source_receipt(
        path,
        source_url = "https://example.invalid/legacy.csv"
      )
    },
    version = function() {
      onet2r:::onet_cached_source_receipt(path, version = "30.3")
    },
    as_of = function() {
      onet2r:::onet_cached_source_receipt(path, as_of = "2026-05")
    },
    expected_sha256 = function() {
      onet2r:::onet_cached_source_receipt(path, expected_sha256 = digest)
    }
  )

  for (constraint in names(constrained_calls)) {
    expect_error(
      constrained_calls[[constraint]](),
      paste0("constraint.*", constraint, ".*force = TRUE"),
      class = "onet2r_unverified_legacy_cache"
    )
    expect_equal(file.exists(paste0(path, ".receipt.rds")), FALSE)
  }
})

test_that("unconstrained legacy cache reuse warns and remains unverified", {
  cache_dir <- withr::local_tempdir()
  path <- file.path(cache_dir, "legacy.csv")
  writeLines("legacy", path)

  expect_warning(
    receipt <- onet2r:::onet_cached_source_receipt(path),
    "legacy cached source without verified provenance",
    class = "onet2r_unverified_legacy_cache"
  )

  expect_equal(receipt$provenance_status, "legacy_unverified")
  expect_equal(receipt$source_url, NA_character_)
  expect_equal(receipt$version, NA_character_)
  expect_equal(receipt$as_of, NA_character_)
  expect_equal(receipt$actual_sha256, onet2r:::onet_sha256(path))
  expect_equal(file.exists(paste0(path, ".receipt.rds")), TRUE)

  expect_error(
    onet2r:::onet_cached_source_receipt(
      path,
      source_url = "https://example.invalid/legacy.csv"
    ),
    "explicitly unverified.*source_url.*force = TRUE",
    class = "onet2r_unverified_legacy_cache"
  )
})

test_that("source receipts and errors redact URL credentials", {
  cache_dir <- withr::local_tempdir()
  path <- file.path(cache_dir, "source.csv")
  writeLines("source", path)
  credential_url <- paste0(
    "https://alice:supersecret@example.invalid/source.csv",
    "?tok%65n=querysecret&client_secret=oauthsecret",
    "&X-Amz-Credential=awscredential&X-Amz-Signature=awssignature",
    "&variant=public#access_token=fragmentsecret"
  )

  receipt <- onet2r:::onet_source_receipt(
    path,
    source_url = credential_url
  )
  expect_equal(
    receipt$source_url,
    paste0(
      "https://example.invalid/source.csv",
      "?token=[REDACTED]&client_secret=[REDACTED]",
      "&X-Amz-Credential=[REDACTED]&X-Amz-Signature=[REDACTED]",
      "&variant=public#access_token=[REDACTED]"
    )
  )
  expect_no_match(
    receipt$source_url,
    paste(
      "alice|supersecret|querysecret|oauthsecret",
      "awscredential|awssignature|fragmentsecret",
      sep = "|"
    )
  )
  expect_equal(
    receipt$source_url_sha256,
    onet2r:::onet_source_url_sha256(credential_url)
  )

  condition <- tryCatch(
    onet2r:::onet_cached_source_receipt(
      path,
      source_url = credential_url
    ),
    error = identity
  )
  message <- conditionMessage(condition)
  expect_no_match(
    message,
    paste(
      "alice|supersecret|querysecret|oauthsecret",
      "awscredential|awssignature|fragmentsecret",
      sep = "|"
    )
  )
  expect_s3_class(condition, "onet2r_unverified_legacy_cache")

  download_warnings <- character()
  download_condition <- tryCatch(
    withCallingHandlers(
      onet2r:::download_import_file(
        paste0(
          "bad://alice:supersecret@example.invalid/source.csv",
          "?token=querysecret"
        ),
        cache_dir = cache_dir,
        force = TRUE
      ),
      warning = function(cnd) {
        download_warnings <<- c(
          download_warnings,
          conditionMessage(cnd)
        )
        invokeRestart("muffleWarning")
      }
    ),
    error = identity
  )
  download_message <- conditionMessage(download_condition)
  expect_length(download_warnings, 0)
  expect_no_match(download_message, "alice|supersecret|querysecret")
  expect_match(download_message, "bad://example.invalid/source.csv")
})

test_that("legacy receipts redact stored credentials when fingerprinted", {
  cache_dir <- withr::local_tempdir()
  path <- file.path(cache_dir, "source.csv")
  writeLines("source", path)
  credential_url <- paste0(
    "https://alice:supersecret@example.invalid/source.csv",
    "?token=querysecret"
  )
  receipt <- onet2r:::onet_source_receipt(path)
  receipt$source_url <- credential_url
  receipt$source_url_sha256 <- NULL
  receipt$provenance_status <- "recorded"
  saveRDS(receipt, paste0(path, ".receipt.rds"))

  result <- onet2r:::onet_cached_source_receipt(
    path,
    source_url = credential_url
  )
  stored <- readRDS(paste0(path, ".receipt.rds"))

  expect_equal(
    result$source_url,
    "https://example.invalid/source.csv?token=[REDACTED]"
  )
  expect_equal(stored$source_url, result$source_url)
  expect_equal(
    stored$source_url_sha256,
    onet2r:::onet_source_url_sha256(credential_url)
  )
  expect_no_match(
    paste(capture.output(str(stored)), collapse = "\n"),
    "alice|supersecret|querysecret"
  )
})

test_that("malformed URLs selectively redact query and fragment parameters", {
  query_url <- "http://[bad]?token=querysecret&variant=public"
  fragment_url <- "http://[bad]#access_token=fragmentsecret"

  expect_equal(
    onet2r:::onet_redact_url_credentials(query_url),
    "http://[bad]?token=[REDACTED]&variant=public"
  )
  expect_equal(
    onet2r:::onet_redact_url_credentials(fragment_url),
    "http://[bad]#access_token=[REDACTED]"
  )

  cache_dir <- withr::local_tempdir()
  condition <- tryCatch(
    onet2r:::download_import_file(
      query_url,
      cache_dir = cache_dir,
      force = TRUE
    ),
    error = identity
  )
  message <- conditionMessage(condition)
  expect_no_match(message, "querysecret")
  expect_match(message, "variant=public", fixed = TRUE)
  expect_match(message, "REDACTED", fixed = TRUE)
})

test_that("multi-at userinfo redacts through the final authority at", {
  normal_url <- paste0(
    "https://alice:normal-secret@decoy:decoy-secret@host.invalid/",
    "path@public?token=query-secret&contact=query@public",
    "#route@public?access_token=fragment-secret&state=fragment@public"
  )
  encoded_url <- paste0(
    "https://alice%40corp:encoded-secret@host.invalid/",
    "encoded@public?contact=encoded-query@public"
  )
  malformed_url <- paste0(
    "http://first:first-secret@second:second-secret@[bad]/",
    "path@public?token=malformed-query-secret&contact=query@public",
    "#route@public?access_token=malformed-fragment-secret",
    "&state=fragment@public"
  )
  secrets <- paste(
    "normal-secret|decoy-secret|query-secret|fragment-secret",
    "encoded-secret|first-secret|second-secret",
    "malformed-query-secret|malformed-fragment-secret",
    sep = "|"
  )

  safe_normal <- onet2r:::onet_redact_url_credentials(normal_url)
  safe_encoded <- onet2r:::onet_redact_url_credentials(encoded_url)
  safe_malformed <- onet2r:::onet_redact_url_credentials(malformed_url)

  expect_identical(
    safe_normal,
    paste0(
      "https://host.invalid/path@public?",
      "token=[REDACTED]&contact=query@public",
      "#route@public?access_token=[REDACTED]&state=fragment@public"
    )
  )
  expect_identical(
    safe_encoded,
    paste0(
      "https://host.invalid/encoded@public?",
      "contact=encoded-query@public"
    )
  )
  expect_identical(
    safe_malformed,
    paste0(
      "http://[bad]/path@public?",
      "token=[REDACTED]&contact=query@public",
      "#route@public?access_token=[REDACTED]&state=fragment@public"
    )
  )

  cache_dir <- withr::local_tempdir()
  path <- file.path(cache_dir, "source.csv")
  writeLines("source", path)
  receipt <- onet2r:::onet_source_receipt(
    path,
    source_url = malformed_url
  )
  expect_identical(receipt$source_url, safe_malformed)

  legacy <- onet2r:::onet_source_receipt(path)
  legacy$source_url <- normal_url
  legacy$source_url_sha256 <- NULL
  legacy$provenance_status <- "recorded"
  saveRDS(legacy, onet2r:::onet_receipt_path(path))
  reused <- onet2r:::onet_cached_source_receipt(
    path,
    source_url = normal_url
  )
  stored <- readRDS(onet2r:::onet_receipt_path(path))
  expect_identical(reused$source_url, safe_normal)
  expect_identical(stored$source_url, safe_normal)
  expect_identical(
    stored$source_url_sha256,
    onet2r:::onet_source_url_sha256(normal_url)
  )

  warning_condition <- NULL
  withCallingHandlers(
    onet2r:::onet_warn_download_completed(normal_url, "fixture"),
    warning = function(cnd) {
      warning_condition <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(warning_condition, "onet2r_download_warning")

  unverified_path <- file.path(cache_dir, "unverified.csv")
  writeLines("source", unverified_path)
  receipt_error <- tryCatch(
    onet2r:::onet_cached_source_receipt(
      unverified_path,
      source_url = malformed_url
    ),
    error = identity
  )
  expect_s3_class(receipt_error, "onet2r_unverified_legacy_cache")

  download_error <- tryCatch(
    onet2r:::download_import_file(
      sub("^http:", "bad:", malformed_url),
      cache_dir = cache_dir,
      force = TRUE
    ),
    error = identity
  )
  expect_s3_class(download_error, "onet2r_download_error")

  surfaces <- paste(
    safe_normal,
    safe_encoded,
    safe_malformed,
    receipt$source_url,
    reused$source_url,
    stored$source_url,
    conditionMessage(warning_condition),
    conditionMessage(receipt_error),
    conditionMessage(download_error),
    collapse = "\n"
  )
  expect_no_match(surfaces, secrets)
  expect_match(surfaces, "path@public", fixed = TRUE)
  expect_match(surfaces, "contact=query@public", fixed = TRUE)
  expect_match(surfaces, "route@public", fixed = TRUE)
  expect_match(surfaces, "state=fragment@public", fixed = TRUE)
})

test_that("network-path authorities redact credentials on every surface", {
  urls <- c(
    normal = paste0(
      "//alice:normal-secret@host.invalid/path@public?",
      "token=query-secret&contact=query@public",
      "#route@public?access_token=fragment-secret&state=fragment@public"
    ),
    multiple = paste0(
      "//first:first-secret@second:second-secret@host.invalid/",
      "multi@public?contact=multi-query@public"
    ),
    encoded = paste0(
      "//alice%40corp:encoded-secret@host.invalid/",
      "encoded@public?contact=encoded-query@public"
    ),
    malformed = paste0(
      "//first:malformed-first@second:malformed-second@[bad]/",
      "bad@public?token=malformed-query-secret",
      "#route@public?state=malformed-fragment@public"
    )
  )
  expected <- c(
    normal = paste0(
      "//host.invalid/path@public?",
      "token=[REDACTED]&contact=query@public",
      "#route@public?access_token=[REDACTED]&state=fragment@public"
    ),
    multiple = paste0(
      "//host.invalid/multi@public?",
      "contact=multi-query@public"
    ),
    encoded = paste0(
      "//host.invalid/encoded@public?",
      "contact=encoded-query@public"
    ),
    malformed = paste0(
      "//[bad]/bad@public?token=[REDACTED]",
      "#route@public?state=malformed-fragment@public"
    )
  )
  secrets <- paste(
    "normal-secret|query-secret|fragment-secret|first-secret",
    "second-secret|encoded-secret|malformed-first|malformed-second",
    "malformed-query-secret",
    sep = "|"
  )

  safe <- vapply(
    urls,
    onet2r:::onet_redact_url_credentials,
    character(1)
  )
  expect_identical(safe, expected)

  cache_dir <- withr::local_tempdir()
  stored_urls <- character()
  warning_messages <- character()
  for (name in names(urls)) {
    source <- file.path(cache_dir, paste0(name, "-source.csv"))
    dest <- file.path(cache_dir, paste0(name, "-cached.csv"))
    writeLines("source", source)
    receipt <- onet2r:::onet_source_receipt(
      source,
      source_url = urls[[name]]
    )
    onet2r:::onet_atomic_commit_source(source, dest, receipt)
    stored <- readRDS(onet2r:::onet_receipt_path(dest))
    stored_urls[[name]] <- stored$source_url

    withCallingHandlers(
      onet2r:::onet_warn_download_completed(urls[[name]], "fixture"),
      warning = function(cnd) {
        warning_messages[[name]] <<- conditionMessage(cnd)
        invokeRestart("muffleWarning")
      }
    )
  }
  expect_identical(stored_urls, expected)

  download_condition <- tryCatch(
    onet2r:::download_import_file(
      urls[["malformed"]],
      cache_dir = cache_dir,
      force = TRUE
    ),
    error = identity
  )
  expect_s3_class(download_condition, "onet2r_download_error")

  surfaces <- paste(
    safe,
    stored_urls,
    warning_messages,
    conditionMessage(download_condition),
    collapse = "\n"
  )
  expect_no_match(surfaces, secrets)
  expect_match(surfaces, "path@public", fixed = TRUE)
  expect_match(surfaces, "contact=query@public", fixed = TRUE)
  expect_match(surfaces, "route@public", fixed = TRUE)
  expect_match(surfaces, "malformed-fragment@public", fixed = TRUE)
})

test_that("OAuth authorization codes are redacted without hiding benign parameters", {
  url <- paste0(
    "https://example.invalid/callback?",
    "co%64e=authorization-secret&state=visible-state&mode=code"
  )
  safe_url <- onet2r:::onet_redact_url_credentials(url)

  expect_equal(
    safe_url,
    paste0(
      "https://example.invalid/callback?",
      "code=[REDACTED]&state=visible-state&mode=code"
    )
  )
  expect_no_match(safe_url, "authorization-secret")
  expect_match(safe_url, "state=visible-state", fixed = TRUE)
  expect_match(safe_url, "mode=code", fixed = TRUE)

  fragment_url <- paste0(
    "https://example.invalid/callback#route?",
    "co%64e=fragment-secret&state=fragment-state&view=summary"
  )
  safe_fragment_url <- onet2r:::onet_redact_url_credentials(fragment_url)
  expect_no_match(safe_fragment_url, "fragment-secret")
  expect_match(safe_fragment_url, "REDACTED", fixed = TRUE)
  expect_match(safe_fragment_url, "state=fragment-state", fixed = TRUE)
  expect_match(safe_fragment_url, "view=summary", fixed = TRUE)

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines("source", path)
  receipt <- onet2r:::onet_source_receipt(path, source_url = url)
  expect_equal(receipt$source_url, safe_url)

  warning_message <- NULL
  withCallingHandlers(
    onet2r:::onet_warn_download_completed(url, "fixture"),
    warning = function(cnd) {
      warning_message <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  )
  expect_no_match(warning_message, "authorization-secret")
  expect_match(warning_message, "state=visible-state", fixed = TRUE)

  cache_dir <- withr::local_tempdir()
  failing_url <- paste0(
    "bad://example.invalid/callback?",
    "code=error-secret&state=visible-state"
  )
  condition <- tryCatch(
    onet2r:::download_import_file(
      failing_url,
      cache_dir = cache_dir,
      force = TRUE
    ),
    error = identity
  )
  error_message <- conditionMessage(condition)
  expect_no_match(error_message, "error-secret")
  expect_match(error_message, "state=visible-state", fixed = TRUE)

  malformed <- "http://[bad]?code=malformed-secret&state=visible-state"
  expect_equal(
    onet2r:::onet_redact_url_credentials(malformed),
    "http://[bad]?code=[REDACTED]&state=visible-state"
  )
  malformed_fragment <- paste0(
    "http://[bad]#route?",
    "CODE=malformed-fragment-secret&state=visible-state"
  )
  expect_equal(
    onet2r:::onet_redact_url_credentials(malformed_fragment),
    paste0(
      "http://[bad]#route?",
      "CODE=[REDACTED]&state=visible-state"
    )
  )
})

test_that("credential matching uses explicit normalized parameter names", {
  names <- c(
    "code", "ACCESS_TOKEN", "refresh-token", "API%5FKEY",
    "client.secret", "password", "sig", "X-Amz-Credential",
    "X-Amz-Security-Token", "x-goog-signature", "authorization",
    "oauth_signature", "api_token", "x-api-key",
    "accessToken", "refreshToken", "clientSecret", "oauthSignature",
    "SecurityToken", "AWSAccessKeyId",
    "codeVerifier", "clientAssertion", "SecretAccessKey",
    "oauth_verifier", "OAuthVerifier", "consumerKey",
    "oauthConsumerKey", "consumerSecret", "oauth%5Fconsumer%5Fkey",
    "author", "monkey", "hockey", "keyboard", "signature_version"
  )
  expect_equal(
    unname(vapply(
      names,
      onet2r:::onet_url_parameter_is_sensitive,
      logical(1)
    )),
    c(
      rep(TRUE, 29),
      rep(FALSE, 5)
    )
  )

  url <- paste0(
    "https://example.invalid/source.csv?",
    "author=alice&monkey=capuchin&hockey=ice&keyboard=qwerty",
    "&signature_version=v4&API%5FKEY=api-secret",
    "&X-Amz-Security-Token=cloud-secret&CoDe=oauth-secret",
    "&oauth_signature=signature-secret&x-api-key=gateway-secret",
    "&api_token=service-secret&clientSecret=client-secret",
    "&AWSAccessKeyId=access-key-secret&codeVerifier=pkce-secret",
    "&clientAssertion=assertion-secret&SecretAccessKey=aws-secret"
  )
  safe_url <- onet2r:::onet_redact_url_credentials(url)

  expect_match(safe_url, "author=alice", fixed = TRUE)
  expect_match(safe_url, "monkey=capuchin", fixed = TRUE)
  expect_match(safe_url, "hockey=ice", fixed = TRUE)
  expect_match(safe_url, "keyboard=qwerty", fixed = TRUE)
  expect_match(safe_url, "signature_version=v4", fixed = TRUE)
  expect_no_match(
    safe_url,
    paste(
      "api-secret|cloud-secret|oauth-secret|signature-secret",
      "gateway-secret|service-secret|client-secret|access-key-secret",
      "pkce-secret|assertion-secret|aws-secret",
      sep = "|"
    )
  )

  fragment_url <- paste0(
    "https://example.invalid/callback#route?",
    "oauth_signature=fragment-secret&codeVerifier=pkce-fragment-secret",
    "&author=alice"
  )
  safe_fragment <- onet2r:::onet_redact_url_credentials(fragment_url)
  expect_no_match(safe_fragment, "fragment-secret")
  expect_match(safe_fragment, "author=alice", fixed = TRUE)

  source <- tempfile(fileext = ".csv")
  dest <- tempfile(fileext = ".csv")
  on.exit(unlink(c(source, dest, paste0(dest, ".receipt.rds"))), add = TRUE)
  writeLines("source", source)
  receipt <- onet2r:::onet_source_receipt(source, source_url = url)
  onet2r:::onet_atomic_commit_source(source, dest, receipt)
  stored <- readRDS(paste0(dest, ".receipt.rds"))

  expect_equal(stored$source_url, safe_url)
  expect_equal(
    stored$source_url_sha256,
    onet2r:::onet_source_url_sha256(url)
  )
  expect_no_match(
    stored$source_url,
    paste(
      "api-secret|cloud-secret|oauth-secret|signature-secret",
      "gateway-secret|service-secret|client-secret|access-key-secret",
      "pkce-secret|assertion-secret|aws-secret",
      sep = "|"
    )
  )
  expect_match(stored$source_url, "author=alice", fixed = TRUE)

  malformed <- paste0(
    "http://[bad]?",
    "author=alice&monkey=capuchin&api-key=malformed-secret",
    "&x-api-key=malformed-gateway-secret"
  )
  expect_equal(
    onet2r:::onet_redact_url_credentials(malformed),
    paste0(
      "http://[bad]?",
      "author=alice&monkey=capuchin&api-key=[REDACTED]",
      "&x-api-key=[REDACTED]"
    )
  )
})

test_that("OAuth consumer credentials are redacted on every URL surface", {
  url <- paste0(
    "https://example.invalid/oauth/callback?",
    "oauth_verifier=verifier-secret&OAuthVerifier=camel-verifier-secret",
    "&consumerKey=consumer-key-secret",
    "&oauthConsumerKey=oauth-consumer-key-secret",
    "&consumerSecret=consumer-secret",
    "&oauth%5Fconsumer%5Fkey=encoded-consumer-secret",
    "&author=alice&monkey=capuchin&state=visible-state",
    "&signature_version=1.0"
  )
  secrets <- paste(
    "verifier-secret|camel-verifier-secret|consumer-key-secret",
    "oauth-consumer-key-secret|consumer-secret|encoded-consumer-secret",
    sep = "|"
  )
  safe_url <- onet2r:::onet_redact_url_credentials(url)

  expect_no_match(safe_url, secrets)
  expect_match(safe_url, "author=alice", fixed = TRUE)
  expect_match(safe_url, "monkey=capuchin", fixed = TRUE)
  expect_match(safe_url, "state=visible-state", fixed = TRUE)
  expect_match(safe_url, "signature_version=1.0", fixed = TRUE)

  fragment_url <- paste0(
    "https://example.invalid/oauth/callback#route?",
    "OAuthVerifier=fragment-verifier-secret",
    "&consumerSecret=fragment-consumer-secret",
    "&state=fragment-state&author=alice"
  )
  safe_fragment <- onet2r:::onet_redact_url_credentials(fragment_url)
  expect_no_match(
    safe_fragment,
    "fragment-verifier-secret|fragment-consumer-secret"
  )
  expect_match(safe_fragment, "state=fragment-state", fixed = TRUE)
  expect_match(safe_fragment, "author=alice", fixed = TRUE)

  malformed_url <- paste0(
    "http://[bad]?",
    "oauth%5Fverifier=malformed-verifier-secret",
    "&oauth%5Fconsumer%5Fkey=malformed-consumer-key-secret",
    "&consumerSecret=malformed-consumer-secret",
    "&state=malformed-state&monkey=capuchin"
  )
  safe_malformed <- onet2r:::onet_redact_url_credentials(malformed_url)
  expect_no_match(
    safe_malformed,
    paste(
      "malformed-verifier-secret|malformed-consumer-key-secret",
      "malformed-consumer-secret",
      sep = "|"
    )
  )
  expect_match(safe_malformed, "state=malformed-state", fixed = TRUE)
  expect_match(safe_malformed, "monkey=capuchin", fixed = TRUE)

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  writeLines("source", path)
  receipt <- onet2r:::onet_source_receipt(path, source_url = url)
  expect_equal(receipt$source_url, safe_url)
  expect_no_match(receipt$source_url, secrets)
  expect_match(receipt$source_url, "state=visible-state", fixed = TRUE)

  warning_message <- NULL
  withCallingHandlers(
    onet2r:::onet_warn_download_completed(url, "OAuth fixture"),
    warning = function(cnd) {
      warning_message <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    }
  )
  expect_no_match(warning_message, secrets)
  expect_match(warning_message, "state=visible-state", fixed = TRUE)
  expect_match(warning_message, "author=alice", fixed = TRUE)

  failing_url <- paste0(
    "bad://example.invalid/oauth/callback?",
    "OAuthVerifier=error-verifier-secret",
    "&oauthConsumerKey=error-consumer-key-secret",
    "&consumerSecret=error-consumer-secret",
    "&state=error-state&author=alice"
  )
  condition <- tryCatch(
    onet2r:::download_import_file(
      failing_url,
      cache_dir = withr::local_tempdir(),
      force = TRUE
    ),
    error = identity
  )
  error_message <- conditionMessage(condition)
  expect_no_match(
    error_message,
    paste(
      "error-verifier-secret|error-consumer-key-secret",
      "error-consumer-secret",
      sep = "|"
    )
  )
  expect_match(error_message, "state=error-state", fixed = TRUE)
  expect_match(error_message, "author=alice", fixed = TRUE)
})

test_that("failed cache snapshot creation removes the private copy", {
  cache_dir <- withr::local_tempdir()
  path <- file.path(cache_dir, "source.csv")
  url <- "https://example.invalid/source.csv"
  writeLines("verified", path)
  saveRDS(
    onet2r:::onet_source_receipt(path, source_url = url),
    paste0(path, ".receipt.rds")
  )
  snapshot_path <- NULL

  local_mocked_bindings(
    onet_copy_cache_snapshot = function(from, to) {
      snapshot_path <<- to
      copied <- file.copy(from, to)
      writeLines("mutated", to)
      copied
    },
    .package = "onet2r"
  )

  expect_error(
    onet2r:::onet_cached_source_snapshot(path, source_url = url),
    "changed while its private snapshot"
  )
  expect_equal(file.exists(snapshot_path), FALSE)
})
