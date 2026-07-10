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

test_that("credential parameter redaction is exact and percent decoded", {
  sensitive_names <- c(
    "token",
    "TOKEN",
    "tok%65n",
    "api%5Fkey",
    "client-secret",
    "client.secret",
    "co%64e",
    "password",
    "sig",
    "X-Amz-Credential",
    "X-Amz-Signature",
    "X-Amz-Security-Token",
    "x-goog-signature",
    "authorization",
    "refresh-token",
    "oauth_signature",
    "oauth_verifier",
    "api_token",
    "x-api-key",
    "idToken",
    "accessToken",
    "refreshToken",
    "clientSecret",
    "oauthSignature",
    "SecurityToken",
    "xAmzCredential",
    "AWSAccessKeyId",
    "codeVerifier",
    "clientAssertion",
    "SecretAccessKey",
    "GoogleAccessId",
    "accesskey",
    "secretkey",
    "consumerkey",
    "clientsecret"
  )
  benign_names <- c(
    "author",
    "AUTHOR",
    "auth%6Fr",
    "monkey",
    "MonKey",
    "mon%6Bey",
    "state",
    "st%61te",
    "keynote",
    "key_board",
    "signature_version",
    "signature_method",
    "credential_type",
    "authorization_mode",
    "hockey",
    "keyboard",
    "donkey",
    "tokenizer"
  )

  expect_true(all(vapply(
    sensitive_names,
    onet2r:::onet_url_parameter_is_sensitive,
    logical(1)
  )))
  expect_false(any(vapply(
    benign_names,
    onet2r:::onet_url_parameter_is_sensitive,
    logical(1)
  )))

  positive <- list(
    list(
      label = "normal query",
      url = paste0(
        "https://example.invalid/data?",
        "token=normal-secret&author=visible-author"
      ),
      secret = "normal-secret",
      visible = "author=visible-author"
    ),
    list(
      label = "fragment",
      url = paste0(
        "https://example.invalid/data#",
        "access_token=fragment-secret&state=visible-state"
      ),
      secret = "fragment-secret",
      visible = "state=visible-state"
    ),
    list(
      label = "hash route",
      url = paste0(
        "https://example.invalid/app#/callback?",
        "client_secret=route-secret&monkey=visible-monkey"
      ),
      secret = "route-secret",
      visible = "monkey=visible-monkey"
    ),
    list(
      label = "malformed URL",
      url = paste0(
        "http://[bad]?api%5Fkey=malformed-secret",
        "&author=visible-author"
      ),
      secret = "malformed-secret",
      visible = "author=visible-author"
    ),
    list(
      label = "case variant",
      url = paste0(
        "https://example.invalid/data?",
        "X-AmZ-SiGnAtUrE=case-secret&state=visible-state"
      ),
      secret = "case-secret",
      visible = "state=visible-state"
    ),
    list(
      label = "encoded key",
      url = paste0(
        "https://example.invalid/data?",
        "cl%69ent%5Fsecret=encoded-secret&mon%6Bey=visible-monkey"
      ),
      secret = "encoded-secret",
      visible = "monkey=visible-monkey"
    ),
    list(
      label = "OAuth code",
      url = paste0(
        "https://example.invalid/callback?",
        "co%64e=oauth-secret&state=visible-state"
      ),
      secret = "oauth-secret",
      visible = "state=visible-state"
    ),
    list(
      label = "camel case secret",
      url = paste0(
        "https://example.invalid/data?",
        "clientSecret=camel-secret&author=visible-author"
      ),
      secret = "camel-secret",
      visible = "author=visible-author"
    ),
    list(
      label = "compact cloud credential",
      url = paste0(
        "https://example.invalid/data?",
        "AWSAccessKeyId=cloud-secret&state=visible-state"
      ),
      secret = "cloud-secret",
      visible = "state=visible-state"
    ),
    list(
      label = "OAuth signature",
      url = paste0(
        "https://example.invalid/data?",
        "oauth_signature=signature-secret&monkey=visible-monkey"
      ),
      secret = "signature-secret",
      visible = "monkey=visible-monkey"
    )
  )

  for (case in positive) {
    safe <- onet2r:::onet_redact_url_credentials(case$url)
    expect_false(
      grepl(case$secret, safe, fixed = TRUE),
      info = case$label
    )
    expect_true(
      grepl("[REDACTED]", safe, fixed = TRUE),
      info = case$label
    )
    expect_true(
      grepl(case$visible, safe, fixed = TRUE),
      info = case$label
    )
  }

  negative <- list(
    list(
      label = "normal query",
      url = paste0(
        "https://example.invalid/data?",
        "author=visible-author&monkey=visible-monkey&state=visible-state"
      ),
      visible = c("visible-author", "visible-monkey", "visible-state")
    ),
    list(
      label = "fragment",
      url = paste0(
        "https://example.invalid/data#",
        "author=visible-author&state=visible-state"
      ),
      visible = c("visible-author", "visible-state")
    ),
    list(
      label = "hash route",
      url = paste0(
        "https://example.invalid/app#/route?",
        "monkey=visible-monkey&mode=code"
      ),
      visible = c("visible-monkey", "mode=code")
    ),
    list(
      label = "malformed URL",
      url = paste0(
        "http://[bad]?author=visible-author",
        "&monkey=visible-monkey&state=visible-state"
      ),
      visible = c("visible-author", "visible-monkey", "visible-state")
    ),
    list(
      label = "case variants",
      url = paste0(
        "https://example.invalid/data?",
        "AUTHOR=visible-author&MonKey=visible-monkey&STATE=visible-state"
      ),
      visible = c("visible-author", "visible-monkey", "visible-state")
    ),
    list(
      label = "encoded benign keys",
      url = paste0(
        "https://example.invalid/data?",
        "auth%6Fr=visible-author&mon%6Bey=visible-monkey",
        "&st%61te=visible-state"
      ),
      visible = c("visible-author", "visible-monkey", "visible-state")
    ),
    list(
      label = "ordinary parameters",
      url = paste0(
        "https://example.invalid/data?",
        "variant=public&mode=code&view=summary"
      ),
      visible = c("variant=public", "mode=code", "view=summary")
    )
  )

  for (case in negative) {
    safe <- onet2r:::onet_redact_url_credentials(case$url)
    expect_false(
      grepl("[REDACTED]", safe, fixed = TRUE),
      info = case$label
    )
    expect_true(
      all(vapply(case$visible, grepl, logical(1), x = safe, fixed = TRUE)),
      info = case$label
    )
  }
})

test_that("legacy receipt URL migration applies precise credential redaction", {
  cases <- list(
    list(
      label = "normal query",
      url = paste0(
        "https://example.invalid/data?",
        "token=legacy-query-secret&author=visible"
      ),
      secret = "legacy-query-secret",
      visible = "author=visible"
    ),
    list(
      label = "fragment",
      url = paste0(
        "https://example.invalid/data#",
        "access_token=legacy-fragment-secret&state=visible"
      ),
      secret = "legacy-fragment-secret",
      visible = "state=visible"
    ),
    list(
      label = "hash route",
      url = paste0(
        "https://example.invalid/app#/callback?",
        "X-Amz-Signature=legacy-route-secret&monkey=visible"
      ),
      secret = "legacy-route-secret",
      visible = "monkey=visible"
    ),
    list(
      label = "malformed encoded key",
      url = "http://[bad]?api%5Fkey=legacy-malformed-secret&state=visible",
      secret = "legacy-malformed-secret",
      visible = "state=visible"
    ),
    list(
      label = "benign query",
      url = paste0(
        "https://example.invalid/data?",
        "author=visible-author&monkey=visible-monkey",
        "&state=visible-state"
      ),
      secret = NULL,
      visible = "visible-author"
    ),
    list(
      label = "benign malformed encoded keys",
      url = paste0(
        "http://[bad]?",
        "auth%6Fr=visible-author&mon%6Bey=visible-monkey",
        "&st%61te=visible-state"
      ),
      secret = NULL,
      visible = "visible-monkey"
    )
  )
  cache_dir <- withr::local_tempdir()

  for (i in seq_along(cases)) {
    case <- cases[[i]]
    path <- file.path(cache_dir, paste0("source-", i, ".csv"))
    writeLines("source", path)
    receipt <- onet2r:::onet_source_receipt(path)
    receipt$source_url <- case$url
    receipt$source_url_sha256 <- NULL
    receipt$provenance_status <- "recorded"
    saveRDS(receipt, paste0(path, ".receipt.rds"))

    result <- onet2r:::onet_cached_source_receipt(
      path,
      source_url = case$url
    )
    stored <- readRDS(paste0(path, ".receipt.rds"))
    expect_equal(
      stored$source_url_sha256,
      onet2r:::onet_source_url_sha256(case$url),
      info = case$label
    )
    expect_equal(stored$source_url, result$source_url, info = case$label)
    expect_true(
      grepl(case$visible, stored$source_url, fixed = TRUE),
      info = case$label
    )
    if (is.null(case$secret)) {
      expect_false(
        grepl("[REDACTED]", stored$source_url, fixed = TRUE),
        info = case$label
      )
    } else {
      expect_false(
        grepl(case$secret, stored$source_url, fixed = TRUE),
        info = case$label
      )
      expect_true(
        grepl("[REDACTED]", stored$source_url, fixed = TRUE),
        info = case$label
      )
    }
  }
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
