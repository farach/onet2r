onet_file_rename <- function(from, to) {
  file.rename(from, to)
}

onet_restore_backup <- function(backup, dest) {
  if (onet_file_rename(backup, dest)) {
    return(TRUE)
  }
  if (!file.copy(backup, dest, overwrite = FALSE)) {
    return(FALSE)
  }
  unlink(backup, force = TRUE)
  TRUE
}

onet_atomic_replace <- function(tmp, dest) {
  if (!file.exists(tmp)) {
    cli::cli_abort("Atomic cache replacement source does not exist: {.file {tmp}}.")
  }
  if (!identical(normalizePath(dirname(tmp)), normalizePath(dirname(dest)))) {
    cli::cli_abort("Atomic cache replacement requires files in the same directory.")
  }

  if (!file.exists(dest)) {
    if (!onet_file_rename(tmp, dest)) {
      cli::cli_abort("Failed to install the new cache file: {.file {dest}}.")
    }
    return(invisible(dest))
  }
  if (isTRUE(suppressWarnings(onet_file_rename(tmp, dest)))) {
    return(invisible(dest))
  }

  backup <- NULL
  if (file.exists(dest)) {
    backup <- tempfile(
      paste0(".", basename(dest), "-backup-"),
      tmpdir = dirname(dest)
    )
    if (!onet_file_rename(dest, backup)) {
      cli::cli_abort(
        "Failed to preserve the existing cache file before replacement: {.file {dest}}."
      )
    }
  }

  if (!onet_file_rename(tmp, dest)) {
    restored <- is.null(backup) || onet_restore_backup(backup, dest)
    if (!restored) {
      cli::cli_abort(
        "Cache replacement failed and the previous file could not be restored: {.file {dest}}."
      )
    }
    cli::cli_abort(
      "Failed to replace the cache file; the previous file was preserved: {.file {dest}}."
    )
  }

  if (!is.null(backup)) {
    unlink(backup, force = TRUE)
  }
  invisible(dest)
}

onet_with_cache_lock <- function(path, code, timeout = 10) {
  lock <- paste0(path, ".lock")
  started <- Sys.time()
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) {
      break
    }
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= timeout) {
      cli::cli_abort(c(
        "Timed out waiting for a cache lock.",
        "i" = "Lock directory: {.file {lock}}",
        "i" = "If no other onet2r process is using this cache file, remove the stale lock directory."
      ))
    }
    Sys.sleep(0.05)
  }
  on.exit(unlink(lock, recursive = TRUE, force = TRUE), add = TRUE)
  force(code)
}

onet_atomic_write <- function(dest, write) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(
    paste0(".", basename(dest), "-write-"),
    tmpdir = dirname(dest)
  )
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  write(tmp)
  if (!file.exists(tmp)) {
    cli::cli_abort("Atomic cache writer did not create a file.")
  }
  onet_atomic_replace(tmp, dest)
}

onet_atomic_save_rds <- function(object, dest) {
  onet_atomic_write(dest, \(tmp) saveRDS(object, tmp))
}

onet_read_cached_rds <- function(path, label, guidance) {
  tryCatch(
    readRDS(path),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Cached {label} is corrupt and cannot be read.",
          "i" = "Cache file: {.file {path}}",
          "i" = guidance
        ),
        class = "onet2r_corrupt_cache",
        parent = cnd
      )
    }
  )
}

onet_normalize_sha256 <- function(expected_sha256) {
  if (is.null(expected_sha256)) {
    return(NULL)
  }
  validate_single_string(expected_sha256, "expected_sha256")
  if (!grepl("^[[:xdigit:]]{64}$", expected_sha256)) {
    cli::cli_abort(
      "{.arg expected_sha256} must be a 64-character hexadecimal SHA-256 digest."
    )
  }
  tolower(expected_sha256)
}

onet_sha256 <- function(path) {
  digest <- tryCatch(
    unname(tools::sha256sum(path)),
    error = function(cnd) {
      cli::cli_abort(
        "Failed to calculate SHA-256 for {.file {path}}.",
        parent = cnd
      )
    }
  )
  if (length(digest) != 1 || is.na(digest) || !nzchar(digest)) {
    cli::cli_abort("Failed to calculate SHA-256 for {.file {path}}.")
  }
  tolower(digest)
}

onet_source_commit <- function(url) {
  if (is.null(url) || is.na(url)) {
    return(NA_character_)
  }
  match <- regexec(
    "^https://raw\\.githubusercontent\\.com/[^/]+/[^/]+/([[:xdigit:]]{40})/",
    url
  )
  parts <- regmatches(url, match)[[1]]
  if (length(parts) == 2) {
    return(tolower(parts[[2]]))
  }
  NA_character_
}

onet_normalize_as_of <- function(as_of) {
  if (is.null(as_of)) {
    return(NULL)
  }
  if (
    length(as_of) != 1 ||
      is.na(as_of) ||
      !(is.character(as_of) || inherits(as_of, c("Date", "POSIXt")))
  ) {
    cli::cli_abort(
      "{.arg as_of} must be a single date, date-time, or character label."
    )
  }
  as.character(as_of)
}

onet_receipt_value <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  if (length(x) != 1 || is.na(x)) {
    return(NA_character_)
  }
  as.character(x)
}

onet_sensitive_url_parameters <- c(
  "access_key",
  "access_key_id",
  "access_token",
  "account_key",
  "api_key",
  "apikey",
  "api_secret",
  "api_token",
  "assertion",
  "auth",
  "auth_key",
  "auth_token",
  "authentication",
  "authorization",
  "authorization_code",
  "aws_access_key_id",
  "aws_secret_access_key",
  "aws_security_token",
  "aws_session_token",
  "bearer_token",
  "client_assertion",
  "client_secret",
  "code",
  "code_verifier",
  "connection_string",
  "consumer_key",
  "consumer_secret",
  "credential",
  "credentials",
  "device_code",
  "hmac",
  "hmac_signature",
  "id_token",
  "jwt",
  "key",
  "key_pair_id",
  "oauth_signature",
  "oauth_token",
  "ocp_apim_subscription_key",
  "passwd",
  "password",
  "pat",
  "personal_access_token",
  "private_key",
  "private_token",
  "pwd",
  "refresh_token",
  "saml_response",
  "sas",
  "sas_token",
  "secret",
  "secret_access_key",
  "secret_key",
  "security_token",
  "session_token",
  "shared_access_key",
  "shared_access_signature",
  "sig",
  "signature",
  "storage_account_key",
  "subscription_key",
  "token",
  "user_code",
  "x_amz_credential",
  "x_amz_security_token",
  "x_amz_signature",
  "x_api_key",
  "x_goog_credential",
  "x_goog_security_token",
  "x_goog_signature"
)

onet_url_parameter_is_sensitive <- function(name) {
  name <- utils::URLdecode(name)
  name <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", name, perl = TRUE)
  name <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", name, perl = TRUE)
  name <- tolower(name)
  name <- gsub("[-.]", "_", name)
  name %in% onet_sensitive_url_parameters ||
    gsub("_", "", name, fixed = TRUE) %in%
      gsub("_", "", onet_sensitive_url_parameters, fixed = TRUE)
}

onet_redact_url_parameters <- function(parameters) {
  if (!nzchar(parameters)) {
    return(parameters)
  }
  parts <- strsplit(parameters, "&", fixed = TRUE)[[1]]
  vapply(parts, function(part) {
    equals <- regexpr("=", part, fixed = TRUE)[[1]]
    if (equals < 0) {
      if (onet_url_parameter_is_sensitive(part)) {
        return("[REDACTED]")
      }
      return(part)
    }
    name <- substr(part, 1, equals - 1L)
    if (!onet_url_parameter_is_sensitive(name)) {
      return(part)
    }
    paste0(name, "=[REDACTED]")
  }, character(1), USE.NAMES = FALSE) |>
    paste(collapse = "&")
}

onet_redact_url_fragment <- function(fragment) {
  query_start <- regexpr("?", fragment, fixed = TRUE)[[1]]
  if (query_start < 0) {
    return(onet_redact_url_parameters(fragment))
  }
  route <- substr(fragment, 1, query_start)
  parameters <- substr(fragment, query_start + 1L, nchar(fragment))
  paste0(route, onet_redact_url_parameters(parameters))
}

onet_redact_url_credentials <- function(url) {
  if (is.null(url) || length(url) != 1 || is.na(url)) {
    return(url)
  }
  parsed <- tryCatch(httr2::url_parse(url), error = function(cnd) NULL)
  if (is.null(parsed)) {
    fragment <- if (grepl("#", url, fixed = TRUE)) {
      sub("^[^#]*#", "", url)
    } else {
      NULL
    }
    url <- sub("#.*$", "", url)
    query <- if (grepl("?", url, fixed = TRUE)) {
      sub("^[^?]*\\?", "", url)
    } else {
      NULL
    }
    url <- sub("\\?.*$", "", url)
    base <- sub(
      "^([[:alpha:]][[:alnum:]+.-]*://)[^/@]*@",
      "\\1",
      url,
      perl = TRUE
    )
    if (!is.null(query)) {
      base <- paste0(base, "?", onet_redact_url_parameters(query))
    }
    if (!is.null(fragment)) {
      base <- paste0(base, "#", onet_redact_url_fragment(fragment))
    }
    return(base)
  }
  parsed$username <- NULL
  parsed$password <- NULL
  if (length(parsed$query) > 0) {
    credential_query <- vapply(
      names(parsed$query),
      onet_url_parameter_is_sensitive,
      logical(1)
    )
    parsed$query[credential_query] <- "[REDACTED]"
  }
  fragment <- NULL
  if (!is.null(parsed$fragment)) {
    fragment <- onet_redact_url_fragment(parsed$fragment)
    parsed$fragment <- NULL
  }
  redacted <- gsub(
    "%5BREDACTED%5D",
    "[REDACTED]",
    httr2::url_build(parsed),
    fixed = TRUE
  )
  if (!is.null(fragment)) {
    redacted <- paste0(redacted, "#", fragment)
  }
  redacted
}

onet_source_url_sha256 <- function(url) {
  if (is.null(url) || length(url) != 1 || is.na(url)) {
    return(NA_character_)
  }
  tmp <- tempfile("onet-source-url-")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  writeBin(charToRaw(enc2utf8(url)), tmp)
  onet_sha256(tmp)
}

onet_warn_download_completed <- function(url, label) {
  safe_url <- onet_redact_url_credentials(url)
  cli::cli_warn(c(
    "The {label} download completed with a warning.",
    "i" = "URL: {.url {safe_url}}",
    "i" = "Verify the downloaded source before use."
  ))
}

onet_source_receipt <- function(
    path,
    source_url = NULL,
    source_path = NULL,
    expected_sha256 = NULL,
    version = NULL,
    as_of = NULL,
    retrieved_at = Sys.time(),
    provenance_status = "recorded") {
  expected_sha256 <- onet_normalize_sha256(expected_sha256)
  as_of <- onet_normalize_as_of(as_of)
  actual_sha256 <- onet_sha256(path)
  if (!is.null(expected_sha256) && !identical(actual_sha256, expected_sha256)) {
    cli::cli_abort(
      c(
        "SHA-256 digest mismatch for source file.",
        "x" = "Expected: {expected_sha256}",
        "x" = "Actual:   {actual_sha256}",
        "i" = "Do not parse this file. Confirm the source and expected digest."
      )
    )
  }

  list(
    source_url = onet_receipt_value(onet_redact_url_credentials(source_url)),
    source_url_sha256 = onet_source_url_sha256(source_url),
    source_path = if (is.null(source_path)) {
      NA_character_
    } else {
      normalizePath(source_path, winslash = "\\", mustWork = TRUE)
    },
    source_commit = onet_source_commit(source_url),
    retrieved_at = as.POSIXct(retrieved_at, tz = "UTC"),
    expected_sha256 = expected_sha256 %||% NA_character_,
    actual_sha256 = actual_sha256,
    file_size = unname(file.info(path)$size),
    version = onet_receipt_value(version),
    as_of = onet_receipt_value(as_of),
    provenance_status = provenance_status
  )
}

onet_receipt_path <- function(path) {
  paste0(path, ".receipt.rds")
}

onet_atomic_commit_source <- function(
    tmp,
    dest,
    receipt,
    return_snapshot = FALSE) {
  receipt_dest <- onet_receipt_path(dest)
  receipt_tmp <- tempfile(
    paste0(".", basename(receipt_dest), "-write-"),
    tmpdir = dirname(dest)
  )
  on.exit(unlink(receipt_tmp, force = TRUE), add = TRUE)
  saveRDS(receipt, receipt_tmp)
  snapshot <- NULL
  snapshot_success <- FALSE
  if (isTRUE(return_snapshot)) {
    extension <- tools::file_ext(dest)
    snapshot <- tempfile(
      "onet-cache-snapshot-",
      fileext = if (nzchar(extension)) paste0(".", extension) else ""
    )
    on.exit({
      if (!snapshot_success) {
        unlink(snapshot, force = TRUE)
      }
    }, add = TRUE)
  }

  result <- onet_with_cache_lock(dest, {
    old_paths <- c(dest, receipt_dest)
    backups <- rep(NA_character_, length(old_paths))
    installed <- character()

    for (i in seq_along(old_paths)) {
      if (!file.exists(old_paths[[i]])) {
        next
      }
      backups[[i]] <- tempfile(
        paste0(".", basename(old_paths[[i]]), "-backup-"),
        tmpdir = dirname(dest)
      )
      if (!onet_file_rename(old_paths[[i]], backups[[i]])) {
        prior <- seq_len(i - 1L)
        restored <- vapply(prior, \(j) {
          is.na(backups[[j]]) ||
            onet_restore_backup(backups[[j]], old_paths[[j]])
        }, logical(1))
        if (!all(restored)) {
          cli::cli_abort(
            "Failed to preserve the existing cache source and receipt, and the previous pair could not be restored."
          )
        }
        cli::cli_abort(
          "Failed to preserve the existing cache source and receipt before replacement."
        )
      }
    }

    committed <- onet_file_rename(tmp, dest)
    if (committed) {
      installed <- c(installed, dest)
      committed <- onet_file_rename(receipt_tmp, receipt_dest)
      if (committed) {
        installed <- c(installed, receipt_dest)
      }
    }

    if (!committed) {
      unlink(installed, force = TRUE)
      restored <- vapply(seq_along(old_paths), \(i) {
        if (is.na(backups[[i]])) {
          return(TRUE)
        }
        onet_restore_backup(backups[[i]], old_paths[[i]])
      }, logical(1))
      if (!all(restored)) {
        cli::cli_abort(
          "Cache source replacement failed and the previous source-receipt pair could not be restored."
        )
      }
      cli::cli_abort(
        "Failed to replace the cache source and receipt; the previous pair was preserved."
      )
    }

    unlink(backups[!is.na(backups)], force = TRUE)
    if (isTRUE(return_snapshot)) {
      onet_copy_verified_snapshot(dest, receipt, snapshot)
    } else {
      invisible(dest)
    }
  })
  snapshot_success <- isTRUE(return_snapshot)
  result
}

onet_read_source_receipt <- function(path) {
  receipt <- onet_read_cached_rds(
    path,
    label = "source receipt",
    guidance = "Re-download with `force = TRUE` to replace the cached source and receipt."
  )
  required <- c(
    "source_url", "source_path", "source_commit", "retrieved_at",
    "expected_sha256", "actual_sha256", "file_size", "version", "as_of"
  )
  if (!is.list(receipt) || !all(required %in% names(receipt))) {
    cli::cli_abort(
      c(
        "Cached source receipt is invalid.",
        "i" = "Receipt file: {.file {path}}",
        "i" = "Re-download with {.code force = TRUE} to replace it."
      )
    )
  }
  if (!"provenance_status" %in% names(receipt)) {
    legacy_unverified <- all(is.na(c(
      onet_receipt_value(receipt$source_url),
      onet_receipt_value(receipt$version),
      onet_receipt_value(receipt$as_of),
      onet_receipt_value(receipt$expected_sha256)
    )))
    receipt$provenance_status <- if (legacy_unverified) {
      "legacy_unverified"
    } else {
      "recorded"
    }
  }
  receipt
}

onet_cached_source_receipt <- function(
    path,
    source_url = NULL,
    expected_sha256 = NULL,
    version = NULL,
    as_of = NULL) {
  onet_with_cache_lock(path, onet_cached_source_receipt_unlocked(
    path = path,
    source_url = source_url,
    expected_sha256 = expected_sha256,
    version = version,
    as_of = as_of
  ))
}

onet_copy_cache_snapshot <- function(from, to) {
  file.copy(from, to, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
}

onet_copy_verified_snapshot <- function(path, receipt, snapshot) {
  if (!onet_copy_cache_snapshot(path, snapshot)) {
    cli::cli_abort("Failed to create a private snapshot of the verified cache source.")
  }
  snapshot_sha256 <- onet_sha256(snapshot)
  if (!identical(snapshot_sha256, tolower(receipt$actual_sha256))) {
    cli::cli_abort(
      "Verified cache source changed while its private snapshot was being created."
    )
  }
  structure(
    snapshot,
    source_receipt = receipt,
    cache_path = path,
    cache_snapshot = TRUE
  )
}

onet_cached_source_snapshot <- function(
    path,
    source_url = NULL,
    expected_sha256 = NULL,
    version = NULL,
    as_of = NULL) {
  extension <- tools::file_ext(path)
  snapshot <- tempfile(
    "onet-cache-snapshot-",
    fileext = if (nzchar(extension)) paste0(".", extension) else ""
  )
  success <- FALSE
  on.exit({
    if (!success) {
      unlink(snapshot, force = TRUE)
    }
  }, add = TRUE)

  result <- onet_with_cache_lock(path, {
    receipt <- onet_cached_source_receipt_unlocked(
      path = path,
      source_url = source_url,
      expected_sha256 = expected_sha256,
      version = version,
      as_of = as_of
    )
    onet_copy_verified_snapshot(path, receipt, snapshot)
  })
  success <- TRUE
  result
}

onet_cached_source_receipt_unlocked <- function(
    path,
    source_url = NULL,
    expected_sha256 = NULL,
    version = NULL,
    as_of = NULL) {
  expected_sha256 <- onet_normalize_sha256(expected_sha256)
  as_of <- onet_normalize_as_of(as_of)
  receipt_path <- onet_receipt_path(path)
  expected_version <- onet_receipt_value(version)
  expected_as_of <- onet_receipt_value(as_of)
  constraints <- c(
    source_url = !is.null(source_url),
    version = !is.null(version),
    as_of = !is.null(as_of),
    expected_sha256 = !is.null(expected_sha256)
  )

  if (!file.exists(receipt_path)) {
    if (any(constraints)) {
      cli::cli_abort(
        c(
          "Cached source has no trustworthy provenance receipt.",
          "x" = "Legacy cached bytes cannot satisfy requested provenance constraint{?s}: {.field {names(constraints)[constraints]}}.",
          "i" = "Use {.code force = TRUE} to download the requested source and create a receipt."
        ),
        class = "onet2r_unverified_legacy_cache"
      )
    }
    receipt <- onet_source_receipt(
      path = path,
      source_path = path,
      retrieved_at = file.info(path)$mtime,
      provenance_status = "legacy_unverified"
    )
    onet_atomic_save_rds(receipt, receipt_path)
    cli::cli_warn(
      c(
        "Reusing a legacy cached source without verified provenance.",
        "i" = "Cache file: {.file {path}}",
        "i" = "A {.val legacy_unverified} receipt was recorded. Redownload before making provenance-sensitive claims."
      ),
      class = "onet2r_unverified_legacy_cache"
    )
    return(receipt)
  }

  receipt <- onet_read_source_receipt(receipt_path)
  receipt_changed <- FALSE
  recorded_url_raw <- onet_receipt_value(receipt$source_url)
  recorded_url_sha256 <- onet_receipt_value(receipt$source_url_sha256)
  if (
    is.na(recorded_url_sha256) &&
      !is.na(recorded_url_raw) &&
      !grepl("[REDACTED]", recorded_url_raw, fixed = TRUE)
  ) {
    receipt$source_url_sha256 <- onet_source_url_sha256(recorded_url_raw)
    receipt_changed <- TRUE
  }
  recorded_url_safe <- onet_receipt_value(
    onet_redact_url_credentials(recorded_url_raw)
  )
  if (!identical(recorded_url_raw, recorded_url_safe)) {
    receipt$source_url <- recorded_url_safe
    receipt_changed <- TRUE
  }
  if (receipt_changed) {
    onet_atomic_save_rds(receipt, receipt_path)
  }
  legacy_unverified <- identical(
    onet_receipt_value(receipt$provenance_status),
    "legacy_unverified"
  )
  if (legacy_unverified && any(constraints)) {
    cli::cli_abort(
      c(
        "Cached source provenance is explicitly unverified.",
        "x" = "Legacy cached bytes cannot satisfy requested provenance constraint{?s}: {.field {names(constraints)[constraints]}}.",
        "i" = "Use {.code force = TRUE} to download the requested source and replace the legacy receipt."
      ),
      class = "onet2r_unverified_legacy_cache"
    )
  }
  expected_url_sha256 <- onet_source_url_sha256(source_url)
  recorded_url_sha256 <- onet_receipt_value(receipt$source_url_sha256)
  if (!is.null(source_url) && is.na(recorded_url_sha256)) {
    recorded_url <- onet_receipt_value(receipt$source_url)
    if (is.na(recorded_url) || grepl("[REDACTED]", recorded_url, fixed = TRUE)) {
      cli::cli_abort(
        c(
          "Cached source receipt cannot prove the requested URL identity.",
          "i" = "Use {.code force = TRUE} to redownload and record a URL fingerprint."
        )
      )
    }
    recorded_url_sha256 <- onet_source_url_sha256(recorded_url)
  }
  requested <- list()
  if (!is.null(version)) {
    requested$version <- expected_version
  }
  if (!is.null(as_of)) {
    requested$as_of <- expected_as_of
  }
  mismatched <- names(requested)[vapply(
    names(requested),
    \(field) !identical(onet_receipt_value(receipt[[field]]), requested[[field]]),
    logical(1)
  )]
  if (
    !is.null(source_url) &&
      !identical(recorded_url_sha256, expected_url_sha256)
  ) {
    mismatched <- c("source_url", mismatched)
  }
  if (length(mismatched) > 0) {
    cli::cli_abort(
      c(
        "Cached source provenance does not match this request.",
        "x" = "Mismatched field{?s}: {.field {mismatched}}.",
        "i" = "Use {.code force = TRUE} to download the requested source."
      )
    )
  }

  actual_sha256 <- onet_sha256(path)
  recorded_sha256 <- tolower(onet_receipt_value(receipt$actual_sha256))
  if (!identical(actual_sha256, recorded_sha256)) {
    cli::cli_abort(
      c(
        "Cached source file changed after its receipt was recorded.",
        "x" = "Recorded SHA-256: {recorded_sha256}",
        "x" = "Current SHA-256:  {actual_sha256}",
        "i" = "Use {.code force = TRUE} to replace the cached source and receipt."
      )
    )
  }
  recorded_expected <- onet_receipt_value(receipt$expected_sha256)
  if (!is.na(recorded_expected) && !identical(actual_sha256, tolower(recorded_expected))) {
    cli::cli_abort(
      c(
        "Cached source does not match the expected SHA-256 in its receipt.",
        "i" = "Use {.code force = TRUE} with the correct {.arg expected_sha256}."
      )
    )
  }
  if (!is.null(expected_sha256) && !identical(actual_sha256, expected_sha256)) {
    cli::cli_abort(
      c(
        "SHA-256 digest mismatch for cached source.",
        "x" = "Expected: {expected_sha256}",
        "x" = "Actual:   {actual_sha256}",
        "i" = "Use {.code force = TRUE} only after confirming the expected source."
      )
    )
  }

  if (!is.null(expected_sha256) && is.na(recorded_expected)) {
    receipt$expected_sha256 <- expected_sha256
    onet_atomic_save_rds(receipt, receipt_path)
  }
  if (legacy_unverified) {
    cli::cli_warn(
      c(
        "Reusing a legacy cached source without verified provenance.",
        "i" = "Cache file: {.file {path}}",
        "i" = "Redownload before making provenance-sensitive claims."
      ),
      class = "onet2r_unverified_legacy_cache"
    )
  }
  receipt
}
