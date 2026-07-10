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

onet_source_receipt <- function(
    path,
    source_url = NULL,
    source_path = NULL,
    expected_sha256 = NULL,
    version = NULL,
    as_of = NULL,
    retrieved_at = Sys.time()) {
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
    source_url = onet_receipt_value(source_url),
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
    as_of = onet_receipt_value(as_of)
  )
}

onet_receipt_path <- function(path) {
  paste0(path, ".receipt.rds")
}

onet_atomic_commit_source <- function(tmp, dest, receipt) {
  receipt_dest <- onet_receipt_path(dest)
  receipt_tmp <- tempfile(
    paste0(".", basename(receipt_dest), "-write-"),
    tmpdir = dirname(dest)
  )
  on.exit(unlink(receipt_tmp, force = TRUE), add = TRUE)
  saveRDS(receipt, receipt_tmp)

  onet_with_cache_lock(dest, {
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
    invisible(dest)
  })
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
  receipt
}

onet_cached_source_receipt <- function(
    path,
    source_url,
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

onet_cached_source_receipt_unlocked <- function(
    path,
    source_url,
    expected_sha256 = NULL,
    version = NULL,
    as_of = NULL) {
  expected_sha256 <- onet_normalize_sha256(expected_sha256)
  as_of <- onet_normalize_as_of(as_of)
  receipt_path <- onet_receipt_path(path)
  expected_url <- onet_receipt_value(source_url)
  expected_version <- onet_receipt_value(version)
  expected_as_of <- onet_receipt_value(as_of)

  if (!file.exists(receipt_path)) {
    if (is.null(expected_sha256)) {
      receipt <- onet_source_receipt(
        path = path,
        source_path = path,
        retrieved_at = file.info(path)$mtime
      )
      onet_atomic_save_rds(receipt, receipt_path)
      return(receipt)
    }
    receipt <- onet_source_receipt(
      path = path,
      source_url = source_url,
      expected_sha256 = expected_sha256,
      version = version,
      as_of = as_of,
      retrieved_at = file.info(path)$mtime
    )
    onet_atomic_save_rds(receipt, receipt_path)
    return(receipt)
  }

  receipt <- onet_read_source_receipt(receipt_path)
  legacy_unverified <- all(is.na(c(
    onet_receipt_value(receipt$source_url),
    onet_receipt_value(receipt$version),
    onet_receipt_value(receipt$as_of)
  )))
  requested <- list(
    source_url = expected_url,
    version = expected_version,
    as_of = expected_as_of
  )
  mismatched <- if (legacy_unverified) {
    character()
  } else {
    names(requested)[vapply(
      names(requested),
      \(field) !identical(onet_receipt_value(receipt[[field]]), requested[[field]]),
      logical(1)
    )]
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

  if (!is.null(expected_sha256) && legacy_unverified) {
    receipt$source_url <- expected_url
    receipt$source_commit <- onet_source_commit(source_url)
    receipt$version <- expected_version
    receipt$as_of <- expected_as_of
    receipt$expected_sha256 <- expected_sha256
    onet_atomic_save_rds(receipt, receipt_path)
  } else if (!is.null(expected_sha256) && is.na(recorded_expected)) {
    receipt$expected_sha256 <- expected_sha256
    onet_atomic_save_rds(receipt, receipt_path)
  }
  receipt
}
