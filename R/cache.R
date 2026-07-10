.onet_cache_transaction_state <- new.env(parent = emptyenv())

onet_cache_sections <- function() {
  c("api", "archives", "crosswalks", "oews", "reference")
}

onet_lock_token <- function() {
  paste0("owner-", Sys.getpid(), "-", basename(tempfile()))
}

onet_remove_lock_owner_marker <- function(path) {
  unlink(path, force = TRUE)
  !file.exists(path)
}

onet_remove_empty_lock_dir <- function(path) {
  if (!dir.exists(path)) {
    return(TRUE)
  }
  path <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  status <- if (.Platform$OS.type == "windows") {
    system2(
      Sys.getenv("COMSPEC", "cmd.exe"),
      c("/d", "/c", "rmdir", shQuote(path)),
      stdout = FALSE,
      stderr = FALSE
    )
  } else {
    system2(
      "rmdir",
      shQuote(path),
      stdout = FALSE,
      stderr = FALSE
    )
  }
  identical(status, 0L) && !dir.exists(path)
}

onet_cache_lock_is_owned <- function(handle) {
  owner <- file.path(handle$path, handle$token)
  if (!file.exists(owner)) {
    return(FALSE)
  }
  recorded <- tryCatch(
    readLines(owner, n = 1L, warn = FALSE),
    error = function(cnd) character()
  )
  entries <- list.files(handle$path, all.files = TRUE, no.. = TRUE)
  identical(recorded, handle$token) &&
    identical(entries, handle$token)
}

onet_acquire_cache_lock <- function(lock, timeout = 10) {
  dir.create(dirname(lock), recursive = TRUE, showWarnings = FALSE)
  started <- Sys.time()

  repeat {
    token <- onet_lock_token()
    if (dir.create(lock, showWarnings = FALSE)) {
      owner <- file.path(lock, token)
      owner_created <- tryCatch(
        {
          writeLines(token, owner, useBytes = TRUE)
          file.exists(owner)
        },
        error = function(cnd) FALSE
      )
      if (owner_created) {
        return(structure(
          list(path = lock, token = token),
          class = "onet2r_cache_lock"
        ))
      }
      if (file.exists(owner)) {
        onet_remove_lock_owner_marker(owner)
      }
      onet_remove_empty_lock_dir(lock)
      cli::cli_abort(
        "Failed to record cache lock ownership.",
        class = "onet2r_cache_lock_error"
      )
    }

    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= timeout) {
      cli::cli_abort(
        c(
          "Timed out waiting for a cache lock.",
          "i" = "Lock directory: {.file {lock}}",
          "i" = paste(
            "If no other onet2r process is using this cache file,",
            "remove the stale lock directory."
          )
        ),
        class = "onet2r_cache_lock_timeout"
      )
    }
    Sys.sleep(0.05)
  }
}

onet_release_cache_lock <- function(handle) {
  owner <- file.path(handle$path, handle$token)
  if (!onet_cache_lock_is_owned(handle)) {
    return(FALSE)
  }
  if (!onet_remove_lock_owner_marker(owner) || file.exists(owner)) {
    return(FALSE)
  }
  if (!dir.exists(handle$path)) {
    return(TRUE)
  }
  if (length(list.files(handle$path, all.files = TRUE, no.. = TRUE)) > 0L) {
    return(FALSE)
  }
  isTRUE(onet_remove_empty_lock_dir(handle$path)) &&
    !dir.exists(handle$path)
}

onet_with_raw_cache_lock <- function(lock, code, timeout = 10) {
  handle <- onet_acquire_cache_lock(lock, timeout = timeout)
  on.exit(onet_release_cache_lock(handle), add = TRUE)
  force(code)
}

onet_cache_section_path <- function(path) {
  current <- normalizePath(path, winslash = "/", mustWork = FALSE)
  repeat {
    if (basename(current) %in% onet_cache_sections()) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(NULL)
    }
    current <- parent
  }
}

onet_cache_coordination_paths <- function(section) {
  root <- file.path(
    dirname(section),
    ".onet2r-locks",
    basename(section)
  )
  list(
    root = root,
    gate = file.path(root, "gate.lock"),
    active = file.path(root, "active")
  )
}

onet_cache_transaction_key <- function(section) {
  normalizePath(section, winslash = "/", mustWork = FALSE)
}

onet_create_tx_marker <- function(path) {
  file.create(path)
}

onet_write_tx_marker <- function(token, marker) {
  writeLines(token, marker, useBytes = TRUE)
  invisible(TRUE)
}

onet_remove_tx_marker <- function(marker) {
  unlink(marker, force = TRUE)
  !file.exists(marker)
}

onet_set_tx_state <- function(key, state) {
  assign(key, state, envir = .onet_cache_transaction_state)
  invisible(TRUE)
}

onet_remove_tx_state <- function(key) {
  rm(list = key, envir = .onet_cache_transaction_state)
  invisible(
    !exists(
      key,
      envir = .onet_cache_transaction_state,
      inherits = FALSE
    )
  )
}

onet_transaction_state_matches <- function(key, marker) {
  if (
    !exists(
      key,
      envir = .onet_cache_transaction_state,
      inherits = FALSE
    )
  ) {
    return(FALSE)
  }
  state <- get(key, envir = .onet_cache_transaction_state, inherits = FALSE)
  identical(state$marker, marker)
}

onet_cleanup_cache_transaction <- function(
    key,
    state,
    context,
    parent = NULL) {
  state$cleanup_pending <- TRUE
  assign(key, state, envir = .onet_cache_transaction_state)

  marker_parent <- parent
  marker_removed <- if (!file.exists(state$marker)) {
    TRUE
  } else {
    tryCatch(
      onet_remove_tx_marker(state$marker),
      error = function(cnd) {
        marker_parent <<- cnd
        FALSE
      }
    )
  }
  if (!isTRUE(marker_removed) || file.exists(state$marker)) {
    cli::cli_abort(
      "Failed to remove a cache transaction marker during {context}.",
      class = "onet2r_cache_transaction_cleanup_error",
      parent = marker_parent
    )
  }

  if (onet_transaction_state_matches(key, state$marker)) {
    state_parent <- NULL
    state_removed <- tryCatch(
      onet_remove_tx_state(key),
      error = function(cnd) {
        state_parent <<- cnd
        FALSE
      }
    )
    if (
      !isTRUE(state_removed) ||
        onet_transaction_state_matches(key, state$marker)
    ) {
      cli::cli_abort(
        "Failed to remove cache transaction state during {context}.",
        class = "onet2r_cache_transaction_cleanup_error",
        parent = state_parent
      )
    }
  }
  invisible(TRUE)
}

onet_begin_cache_transaction <- function(path, timeout = 10) {
  section <- onet_cache_section_path(path)
  if (is.null(section)) {
    return(NULL)
  }

  key <- onet_cache_transaction_key(section)
  if (exists(key, envir = .onet_cache_transaction_state, inherits = FALSE)) {
    state <- get(key, envir = .onet_cache_transaction_state, inherits = FALSE)
    if (isTRUE(state$cleanup_pending)) {
      onet_cleanup_cache_transaction(
        key,
        state,
        context = "transaction retry"
      )
    } else {
      state$count <- state$count + 1L
      onet_set_tx_state(key, state)
      return(structure(
        list(key = key),
        class = "onet2r_cache_transaction"
      ))
    }
  }

  coordination <- onet_cache_coordination_paths(section)
  dir.create(coordination$active, recursive = TRUE, showWarnings = FALSE)
  gate <- onet_acquire_cache_lock(coordination$gate, timeout = timeout)
  on.exit(onet_release_cache_lock(gate), add = TRUE)

  token <- onet_lock_token()
  marker <- file.path(coordination$active, token)
  state <- list(
    count = 1L,
    marker = marker,
    cleanup_pending = FALSE
  )
  marker_created <- FALSE
  registration_complete <- FALSE
  on.exit({
    if (!registration_complete && marker_created) {
      if (onet_transaction_state_matches(key, marker)) {
        state <- get(
          key,
          envir = .onet_cache_transaction_state,
          inherits = FALSE
        )
      }
      state$count <- 0L
      onet_cleanup_cache_transaction(
        key,
        state,
        context = "transaction registration"
      )
    }
  }, add = TRUE)

  if (!isTRUE(onet_create_tx_marker(marker))) {
    cli::cli_abort(
      "Failed to create an active cache transaction marker.",
      class = "onet2r_cache_transaction_registration_error"
    )
  }
  marker_created <- TRUE
  tryCatch(
    onet_write_tx_marker(token, marker),
    error = function(cnd) {
      cli::cli_abort(
        "Failed to write an active cache transaction marker.",
        class = "onet2r_cache_transaction_registration_error",
        parent = cnd
      )
    }
  )
  tryCatch(
    onet_set_tx_state(key, state),
    error = function(cnd) {
      cli::cli_abort(
        "Failed to register in-memory cache transaction state.",
        class = "onet2r_cache_transaction_state_error",
        parent = cnd
      )
    }
  )
  if (!onet_transaction_state_matches(key, marker)) {
    cli::cli_abort(
      "Failed to confirm in-memory cache transaction state.",
      class = "onet2r_cache_transaction_state_error"
    )
  }
  registration_complete <- TRUE
  structure(
    list(key = key),
    class = "onet2r_cache_transaction"
  )
}

onet_end_cache_transaction <- function(handle) {
  if (is.null(handle)) {
    return(invisible(TRUE))
  }
  if (
    !exists(
      handle$key,
      envir = .onet_cache_transaction_state,
      inherits = FALSE
    )
  ) {
    return(invisible(FALSE))
  }

  state <- get(
    handle$key,
    envir = .onet_cache_transaction_state,
    inherits = FALSE
  )
  if (isTRUE(state$cleanup_pending)) {
    return(onet_cleanup_cache_transaction(
      handle$key,
      state,
      context = "transaction teardown retry"
    ))
  }
  if (state$count > 1L) {
    state$count <- state$count - 1L
    tryCatch(
      onet_set_tx_state(handle$key, state),
      error = function(cnd) {
        cli::cli_abort(
          "Failed to update nested cache transaction state.",
          class = "onet2r_cache_transaction_state_error",
          parent = cnd
        )
      }
    )
    return(invisible(TRUE))
  }

  state$cleanup_pending <- TRUE
  tryCatch(
    onet_set_tx_state(handle$key, state),
    error = function(cnd) {
      cli::cli_abort(
        "Failed to prepare cache transaction teardown.",
        class = "onet2r_cache_transaction_state_error",
        parent = cnd
      )
    }
  )
  onet_cleanup_cache_transaction(
    handle$key,
    state,
    context = "transaction teardown"
  )
}

onet_with_cache_transaction <- function(path, code, timeout = 10) {
  handle <- onet_begin_cache_transaction(path, timeout = timeout)
  on.exit(onet_end_cache_transaction(handle), add = TRUE)
  force(code)
}

onet_clear_cache_section <- function(section, timeout = 10) {
  section <- normalizePath(section, winslash = "/", mustWork = FALSE)
  key <- onet_cache_transaction_key(section)
  if (exists(key, envir = .onet_cache_transaction_state, inherits = FALSE)) {
    state <- get(key, envir = .onet_cache_transaction_state, inherits = FALSE)
    if (isTRUE(state$cleanup_pending)) {
      onet_cleanup_cache_transaction(
        key,
        state,
        context = "cache clear"
      )
    } else {
      cli::cli_abort(
        paste(
          "Cannot clear a cache section from inside one of its",
          "active transactions."
        ),
        class = "onet2r_cache_clear_conflict"
      )
    }
  }

  coordination <- onet_cache_coordination_paths(section)
  dir.create(coordination$active, recursive = TRUE, showWarnings = FALSE)
  gate <- onet_acquire_cache_lock(coordination$gate, timeout = timeout)
  on.exit(onet_release_cache_lock(gate), add = TRUE)

  started <- Sys.time()
  repeat {
    active <- list.files(
      coordination$active,
      all.files = TRUE,
      no.. = TRUE
    )
    if (length(active) == 0L) {
      break
    }
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= timeout) {
      cli::cli_abort(
        c(
          "Timed out waiting to clear an active cache section.",
          "i" = "Cache section: {.file {section}}"
        ),
        class = "onet2r_cache_clear_timeout"
      )
    }
    Sys.sleep(0.05)
  }

  unlink(section, recursive = TRUE, force = TRUE)
  if (dir.exists(section) || file.exists(section)) {
    cli::cli_abort(
      "Failed to clear cache section {.file {section}}.",
      class = "onet2r_cache_clear_error"
    )
  }
  invisible(section)
}

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
  onet_with_cache_transaction(path, {
    onet_with_raw_cache_lock(
      paste0(path, ".lock"),
      force(code),
      timeout = timeout
    )
  }, timeout = timeout)
}

onet_atomic_write <- function(dest, write) {
  onet_with_cache_lock(dest, {
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
  })
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
      paste(
        "{.arg expected_sha256} must be a 64-character hexadecimal",
        "SHA-256 digest."
      ),
      class = "onet2r_invalid_digest"
    )
  }
  tolower(expected_sha256)
}

onet_sha256 <- function(path) {
  sha256 <- tryCatch(
    digest::digest(
      file = path,
      algo = "sha256",
      serialize = FALSE
    ),
    error = function(cnd) {
      cli::cli_abort(
        "Failed to calculate SHA-256 for {.file {path}}.",
        class = "onet2r_sha256_error",
        parent = cnd
      )
    }
  )
  if (length(sha256) != 1 || is.na(sha256) || !nzchar(sha256)) {
    cli::cli_abort(
      "Failed to calculate SHA-256 for {.file {path}}.",
      class = "onet2r_sha256_error"
    )
  }
  tolower(sha256)
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

onet_url_parameter_is_sensitive <- function(name) {
  name <- utils::URLdecode(name)
  name <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", name, perl = TRUE)
  name <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", name, perl = TRUE)
  name <- tolower(name)
  name <- gsub("[-.]", "_", name)
  sensitive_names <- c(
    "auth",
    "authorization",
    "code",
    "code_verifier",
    "device_code",
    "user_code",
    "credential",
    "credentials",
    "key",
    "api_key",
    "apikey",
    "x_api_key",
    "subscription_key",
    "ocp_apim_subscription_key",
    "token",
    "api_token",
    "auth_token",
    "access_token",
    "id_token",
    "refresh_token",
    "oauth_token",
    "oauth_verifier",
    "consumer_key",
    "oauth_consumer_key",
    "consumer_secret",
    "bearer_token",
    "security_token",
    "session_token",
    "client_secret",
    "client_assertion",
    "assertion",
    "jwt",
    "saml_response",
    "secret",
    "secret_key",
    "secret_access_key",
    "private_key",
    "private_token",
    "personal_access_token",
    "password",
    "passwd",
    "signature",
    "sig",
    "oauth_signature",
    "sas",
    "shared_access_signature",
    "access_key",
    "access_key_id",
    "aws_access_key_id",
    "aws_secret_access_key",
    "aws_session_token",
    "aws_security_token",
    "account_key",
    "storage_account_key",
    "shared_access_key",
    "connection_string",
    "x_amz_credential",
    "x_amz_signature",
    "x_amz_security_token",
    "x_goog_credential",
    "x_goog_signature",
    "x_goog_security_token"
  )
  name %in% sensitive_names ||
    gsub("_", "", name, fixed = TRUE) %in%
      gsub("_", "", sensitive_names, fixed = TRUE)
}

onet_url_path_segment_is_sensitive <- function(segment) {
  decoded <- tryCatch(
    utils::URLdecode(segment),
    error = function(cnd) segment
  )
  if (grepl("[/\\\\]", decoded)) {
    return(TRUE)
  }
  stem <- tools::file_path_sans_ext(basename(decoded))
  parts <- strsplit(stem, "[^[:alnum:]]+", perl = TRUE)[[1]]
  candidates <- unique(c(stem, parts[nzchar(parts)]))
  any(vapply(
    candidates,
    onet_url_parameter_is_sensitive,
    logical(1)
  ))
}

onet_redact_url_path_credentials <- function(url) {
  authority <- regexpr(
    "^[[:alpha:]][[:alnum:]+.-]*://",
    url,
    perl = TRUE
  )
  network_path <- startsWith(url, "//")
  path_start <- if (authority[[1]] == 1L || network_path) {
    start <- if (network_path) {
      3L
    } else {
      attr(authority, "match.length") + 1L
    }
    remainder <- substr(url, start, nchar(url))
    delimiter <- regexpr("[/?#]", remainder, perl = TRUE)[[1]]
    if (
      delimiter < 0L ||
        !identical(substr(remainder, delimiter, delimiter), "/")
    ) {
      return(url)
    }
    start + delimiter - 1L
  } else {
    scheme <- regexpr(
      "^[[:alpha:]][[:alnum:]+.-]*:",
      url,
      perl = TRUE
    )
    if (scheme[[1]] == 1L) {
      attr(scheme, "match.length") + 1L
    } else {
      1L
    }
  }
  suffix_start <- regexpr(
    "[?#]",
    substr(url, path_start, nchar(url)),
    perl = TRUE
  )[[1]]
  path_end <- if (suffix_start < 0L) {
    nchar(url)
  } else {
    path_start + suffix_start - 2L
  }
  path <- substr(url, path_start, path_end)
  if (!nzchar(path) || identical(path, "/")) {
    return(url)
  }
  leading <- startsWith(path, "/")
  trailing <- endsWith(path, "/")
  inner <- path
  if (leading) {
    inner <- substr(inner, 2L, nchar(inner))
  }
  if (trailing && nzchar(inner)) {
    inner <- substr(inner, 1L, nchar(inner) - 1L)
  }
  segments <- if (nzchar(inner)) {
    strsplit(inner, "/", fixed = TRUE)[[1]]
  } else {
    character()
  }
  sensitive <- vapply(
    segments,
    onet_url_path_segment_is_sensitive,
    logical(1)
  )
  segments[sensitive] <- "[REDACTED]"
  redacted_path <- paste0(
    if (leading) "/" else "",
    paste(segments, collapse = "/"),
    if (trailing) "/" else ""
  )
  suffix <- if (suffix_start < 0L) {
    ""
  } else {
    substr(url, path_end + 1L, nchar(url))
  }
  paste0(
    substr(url, 1L, path_start - 1L),
    redacted_path,
    suffix
  )
}

onet_redact_url_parameters <- function(parameters) {
  if (!nzchar(parameters)) {
    return(parameters)
  }
  parts <- strsplit(parameters, "&", fixed = TRUE)[[1]]
  vapply(parts, function(part) {
    equals <- regexpr("=", part, fixed = TRUE)[[1]]
    if (equals < 0) {
      return("[REDACTED]")
    }
    name <- substr(part, 1, equals - 1L)
    if (!nzchar(name)) {
      return("[REDACTED]")
    }
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

onet_redact_url_authority <- function(url) {
  scheme <- regexpr(
    "^[[:alpha:]][[:alnum:]+.-]*://",
    url,
    perl = TRUE
  )
  network_path <- startsWith(url, "//")
  if (scheme[[1]] != 1L && !network_path) {
    return(url)
  }

  authority_start <- if (network_path) {
    3L
  } else {
    attr(scheme, "match.length") + 1L
  }
  remainder <- substr(url, authority_start, nchar(url))
  delimiter <- regexpr("[/?#]", remainder, perl = TRUE)[[1]]
  authority_end <- if (delimiter < 0L) {
    nchar(remainder)
  } else {
    delimiter - 1L
  }
  authority <- substr(remainder, 1L, authority_end)
  at <- gregexpr("@", authority, fixed = TRUE)[[1]]
  if (identical(at, -1L)) {
    return(url)
  }

  host <- substr(authority, max(at) + 1L, nchar(authority))
  suffix <- if (delimiter < 0L) {
    ""
  } else {
    substr(remainder, delimiter, nchar(remainder))
  }
  paste0(
    substr(url, 1L, authority_start - 1L),
    host,
    suffix
  )
}

onet_redact_url_credentials <- function(url) {
  if (is.null(url) || length(url) != 1 || is.na(url)) {
    return(url)
  }
  scheme <- regexpr(
    "^[[:alpha:]][[:alnum:]+.-]*:",
    url,
    perl = TRUE
  )
  if (scheme[[1]] == 1L) {
    scheme_end <- attr(scheme, "match.length")
    scheme_value <- substr(url, scheme_end + 1L, nchar(url))
    if (!startsWith(scheme_value, "/")) {
      return(paste0(substr(url, 1L, scheme_end), "[REDACTED]"))
    }
  }
  url <- onet_redact_url_authority(url)
  url <- onet_redact_url_path_credentials(url)
  literal_at <- "ONET2RLITERALAT9C3E"
  url <- gsub("@", literal_at, url, fixed = TRUE)
  query_source <- sub("#.*$", "", url)
  raw_query <- if (grepl("?", query_source, fixed = TRUE)) {
    sub("^[^?]*\\?", "", query_source)
  } else {
    NULL
  }
  query_parts <- if (is.null(raw_query)) {
    character()
  } else {
    strsplit(raw_query, "&", fixed = TRUE)[[1]]
  }
  query_equals <- vapply(
    query_parts,
    \(part) regexpr("=", part, fixed = TRUE)[[1]],
    integer(1)
  )
  query_has_bare <- any(query_equals <= 1L)
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
    base <- url
    if (!is.null(query)) {
      base <- paste0(base, "?", onet_redact_url_parameters(query))
    }
    if (!is.null(fragment)) {
      base <- paste0(base, "#", onet_redact_url_fragment(fragment))
    }
    return(gsub(literal_at, "@", base, fixed = TRUE))
  }
  parsed$username <- NULL
  parsed$password <- NULL
  if (query_has_bare) {
    parsed$query <- NULL
  } else if (length(parsed$query) > 0) {
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
  if (query_has_bare) {
    redacted <- paste0(redacted, "?[REDACTED]")
  }
  if (!is.null(fragment)) {
    redacted <- paste0(redacted, "#", fragment)
  }
  gsub(literal_at, "@", redacted, fixed = TRUE)
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
  ), class = "onet2r_download_warning")
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
      ),
      class = "onet2r_digest_mismatch"
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

  result <- onet_with_cache_lock(
    dest,
    onet_commit_source_unlocked(
      tmp = tmp,
      dest = dest,
      receipt = receipt,
      snapshot = snapshot
    )
  )
  snapshot_success <- isTRUE(return_snapshot)
  result
}

onet_commit_source_unlocked <- function(
    tmp,
    dest,
    receipt,
    snapshot = NULL) {
  receipt_dest <- onet_receipt_path(dest)
  receipt_tmp <- tempfile(
    paste0(".", basename(receipt_dest), "-write-"),
    tmpdir = dirname(dest)
  )
  on.exit(unlink(receipt_tmp, force = TRUE), add = TRUE)
  saveRDS(receipt, receipt_tmp)

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
          paste(
            "Failed to preserve the existing cache source and receipt,",
            "and the previous pair could not be restored."
          )
        )
      }
      cli::cli_abort(
        paste(
          "Failed to preserve the existing cache source and receipt",
          "before replacement."
        )
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
        paste(
          "Cache source replacement failed and the previous",
          "source-receipt pair could not be restored."
        )
      )
    }
    cli::cli_abort(
      paste(
        "Failed to replace the cache source and receipt;",
        "the previous pair was preserved."
      )
    )
  }

  unlink(backups[!is.na(backups)], force = TRUE)
  if (!is.null(snapshot)) {
    return(onet_copy_verified_snapshot(dest, receipt, snapshot))
  }
  invisible(dest)
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

onet_local_source_snapshot <- function(
    path,
    expected_sha256 = NULL,
    as_of = NULL) {
  source_path <- normalizePath(path, winslash = "\\", mustWork = TRUE)
  extension <- tools::file_ext(path)
  snapshot <- tempfile(
    "onet-local-source-",
    fileext = if (nzchar(extension)) paste0(".", extension) else ""
  )
  success <- FALSE
  on.exit({
    if (!success) {
      unlink(snapshot, force = TRUE)
    }
  }, add = TRUE)

  if (!onet_copy_cache_snapshot(source_path, snapshot)) {
    cli::cli_abort(
      "Failed to create a private snapshot of the local source.",
      class = "onet2r_local_snapshot_error"
    )
  }
  receipt <- onet_source_receipt(
    path = snapshot,
    source_path = source_path,
    expected_sha256 = expected_sha256,
    as_of = as_of
  )
  success <- TRUE
  structure(
    snapshot,
    source_receipt = receipt,
    local_snapshot = TRUE
  )
}

onet_copy_verified_snapshot <- function(path, receipt, snapshot) {
  if (!onet_copy_cache_snapshot(path, snapshot)) {
    cli::cli_abort(
      "Failed to create a private snapshot of the verified cache source.",
      class = "onet2r_cache_snapshot_error"
    )
  }
  snapshot_sha256 <- onet_sha256(snapshot)
  if (!identical(snapshot_sha256, tolower(receipt$actual_sha256))) {
    cli::cli_abort(
      paste(
        "Verified cache source changed while its private snapshot",
        "was being created."
      ),
      class = "onet2r_cache_snapshot_mismatch"
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
      ),
      class = "onet2r_digest_mismatch"
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
