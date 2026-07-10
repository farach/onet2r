wait_for_test_path <- function(path, processes = list(), timeout = 60) {
  deadline <- Sys.time() + timeout
  repeat {
    if (file.exists(path) || dir.exists(path)) {
      return(invisible(path))
    }
    exited <- vapply(processes, function(process) {
      !process$is_alive()
    }, logical(1))
    if (any(exited)) {
      process <- processes[[which(exited)[[1]]]]
      if (!identical(process$get_exit_status(), 0L)) {
        process$get_result()
      }
      stop("Worker exited without creating synchronization path: ", path)
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for test synchronization path: ", path)
    }
    Sys.sleep(0.02)
  }
}

wait_for_test_process <- function(process, timeout = 60000) {
  process$wait(timeout = timeout)
  if (process$is_alive()) {
    stop("Timed out waiting for a concurrency test worker to exit.")
  }
  process$get_result()
}

cache_concurrency_worker <- function(
    package_path,
    operation,
    cache_dir,
    dest,
    value,
    attempted,
    entered,
    proceed,
    completed) {
  if (file.exists(file.path(package_path, "R", "cache.R"))) {
    pkgload::load_all(package_path, quiet = TRUE)
  } else {
    library(
      "onet2r",
      character.only = TRUE,
      lib.loc = dirname(package_path)
    )
  }
  namespace <- asNamespace("onet2r")
  signal <- function(path, text = "ready") {
    writeLines(text, path, useBytes = TRUE)
  }
  wait_for_signal <- function(path, timeout = 60) {
    deadline <- Sys.time() + timeout
    while (!file.exists(path)) {
      if (Sys.time() >= deadline) {
        stop("Timed out waiting for worker signal: ", path)
      }
      Sys.sleep(0.02)
    }
  }

  signal(attempted)
  if (identical(operation, "clear")) {
    get("onet_cache_clear", namespace)(
      cache_dir = cache_dir,
      what = "reference"
    )
    signal(completed)
    return(invisible(NULL))
  }

  get("onet_with_cache_transaction", namespace)(dest, {
    signal(entered)
    wait_for_signal(proceed)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(
      "concurrent-refresh-",
      tmpdir = dirname(dest),
      fileext = ".csv"
    )
    on.exit(unlink(tmp, force = TRUE), add = TRUE)
    writeBin(charToRaw(value), tmp)
    receipt <- get("onet_source_receipt", namespace)(tmp)
    get("onet_atomic_commit_source", namespace)(
      tmp = tmp,
      dest = dest,
      receipt = receipt
    )
    actual <- get("onet_sha256", namespace)(dest)
    if (!identical(actual, receipt$actual_sha256)) {
      stop("Worker committed a mixed source and receipt.")
    }
    signal(completed, actual)
  })
  invisible(NULL)
}

test_that("SHA-256 hashes exact file bytes on supported R versions", {
  fixture_dir <- withr::local_tempdir()
  empty <- file.path(fixture_dir, "empty.bin")
  abc <- file.path(fixture_dir, "abc.bin")
  file.create(empty)
  writeBin(charToRaw("abc"), abc)

  expect_identical(
    onet2r:::onet_sha256(empty),
    "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
  )
  expect_identical(
    onet2r:::onet_sha256(abc),
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
  )
})

test_that("SHA-256 failures have explicit package condition classes", {
  missing <- file.path(withr::local_tempdir(), "missing.bin")
  condition <- tryCatch(
    onet2r:::onet_sha256(missing),
    error = identity
  )

  expect_s3_class(condition, "onet2r_sha256_error")
  expect_match(conditionMessage(condition), "Failed to calculate SHA-256")
})

test_that("cache lock release preserves a replacement injected before rmdir", {
  lock <- file.path(withr::local_tempdir(), "source.csv.lock")
  old <- onet2r:::onet_acquire_cache_lock(lock)
  old_owner <- file.path(lock, old$token)
  replacement <- structure(
    list(path = lock, token = "owner-injected-replacement"),
    class = "onet2r_cache_lock"
  )
  original_rmdir <- onet2r:::onet_remove_empty_lock_dir
  injected <- FALSE
  owner_was_removed <- FALSE

  released <- testthat::with_mocked_bindings(
    onet2r:::onet_release_cache_lock(old),
    onet_remove_empty_lock_dir = function(path) {
      owner_was_removed <<- !file.exists(old_owner)
      if (!injected) {
        writeLines(
          replacement$token,
          file.path(path, replacement$token),
          useBytes = TRUE
        )
        injected <<- TRUE
      }
      original_rmdir(path)
    },
    .package = "onet2r"
  )

  expect_identical(released, FALSE)
  expect_identical(owner_was_removed, TRUE)
  expect_identical(
    file.exists(file.path(lock, replacement$token)),
    TRUE
  )
  contention <- tryCatch(
    onet2r:::onet_acquire_cache_lock(lock, timeout = 0),
    error = identity
  )
  expect_s3_class(contention, "onet2r_cache_lock_timeout")
  expect_identical(onet2r:::onet_release_cache_lock(replacement), TRUE)
  expect_identical(dir.exists(lock), FALSE)
})

test_that("transaction marker write failure cleans registration for retry", {
  cache_dir <- withr::local_tempdir()
  section <- file.path(cache_dir, "reference")
  path <- file.path(section, "source.csv")
  coordination <- onet2r:::onet_cache_coordination_paths(section)
  key <- onet2r:::onet_cache_transaction_key(section)

  condition <- testthat::with_mocked_bindings(
    tryCatch(
      onet2r:::onet_begin_cache_transaction(path),
      error = identity
    ),
    onet_write_tx_marker = function(...) {
      stop("injected marker write failure")
    },
    .package = "onet2r"
  )

  expect_s3_class(
    condition,
    "onet2r_cache_transaction_registration_error"
  )
  expect_identical(
    list.files(coordination$active, all.files = TRUE, no.. = TRUE),
    character()
  )
  expect_identical(
    exists(
      key,
      envir = onet2r:::.onet_cache_transaction_state,
      inherits = FALSE
    ),
    FALSE
  )

  handle <- onet2r:::onet_begin_cache_transaction(path)
  expect_identical(onet2r:::onet_end_cache_transaction(handle), TRUE)
  expect_identical(
    onet2r:::onet_clear_cache_section(section, timeout = 1),
    normalizePath(section, winslash = "/", mustWork = FALSE)
  )
})

test_that("transaction state failure cleans marker and partial state", {
  cache_dir <- withr::local_tempdir()
  section <- file.path(cache_dir, "reference")
  path <- file.path(section, "source.csv")
  coordination <- onet2r:::onet_cache_coordination_paths(section)
  key <- onet2r:::onet_cache_transaction_key(section)

  condition <- testthat::with_mocked_bindings(
    tryCatch(
      onet2r:::onet_begin_cache_transaction(path),
      error = identity
    ),
    onet_set_tx_state = function(key, state) {
      assign(
        key,
        state,
        envir = onet2r:::.onet_cache_transaction_state
      )
      stop("injected state assignment failure")
    },
    .package = "onet2r"
  )

  expect_s3_class(condition, "onet2r_cache_transaction_state_error")
  expect_identical(
    list.files(coordination$active, all.files = TRUE, no.. = TRUE),
    character()
  )
  expect_identical(
    exists(
      key,
      envir = onet2r:::.onet_cache_transaction_state,
      inherits = FALSE
    ),
    FALSE
  )

  handle <- onet2r:::onet_begin_cache_transaction(path)
  expect_identical(onet2r:::onet_end_cache_transaction(handle), TRUE)
  expect_identical(
    onet2r:::onet_clear_cache_section(section, timeout = 1),
    normalizePath(section, winslash = "/", mustWork = FALSE)
  )
})

test_that("transaction marker delete failure retains state for cleanup", {
  cache_dir <- withr::local_tempdir()
  section <- file.path(cache_dir, "reference")
  path <- file.path(section, "source.csv")
  key <- onet2r:::onet_cache_transaction_key(section)
  handle <- onet2r:::onet_begin_cache_transaction(path)

  condition <- testthat::with_mocked_bindings(
    tryCatch(
      onet2r:::onet_end_cache_transaction(handle),
      error = identity
    ),
    onet_remove_tx_marker = function(...) FALSE,
    .package = "onet2r"
  )

  expect_s3_class(condition, "onet2r_cache_transaction_cleanup_error")
  state <- get(
    key,
    envir = onet2r:::.onet_cache_transaction_state,
    inherits = FALSE
  )
  expect_identical(state$cleanup_pending, TRUE)
  expect_identical(file.exists(state$marker), TRUE)
  expect_identical(onet2r:::onet_end_cache_transaction(handle), TRUE)
  expect_identical(file.exists(state$marker), FALSE)
  expect_identical(
    exists(
      key,
      envir = onet2r:::.onet_cache_transaction_state,
      inherits = FALSE
    ),
    FALSE
  )

  handle <- onet2r:::onet_begin_cache_transaction(path)
  condition <- testthat::with_mocked_bindings(
    tryCatch(
      onet2r:::onet_end_cache_transaction(handle),
      error = identity
    ),
    onet_remove_tx_marker = function(...) FALSE,
    .package = "onet2r"
  )
  expect_s3_class(condition, "onet2r_cache_transaction_cleanup_error")
  expect_identical(
    onet2r:::onet_clear_cache_section(section, timeout = 1),
    normalizePath(section, winslash = "/", mustWork = FALSE)
  )
  expect_identical(
    exists(
      key,
      envir = onet2r:::.onet_cache_transaction_state,
      inherits = FALSE
    ),
    FALSE
  )
})

test_that("cache clear coordinates two concurrent source refreshes", {
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  cache_dir <- withr::local_tempdir()
  reference_dir <- file.path(cache_dir, "reference")
  sync_dir <- withr::local_tempdir()
  dir.create(reference_dir, recursive = TRUE)
  dest <- file.path(reference_dir, "source.csv")
  writeBin(charToRaw("initial"), dest)
  saveRDS(
    onet2r:::onet_source_receipt(dest),
    onet2r:::onet_receipt_path(dest)
  )

  marker <- function(name) file.path(sync_dir, name)
  package_path <- find.package("onet2r")
  worker_args <- function(
      operation,
      value,
      attempted,
      entered,
      proceed,
      completed) {
    list(
      package_path = package_path,
      operation = operation,
      cache_dir = cache_dir,
      dest = dest,
      value = value,
      attempted = marker(attempted),
      entered = marker(entered),
      proceed = marker(proceed),
      completed = marker(completed)
    )
  }

  refresh_one <- callr::r_bg(
    cache_concurrency_worker,
    args = worker_args(
      "refresh",
      "first-refresh",
      "refresh-one-attempted",
      "refresh-one-entered",
      "refresh-one-proceed",
      "refresh-one-completed"
    ),
    supervise = TRUE
  )
  withr::defer({
    if (refresh_one$is_alive()) {
      refresh_one$kill()
    }
  })
  wait_for_test_path(
    marker("refresh-one-entered"),
    list(refresh_one)
  )

  clear <- callr::r_bg(
    cache_concurrency_worker,
    args = worker_args(
      "clear",
      "",
      "clear-attempted",
      "unused-clear-entered",
      "unused-clear-proceed",
      "clear-completed"
    ),
    supervise = TRUE
  )
  withr::defer({
    if (clear$is_alive()) {
      clear$kill()
    }
  })
  coordination <- onet2r:::onet_cache_coordination_paths(reference_dir)
  wait_for_test_path(marker("clear-attempted"), list(clear))
  wait_for_test_path(coordination$gate, list(clear))

  refresh_two <- callr::r_bg(
    cache_concurrency_worker,
    args = worker_args(
      "refresh",
      "second-refresh",
      "refresh-two-attempted",
      "refresh-two-entered",
      "refresh-two-proceed",
      "refresh-two-completed"
    ),
    supervise = TRUE
  )
  withr::defer({
    if (refresh_two$is_alive()) {
      refresh_two$kill()
    }
  })
  wait_for_test_path(
    marker("refresh-two-attempted"),
    list(refresh_two)
  )

  expect_identical(file.exists(marker("refresh-two-entered")), FALSE)
  expect_length(
    list.files(coordination$active, all.files = TRUE, no.. = TRUE),
    1L
  )

  file.create(marker("refresh-one-proceed"))
  wait_for_test_path(
    marker("refresh-one-completed"),
    list(refresh_one)
  )
  wait_for_test_path(marker("clear-completed"), list(clear))
  wait_for_test_path(
    marker("refresh-two-entered"),
    list(refresh_two)
  )
  file.create(marker("refresh-two-proceed"))
  wait_for_test_path(
    marker("refresh-two-completed"),
    list(refresh_two)
  )

  wait_for_test_process(refresh_one)
  wait_for_test_process(clear)
  wait_for_test_process(refresh_two)

  receipt <- readRDS(onet2r:::onet_receipt_path(dest))
  expect_identical(
    readBin(dest, "raw", file.info(dest)$size),
    charToRaw("second-refresh")
  )
  expect_identical(onet2r:::onet_sha256(dest), receipt$actual_sha256)
  expect_identical(
    list.files(coordination$active, all.files = TRUE, no.. = TRUE),
    character()
  )
  expect_identical(dir.exists(coordination$gate), FALSE)
  expect_identical(
    any(grepl(
      "\\.lock$",
      list.dirs(cache_dir, recursive = TRUE, full.names = TRUE)
    )),
    FALSE
  )
})
