# =============================================================================
# External AI-exposure measure import adapters
# =============================================================================
#
# Thin adapters that ingest published occupation-level exposure scores and
# broadcast them onto the tasks of a caller-supplied panel, handing the result
# to the bring-your-own-measure machinery unchanged. The package does not
# bundle, cache in source, or transform these third-party scores; it only reads
# a user-supplied file or an explicit download URL and records provenance.

# Pinned raw-file locations. Users may override `url` or pass a local `path`.
# Eloundou et al. (2023) occupation-level exposure table (MIT licensed).
onet_eloundou_url <- paste0(
  "https://raw.githubusercontent.com/openai/GPTs-are-GPTs/",
  "0471612fef3cc22b74fb884d27bff9dbd3770582/data/occ_level.csv"
)
# Felten, Raj, and Seamans AI Occupational Exposure workbook.
onet_felten_aioe_url <- paste0(
  "https://raw.githubusercontent.com/AIOE-Data/AIOE/",
  "adca5fc2cd0e9a659ff05278b7fa7a53f4f324c1/AIOE_DataAppendix.xlsx"
)

#' Import Eloundou et al. (2023) GPT-Exposure Scores as a Task-Grain Measure
#'
#' Reads the occupation-level GPT-exposure table released with Eloundou,
#' Manning, Mishkin, and Rock (2023), "GPTs are GPTs: An Early Look at the Labor
#' Market Impact Potential of Large Language Models", and broadcasts it onto the
#' tasks of a Task Ratings style `panel`, returning a task-grain [onet_measure()]
#' keyed on `(occupation, task)`. Every task inherits its occupation's published
#' exposure. This is a thin adapter: it selects the score column, standardizes
#' the O&#42;NET-SOC code, joins to the panel, and records provenance. It does
#' not rescale, average, or otherwise transform the published exposure values.
#'
#' @param panel A Task Ratings style panel with an occupation column
#'   (`occupation_code`, default `"onet_soc_code"`) and a task column (`task_id`,
#'   default `"task_id"`). Its distinct occupation-task pairs set the grain of
#'   the returned measure.
#' @param path Optional path to a local copy of the exposure file. A comma or
#'   tab separated file or an Excel workbook is accepted. When supplied, no
#'   download is attempted.
#' @param url Download URL used when `path` is `NULL`. Defaults to the pinned
#'   occupation-level `occ_level.csv` in the authors' public repository.
#' @param score Name of the exposure column to use as the measure score. The
#'   published `occ_level.csv` carries `human_rating_alpha`, `human_rating_beta`,
#'   `human_rating_gamma` (human-labeled exposure at the alpha, beta, and gamma
#'   definitions) and the `dv_rating_*` model-labeled counterparts. Defaults to
#'   `"human_rating_beta"`.
#' @param key Optional name of the O&#42;NET-SOC code column in the exposure
#'   file. When `NULL`, common column names such as `"O*NET-SOC Code"` are
#'   detected automatically.
#' @param sheet Optional worksheet name or index used when reading an Excel
#'   workbook.
#' @param occupation_code,task_id Names of the occupation and task columns in
#'   `panel`.
#' @param measure_id,measure_name Identifiers recorded on the returned measure.
#' @param force Logical; re-download even when a cached copy exists.
#' @param ... Additional arguments passed to [onet_measure()], such as
#'   `universe` or `weight_panel`.
#' @param expected_sha256 Optional expected SHA-256 digest for the local or
#'   downloaded source. It is verified before parsing and on cache reuse.
#' @param as_of Optional source date or label recorded in provenance. Cached
#'   downloads with different `as_of` metadata are not silently reused.
#'
#' @return A task-grain `onet_measure` object (`key_type = "task"`) keyed on
#'   `task_id` and scored on the selected exposure column, with the occupation
#'   code retained. It is ready for [onet_task_to_occupation()].
#'
#' @details
#' The published scores are occupation-level; broadcasting them to every task of
#' an occupation is the structurally blind aggregate construction the source
#' paper contrasts against task-aware measures. Tasks whose occupation has no
#' published score are dropped with a warning. Pass a single-release panel so
#' each task id is unique.
#'
#' The exposure data are distributed by OpenAI under the MIT License. onet2r
#' never bundles or ships the file; you must supply `path` or download it from
#' `url`. Downloads are cached under `tools::R_user_dir("onet2r", "cache")` in
#' the `reference` section and can be cleared with
#' `onet_cache_clear(what = "reference")`. Cite the source paper when you use the
#' scores. The returned measure includes `metadata$source_receipt` with the
#' source URL or local path, retrieval time, digest, file size, source commit
#' when inferable from a pinned GitHub raw URL, and version or `as_of` metadata.
#'
#' The three exposure definitions follow the paper: alpha counts tasks exposed
#' by direct model access, beta adds tasks reachable with complementary software
#' built on the model, and gamma is the broadest definition. Choosing among them
#' is a substantive decision left to the caller; the adapter does not endorse a
#' definition.
#' @export
#'
#' @examples
#' # Offline: broadcast a small local extract onto a tiny panel.
#' extract <- tempfile(fileext = ".csv")
#' utils::write.csv(
#'   data.frame(
#'     `O*NET-SOC Code` = c("15-1252.00", "29-1141.00"),
#'     Title = c("Software Developers", "Registered Nurses"),
#'     human_rating_beta = c(0.63, 0.14),
#'     check.names = FALSE
#'   ),
#'   extract,
#'   row.names = FALSE
#' )
#' panel <- tibble::tibble(
#'   onet_soc_code = rep(c("15-1252.00", "29-1141.00"), each = 2),
#'   task_id = c("1", "2", "3", "4")
#' )
#' measure <- onet_import_eloundou(panel, path = extract)
#' measure$data
#'
#' # Online: download the published occupation-level table.
#' if (interactive()) {
#'   exposure <- onet_import_eloundou(panel)
#'   head(exposure$data)
#' }
onet_import_eloundou <- function(
    panel,
    path = NULL,
    url = onet_eloundou_url,
    score = "human_rating_beta",
    key = NULL,
    sheet = NULL,
    occupation_code = "onet_soc_code",
    task_id = "task_id",
    measure_id = "eloundou_gpt_exposure",
    measure_name = "Eloundou et al. (2023) GPT exposure",
    force = FALSE,
    ...,
    expected_sha256 = NULL,
    as_of = NULL) {
  validate_single_string(score, "score")

  external <- read_import_measure_file(
    path = path,
    url = url,
    key = key,
    sheet = sheet,
    force = force,
    expected_sha256 = expected_sha256,
    as_of = as_of,
    key_candidates = onet_soc_key_candidates(),
    key_column = "onet_soc_code",
    key_standardize = standardize_onet_soc_code
  )
  source_receipt <- attr(external, "source_receipt", exact = TRUE)
  if (!score %in% names(external)) {
    import_abort_missing_score(score, external, "onet_soc_code")
  }

  broadcast_import_to_tasks(
    external = external,
    panel = panel,
    score = score,
    join_key = "onet_soc_code",
    panel_join_value = standardize_onet_soc_code,
    occupation_code = occupation_code,
    task_id = task_id,
    measure_id = measure_id,
    measure_name = measure_name,
    source = "Eloundou et al. (2023) GPTs are GPTs (MIT License)",
    source_receipt = source_receipt,
    ...
  )
}

#' Import the Felten, Raj, and Seamans AIOE Scores as a Task-Grain Measure
#'
#' Reads the occupation-level AI Occupational Exposure (AIOE) scores from Felten,
#' Raj, and Seamans (2021), "Occupational, Industry, and Geographic Exposure to
#' Artificial Intelligence", and broadcasts them onto the tasks of a Task Ratings
#' style `panel`, returning a task-grain [onet_measure()] keyed on
#' `(occupation, task)`. Every task inherits its occupation's published AIOE
#' score. This is a thin adapter: it selects the score column, standardizes the
#' SOC code, joins to the panel, and records provenance without transforming the
#' published values.
#'
#' @param panel A Task Ratings style panel with an occupation column
#'   (`occupation_code`, default `"onet_soc_code"`) and a task column (`task_id`,
#'   default `"task_id"`). Its distinct occupation-task pairs set the grain of
#'   the returned measure.
#' @param path Optional path to a local copy of the AIOE workbook or a comma or
#'   tab separated export. When supplied, no download is attempted.
#' @param url Download URL used when `path` is `NULL`. Defaults to the pinned
#'   `AIOE_DataAppendix.xlsx` in the authors' public repository.
#' @param score Name of the exposure column to use as the measure score.
#'   Defaults to `"AIOE"`.
#' @param key Optional name of the SOC code column in the workbook. When `NULL`,
#'   common column names such as `"SOC Code"` are detected automatically.
#' @param sheet Worksheet holding the occupation scores. Defaults to
#'   `"Appendix A"`, the occupation sheet of the published workbook.
#' @param occupation_code,task_id Names of the occupation and task columns in
#'   `panel`.
#' @param measure_id,measure_name Identifiers recorded on the returned measure.
#' @param force Logical; re-download even when a cached copy exists.
#' @param ... Additional arguments passed to [onet_measure()], such as
#'   `universe` or `weight_panel`.
#' @param expected_sha256 Optional expected SHA-256 digest for the local or
#'   downloaded source. It is verified before parsing and on cache reuse.
#' @param as_of Optional source date or label recorded in provenance. Cached
#'   downloads with different `as_of` metadata are not silently reused.
#'
#' @return A task-grain `onet_measure` object (`key_type = "task"`) keyed on
#'   `task_id` and scored on the selected AIOE column, with the occupation code
#'   retained. It is ready for [onet_task_to_occupation()].
#'
#' @details
#' AIOE scores are indexed by 6-digit SOC code, not by 8-digit O&#42;NET-SOC
#' code. The adapter derives a 6-digit SOC code from the panel's occupation code
#' and joins on it, then broadcasts each occupation's score to its tasks. Tasks
#' whose occupation has no published score are dropped with a warning. Pass a
#' single-release panel so each task id is unique.
#'
#' onet2r never bundles or ships the workbook; you must supply `path` or download
#' it from `url`. Downloads are cached under `tools::R_user_dir("onet2r",
#' "cache")` in the `reference` section and can be cleared with
#' `onet_cache_clear(what = "reference")`. The AIOE workbook is provided for
#' research use; cite Felten, Raj, and Seamans (2021) when you use the scores.
#' The returned measure includes `metadata$source_receipt` with the source URL
#' or local path, retrieval time, digest, file size, source commit when
#' inferable from a pinned GitHub raw URL, and version or `as_of` metadata.
#' @export
#'
#' @examples
#' # Offline: broadcast a small local extract onto a tiny panel.
#' extract <- tempfile(fileext = ".csv")
#' utils::write.csv(
#'   data.frame(
#'     `SOC Code` = c("15-1252", "29-1141"),
#'     `Occupation Title` = c("Software Developers", "Registered Nurses"),
#'     AIOE = c(1.08, -0.32),
#'     check.names = FALSE
#'   ),
#'   extract,
#'   row.names = FALSE
#' )
#' panel <- tibble::tibble(
#'   onet_soc_code = rep(c("15-1252.00", "29-1141.00"), each = 2),
#'   task_id = c("1", "2", "3", "4")
#' )
#' measure <- onet_import_felten_aioe(panel, path = extract)
#' measure$data
#'
#' # Online: download the published AIOE workbook.
#' if (interactive()) {
#'   aioe <- onet_import_felten_aioe(panel)
#'   head(aioe$data)
#' }
onet_import_felten_aioe <- function(
    panel,
    path = NULL,
    url = onet_felten_aioe_url,
    score = "AIOE",
    key = NULL,
    sheet = "Appendix A",
    occupation_code = "onet_soc_code",
    task_id = "task_id",
    measure_id = "felten_aioe",
    measure_name = "Felten, Raj, and Seamans (2021) AIOE",
    force = FALSE,
    ...,
    expected_sha256 = NULL,
    as_of = NULL) {
  validate_single_string(score, "score")

  external <- read_import_measure_file(
    path = path,
    url = url,
    key = key,
    sheet = sheet,
    force = force,
    expected_sha256 = expected_sha256,
    as_of = as_of,
    key_candidates = soc_key_candidates(),
    key_column = "soc_code",
    key_standardize = standardize_soc_code
  )
  source_receipt <- attr(external, "source_receipt", exact = TRUE)
  if (!score %in% names(external)) {
    import_abort_missing_score(score, external, "soc_code")
  }

  broadcast_import_to_tasks(
    external = external,
    panel = panel,
    score = score,
    join_key = "soc_code",
    panel_join_value = standardize_soc_code,
    occupation_code = occupation_code,
    task_id = task_id,
    measure_id = measure_id,
    measure_name = measure_name,
    source = "Felten, Raj, and Seamans (2021) AIOE",
    source_receipt = source_receipt,
    ...
  )
}

# --- shared import helpers ---------------------------------------------------

# Broadcast an occupation-level external score table onto the distinct
# occupation-task pairs of a panel, producing a task-grain measure. Every task
# inherits its occupation's score; tasks whose occupation has no score are
# dropped with a warning. This intentional broadcast is the structurally blind
# aggregate construction that task-aware measures are contrasted against.
broadcast_import_to_tasks <- function(
    external,
    panel,
    score,
    join_key,
    panel_join_value,
    occupation_code,
    task_id,
    measure_id,
    measure_name,
    source,
    source_receipt,
    ...) {
  validate_single_string(occupation_code, "occupation_code")
  validate_single_string(task_id, "task_id")
  if (!is.data.frame(panel)) {
    cli::cli_abort("{.arg panel} must be a data frame.")
  }
  validate_columns_present(panel, c(occupation_code, task_id), "panel")

  pairs <- dplyr::distinct(
    tibble::as_tibble(panel),
    dplyr::across(dplyr::all_of(c(occupation_code, task_id)))
  )
  pairs[[join_key]] <- panel_join_value(pairs[[occupation_code]])

  scores <- tibble::as_tibble(external)[, c(join_key, score), drop = FALSE]
  joined <- dplyr::left_join(
    pairs,
    scores,
    by = join_key,
    relationship = "many-to-one"
  )

  matched <- !is.na(joined[[score]])
  if (any(!matched)) {
    missing_occ <- unique(joined[[occupation_code]][!matched])
    cli::cli_warn(
      c(
        "Dropped {sum(!matched)} task{?s} with no matching exposure score.",
        "i" = "{length(missing_occ)} occupation{?s} had no score in the import file."
      )
    )
    joined <- joined[matched, , drop = FALSE]
  }
  if (nrow(joined) == 0) {
    cli::cli_abort(
      c(
        "No panel tasks matched an exposure score.",
        "i" = "Check that the panel occupation codes align with the import file."
      )
    )
  }

  measure <- onet_measure(
    joined,
    key = task_id,
    score = score,
    key_type = "task",
    measure_id = measure_id,
    measure_name = measure_name,
    source = source,
    ...
  )
  measure$metadata$source_receipt <- source_receipt
  measure
}

onet_soc_key_candidates <- function() {
  c(
    "onet_soc_code", "O*NET-SOC Code", "onetsoc_code", "onet_soc",
    "ONET-SOC Code", "O*NET SOC Code", "code"
  )
}

soc_key_candidates <- function() {
  c("soc_code", "SOC Code", "SOC", "soc", "OCC_CODE", "occ_code", "code")
}

# Resolve, read, and key-standardize an external measure file. Returns a tibble
# with a standardized key column moved to the front and every published column
# preserved.
read_import_measure_file <- function(
    path,
    url,
    key,
    sheet,
    force,
    expected_sha256,
    as_of,
    key_candidates,
    key_column,
    key_standardize) {
  if (!is.null(key)) {
    validate_single_string(key, "key")
  }
  if (!is.logical(force) || length(force) != 1 || is.na(force)) {
    cli::cli_abort("{.arg force} must be `TRUE` or `FALSE`.")
  }
  as_of <- onet_normalize_as_of(as_of)
  if (is.null(path) && is.null(url)) {
    cli::cli_abort(
      c(
        "Supply {.arg path} to a local file or a download {.arg url}.",
        "i" = "onet2r never bundles third-party exposure data."
      )
    )
  }

  file <- if (!is.null(path)) {
    validate_single_string(path, "path")
    if (!file.exists(path)) {
      cli::cli_abort("{.arg path} does not exist: {.file {path}}.")
    }
    receipt <- onet_source_receipt(
      path = path,
      source_path = path,
      expected_sha256 = expected_sha256,
      as_of = as_of
    )
    structure(path, source_receipt = receipt)
  } else {
    download_import_file(
      url,
      force = force,
      expected_sha256 = expected_sha256,
      as_of = as_of
    )
  }
  source_receipt <- attr(file, "source_receipt", exact = TRUE)

  data <- read_import_table(file, sheet = sheet)
  if (ncol(data) == 0 || nrow(data) == 0) {
    cli::cli_abort("The import file contains no rows to read.")
  }

  key_name <- key %||% detect_import_column(names(data), key_candidates, "key")
  if (!key_name %in% names(data)) {
    cli::cli_abort("{.arg key} must name a column in the import file: {.val {key_name}}.")
  }

  standardized <- key_standardize(data[[key_name]])
  if (key_name != key_column && key_column %in% names(data)) {
    data[[key_column]] <- NULL
  }
  data[[key_name]] <- NULL
  out <- tibble::as_tibble(data)
  out[[key_column]] <- standardized
  out <- out[, c(key_column, setdiff(names(out), key_column)), drop = FALSE]
  attr(out, "source_receipt") <- source_receipt
  out
}

read_import_table <- function(file, sheet = NULL) {
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("xlsx", "xls", "xlsm")) {
    rlang::check_installed("readxl", reason = "to read an Excel import file.")
    sheet_arg <- sheet %||% 1
    return(tibble::as_tibble(
      readxl::read_excel(file, sheet = sheet_arg)
    ))
  }
  sep <- if (ext %in% c("tsv", "tab", "txt")) "\t" else ","
  tibble::as_tibble(utils::read.delim(
    file,
    sep = sep,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    colClasses = "character"
  ))
}

# Match a wanted column against the available names, tolerant of case,
# punctuation, and whitespace differences.
detect_import_column <- function(available, candidates, arg) {
  norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))
  available_norm <- norm(available)
  for (candidate in candidates) {
    hit <- which(available_norm == norm(candidate))
    if (length(hit) >= 1) {
      return(available[hit[[1]]])
    }
  }
  cli::cli_abort(
    c(
      "Could not find a {.arg {arg}} column in the import file.",
      "i" = "Available columns: {.val {available}}.",
      "i" = "Pass {.arg {arg}} explicitly to select one."
    )
  )
}

download_import_file <- function(
    url,
    cache_dir = onet_cache_dir(),
    force = FALSE,
    expected_sha256 = NULL,
    as_of = NULL) {
  validate_single_string(url, "url")
  expected_sha256 <- onet_normalize_sha256(expected_sha256)
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)

  dest_name <- basename(sub("[?#].*$", "", url))
  if (!nzchar(dest_name)) {
    dest_name <- paste0("onet-import-", substr(rlang::hash(url), 1, 10))
  }
  dest <- file.path(reference_dir, dest_name)
  if (file.exists(dest) && file.info(dest)$size > 0 && !isTRUE(force)) {
    receipt <- onet_cached_source_receipt(
      path = dest,
      source_url = url,
      expected_sha256 = expected_sha256,
      version = onet_source_commit(url),
      as_of = as_of
    )
    return(structure(dest, source_receipt = receipt))
  }

  tmp <- tempfile("onet-import-", tmpdir = reference_dir)
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  old_options <- options(
    HTTPUserAgent = "onet2r (https://github.com/farach/onet2r)",
    timeout = max(300, getOption("timeout"))
  )
  on.exit(options(old_options), add = TRUE)
  status <- tryCatch(
    utils::download.file(url, tmp, mode = "wb", quiet = TRUE),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to download the import file.",
          "i" = "URL: {.url {url}}",
          "i" = "Download it manually and pass {.arg path}."
        ),
        parent = cnd
      )
    }
  )
  if (!identical(status, 0L) || file.info(tmp)$size <= 0) {
    cli::cli_abort("Failed to download the import file from {.url {url}}.")
  }
  source_commit <- onet_source_commit(url)
  receipt <- onet_source_receipt(
    path = tmp,
    source_url = url,
    expected_sha256 = expected_sha256,
    version = source_commit,
    as_of = as_of
  )
  onet_atomic_commit_source(tmp, dest, receipt)
  structure(dest, source_receipt = receipt)
}

import_abort_missing_score <- function(score, data, key_column) {
  numeric_like <- setdiff(names(data), key_column)
  cli::cli_abort(
    c(
      "{.arg score} column {.val {score}} is not present in the import file.",
      "i" = "Available columns: {.val {numeric_like}}.",
      "i" = "Pass {.arg score} to select the exposure column."
    )
  )
}
