# =============================================================================
# External AI-exposure measure import adapters
# =============================================================================
#
# Thin adapters that ingest published occupation-level exposure scores and hand
# them to the bring-your-own-measure machinery unchanged. The package does not
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

#' Import Eloundou et al. (2023) Occupation GPT-Exposure Scores
#'
#' Reads the occupation-level GPT-exposure table released with Eloundou,
#' Manning, Mishkin, and Rock (2023), "GPTs are GPTs: An Early Look at the Labor
#' Market Impact Potential of Large Language Models", and returns it as an
#' occupation-grain [onet_measure()] object. This is a thin adapter: it selects
#' the key and score columns, standardizes the O&#42;NET-SOC code, and records
#' provenance. It does not rescale, average, or otherwise transform the
#' published exposure values.
#'
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
#' @param key Optional name of the O&#42;NET-SOC code column. When `NULL`, common
#'   column names such as `"O*NET-SOC Code"` are detected automatically.
#' @param sheet Optional worksheet name or index used when reading an Excel
#'   workbook.
#' @param measure_id,measure_name Identifiers recorded on the returned measure.
#' @param force Logical; re-download even when a cached copy exists.
#' @param ... Additional arguments passed to [onet_measure()], such as
#'   `universe` or `weight_panel`.
#'
#' @return An occupation-grain `onet_measure` object. The unchanged published
#'   columns are preserved alongside `measure_key` (the standardized 8-digit
#'   O&#42;NET-SOC code) and `measure_score` (the selected exposure column).
#'
#' @details
#' The exposure data are distributed by OpenAI under the MIT License. onet2r
#' never bundles or ships the file; you must supply `path` or download it from
#' `url`. Downloads are cached under `tools::R_user_dir("onet2r", "cache")` in
#' the `reference` section and can be cleared with
#' `onet_cache_clear(what = "reference")`. Cite the source paper when you use the
#' scores.
#'
#' The three exposure definitions follow the paper: alpha counts tasks exposed
#' by direct model access, beta adds tasks reachable with complementary software
#' built on the model, and gamma is the broadest definition. Choosing among them
#' is a substantive decision left to the caller; the adapter does not endorse a
#' definition.
#' @export
#'
#' @examples
#' # Offline: read a small local extract in the published layout.
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
#' measure <- onet_import_eloundou(path = extract)
#' onet_coverage(measure)
#'
#' # Online: download the published occupation-level table.
#' if (interactive()) {
#'   exposure <- onet_import_eloundou()
#'   head(exposure$data)
#' }
onet_import_eloundou <- function(
    path = NULL,
    url = onet_eloundou_url,
    score = "human_rating_beta",
    key = NULL,
    sheet = NULL,
    measure_id = "eloundou_gpt_exposure",
    measure_name = "Eloundou et al. (2023) GPT exposure",
    force = FALSE,
    ...) {
  validate_single_string(score, "score")

  data <- read_import_measure_file(
    path = path,
    url = url,
    key = key,
    sheet = sheet,
    force = force,
    key_candidates = onet_soc_key_candidates(),
    key_column = "onet_soc_code",
    key_standardize = standardize_onet_soc_code
  )
  if (!score %in% names(data)) {
    import_abort_missing_score(score, data, "onet_soc_code")
  }

  onet_measure(
    data,
    key = "onet_soc_code",
    score = score,
    key_type = "occupation",
    measure_id = measure_id,
    measure_name = measure_name,
    source = "Eloundou et al. (2023) GPTs are GPTs (MIT License)",
    ...
  )
}

#' Import the Felten, Raj, and Seamans AI Occupational Exposure (AIOE) Scores
#'
#' Reads the occupation-level AI Occupational Exposure (AIOE) scores from Felten,
#' Raj, and Seamans (2021), "Occupational, Industry, and Geographic Exposure to
#' Artificial Intelligence", and returns them as an occupation-grain
#' [onet_measure()] object. This is a thin adapter: it selects the key and score
#' columns and records provenance without transforming the published values.
#'
#' @param path Optional path to a local copy of the AIOE workbook or a comma or
#'   tab separated export. When supplied, no download is attempted.
#' @param url Download URL used when `path` is `NULL`. Defaults to the pinned
#'   `AIOE_DataAppendix.xlsx` in the authors' public repository.
#' @param score Name of the exposure column to use as the measure score.
#'   Defaults to `"AIOE"`.
#' @param key Optional name of the SOC code column. When `NULL`, common column
#'   names such as `"SOC Code"` are detected automatically.
#' @param sheet Worksheet holding the occupation scores. Defaults to
#'   `"Appendix A"`, the occupation sheet of the published workbook.
#' @param measure_id,measure_name Identifiers recorded on the returned measure.
#' @param force Logical; re-download even when a cached copy exists.
#' @param ... Additional arguments passed to [onet_measure()], such as
#'   `universe` or `weight_panel`.
#'
#' @return An occupation-grain `onet_measure` object keyed on the 6-digit SOC
#'   code (`measure_key`) with `measure_score` set to the selected AIOE column.
#'
#' @details
#' AIOE scores are indexed by 6-digit SOC code, not by 8-digit O&#42;NET-SOC
#' code, so the returned `measure_key` is a 6-digit SOC code suitable for direct
#' joins to OEWS or PUMS weight panels. onet2r never bundles or ships the
#' workbook; you must supply `path` or download it from `url`. Downloads are
#' cached under `tools::R_user_dir("onet2r", "cache")` in the `reference`
#' section and can be cleared with `onet_cache_clear(what = "reference")`.
#'
#' The AIOE workbook is provided for research use; cite Felten, Raj, and Seamans
#' (2021) when you use the scores.
#' @export
#'
#' @examples
#' # Offline: read a small local extract in the published layout.
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
#' measure <- onet_import_felten_aioe(path = extract)
#' onet_coverage(measure)
#'
#' # Online: download the published AIOE workbook.
#' if (interactive()) {
#'   aioe <- onet_import_felten_aioe()
#'   head(aioe$data)
#' }
onet_import_felten_aioe <- function(
    path = NULL,
    url = onet_felten_aioe_url,
    score = "AIOE",
    key = NULL,
    sheet = "Appendix A",
    measure_id = "felten_aioe",
    measure_name = "Felten, Raj, and Seamans (2021) AIOE",
    force = FALSE,
    ...) {
  validate_single_string(score, "score")

  data <- read_import_measure_file(
    path = path,
    url = url,
    key = key,
    sheet = sheet,
    force = force,
    key_candidates = soc_key_candidates(),
    key_column = "soc_code",
    key_standardize = standardize_soc_code
  )
  if (!score %in% names(data)) {
    import_abort_missing_score(score, data, "soc_code")
  }

  onet_measure(
    data,
    key = "soc_code",
    score = score,
    key_type = "occupation",
    measure_id = measure_id,
    measure_name = measure_name,
    source = "Felten, Raj, and Seamans (2021) AIOE",
    ...
  )
}

# --- shared import helpers ---------------------------------------------------

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
    key_candidates,
    key_column,
    key_standardize) {
  if (!is.null(key)) {
    validate_single_string(key, "key")
  }
  if (!is.logical(force) || length(force) != 1 || is.na(force)) {
    cli::cli_abort("{.arg force} must be `TRUE` or `FALSE`.")
  }
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
    path
  } else {
    download_import_file(url, force = force)
  }

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
  out[, c(key_column, setdiff(names(out), key_column)), drop = FALSE]
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

download_import_file <- function(url, cache_dir = onet_cache_dir(), force = FALSE) {
  validate_single_string(url, "url")
  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)

  dest_name <- basename(sub("[?#].*$", "", url))
  if (!nzchar(dest_name)) {
    dest_name <- paste0("onet-import-", substr(rlang::hash(url), 1, 10))
  }
  dest <- file.path(reference_dir, dest_name)
  if (file.exists(dest) && file.info(dest)$size > 0 && !isTRUE(force)) {
    return(dest)
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
  if (file.exists(dest)) {
    unlink(dest, force = TRUE)
  }
  if (!file.rename(tmp, dest)) {
    cli::cli_abort("Failed to move the downloaded import file into the cache.")
  }
  dest
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
