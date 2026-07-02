# =============================================================================
# O*NET longitudinal data updates
# =============================================================================

# Verified from https://www.onetcenter.org/db_releases.html on 2026-07-02.
onet_data_updates_url <- "https://www.onetcenter.org/dl_files/Longitudinal_Data_Updates.xlsx"

#' Get the O&#42;NET Longitudinal Data Updates Record
#'
#' Downloads and parses the official O&#42;NET Longitudinal Data Updates workbook,
#' which records which occupations were re-rated in each data collection stream.
#' Use it as ground truth alongside the change classification from
#' [onet_panel_reconcile()]: a `real_update` row for an occupation the official
#' record says was not re-rated deserves scrutiny, and vice versa.
#'
#' @param path Optional path to a local copy of the workbook. If supplied, no
#'   download is attempted.
#' @param force Logical; re-download even when a cached copy exists.
#'
#' @return A tibble with `data_update_type`, `onet_soc_code`, `title`, and the
#'   normalized columns published in the workbook, such as
#'   `number_of_updates_as_of_08_2025`, `current_date`, `current_database`, and
#'   previous update date/database columns.
#'
#' @details
#' The current workbook contains separate sheets for Incumbent or OE updates,
#' Abilities Analyst updates, and Skills Analyst updates. `onet_data_updates()`
#' combines those sheets and preserves the sheet label in `data_update_type`.
#'
#' O&#42;NET restates historical update counts against the current O&#42;NET-SOC
#' 2019 taxonomy, so per-cycle counts are not comparable across taxonomy
#' versions without bridging; see [onet_crosswalk_bridge()].
#'
#' The file is cached under `tools::R_user_dir("onet2r", "cache")` in the
#' `reference` section; clear it with `onet_cache_clear(what = "reference")`.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   updates <- onet_data_updates()
#'   head(updates)
#' }
onet_data_updates <- function(path = NULL, force = FALSE) {
  rlang::check_installed("readxl", reason = "to read the data updates workbook.")
  if (!is.null(path)) {
    validate_single_string(path, "path")
  }
  if (!is.logical(force) || length(force) != 1 || is.na(force)) {
    cli::cli_abort("{.arg force} must be `TRUE` or `FALSE`.")
  }

  file <- path %||% download_data_updates_file(force = force)
  sheets <- readxl::excel_sheets(file)
  sheets <- sheets[!grepl("^About", sheets, ignore.case = TRUE)]
  data <- stats::setNames(
    lapply(sheets, \(sheet) {
      readxl::read_excel(file, sheet = sheet, col_types = "text")
    }),
    sheets
  )
  parse_data_updates(data)
}

download_data_updates_file <- function(cache_dir = onet_cache_dir(), force = FALSE) {
  validate_single_string(cache_dir, "cache_dir")
  if (!is.logical(force) || length(force) != 1 || is.na(force)) {
    cli::cli_abort("{.arg force} must be `TRUE` or `FALSE`.")
  }

  reference_dir <- file.path(cache_dir, "reference")
  dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(reference_dir, basename(onet_data_updates_url))
  if (file.exists(dest) && file.info(dest)$size > 0 && !isTRUE(force)) {
    return(dest)
  }

  tmp <- tempfile("onet-updates-", tmpdir = reference_dir, fileext = ".xlsx")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  old_options <- options(
    HTTPUserAgent = "onet2r (https://github.com/farach/onet2r)",
    timeout = max(300, getOption("timeout"))
  )
  on.exit(options(old_options), add = TRUE)
  status <- tryCatch(
    utils::download.file(onet_data_updates_url, tmp, mode = "wb", quiet = TRUE),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to download the O*NET data updates workbook.",
          "i" = "URL: {.url {onet_data_updates_url}}",
          "i" = "Download it manually and pass {.arg path}."
        ),
        parent = cnd
      )
    }
  )
  if (!identical(status, 0L) || file.info(tmp)$size <= 0) {
    cli::cli_abort("Failed to download the O*NET data updates workbook.")
  }
  if (file.exists(dest)) {
    unlink(dest, force = TRUE)
  }
  if (!file.rename(tmp, dest)) {
    cli::cli_abort("Failed to move the data updates workbook into the cache.")
  }
  dest
}

parse_data_updates <- function(data) {
  if (is.data.frame(data)) {
    data <- list(data)
  }
  if (!is.list(data)) {
    cli::cli_abort("{.arg data} must be a data frame or list of data frames.")
  }

  parsed <- purrr::imap(data, \(sheet, sheet_name) {
    if (!is.data.frame(sheet)) {
      cli::cli_abort("Each data updates sheet must be a data frame.")
    }
    parse_data_update_sheet(sheet, sheet_name)
  })
  parsed <- purrr::compact(parsed)
  if (length(parsed) == 0) {
    cli::cli_abort("No data update sheets with O*NET-SOC codes were found.")
  }

  dplyr::bind_rows(parsed)
}

parse_data_update_sheet <- function(data, sheet_name) {
  data <- tibble::as_tibble(data)
  names(data) <- normalize_data_update_names(names(data))
  if (!"onet_soc_code" %in% names(data)) {
    return(NULL)
  }

  if ("onet_soc_title" %in% names(data)) {
    names(data)[names(data) == "onet_soc_title"] <- "title"
  }
  if (!"title" %in% names(data)) {
    return(NULL)
  }

  soc_columns <- grep("onet_soc_code", names(data), value = TRUE)
  for (column in soc_columns) {
    data[[column]] <- standardize_onet_soc_code(data[[column]])
  }
  tibble::add_column(
    data,
    data_update_type = as.character(sheet_name),
    .before = 1
  )
}

normalize_data_update_names <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- to_snake_case(x)
  x <- gsub("^o_net", "onet", x)
  gsub("_o_net", "_onet", x)
}
