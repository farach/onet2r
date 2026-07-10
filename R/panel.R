# =============================================================================
# O*NET longitudinal archive panel
# =============================================================================

onet_release_archive_url <- "https://www.onetcenter.org/db_releases.html"
onet_resource_url <- "https://www.onetcenter.org"
onet_vintage_levels <- c("2000", "2006", "2009", "2010", "2019")

.onet2r_release_cache <- new.env(parent = emptyenv())

clear_release_cache <- function() {
  rm(list = ls(.onet2r_release_cache), envir = .onet2r_release_cache)
}

#' List O&#42;NET Archive Releases
#'
#' Parses the O&#42;NET database releases archive and returns downloadable text
#' archives. The release archive states release months, not days, so
#' `release_date` uses the first day of each stated month.
#'
#' @param refresh Set `TRUE` to re-scrape the releases page instead of using
#'   the per-session cache.
#' @return A tibble with release metadata and text archive URLs.
#' @details
#' Text archives (`format = "text"`) exist for releases 20.1 (October 2015)
#' onward. Earlier releases (5.0-20.0) are published as legacy ZIPs
#' (`format = "legacy_zip"`); onet2r can download them, but their internal
#' file layouts vary and parsing is verified only for 20.1+. The O&#42;NET
#' production archive begins at 5.0 (April 2003), the first release of the
#' Data Collection Program - no survey-based data exists before that.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   onet_releases()
#' }
onet_releases <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && !is.null(.onet2r_release_cache$releases)) {
    return(.onet2r_release_cache$releases)
  }
  html <- paste(onet_read_lines(onet_release_archive_url), collapse = "\n")
  links <- onet_extract_links(html)

  text_links <- links[
    grepl("/dl_files/database/db_[0-9_]+_text\\.zip$", links) |
      grepl("/dl_files/db_[0-9_]+\\.zip$", links)
  ]
  versions <- purrr::map_chr(text_links, archive_version_from_link)

  headings <- onet_release_headings(html)

  out <- purrr::map(seq_along(versions), \(i) {
    version <- versions[[i]]
    heading <- headings[headings$version == version, , drop = FALSE]
    release_date <- if (nrow(heading) > 0) heading$release_date[[1]] else NA
    month <- if (nrow(heading) > 0) heading$month[[1]] else NA_character_
    year <- if (nrow(heading) > 0) heading$year[[1]] else NA_character_
    tibble::tibble(
      version = version,
      release_date = as.Date(release_date, origin = "1970-01-01"),
      year = parse_onet_integer(year),
      month = month,
      soc_vintage = onet_soc_vintage(version),
      format = if (grepl("_text\\.zip$", text_links[[i]])) "text" else "legacy_zip",
      text_url = onet_absolute_url(text_links[[i]]),
      dictionary_url = onet_dictionary_url(version, links)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::distinct(.data$version, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$release_date), dplyr::desc(.data$version))

  .onet2r_release_cache$releases <- out
  out
}

archive_version_from_link <- function(link) {
  raw <- sub(".*/db_([0-9_]+)(_text)?\\.zip$", "\\1", link)
  if (!grepl("_", raw) && nchar(raw) == 2) {
    return(paste0(substr(raw, 1, 1), ".", substr(raw, 2, 2)))
  }
  gsub("_", ".", raw, fixed = TRUE)
}

#' Download an O&#42;NET Archive
#'
#' Downloads and caches one O&#42;NET text archive. Existing non-empty cached files
#' are reused only when their recorded provenance and digest still match.
#'
#' @param version O&#42;NET database version, for example `"30.3"`.
#' @param dir Cache directory.
#' @param force Logical; if `TRUE`, re-download even when a cached archive exists.
#' @param expected_sha256 Optional expected SHA-256 digest. The digest is checked
#'   before the archive is parsed and whenever a cached file is reused.
#' @param as_of Optional source date or label to record in the receipt. A cached
#'   archive with different `as_of` metadata is not reused.
#'
#' @return The path to the cached ZIP file.
#'
#' @details
#' Downloaded archives are cached under `tools::R_user_dir("onet2r", "cache")`
#' in the `archives` section. Use `onet_cache_clear(what = "archives")` or
#' `onet_cache_clear(what = "all")` to remove them. An RDS receipt beside each
#' archive records its URL, retrieval time, SHA-256 digest, size, version, and
#' optional `as_of` metadata. The archive URLs are not assumed to be immutable;
#' supply `expected_sha256` when an independently verified digest is available.
#' A cached archive without a receipt cannot satisfy the requested URL or
#' version provenance. Use `force = TRUE` to replace legacy cached bytes.
#' The returned path names the shared cache entry, which a later `force = TRUE`
#' call may replace. Use [onet_archive_read()] to parse a verified private
#' snapshot rather than reopening the shared path.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   path <- onet_archive_download("30.3")
#'   basename(path)
#' }
onet_archive_download <- function(
    version,
    dir = onet_cache_dir(),
    force = FALSE,
    expected_sha256 = NULL,
    as_of = NULL) {
  acquired <- onet_archive_acquire(
    version = version,
    dir = dir,
    force = force,
    expected_sha256 = expected_sha256,
    as_of = as_of
  )
  on.exit(unlink(acquired$snapshot, force = TRUE), add = TRUE)
  acquired$cache_path
}

onet_archive_acquire <- function(
    version,
    dir = onet_cache_dir(),
    force = FALSE,
    expected_sha256 = NULL,
    as_of = NULL) {
  validate_single_string(version, "version")
  validate_single_string(dir, "dir")
  if (!is.logical(force) || length(force) != 1 || is.na(force)) {
    cli::cli_abort("{.arg force} must be `TRUE` or `FALSE`.")
  }
  expected_sha256 <- onet_normalize_sha256(expected_sha256)
  as_of <- onet_normalize_as_of(as_of)

  releases <- onet_releases()
  release <- releases[releases$version == version, , drop = FALSE]
  if (nrow(release) == 0) {
    cli::cli_abort("No downloadable text archive found for version {.val {version}}.")
  }

  archive_dir <- file.path(dir, "archives")
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  dest_name <- basename(sub("[?#].*$", "", release$text_url[[1]]))
  dest <- file.path(archive_dir, dest_name)

  onet_with_cache_lock(
    dest,
    {
      if (file.exists(dest) && file.info(dest)$size > 0 && !isTRUE(force)) {
        snapshot <- onet_cached_source_snapshot_unlocked(
          path = dest,
          source_url = release$text_url[[1]],
          expected_sha256 = expected_sha256,
          version = version,
          as_of = as_of
        )
        snapshot <- onet_validate_archive_snapshot(snapshot)
        receipt <- attr(snapshot, "source_receipt", exact = TRUE)
      } else {
        tmp <- tempfile("onet-archive-", tmpdir = archive_dir, fileext = ".zip")
        on.exit(unlink(tmp, force = TRUE), add = TRUE)
        onet_archive_download_file(release$text_url[[1]], tmp)
        receipt <- onet_source_receipt(
          path = tmp,
          source_url = release$text_url[[1]],
          expected_sha256 = expected_sha256,
          version = version,
          as_of = as_of
        )
        validate_archive_zip(tmp)
        snapshot <- onet_atomic_commit_source_unlocked(
          tmp,
          dest,
          receipt,
          return_snapshot = TRUE
        )
        snapshot <- onet_validate_archive_snapshot(snapshot)
      }
      list(
        snapshot = snapshot,
        receipt = receipt,
        cache_path = dest
      )
    },
    timeout = max(300, getOption("timeout", 60))
  )
}

onet_archive_download_file <- function(url, dest) {
  download_warned <- FALSE
  status <- tryCatch(
    withCallingHandlers(
      utils::download.file(
        url = url,
        destfile = dest,
        mode = "wb",
        quiet = TRUE
      ),
      warning = function(cnd) {
        download_warned <<- TRUE
        invokeRestart("muffleWarning")
      }
    ),
    error = function(cnd) {
      safe_url <- onet_redact_url_credentials(url)
      cli::cli_abort(
        c(
          "Failed to download O*NET archive.",
          "i" = "URL: {.url {safe_url}}"
        )
      )
    }
  )
  if (download_warned && identical(status, 0L)) {
    onet_warn_download_completed(url, "O*NET archive")
  }
  if (!identical(status, 0L)) {
    safe_url <- onet_redact_url_credentials(url)
    cli::cli_abort(
      c(
        "Failed to download O*NET archive.",
        "i" = "URL: {.url {safe_url}}"
      )
    )
  }
  invisible(dest)
}

onet_validate_archive_snapshot <- function(snapshot) {
  success <- FALSE
  on.exit({
    if (!success) {
      unlink(snapshot, force = TRUE)
    }
  }, add = TRUE)
  validate_archive_zip(snapshot)
  success <- TRUE
  snapshot
}

#' Read an O&#42;NET Archive Table
#'
#' Reads a descriptor-like table from a cached O&#42;NET text archive and normalizes
#' it to the longitudinal panel schema.
#'
#' @param version O&#42;NET database version, for example `"30.3"`.
#' @param table Archive table name, for example `"Abilities"` or
#'   `"Work Activities"`.
#' @param path Optional path to a local text archive ZIP file or extracted
#'   archive directory. If supplied, no download is attempted.
#' @param release_date Optional release date for a local archive. Downloaded
#'   archives use the release date from [onet_releases()].
#'
#' @return A tibble in the longitudinal panel schema.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   abilities <- onet_archive_read("30.3", "Abilities")
#'   head(abilities)
#' }
onet_archive_read <- function(version, table, path = NULL, release_date = NULL) {
  validate_single_string(version, "version")
  validate_single_string(table, "table")
  if (!is.null(path)) {
    validate_single_string(path, "path")
    validate_existing_path(path)
  }

  archive <- if (is.null(path)) {
    acquired <- onet_archive_acquire(version)
    on.exit(unlink(acquired$snapshot, force = TRUE), add = TRUE)
    acquired$snapshot
  } else {
    path
  }
  member <- onet_archive_member(archive, table)
  member_path <- if (dir.exists(archive)) {
    member
  } else {
    tmpdir <- tempfile("onet-archive-")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
    utils::unzip(archive, files = member, exdir = tmpdir)
    file.path(tmpdir, member)
  }
  data <- utils::read.delim(
    member_path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    quote = "",
    fileEncoding = "UTF-8",
    na.strings = c("NA", "n/a", "N/A")
  )

  release_date <- resolve_archive_release_date(version, path, release_date)

  onet_standardize_archive_table(data, version, table, release_date)
}

#' Assemble an O&#42;NET Longitudinal Panel
#'
#' Reads the same archive table across releases and row-binds the normalized
#' descriptor rows.
#'
#' @param table_or_element Archive table name in the first implementation.
#' @param versions Character vector of O&#42;NET database versions.
#' @param scale Optional scale id filter.
#' @param archives Optional local archive ZIP files or extracted archive
#'   directories. Use a named character vector keyed by version, or an unnamed
#'   vector the same length and order as `versions`.
#' @param release_dates Optional release dates for local archives. Use a named
#'   vector keyed by version, or an unnamed vector the same length and order as
#'   `versions`.
#'
#' @return A tibble in the longitudinal panel schema.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   panel <- onet_panel("Abilities", versions = c("30.2", "30.3"), scale = "IM")
#'   head(panel)
#' }
onet_panel <- function(
    table_or_element,
    versions,
    scale = NULL,
    archives = NULL,
    release_dates = NULL) {
  validate_single_string(table_or_element, "table_or_element")
  if (!is.character(versions) || length(versions) == 0 || anyNA(versions)) {
    cli::cli_abort("{.arg versions} must be a non-empty character vector.")
  }

  panel <- purrr::map(
    versions,
    \(version) {
      onet_archive_read(
        version,
        table_or_element,
        path = archive_value_for_version(archives, versions, version, "archives"),
        release_date = archive_value_for_version(
          release_dates,
          versions,
          version,
          "release_dates"
        )
      )
    }
  ) |>
    purrr::list_rbind()

  if (!is.null(scale)) {
    if (!is.character(scale) || anyNA(scale)) {
      cli::cli_abort("{.arg scale} must be a character vector or `NULL`.")
    }
    panel <- panel |>
      dplyr::filter(as.character(.data$scale_id) %in% scale)
  }

  panel
}

#' Build an O&#42;NET-SOC Crosswalk Bridge
#'
#' Builds an adjacent or chained O&#42;NET-SOC bridge between taxonomy vintages.
#' Bridges keep the native 8-digit O&#42;NET-SOC code and include a derived
#' 6-digit SOC code for employment joins. Equal weights are used for transparent
#' split apportionment.
#'
#' @param from_vintage Source taxonomy vintage.
#' @param to_vintage Target taxonomy vintage.
#' @param weight Weighting method. Only `"equal"` is implemented.
#'
#' @return A tibble with source and target O&#42;NET-SOC codes, derived SOC codes,
#'   vintages, map type, and equal split weights.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   onet_crosswalk_bridge("2010", "2019")
#' }
onet_crosswalk_bridge <- function(
    from_vintage,
    to_vintage,
    weight = c("equal", "employment")) {
  validate_single_string(from_vintage, "from_vintage")
  validate_single_string(to_vintage, "to_vintage")
  weight <- match.arg(weight)
  if (weight == "employment") {
    cli::cli_abort(
      "{.val employment} bridge weights are not implemented yet."
    )
  }

  from_vintage <- as.character(from_vintage)
  to_vintage <- as.character(to_vintage)
  validate_vintage(from_vintage, "from_vintage")
  validate_vintage(to_vintage, "to_vintage")

  if (from_vintage == to_vintage) {
    return(empty_crosswalk_bridge())
  }

  path <- vintage_path(from_vintage, to_vintage)
  bridges <- purrr::map(
    seq_len(length(path) - 1),
    \(i) read_adjacent_crosswalk(path[[i]], path[[i + 1]])
  )

  bridge <- bridges[[1]]
  if (length(bridges) > 1) {
    for (i in 2:length(bridges)) {
      bridge <- chain_crosswalks(bridge, bridges[[i]])
    }
  }

  bridge$step_count <- length(path) - 1L
  bridge$from_vintage <- factor(bridge$from_vintage, levels = onet_vintage_levels)
  bridge$to_vintage <- factor(bridge$to_vintage, levels = onet_vintage_levels)
  bridge$map_type <- factor(
    bridge$map_type,
    levels = c("one_to_one", "split", "merge", "new", "dropped")
  )

  bridge |>
    dplyr::select(
      "from_vintage",
      "to_vintage",
      "from_onet_soc_code",
      "to_onet_soc_code",
      "from_soc_code",
      "to_soc_code",
      "from_title",
      "to_title",
      "step_count",
      "map_type",
      "crosswalk_weight"
    )
}

#' Reconcile O&#42;NET Panel Changes
#'
#' Compares adjacent releases in a panel and classifies changes using the value
#' change by collection-date change truth table.
#'
#' @param panel A tibble from [onet_panel()] or the same schema.
#' @param bridge A bridge from [onet_crosswalk_bridge()].
#' @param weight Weighting method. Only `"equal"` is implemented.
#'
#' @return A tibble of adjacent-release comparisons with change and coverage
#'   flags. Rows with coverage_status `"unmapped_source"` or
#'   `"unmapped_target"` mark occupations absent from the supplied bridge; they
#'   are never safely_comparable. An incomplete hand-built bridge produces many
#'   of these, so inspect them before interpreting change shares.
#' @export
#'
#' @examples
#' panel <- tibble::tibble(
#'   release_version = c("29.0", "30.0", "29.0", "30.0"),
#'   release_date = as.Date(c("2024-08-01", "2025-08-01", "2024-08-01", "2025-08-01")),
#'   soc_vintage = rep("2019", 4),
#'   domain = "Abilities",
#'   onet_soc_code = rep("11-1011.00", 4),
#'   soc_code = rep("11-1011", 4),
#'   element_id = c("1.A.1.a.1", "1.A.1.a.1", "1.A.1.a.2", "1.A.1.a.2"),
#'   element_name = c(
#'     "Oral Comprehension", "Oral Comprehension",
#'     "Written Comprehension", "Written Comprehension"
#'   ),
#'   scale_id = rep("IM", 4),
#'   data_value = c(4.5, 4.7, 4.2, 4.2),
#'   source_date = as.Date(c("2023-08-01", "2024-08-01", "2023-08-01", "2023-08-01")),
#'   domain_source = rep("Analyst", 4)
#' )
#' bridge <- tibble::tibble(
#'   from_vintage = "2019",
#'   to_vintage = "2019",
#'   from_soc_code = "11-1011",
#'   to_soc_code = "11-1011",
#'   map_type = "one_to_one",
#'   crosswalk_weight = 1
#' )
#' onet_panel_reconcile(panel, bridge)
onet_panel_reconcile <- function(panel, bridge, weight = "equal") {
  weight <- match.arg(weight, c("equal", "employment"))
  if (weight == "employment") {
    cli::cli_abort("{.val employment} reconciliation weights are not implemented yet.")
  }
  validate_panel(panel)
  panel <- normalize_reconcile_panel(panel)
  validate_bridge(bridge)
  bridge <- normalize_bridge(bridge)

  releases <- panel |>
    dplyr::distinct(.data$release_version, .data$release_date) |>
    dplyr::arrange(.data$release_date, .data$release_version)

  if (anyNA(releases$release_date)) {
    missing_versions <- releases$release_version[is.na(releases$release_date)]
    cli::cli_abort(c(
      "Every release in {.arg panel} needs a non-missing {.var release_date} to order comparisons.",
      "x" = "Missing for version{?s}: {.val {missing_versions}}.",
      "i" = "For local archives, pass {.arg release_dates} to {.fun onet_panel} or {.arg release_date} to {.fun onet_archive_read}."
    ))
  }

  if (nrow(releases) < 2) {
    return(empty_reconciled_panel())
  }

  comparisons <- purrr::map(seq_len(nrow(releases) - 1), \(i) {
    reconcile_release_pair(
      panel = panel,
      bridge = bridge,
      from_release = releases$release_version[[i]],
      to_release = releases$release_version[[i + 1]]
    )
  })

  purrr::list_rbind(comparisons)
}

#' Summarise O&#42;NET Panel Change Types
#'
#' Counts reconciliation change types overall and by two-digit job family.
#'
#' @param reconciled A tibble from [onet_panel_reconcile()].
#' @param by Summary level to return. `"overall"` returns one package-level row;
#'   `"job_family"` returns the overall row plus one row per two-digit SOC
#'   family.
#'
#' @return A tibble with one row per summary group and change type. The `n`
#'   column counts rows of that change type within the group, and `share` is
#'   the within-group share. `n_weighted` and `share_weighted` down-weight
#'   split/merge branch rows by their crosswalk_weight so a one-to-many split
#'   counts as one occupation, not multiple branch rows.
#' @export
#'
#' @examples
#' reconciled <- tibble::tibble(
#'   to_soc_code = c("11-1011", "11-1011", "15-1252"),
#'   change_type = factor(c("real_update", "stale_carryforward", "real_update")),
#'   safely_comparable = c(TRUE, TRUE, TRUE)
#' )
#' onet_change_summary(reconciled)
onet_change_summary <- function(reconciled, by = c("overall", "job_family")) {
  by <- match.arg(by)
  required <- c("change_type", "safely_comparable")
  validate_columns_present(reconciled, required, "reconciled")

  data <- tibble::as_tibble(reconciled)
  if (!"to_soc_code" %in% names(data)) {
    data$to_soc_code <- NA_character_
  }
  if (!"value_change" %in% names(data)) {
    data$value_change <- NA_real_
  }
  if (!"method_break" %in% names(data)) {
    data$method_break <- NA
  }
  if (!"crosswalk_uncertain" %in% names(data)) {
    data$crosswalk_uncertain <- NA
  }
  if (!"crosswalk_weight" %in% names(data)) {
    data$crosswalk_weight <- 1
  }
  data$crosswalk_weight[is.na(data$crosswalk_weight)] <- 1
  data$job_family <- substr(data$to_soc_code, 1, 2)
  total_pairs <- nrow(data)

  overall <- summarize_change_group(
    data = data,
    summary_level = "overall",
    job_family = NA_character_,
    total_pairs = total_pairs
  )
  if (by == "overall") {
    return(overall)
  }

  by_family <- data |>
    dplyr::filter(!is.na(.data$job_family), nzchar(.data$job_family)) |>
    dplyr::group_split(.data$job_family) |>
    purrr::map(\(x) {
      summarize_change_group(
        data = x,
        summary_level = "job_family",
        job_family = x$job_family[[1]],
        total_pairs = total_pairs
      )
    }) |>
    purrr::list_rbind()

  dplyr::bind_rows(overall, by_family)
}

summarize_change_group <- function(data, summary_level, job_family, total_pairs) {
  n_pairs <- nrow(data)
  group_stats <- tibble::tibble(
    summary_level = summary_level,
    job_family = job_family,
    n_group = n_pairs,
    share_group = n_pairs / total_pairs,
    mean_value_change = safe_mean(data$value_change),
    median_abs_value_change = safe_median(abs(data$value_change)),
    share_safely_comparable = safe_mean(data$safely_comparable),
    share_method_break = safe_mean(data$method_break),
    share_crosswalk_uncertain = safe_mean(data$crosswalk_uncertain)
  )
  if (n_pairs == 0) {
    return(dplyr::bind_cols(
      group_stats,
      tibble::tibble(
        change_type = NA_character_,
        n = 0L,
        n_weighted = 0,
        share = NA_real_,
        share_weighted = NA_real_
      )
    ))
  }
  total_weight <- sum(data$crosswalk_weight)
  distribution <- data |>
    dplyr::summarise(
      n = dplyr::n(),
      n_weighted = sum(.data$crosswalk_weight),
      .by = "change_type"
    ) |>
    dplyr::mutate(
      change_type = as.character(.data$change_type),
      share = .data$n / n_pairs,
      share_weighted = .data$n_weighted / total_weight
    ) |>
    dplyr::arrange(.data$change_type)
  dplyr::bind_cols(group_stats[rep(1, nrow(distribution)), ], distribution)
}

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_median <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  stats::median(x, na.rm = TRUE)
}

onet_cache_dir <- function() {
  tools::R_user_dir("onet2r", "cache")
}

onet_read_lines <- function(url) {
  old_options <- options(
    HTTPUserAgent = "onet2r (https://github.com/farach/onet2r)",
    timeout = max(300, getOption("timeout"))
  )
  on.exit(options(old_options), add = TRUE)
  tryCatch(
    readLines(url, warn = FALSE),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to read the O*NET releases page.",
          "i" = "URL: {.url {url}}"
        ),
        parent = cnd
      )
    }
  )
}

onet_extract_links <- function(html) {
  links <- regmatches(html, gregexpr("href=\"[^\"]+\"", html))[[1]]
  links <- sub("^href=\"", "", links)
  links <- sub("\"$", "", links)
  gsub("&amp;", "&", links, fixed = TRUE)
}

onet_absolute_url <- function(path) {
  if (grepl("^https?://", path)) {
    return(path)
  }
  paste0(onet_resource_url, path)
}

onet_release_headings <- function(html) {
  html <- gsub("\\s+", " ", html)
  span_pattern <- paste0(
    "O\\*NET\\s+([0-9]+(?:\\.[0-9]+)?)\\s*",
    "<span[^>]*>\\s*([A-Za-z]+)\\s+([0-9]{4})\\s*</span>"
  )
  table_pattern <- paste0(
    "O\\*NET\\s+([0-9]+(?:\\.[0-9]+)?)\\s+Database\\s*</td>\\s*",
    "<td[^>]*>\\s*([A-Za-z]+)\\s+([0-9]{4})\\s*</td>"
  )
  span_matches <- regmatches(html, gregexpr(span_pattern, html, perl = TRUE))[[1]]
  table_matches <- regmatches(html, gregexpr(table_pattern, html, perl = TRUE))[[1]]

  span_rows <- release_match_table(span_matches, span_pattern)
  table_rows <- release_match_table(table_matches, table_pattern)
  dplyr::bind_rows(span_rows, table_rows) |>
    dplyr::distinct(.data$version, .keep_all = TRUE) |>
    dplyr::mutate(
      release_date = onet_release_date(.data$month, .data$year)
    )
}

release_match_table <- function(matches, pattern) {
  if (length(matches) == 0) {
    return(tibble::tibble(
      version = character(),
      month = character(),
      year = character()
    ))
  }
  tibble::tibble(
    version = sub(pattern, "\\1", matches, perl = TRUE),
    month = sub(pattern, "\\2", matches, perl = TRUE),
    year = sub(pattern, "\\3", matches, perl = TRUE)
  )
}

onet_release_date <- function(month, year) {
  month_num <- match(month, month.name)
  as.Date(sprintf("%s-%02d-01", year, month_num))
}

onet_dictionary_url <- function(version, links) {
  needle <- paste0("/dictionary/", version, "/excel/")
  link <- links[links == needle]
  if (length(link) == 0) {
    return(NA_character_)
  }
  onet_absolute_url(link[[1]])
}

onet_archive_member <- function(archive, table) {
  files <- if (dir.exists(archive)) {
    list.files(archive, recursive = TRUE, full.names = TRUE)
  } else {
    utils::unzip(archive, list = TRUE)$Name
  }
  target <- archive_table_keys(table)
  candidates <- purrr::map(
    tools::file_path_sans_ext(basename(files)),
    archive_table_keys
  )
  match <- files[purrr::map_lgl(candidates, \(x) any(x %in% target))]
  if (length(match) == 0) {
    cli::cli_abort("Archive table {.val {table}} was not found.")
  }
  if (length(match) > 1) {
    cli::cli_abort(
      c(
        "Archive table {.val {table}} matched multiple files.",
        "i" = "Candidates: {.path {match}}"
      )
    )
  }
  match[[1]]
}

validate_archive_zip <- function(path) {
  tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(cnd) {
      unlink(path, force = TRUE)
      cli::cli_abort("Downloaded archive is not a readable ZIP file.", parent = cnd)
    }
  )
  invisible(path)
}

resolve_archive_release_date <- function(version, path, release_date) {
  if (!is.null(release_date)) {
    release_date <- as.Date(release_date)
    if (length(release_date) != 1 || is.na(release_date)) {
      cli::cli_abort("{.arg release_date} must be a single valid date.")
    }
    return(release_date)
  }
  if (!is.null(path)) {
    return(as.Date(NA))
  }

  release <- onet_releases()
  release <- release[release$version == version, , drop = FALSE]
  if (nrow(release) > 0) release$release_date[[1]] else as.Date(NA)
}

archive_value_for_version <- function(values, versions, version, arg) {
  if (is.null(values)) {
    return(NULL)
  }
  if (!is.atomic(values) || length(values) == 0 || anyNA(values)) {
    cli::cli_abort("{.arg {arg}} must be a non-empty vector without missing values.")
  }

  value_names <- names(values)
  if (!is.null(value_names) && any(nzchar(value_names))) {
    match_index <- match(version, value_names)
    if (is.na(match_index)) {
      cli::cli_abort("{.arg {arg}} must include a value named {.val {version}}.")
    }
    return(values[[match_index]])
  }

  if (length(values) != length(versions)) {
    cli::cli_abort(
      "{.arg {arg}} must be named by version or have the same length as {.arg versions}."
    )
  }

  values[[match(version, versions)]]
}

normalize_archive_table_name <- function(x) {
  to_snake_case(gsub("[^A-Za-z0-9]+", "_", x))
}

archive_table_keys <- function(x) {
  relaxed <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  relaxed <- gsub("^_+|_+$", "", relaxed)
  unique(c(normalize_archive_table_name(x), relaxed))
}

onet_standardize_archive_table <- function(data, version, table, release_date) {
  n_rows <- nrow(data)
  onet_soc_code <- as.character(col_or_na(data, "O*NET-SOC Code", n_rows))
  if (n_rows > 0 && all(is.na(onet_soc_code))) {
    cli::cli_abort(c(
      "Archive table {.val {table}} (version {.val {version}}) has no {.val O*NET-SOC Code} column.",
      "i" = "Columns found: {.val {names(data)}}.",
      "i" = "This release's file layout is not supported for panel assembly."
    ))
  }
  task_id <- as.character(col_or_na(data, "Task ID", n_rows))
  task <- as.character(col_or_na(data, "Task", n_rows))
  dwa_element_id <- as.character(col_or_na(
    data,
    "DWA Element ID",
    n_rows,
    col_or_na(data, "DWA ID", n_rows)
  ))
  dwa_element_name <- as.character(col_or_na(
    data,
    "DWA Element Name",
    n_rows,
    col_or_na(data, "DWA Title", n_rows)
  ))
  iwa_element_id <- as.character(col_or_na(
    data,
    "IWA Element ID",
    n_rows,
    col_or_na(data, "IWA ID", n_rows)
  ))
  iwa_element_name <- as.character(col_or_na(
    data,
    "IWA Element Name",
    n_rows,
    col_or_na(data, "IWA Title", n_rows)
  ))
  gwa_element_id <- as.character(col_or_na(data, "GWA Element ID", n_rows))
  gwa_element_name <- as.character(col_or_na(data, "GWA Element Name", n_rows))
  tibble::tibble(
    release_version = as.character(version),
    release_date = as.Date(rep(release_date, n_rows), origin = "1970-01-01"),
    soc_vintage = factor(onet_soc_vintage(version), levels = onet_vintage_levels),
    domain = table,
    onet_soc_code = onet_soc_code,
    soc_code = standardize_soc_code(onet_soc_code),
    title = as.character(col_or_na(data, "Title", n_rows)),
    task_id = task_id,
    task = task,
    task_type = as.character(col_or_na(data, "Task Type", n_rows)),
    incumbents_responding = parse_onet_integer(col_or_na(
      data,
      "Incumbents Responding",
      n_rows,
      NA_integer_
    )),
    dwa_element_id = dwa_element_id,
    dwa_element_name = dwa_element_name,
    iwa_element_id = iwa_element_id,
    iwa_element_name = iwa_element_name,
    gwa_element_id = gwa_element_id,
    gwa_element_name = gwa_element_name,
    element_id = as.character(col_or_na(
      data,
      "Element ID",
      n_rows,
      col_or_na(data, "Task ID", n_rows, dwa_element_id)
    )),
    element_name = as.character(col_or_na(
      data,
      "Element Name",
      n_rows,
      col_or_na(data, "Task", n_rows, dwa_element_name)
    )),
    scale_id = factor(as.character(col_or_na(
      data,
      "Scale ID",
      n_rows,
      col_or_na(data, "Scale", n_rows)
    ))),
    scale_name = as.character(col_or_na(data, "Scale Name", n_rows)),
    category = parse_onet_integer(col_or_na(data, "Category", n_rows, NA_integer_)),
    data_value = parse_onet_number(col_or_na(data, "Data Value", n_rows, NA_real_)),
    n = parse_onet_integer(col_or_na(data, "N", n_rows, NA_integer_)),
    standard_error = parse_onet_number(col_or_na(data, "Standard Error", n_rows, NA_real_)),
    lower_ci_bound = parse_onet_number(col_or_na(data, "Lower CI Bound", n_rows, NA_real_)),
    upper_ci_bound = parse_onet_number(col_or_na(data, "Upper CI Bound", n_rows, NA_real_)),
    recommend_suppress = as.character(col_or_na(data, "Recommend Suppress", n_rows)),
    not_relevant = as.character(col_or_na(data, "Not Relevant", n_rows)),
    source_date = parse_onet_month(col_or_na(data, "Date", n_rows)),
    domain_source = factor(as.character(col_or_na(data, "Domain Source", n_rows)))
  )
}

col_or_na <- function(data, column, n, default = NA_character_) {
  if (column %in% names(data)) {
    return(data[[column]])
  }
  if (length(default) == n) {
    return(default)
  }
  rep(default, n)
}

parse_onet_number <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))
}

parse_onet_integer <- function(x) {
  suppressWarnings(as.integer(parse_onet_number(x)))
}

parse_onet_month <- function(x) {
  x <- as.character(x)
  out <- as.Date(rep(NA_character_, length(x)))
  keep <- !is.na(x) & nzchar(x)
  out[keep] <- as.Date(paste0("01/", x[keep]), format = "%d/%m/%Y")
  out
}

onet_soc_vintage <- function(version) {
  version <- as.character(version)[[1]]
  valid <- !is.na(version) && grepl("^[0-9]+(\\.[0-9]+)?$", version)
  if (!valid) {
    return(NA_character_)
  }
  if (utils::compareVersion(version, "25.1") >= 0) {
    "2019"
  } else if (utils::compareVersion(version, "15.1") >= 0) {
    "2010"
  } else if (utils::compareVersion(version, "14.0") >= 0) {
    "2009"
  } else if (utils::compareVersion(version, "10.0") >= 0) {
    "2006"
  } else {
    "2000"
  }
}

empty_crosswalk_bridge <- function() {
  tibble::tibble(
    from_vintage = factor(character(), levels = onet_vintage_levels),
    to_vintage = factor(character(), levels = onet_vintage_levels),
    from_onet_soc_code = character(),
    to_onet_soc_code = character(),
    from_soc_code = character(),
    to_soc_code = character(),
    from_title = character(),
    to_title = character(),
    step_count = integer(),
    map_type = factor(
      character(),
      levels = c("one_to_one", "split", "merge", "new", "dropped")
    ),
    crosswalk_weight = double()
  )
}

validate_vintage <- function(vintage, arg) {
  if (!vintage %in% onet_vintage_levels) {
    cli::cli_abort(
      "{.arg {arg}} must be one of {.val {onet_vintage_levels}}."
    )
  }
  invisible(vintage)
}

vintage_path <- function(from_vintage, to_vintage) {
  from_index <- match(from_vintage, onet_vintage_levels)
  to_index <- match(to_vintage, onet_vintage_levels)
  if (from_index < to_index) {
    onet_vintage_levels[from_index:to_index]
  } else {
    rev(onet_vintage_levels[to_index:from_index])
  }
}

download_crosswalk_file <- function(url, cache_dir = onet_cache_dir()) {
  crosswalk_dir <- file.path(cache_dir, "crosswalks")
  dir.create(crosswalk_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(crosswalk_dir, sub("\\?.*$", "", basename(url)))
  if (file.exists(dest) && file.info(dest)$size > 0) {
    return(dest)
  }
  tmp <- tempfile("onet-crosswalk-", tmpdir = crosswalk_dir, fileext = ".csv")
  on.exit(unlink(tmp, force = TRUE), add = TRUE)
  old_options <- options(
    HTTPUserAgent = "onet2r (https://github.com/farach/onet2r)",
    timeout = max(300, getOption("timeout"))
  )
  on.exit(options(old_options), add = TRUE)
  status <- tryCatch(
    utils::download.file(url = url, destfile = tmp, mode = "wb", quiet = TRUE),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to download O*NET crosswalk.",
          "i" = "URL: {.url {url}}"
        ),
        parent = cnd
      )
    }
  )
  if (!identical(status, 0L)) {
    cli::cli_abort(
      c(
        "Failed to download O*NET crosswalk.",
        "i" = "URL: {.url {url}}"
      )
    )
  }
  if (file.info(tmp)$size <= 0) {
    cli::cli_abort("Downloaded O*NET crosswalk was empty.")
  }
  if (!file.rename(tmp, dest)) {
    cli::cli_abort("Failed to move downloaded O*NET crosswalk into the cache.")
  }
  dest
}

read_adjacent_crosswalk <- function(from_vintage, to_vintage) {
  reverse <- match(from_vintage, onet_vintage_levels) > match(to_vintage, onet_vintage_levels)
  low <- if (reverse) to_vintage else from_vintage
  high <- if (reverse) from_vintage else to_vintage
  url <- adjacent_crosswalk_url(low, high)
  path <- download_crosswalk_file(url)
  data <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)

  from_col <- grep(paste0("O\\*NET-SOC ", low, " Code"), names(data), value = TRUE)
  to_col <- grep(paste0("O\\*NET-SOC ", high, " Code"), names(data), value = TRUE)
  from_title_col <- grep(paste0("O\\*NET-SOC ", low, " Title"), names(data), value = TRUE)
  to_title_col <- grep(paste0("O\\*NET-SOC ", high, " Title"), names(data), value = TRUE)
  if (length(from_col) != 1 || length(to_col) != 1) {
    cli::cli_abort("Unexpected crosswalk columns from {.url {url}}.")
  }

  out <- tibble::tibble(
    from_onet_soc_code = standardize_onet_soc_code(data[[from_col]]),
    from_soc_code = standardize_soc_code(data[[from_col]]),
    from_vintage = low,
    to_onet_soc_code = standardize_onet_soc_code(data[[to_col]]),
    to_soc_code = standardize_soc_code(data[[to_col]]),
    to_vintage = high,
    from_title = crosswalk_title_column(data, from_title_col),
    to_title = crosswalk_title_column(data, to_title_col)
  )
  if (reverse) {
    out <- tibble::tibble(
      from_onet_soc_code = out$to_onet_soc_code,
      from_soc_code = out$to_soc_code,
      from_vintage = high,
      to_onet_soc_code = out$from_onet_soc_code,
      to_soc_code = out$from_soc_code,
      to_vintage = low,
      from_title = out$to_title,
      to_title = out$from_title
    )
  }

  classify_crosswalk(out)
}

crosswalk_title_column <- function(data, column) {
  if (length(column) == 1) {
    return(as.character(data[[column]]))
  }
  rep(NA_character_, nrow(data))
}

adjacent_crosswalk_url <- function(from_vintage, to_vintage) {
  key <- paste(from_vintage, to_vintage, sep = "_")
  path <- switch(key,
    `2000_2006` = "/taxonomy/2006/walk/2000_to_2006_Crosswalk.csv?fmt=csv",
    `2006_2009` = "/taxonomy/2009/walk/2006_to_2009_Crosswalk.csv?fmt=csv",
    `2009_2010` = "/taxonomy/2010/walk/2009_to_2010_Crosswalk.csv?fmt=csv",
    `2010_2019` = "/taxonomy/2019/walk/2010_to_2019_Crosswalk.csv?fmt=csv",
    cli::cli_abort("No adjacent O*NET crosswalk for {.val {key}}.")
  )
  onet_absolute_url(path)
}

classify_crosswalk <- function(data) {
  data <- normalize_bridge(data)
  from_counts <- counts_by_key(data$from_onet_soc_code)
  to_counts <- counts_by_key(data$to_onet_soc_code)
  data$map_type <- rep("one_to_one", length(from_counts))
  data$map_type[to_counts > 1] <- "merge"
  data$map_type[from_counts > 1] <- "split"
  data$crosswalk_weight <- ifelse(from_counts > 1, 1 / from_counts, 1)
  tibble::as_tibble(data)
}

counts_by_key <- function(x) {
  counts <- table(x)
  as.integer(counts[match(x, names(counts))])
}

chain_crosswalks <- function(left, right) {
  left <- normalize_bridge(left)
  right <- normalize_bridge(right)
  right$mid_onet_soc_code <- right$from_onet_soc_code
  right$mid_soc_code <- right$from_soc_code
  right$final_onet_soc_code <- right$to_onet_soc_code
  right$final_soc_code <- right$to_soc_code
  right$mid_vintage <- right$from_vintage
  right$final_vintage <- right$to_vintage
  right$final_title <- right$to_title
  right$from_onet_soc_code <- NULL
  right$to_onet_soc_code <- NULL
  right$from_soc_code <- NULL
  right$to_soc_code <- NULL
  right$from_vintage <- NULL
  right$to_vintage <- NULL
  right$from_title <- NULL
  right$to_title <- NULL

  joined <- dplyr::inner_join(
    left,
    right,
    by = dplyr::join_by(
      to_onet_soc_code == mid_onet_soc_code,
      to_vintage == mid_vintage
    ),
    relationship = "many-to-many"
  )

  tibble::tibble(
    from_vintage = as.character(joined$from_vintage),
    to_vintage = as.character(joined$final_vintage),
    from_onet_soc_code = joined$from_onet_soc_code,
    to_onet_soc_code = joined$final_onet_soc_code,
    from_soc_code = joined$from_soc_code,
    to_soc_code = joined$final_soc_code,
    from_title = joined$from_title,
    to_title = joined$final_title,
    map_type = dplyr::case_when(
      joined$map_type.x == "split" | joined$map_type.y == "split" ~ "split",
      joined$map_type.x == "merge" | joined$map_type.y == "merge" ~ "merge",
      TRUE ~ "one_to_one"
    ),
    crosswalk_weight = joined$crosswalk_weight.x * joined$crosswalk_weight.y
  )
}

validate_panel <- function(panel) {
  required <- c(
    "onet_soc_code", "soc_code", "soc_vintage", "release_version", "domain",
    "release_date", "element_id", "scale_id", "data_value", "source_date",
    "domain_source"
  )
  validate_columns_present(panel, required, "panel")
}

normalize_reconcile_panel <- function(panel) {
  panel <- tibble::as_tibble(panel)
  if (!"recommend_suppress" %in% names(panel)) {
    panel$recommend_suppress <- NA_character_
  }
  if (!"element_name" %in% names(panel)) {
    panel$element_name <- NA_character_
  }
  panel
}

validate_bridge <- function(bridge) {
  required <- c(
    "from_vintage", "to_vintage", "map_type", "crosswalk_weight"
  )
  validate_columns_present(bridge, required, "bridge")
  has_native <- all(c("from_onet_soc_code", "to_onet_soc_code") %in% names(bridge))
  has_legacy <- all(c("from_soc_code", "to_soc_code") %in% names(bridge))
  if (!has_native && !has_legacy) {
    cli::cli_abort(
      "{.arg bridge} must include native O*NET-SOC code columns or legacy SOC code columns."
    )
  }
}

normalize_bridge <- function(bridge) {
  bridge <- tibble::as_tibble(bridge)
  if (!"from_onet_soc_code" %in% names(bridge)) {
    bridge$from_onet_soc_code <- bridge$from_soc_code
  }
  if (!"to_onet_soc_code" %in% names(bridge)) {
    bridge$to_onet_soc_code <- bridge$to_soc_code
  }
  if (!"from_soc_code" %in% names(bridge)) {
    bridge$from_soc_code <- standardize_soc_code(bridge$from_onet_soc_code)
  }
  if (!"to_soc_code" %in% names(bridge)) {
    bridge$to_soc_code <- standardize_soc_code(bridge$to_onet_soc_code)
  }
  if (!"from_title" %in% names(bridge)) {
    bridge$from_title <- NA_character_
  }
  if (!"to_title" %in% names(bridge)) {
    bridge$to_title <- NA_character_
  }
  if (!"step_count" %in% names(bridge)) {
    bridge$step_count <- NA_integer_
  }
  if (!"map_type" %in% names(bridge)) {
    bridge$map_type <- "one_to_one"
  }
  if (!"crosswalk_weight" %in% names(bridge)) {
    bridge$crosswalk_weight <- 1
  }

  bridge$from_onet_soc_code <- standardize_onet_soc_code(bridge$from_onet_soc_code)
  bridge$to_onet_soc_code <- standardize_onet_soc_code(bridge$to_onet_soc_code)
  bridge$from_soc_code <- standardize_soc_code(bridge$from_onet_soc_code)
  bridge$to_soc_code <- standardize_soc_code(bridge$to_onet_soc_code)
  bridge$from_vintage <- factor(as.character(bridge$from_vintage), levels = onet_vintage_levels)
  bridge$to_vintage <- factor(as.character(bridge$to_vintage), levels = onet_vintage_levels)
  bridge$map_type <- factor(as.character(bridge$map_type), levels = map_type_levels())
  bridge$crosswalk_weight <- parse_onet_number(bridge$crosswalk_weight)

  bridge
}

map_type_levels <- function() {
  c("one_to_one", "split", "merge", "new", "dropped")
}

change_type_levels <- function() {
  c(
    "stale_carryforward",
    "real_update",
    "resampled_stable",
    "recode_or_recalc_flag",
    "source_date_missing",
    "transition_data",
    "suppressed_change",
    "new",
    "dropped",
    "unmapped_source",
    "unmapped_target"
  )
}

coverage_status_levels <- function() {
  c("matched", "new", "dropped", "unmapped_source", "unmapped_target")
}

validate_columns_present <- function(data, columns, arg) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg {arg}} must be a data frame.")
  }
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg {arg}} is missing columns: {.var {missing}}.")
  }
  invisible(data)
}

reconcile_coverage_rows <- function(
    from_panel,
    to_panel,
    pair_bridge,
    from_release,
    to_release) {
  mapped_from <- from_panel |>
    dplyr::inner_join(
      pair_bridge,
      by = dplyr::join_by(
        onet_soc_code == from_onet_soc_code,
        soc_vintage == from_vintage
      ),
      relationship = "many-to-many"
    )

  mapped_targets <- mapped_from |>
    dplyr::distinct(
      .data$to_onet_soc_code,
      .data$to_vintage,
      .data$domain,
      .data$element_id,
      .data$scale_id
    )

  unmapped_source <- from_panel |>
    dplyr::anti_join(
      pair_bridge,
      by = dplyr::join_by(
        onet_soc_code == from_onet_soc_code,
        soc_vintage == from_vintage
      )
    )

  dropped <- mapped_from |>
    dplyr::anti_join(
      to_panel,
      by = dplyr::join_by(
        to_onet_soc_code == onet_soc_code,
        to_vintage == soc_vintage,
        domain == domain,
        element_id == element_id,
        scale_id == scale_id
      )
    )

  new <- to_panel |>
    dplyr::anti_join(
      mapped_targets,
      by = dplyr::join_by(
        onet_soc_code == to_onet_soc_code,
        soc_vintage == to_vintage,
        domain == domain,
        element_id == element_id,
        scale_id == scale_id
      )
    )

  dplyr::bind_rows(
    dropped_coverage_rows(dropped, from_release, to_release, to_panel),
    new_coverage_rows(new, from_release, to_release, from_panel, pair_bridge),
    unmapped_source_rows(unmapped_source, from_release, to_release, to_panel)
  )
}

dropped_coverage_rows <- function(dropped, from_release, to_release, to_panel) {
  if (nrow(dropped) == 0) {
    return(empty_reconciled_panel())
  }

  original_map_type <- as.character(dropped$map_type)
  tibble::tibble(
    from_release = from_release,
    to_release = to_release,
    from_release_date = dropped$release_date,
    to_release_date = single_date(to_panel$release_date),
    from_onet_soc_code = dropped$onet_soc_code,
    to_onet_soc_code = dropped$to_onet_soc_code,
    from_soc_code = standardize_soc_code(dropped$onet_soc_code),
    to_soc_code = standardize_soc_code(dropped$to_onet_soc_code),
    soc_vintage_from = dropped$soc_vintage,
    soc_vintage_to = dropped$to_vintage,
    domain = dropped$domain,
    element_id = dropped$element_id,
    element_name = dropped$element_name,
    scale_id = dropped$scale_id,
    from_value = dropped$data_value,
    to_value = NA_real_,
    value_change = NA_real_,
    value_percent_change = NA_real_,
    from_source_date = dropped$source_date,
    to_source_date = as.Date(NA),
    from_domain_source = as.character(dropped$domain_source),
    to_domain_source = NA_character_,
    from_recommend_suppress = as.character(dropped$recommend_suppress),
    to_recommend_suppress = NA_character_,
    from_not_relevant = as.character(col_or_na(dropped, "not_relevant", nrow(dropped))),
    to_not_relevant = NA_character_,
    date_changed = NA,
    value_changed = NA,
    transition_data = is_transition_source(dropped$domain_source),
    suppressed_change = is_suppressed_estimate(dropped$recommend_suppress),
    change_type = factor("dropped", levels = change_type_levels()),
    coverage_status = factor("dropped", levels = coverage_status_levels()),
    method_break = FALSE,
    crosswalk_uncertain = original_map_type %in% c("split", "merge"),
    safely_comparable = FALSE,
    map_type = factor("dropped", levels = map_type_levels()),
    crosswalk_weight = dropped$crosswalk_weight
  )
}

unmapped_source_rows <- function(unmapped, from_release, to_release, to_panel) {
  if (nrow(unmapped) == 0) {
    return(empty_reconciled_panel())
  }

  tibble::tibble(
    from_release = from_release,
    to_release = to_release,
    from_release_date = unmapped$release_date,
    to_release_date = single_date(to_panel$release_date),
    from_onet_soc_code = unmapped$onet_soc_code,
    to_onet_soc_code = NA_character_,
    from_soc_code = standardize_soc_code(unmapped$onet_soc_code),
    to_soc_code = NA_character_,
    soc_vintage_from = unmapped$soc_vintage,
    soc_vintage_to = single_vintage(to_panel$soc_vintage),
    domain = unmapped$domain,
    element_id = unmapped$element_id,
    element_name = unmapped$element_name,
    scale_id = unmapped$scale_id,
    from_value = unmapped$data_value,
    to_value = NA_real_,
    value_change = NA_real_,
    value_percent_change = NA_real_,
    from_source_date = unmapped$source_date,
    to_source_date = as.Date(NA),
    from_domain_source = as.character(unmapped$domain_source),
    to_domain_source = NA_character_,
    from_recommend_suppress = as.character(unmapped$recommend_suppress),
    to_recommend_suppress = NA_character_,
    from_not_relevant = as.character(col_or_na(unmapped, "not_relevant", nrow(unmapped))),
    to_not_relevant = NA_character_,
    date_changed = NA,
    value_changed = NA,
    transition_data = is_transition_source(unmapped$domain_source),
    suppressed_change = is_suppressed_estimate(unmapped$recommend_suppress),
    change_type = factor("unmapped_source", levels = change_type_levels()),
    coverage_status = factor("unmapped_source", levels = coverage_status_levels()),
    method_break = FALSE,
    crosswalk_uncertain = TRUE,
    safely_comparable = FALSE,
    map_type = factor(NA_character_, levels = map_type_levels()),
    crosswalk_weight = NA_real_
  )
}

new_coverage_rows <- function(new, from_release, to_release, from_panel, pair_bridge) {
  if (nrow(new) == 0) {
    return(empty_reconciled_panel())
  }

  status <- dplyr::if_else(
    new$onet_soc_code %in% pair_bridge$to_onet_soc_code,
    "new",
    "unmapped_target"
  )

  tibble::tibble(
    from_release = from_release,
    to_release = to_release,
    from_release_date = single_date(from_panel$release_date),
    to_release_date = new$release_date,
    from_onet_soc_code = NA_character_,
    to_onet_soc_code = new$onet_soc_code,
    from_soc_code = NA_character_,
    to_soc_code = standardize_soc_code(new$onet_soc_code),
    soc_vintage_from = single_vintage(from_panel$soc_vintage),
    soc_vintage_to = new$soc_vintage,
    domain = new$domain,
    element_id = new$element_id,
    element_name = new$element_name,
    scale_id = new$scale_id,
    from_value = NA_real_,
    to_value = new$data_value,
    value_change = NA_real_,
    value_percent_change = NA_real_,
    from_source_date = as.Date(NA),
    to_source_date = new$source_date,
    from_domain_source = NA_character_,
    to_domain_source = as.character(new$domain_source),
    from_recommend_suppress = NA_character_,
    to_recommend_suppress = as.character(new$recommend_suppress),
    from_not_relevant = NA_character_,
    to_not_relevant = as.character(col_or_na(new, "not_relevant", nrow(new))),
    date_changed = NA,
    value_changed = NA,
    transition_data = is_transition_source(new$domain_source),
    suppressed_change = is_suppressed_estimate(new$recommend_suppress),
    change_type = factor(status, levels = change_type_levels()),
    coverage_status = factor(status, levels = coverage_status_levels()),
    method_break = FALSE,
    crosswalk_uncertain = FALSE,
    safely_comparable = FALSE,
    map_type = factor(
      dplyr::if_else(status == "new", "new", NA_character_),
      levels = map_type_levels()
    ),
    crosswalk_weight = 1
  )
}

is_transition_source <- function(x) {
  out <- grepl("transition", as.character(x), ignore.case = TRUE)
  out[is.na(out)] <- FALSE
  out
}

is_suppressed_estimate <- function(x) {
  out <- toupper(trimws(as.character(x))) == "Y"
  out[is.na(out)] <- FALSE
  out
}

single_date <- function(x) {
  out <- unique(as.Date(x))
  out <- out[!is.na(out)]
  if (length(out) == 0) {
    return(as.Date(NA))
  }
  out[[1]]
}

single_vintage <- function(x) {
  out <- unique(as.character(x))
  out <- out[!is.na(out)]
  if (length(out) == 0) {
    return(factor(NA_character_, levels = onet_vintage_levels))
  }
  factor(out[[1]], levels = onet_vintage_levels)
}

empty_reconciled_panel <- function() {
  tibble::tibble(
    from_release = character(),
    to_release = character(),
    from_release_date = as.Date(character()),
    to_release_date = as.Date(character()),
    from_onet_soc_code = character(),
    to_onet_soc_code = character(),
    from_soc_code = character(),
    to_soc_code = character(),
    soc_vintage_from = factor(character(), levels = onet_vintage_levels),
    soc_vintage_to = factor(character(), levels = onet_vintage_levels),
    domain = character(),
    element_id = character(),
    element_name = character(),
    scale_id = factor(character()),
    from_value = double(),
    to_value = double(),
    value_change = double(),
    value_percent_change = double(),
    from_source_date = as.Date(character()),
    to_source_date = as.Date(character()),
    from_domain_source = character(),
    to_domain_source = character(),
    from_recommend_suppress = character(),
    to_recommend_suppress = character(),
    from_not_relevant = character(),
    to_not_relevant = character(),
    date_changed = logical(),
    value_changed = logical(),
    transition_data = logical(),
    suppressed_change = logical(),
    change_type = factor(
      character(),
      levels = change_type_levels()
    ),
    coverage_status = factor(character(), levels = coverage_status_levels()),
    method_break = logical(),
    crosswalk_uncertain = logical(),
    safely_comparable = logical(),
    map_type = factor(
      character(),
      levels = c("one_to_one", "split", "merge", "new", "dropped")
    ),
    crosswalk_weight = double()
  )
}

reconcile_release_pair <- function(panel, bridge, from_release, to_release) {
  from_panel <- panel |>
    dplyr::filter(.data$release_version == from_release)
  to_panel <- panel |>
    dplyr::filter(.data$release_version == to_release)

  pair_bridge <- bridge |>
    dplyr::filter(
      .data$from_vintage %in% unique(from_panel$soc_vintage),
      .data$to_vintage %in% unique(to_panel$soc_vintage)
    )

  if (nrow(pair_bridge) == 0 && identical(unique(from_panel$soc_vintage), unique(to_panel$soc_vintage))) {
    codes <- intersect(from_panel$onet_soc_code, to_panel$onet_soc_code)
    pair_bridge <- tibble::tibble(
      from_onet_soc_code = codes,
      to_onet_soc_code = codes,
      from_soc_code = standardize_soc_code(codes),
      to_soc_code = standardize_soc_code(codes),
      from_vintage = unique(from_panel$soc_vintage)[[1]],
      to_vintage = unique(to_panel$soc_vintage)[[1]],
      from_title = NA_character_,
      to_title = NA_character_,
      step_count = 0L,
      map_type = "one_to_one",
      crosswalk_weight = 1
    )
  }

  if (nrow(pair_bridge) == 0) {
    return(empty_reconciled_panel())
  }
  pair_bridge <- normalize_bridge(pair_bridge)

  joined <- from_panel |>
    dplyr::inner_join(
      pair_bridge,
      by = dplyr::join_by(
        onet_soc_code == from_onet_soc_code,
        soc_vintage == from_vintage
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::inner_join(
      to_panel,
      by = dplyr::join_by(
        to_onet_soc_code == onet_soc_code,
        to_vintage == soc_vintage,
        domain == domain,
        element_id == element_id,
        scale_id == scale_id
      ),
      suffix = c("_from", "_to"),
      relationship = "many-to-many"
    )

  coverage_rows <- reconcile_coverage_rows(
    from_panel = from_panel,
    to_panel = to_panel,
    pair_bridge = pair_bridge,
    from_release = from_release,
    to_release = to_release
  )

  if (nrow(joined) == 0) {
    return(dplyr::bind_rows(empty_reconciled_panel(), coverage_rows))
  }

  value_changed <- !same_number(joined$data_value_from, joined$data_value_to)
  date_missing <- is.na(joined$source_date_from) | is.na(joined$source_date_to)
  date_changed <- joined$source_date_from != joined$source_date_to
  date_changed[date_missing] <- NA
  transition_data <- is_transition_source(joined$domain_source_from) |
    is_transition_source(joined$domain_source_to)
  suppressed_change <- is_suppressed_estimate(joined$recommend_suppress_from) |
    is_suppressed_estimate(joined$recommend_suppress_to)

  change_type <- dplyr::case_when(
    transition_data ~ "transition_data",
    suppressed_change ~ "suppressed_change",
    date_missing ~ "source_date_missing",
    !value_changed & !date_changed ~ "stale_carryforward",
    value_changed & date_changed ~ "real_update",
    !value_changed & date_changed ~ "resampled_stable",
    TRUE ~ "recode_or_recalc_flag"
  )
  method_break <- as.character(joined$domain_source_from) != as.character(joined$domain_source_to)
  method_break[is.na(method_break)] <- FALSE
  crosswalk_uncertain <- as.character(joined$map_type) %in% c("split", "merge")
  comparable <- change_type %in% c(
    "stale_carryforward", "real_update", "resampled_stable"
  )

  tibble::tibble(
    from_release = from_release,
    to_release = to_release,
    from_release_date = joined$release_date_from,
    to_release_date = joined$release_date_to,
    from_onet_soc_code = joined$onet_soc_code,
    to_onet_soc_code = joined$to_onet_soc_code,
    from_soc_code = standardize_soc_code(joined$onet_soc_code),
    to_soc_code = standardize_soc_code(joined$to_onet_soc_code),
    soc_vintage_from = joined$soc_vintage,
    soc_vintage_to = joined$to_vintage,
    domain = joined[["domain"]] %||% NA_character_,
    element_id = joined$element_id,
    element_name = joined[["element_name_from"]] %||% NA_character_,
    scale_id = joined$scale_id,
    from_value = joined$data_value_from,
    to_value = joined$data_value_to,
    value_change = joined$data_value_to - joined$data_value_from,
    value_percent_change = dplyr::if_else(
      joined$data_value_from == 0 | is.na(joined$data_value_from),
      NA_real_,
      (joined$data_value_to - joined$data_value_from) / joined$data_value_from
    ),
    from_source_date = joined$source_date_from,
    to_source_date = joined$source_date_to,
    from_domain_source = as.character(joined$domain_source_from),
    to_domain_source = as.character(joined$domain_source_to),
    from_recommend_suppress = as.character(joined$recommend_suppress_from),
    to_recommend_suppress = as.character(joined$recommend_suppress_to),
    from_not_relevant = as.character(col_or_na(joined, "not_relevant_from", nrow(joined))),
    to_not_relevant = as.character(col_or_na(joined, "not_relevant_to", nrow(joined))),
    date_changed = date_changed,
    value_changed = value_changed,
    transition_data = transition_data,
    suppressed_change = suppressed_change,
    change_type = factor(
      change_type,
      levels = change_type_levels()
    ),
    coverage_status = factor("matched", levels = coverage_status_levels()),
    method_break = method_break,
    crosswalk_uncertain = crosswalk_uncertain,
    safely_comparable = comparable & !method_break & !crosswalk_uncertain &
      !transition_data & !suppressed_change,
    map_type = joined$map_type,
    crosswalk_weight = joined$crosswalk_weight
  ) |>
    dplyr::bind_rows(coverage_rows)
}

same_number <- function(x, y, tolerance = sqrt(.Machine$double.eps)) {
  same <- abs(x - y) <= tolerance
  same[is.na(same)] <- is.na(x[is.na(same)]) & is.na(y[is.na(same)])
  same
}
