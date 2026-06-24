# =============================================================================
# O*NET longitudinal archive panel
# =============================================================================

onet_release_archive_url <- "https://www.onetcenter.org/db_releases.html"
onet_resource_url <- "https://www.onetcenter.org"
onet_vintage_levels <- c("2000", "2006", "2009", "2010", "2019")

#' List O\*NET Archive Releases
#'
#' Parses the O\*NET database releases archive and returns downloadable text
#' archives. The release archive states release months, not days, so
#' `release_date` uses the first day of each stated month.
#'
#' @return A tibble with release metadata and text archive URLs.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   onet_releases()
#' }
onet_releases <- function() {
  html <- paste(onet_read_lines(onet_release_archive_url), collapse = "\n")
  links <- onet_extract_links(html)

  text_links <- links[grepl("/dl_files/database/db_[0-9_]+_text\\.zip$", links)]
  versions <- sub(
    ".*/db_([0-9_]+)_text\\.zip$",
    "\\1",
    text_links
  )
  versions <- gsub("_", ".", versions, fixed = TRUE)

  headings <- onet_release_headings(html)

  purrr::map(seq_along(versions), \(i) {
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
      text_url = onet_absolute_url(text_links[[i]]),
      dictionary_url = onet_dictionary_url(version, links)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::distinct(.data$version, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(.data$release_date), dplyr::desc(.data$version))
}

#' Download an O\*NET Archive
#'
#' Downloads and caches one O\*NET text archive. Existing non-empty cached files
#' are reused.
#'
#' @param version O\*NET database version, for example `"30.3"`.
#' @param dir Cache directory.
#'
#' @return The path to the cached ZIP file.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   path <- onet_archive_download("30.3")
#'   basename(path)
#' }
onet_archive_download <- function(version, dir = onet_cache_dir()) {
  validate_single_string(version, "version")
  validate_single_string(dir, "dir")

  releases <- onet_releases()
  release <- releases[releases$version == version, , drop = FALSE]
  if (nrow(release) == 0) {
    cli::cli_abort("No downloadable text archive found for version {.val {version}}.")
  }

  archive_dir <- file.path(dir, "archives")
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(archive_dir, basename(release$text_url[[1]]))

  if (file.exists(dest) && file.info(dest)$size > 0) {
    return(dest)
  }

  tryCatch(
    utils::download.file(
      url = release$text_url[[1]],
      destfile = dest,
      mode = "wb",
      quiet = TRUE
    ),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to download O*NET archive.",
          "i" = "URL: {.url {release$text_url[[1]]}}"
        ),
        parent = cnd
      )
    }
  )

  tryCatch(
    utils::unzip(dest, list = TRUE),
    error = function(cnd) {
      unlink(dest, force = TRUE)
      cli::cli_abort("Downloaded archive is not a readable ZIP file.", parent = cnd)
    }
  )

  dest
}

#' Read an O\*NET Archive Table
#'
#' Reads a descriptor-like table from a cached O\*NET text archive and normalizes
#' it to the longitudinal panel schema.
#'
#' @param version O\*NET database version, for example `"30.3"`.
#' @param table Archive table name, for example `"Abilities"` or
#'   `"Work Activities"`.
#'
#' @return A tibble in the longitudinal panel schema.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   abilities <- onet_archive_read("30.3", "Abilities")
#'   head(abilities)
#' }
onet_archive_read <- function(version, table) {
  validate_single_string(version, "version")
  validate_single_string(table, "table")

  archive <- onet_archive_download(version)
  member <- onet_archive_member(archive, table)
  tmpdir <- tempfile("onet-archive-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  utils::unzip(archive, files = member, exdir = tmpdir)
  path <- file.path(tmpdir, member)
  data <- utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)

  release <- onet_releases()
  release <- release[release$version == version, , drop = FALSE]
  release_date <- if (nrow(release) > 0) release$release_date[[1]] else NA

  onet_standardize_archive_table(data, version, table, release_date)
}

#' Assemble an O\*NET Longitudinal Panel
#'
#' Reads the same archive table across releases and row-binds the normalized
#' descriptor rows.
#'
#' @param table_or_element Archive table name in the first implementation.
#' @param versions Character vector of O\*NET database versions.
#' @param scale Optional scale id filter.
#'
#' @return A tibble in the longitudinal panel schema.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   panel <- onet_panel("Abilities", versions = c("30.2", "30.3"), scale = "IM")
#'   head(panel)
#' }
onet_panel <- function(table_or_element, versions, scale = NULL) {
  validate_single_string(table_or_element, "table_or_element")
  if (!is.character(versions) || length(versions) == 0 || anyNA(versions)) {
    cli::cli_abort("{.arg versions} must be a non-empty character vector.")
  }

  panel <- purrr::map(
    versions,
    \(version) onet_archive_read(version, table_or_element)
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

#' Build an O\*NET-SOC Crosswalk Bridge
#'
#' Builds an adjacent or chained O\*NET-SOC bridge between taxonomy vintages.
#' Equal weights are used for transparent split apportionment.
#'
#' @param from_vintage Source taxonomy vintage.
#' @param to_vintage Target taxonomy vintage.
#' @param weight Weighting method. Only `"equal"` is implemented.
#'
#' @return A tibble with source and target codes, vintages, map type, and
#'   equal split weights.
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
      "from_soc_code",
      "to_soc_code",
      "from_title",
      "to_title",
      "step_count",
      "map_type",
      "crosswalk_weight"
    )
}

#' Reconcile O\*NET Panel Changes
#'
#' Compares adjacent releases in a panel and classifies changes using the value
#' change by collection-date change truth table.
#'
#' @param panel A tibble from [onet_panel()] or the same schema.
#' @param bridge A bridge from [onet_crosswalk_bridge()].
#' @param weight Weighting method. Only `"equal"` is implemented.
#'
#' @return A tibble of adjacent-release comparisons with change flags.
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
  validate_bridge(bridge)

  releases <- panel |>
    dplyr::distinct(.data$release_version, .data$release_date) |>
    dplyr::arrange(.data$release_date, .data$release_version)

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

#' Summarise O\*NET Panel Change Types
#'
#' Counts reconciliation change types overall and by two-digit job family.
#'
#' @param reconciled A tibble from [onet_panel_reconcile()].
#' @param by Summary level to return. `"overall"` returns one package-level row;
#'   `"job_family"` returns the overall row plus one row per two-digit SOC
#'   family.
#'
#' @return A tibble with `job_family`, `change_type`, `safely_comparable`,
#'   `n`, and `share`.
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
  change_type <- if (n_pairs == 0) {
    NA_character_
  } else {
    data |>
      dplyr::summarise(n = dplyr::n(), .by = "change_type") |>
      dplyr::arrange(dplyr::desc(.data$n), .data$change_type) |>
      dplyr::pull(.data$change_type) |>
      as.character() |>
      utils::head(1)
  }

  tibble::tibble(
    summary_level = summary_level,
    job_family = job_family,
    change_type = change_type,
    n_pairs = n_pairs,
    share_pairs = n_pairs / total_pairs,
    mean_value_change = safe_mean(data$value_change),
    median_abs_value_change = safe_median(abs(data$value_change)),
    share_safely_comparable = safe_mean(data$safely_comparable),
    share_method_break = safe_mean(data$method_break),
    share_crosswalk_uncertain = safe_mean(data$crosswalk_uncertain)
  )
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
  readLines(url, warn = FALSE)
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
  files <- utils::unzip(archive, list = TRUE)$Name
  target <- normalize_archive_table_name(table)
  candidates <- normalize_archive_table_name(tools::file_path_sans_ext(basename(files)))
  match <- files[candidates == target]
  if (length(match) == 0) {
    cli::cli_abort("Archive table {.val {table}} was not found.")
  }
  match[[1]]
}

normalize_archive_table_name <- function(x) {
  to_snake_case(gsub("[^A-Za-z0-9]+", "_", x))
}

onet_standardize_archive_table <- function(data, version, table, release_date) {
  n_rows <- nrow(data)
  onet_soc_code <- as.character(col_or_na(data, "O*NET-SOC Code", n_rows))
  tibble::tibble(
    release_version = as.character(version),
    release_date = as.Date(rep(release_date, n_rows), origin = "1970-01-01"),
    soc_vintage = factor(onet_soc_vintage(version), levels = onet_vintage_levels),
    domain = table,
    onet_soc_code = onet_soc_code,
    soc_code = standardize_soc_code(onet_soc_code),
    title = as.character(col_or_na(data, "Title", n_rows)),
    element_id = as.character(col_or_na(data, "Element ID", n_rows, col_or_na(data, "Task ID", n_rows))),
    element_name = as.character(col_or_na(data, "Element Name", n_rows)),
    scale_id = factor(as.character(col_or_na(
      data,
      "Scale ID",
      n_rows,
      col_or_na(data, "Scale", n_rows)
    ))),
    data_value = parse_onet_number(col_or_na(data, "Data Value", n_rows, NA_real_)),
    n = parse_onet_integer(col_or_na(data, "N", n_rows, NA_integer_)),
    standard_error = parse_onet_number(col_or_na(data, "Standard Error", n_rows, NA_real_)),
    lower_ci_bound = parse_onet_number(col_or_na(data, "Lower CI Bound", n_rows, NA_real_)),
    upper_ci_bound = parse_onet_number(col_or_na(data, "Upper CI Bound", n_rows, NA_real_)),
    recommend_suppress = as.character(col_or_na(data, "Recommend Suppress", n_rows)),
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
  version_num <- suppressWarnings(as.numeric(version))
  if (is.na(version_num)) {
    return(NA_character_)
  }
  if (version_num >= 25.1) {
    "2019"
  } else if (version_num >= 15.1) {
    "2010"
  } else if (version_num >= 14.0) {
    "2009"
  } else if (version_num >= 10.0) {
    "2006"
  } else {
    "2000"
  }
}

empty_crosswalk_bridge <- function() {
  tibble::tibble(
    from_vintage = factor(character(), levels = onet_vintage_levels),
    to_vintage = factor(character(), levels = onet_vintage_levels),
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

read_adjacent_crosswalk <- function(from_vintage, to_vintage) {
  reverse <- match(from_vintage, onet_vintage_levels) > match(to_vintage, onet_vintage_levels)
  low <- if (reverse) to_vintage else from_vintage
  high <- if (reverse) from_vintage else to_vintage
  url <- adjacent_crosswalk_url(low, high)
  data <- utils::read.csv(url, check.names = FALSE, stringsAsFactors = FALSE)

  from_col <- grep(paste0("O\\*NET-SOC ", low, " Code"), names(data), value = TRUE)
  to_col <- grep(paste0("O\\*NET-SOC ", high, " Code"), names(data), value = TRUE)
  from_title_col <- grep(paste0("O\\*NET-SOC ", low, " Title"), names(data), value = TRUE)
  to_title_col <- grep(paste0("O\\*NET-SOC ", high, " Title"), names(data), value = TRUE)
  if (length(from_col) != 1 || length(to_col) != 1) {
    cli::cli_abort("Unexpected crosswalk columns from {.url {url}}.")
  }

  out <- tibble::tibble(
    from_soc_code = standardize_soc_code(data[[from_col]]),
    from_vintage = low,
    to_soc_code = standardize_soc_code(data[[to_col]]),
    to_vintage = high,
    from_title = crosswalk_title_column(data, from_title_col),
    to_title = crosswalk_title_column(data, to_title_col)
  )
  if (reverse) {
    out <- tibble::tibble(
      from_soc_code = out$to_soc_code,
      from_vintage = high,
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
  from_counts <- counts_by_key(data$from_soc_code)
  to_counts <- counts_by_key(data$to_soc_code)
  data$map_type <- dplyr::case_when(
    from_counts > 1 ~ "split",
    to_counts > 1 ~ "merge",
    TRUE ~ "one_to_one"
  )
  data$crosswalk_weight <- ifelse(from_counts > 1, 1 / from_counts, 1)
  tibble::as_tibble(data)
}

counts_by_key <- function(x) {
  counts <- table(x)
  as.integer(counts[match(x, names(counts))])
}

chain_crosswalks <- function(left, right) {
  right <- tibble::as_tibble(right)
  right$mid_soc_code <- right$from_soc_code
  right$final_soc_code <- right$to_soc_code
  right$mid_vintage <- right$from_vintage
  right$final_vintage <- right$to_vintage
  right$final_title <- right$to_title
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
      to_soc_code == mid_soc_code,
      to_vintage == mid_vintage
    ),
    relationship = "many-to-many"
  )

  tibble::tibble(
    from_vintage = as.character(joined$from_vintage),
    to_vintage = as.character(joined$final_vintage),
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
    "onet_soc_code", "soc_code", "soc_vintage", "release_version",
    "release_date", "element_id", "scale_id", "data_value", "source_date",
    "domain_source"
  )
  validate_columns_present(panel, required, "panel")
}

validate_bridge <- function(bridge) {
  required <- c(
    "from_soc_code", "from_vintage", "to_soc_code", "to_vintage",
    "map_type", "crosswalk_weight"
  )
  validate_columns_present(bridge, required, "bridge")
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

empty_reconciled_panel <- function() {
  tibble::tibble(
    from_release = character(),
    to_release = character(),
    from_release_date = as.Date(character()),
    to_release_date = as.Date(character()),
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
    date_changed = logical(),
    value_changed = logical(),
    change_type = factor(
      character(),
      levels = c(
        "stale_carryforward", "real_update", "resampled_stable",
        "recode_or_recalc_flag"
      )
    ),
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
    codes <- intersect(from_panel$soc_code, to_panel$soc_code)
    pair_bridge <- tibble::tibble(
      from_soc_code = codes,
      from_vintage = unique(from_panel$soc_vintage)[[1]],
      to_soc_code = codes,
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

  joined <- from_panel |>
    dplyr::inner_join(
      pair_bridge,
      by = dplyr::join_by(soc_code == from_soc_code, soc_vintage == from_vintage),
      relationship = "many-to-many"
    ) |>
    dplyr::inner_join(
      to_panel,
      by = dplyr::join_by(
        to_soc_code == soc_code,
        to_vintage == soc_vintage,
        element_id == element_id,
        scale_id == scale_id
      ),
      suffix = c("_from", "_to"),
      relationship = "many-to-many"
    )

  if (nrow(joined) == 0) {
    return(empty_reconciled_panel())
  }

  value_changed <- !same_number(joined$data_value_from, joined$data_value_to)
  date_changed <- joined$source_date_from != joined$source_date_to
  date_changed[is.na(date_changed)] <- FALSE

  change_type <- dplyr::case_when(
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
    from_soc_code = joined$soc_code,
    to_soc_code = joined$to_soc_code,
    soc_vintage_from = joined$soc_vintage,
    soc_vintage_to = joined$to_vintage,
    domain = joined[["domain_from"]] %||% NA_character_,
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
    date_changed = date_changed,
    value_changed = value_changed,
    change_type = factor(
      change_type,
      levels = c(
        "stale_carryforward", "real_update", "resampled_stable",
        "recode_or_recalc_flag"
      )
    ),
    method_break = method_break,
    crosswalk_uncertain = crosswalk_uncertain,
    safely_comparable = comparable & !method_break & !crosswalk_uncertain,
    map_type = joined$map_type,
    crosswalk_weight = joined$crosswalk_weight
  )
}

same_number <- function(x, y, tolerance = sqrt(.Machine$double.eps)) {
  same <- abs(x - y) <= tolerance
  same[is.na(same)] <- is.na(x[is.na(same)]) & is.na(y[is.na(same)])
  same
}
