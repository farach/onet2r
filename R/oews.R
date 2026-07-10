# =============================================================================
# BLS Occupational Employment and Wage Statistics (OEWS)
# =============================================================================

#' Download OEWS Estimates
#'
#' Downloads Occupational Employment and Wage Statistics (OEWS) estimates from
#' the U.S. Bureau of Labor Statistics and returns them as a tibble. OEWS
#' estimates include employment counts and wage percentiles by Standard
#' Occupational Classification (SOC) code.
#'
#' @param type OEWS file type: `"national"`, `"state"`, `"metro"`, or
#'   `"industry"`.
#' @param year Integer estimate year. Defaults to the most recent May estimates
#'   assumed available (previous calendar year from April onward, two years
#'   back before April). If BLS has not yet published that release, the
#'   download fails - pass the prior year explicitly.
#' @param path Optional path to a manually downloaded OEWS ZIP, CSV, TXT, DAT, or
#'   Excel file. If supplied, no download is attempted.
#' @param cache_dir Directory used to cache the downloaded BLS ZIP file.
#' @param force Logical; if `TRUE`, re-download even when a cached file exists.
#' @param quiet Logical; if `FALSE`, show download progress.
#'
#' @return A tibble of OEWS estimates with `year`, `oews_type`, and snake_case
#'   OEWS columns. Special OEWS markers are preserved as indicator columns when
#'   present: `#` as top-coded, `*` as wage suppressed, `**` as employment
#'   suppressed, and `~` as less than 0.5 percent.
#' @details
#' ## Wage-field semantics
#'
#' OEWS publishes both hourly (`h_*`) and annual (`a_*`) wage fields. Some
#' occupations are annual-only (`annual = TRUE`; e.g., teachers) or
#' hourly-only (`hourly = TRUE`; e.g., actors) - their missing counterpart
#' fields are structural, not suppressed. Suppression and top-coding are
#' flagged separately (see the `*_topcoded` and suppression indicator
#' columns). `*_prse` columns are percent relative standard errors of the
#' corresponding estimates.
#'
#' ## BLS download blocking
#'
#' BLS may reject automated ZIP downloads with HTTP 403 even when the same URL
#' downloads in a browser. `onet_oews()` handles that case in three steps:
#' first it checks the package cache, then it looks for the matching ZIP in your
#' Downloads folder, then in interactive sessions it opens the official BLS URL
#' in your browser and waits for the downloaded file to appear. Set
#' `options(onet2r.oews_download_dir = "path/to/downloads")` to search a
#' different manual-download folder. Set
#' `options(onet2r.oews_browser_fallback = FALSE)` to disable opening a browser.
#' Set `options(onet2r.oews_browser_wait = 120)` to control how many seconds
#' `onet_oews()` waits for a browser download. You can always pass a ZIP path
#' directly with `path`; this is the stable fallback if BLS changes OEWS URLs or
#' file names.
#' @export
#'
#' @examplesIf interactive()
#' oews <- onet_oews("national", 2024)
#' head(oews)
onet_oews <- function(
    type = c("national", "state", "metro", "industry"),
    year = latest_oews_year(),
    path = NULL,
    cache_dir = tools::R_user_dir("onet2r", "cache"),
    force = FALSE,
    quiet = TRUE) {
  type <- match.arg(type)
  year <- validate_oews_year(year)

  if (is.null(path)) {
    path <- download_oews_file(
      type = type,
      year = year,
      cache_dir = cache_dir,
      force = force,
      quiet = quiet
    )
  } else {
    validate_existing_path(path)
    warn_oews_path_year(path, year)
  }

  out <- read_oews_file(path)
  out$year <- year
  out$oews_type <- type
  dplyr::relocate(out, dplyr::all_of(c("year", "oews_type")))
}

#' Download National OEWS Estimates
#'
#' Downloads national OEWS employment and wage estimates.
#'
#' @inheritParams onet_oews
#' @return A tibble of national OEWS estimates with snake_case columns.
#' @export
#'
#' @examplesIf interactive()
#' onet_oews_national(2024)
onet_oews_national <- function(
    year = latest_oews_year(),
    path = NULL,
    cache_dir = tools::R_user_dir("onet2r", "cache"),
    force = FALSE,
    quiet = TRUE) {
  onet_oews(
    type = "national",
    year = year,
    path = path,
    cache_dir = cache_dir,
    force = force,
    quiet = quiet
  )
}

#' Download State OEWS Estimates
#'
#' Downloads state-level OEWS employment and wage estimates.
#'
#' @inheritParams onet_oews
#' @return A tibble of state OEWS estimates with snake_case columns.
#' @export
#'
#' @examplesIf interactive()
#' onet_oews_state(2024)
onet_oews_state <- function(
    year = latest_oews_year(),
    path = NULL,
    cache_dir = tools::R_user_dir("onet2r", "cache"),
    force = FALSE,
    quiet = TRUE) {
  onet_oews(
    type = "state",
    year = year,
    path = path,
    cache_dir = cache_dir,
    force = force,
    quiet = quiet
  )
}

#' Download Metropolitan Area OEWS Estimates
#'
#' Downloads metropolitan-area OEWS employment and wage estimates.
#'
#' @inheritParams onet_oews
#' @return A tibble of metropolitan-area OEWS estimates with snake_case columns.
#' @export
#'
#' @examplesIf interactive()
#' onet_oews_metro(2024)
onet_oews_metro <- function(
    year = latest_oews_year(),
    path = NULL,
    cache_dir = tools::R_user_dir("onet2r", "cache"),
    force = FALSE,
    quiet = TRUE) {
  onet_oews(
    type = "metro",
    year = year,
    path = path,
    cache_dir = cache_dir,
    force = force,
    quiet = quiet
  )
}

#' Download Industry OEWS Estimates
#'
#' Downloads industry-level OEWS employment and wage estimates.
#'
#' @inheritParams onet_oews
#' @return A tibble of industry OEWS estimates with snake_case columns.
#' @export
#'
#' @examplesIf interactive()
#' onet_oews_industry(2024)
onet_oews_industry <- function(
    year = latest_oews_year(),
    path = NULL,
    cache_dir = tools::R_user_dir("onet2r", "cache"),
    force = FALSE,
    quiet = TRUE) {
  onet_oews(
    type = "industry",
    year = year,
    path = path,
    cache_dir = cache_dir,
    force = force,
    quiet = quiet
  )
}

#' Join O&#42;NET Occupations to National OEWS Estimates
#'
#' Joins O&#42;NET occupation data to national OEWS employment and wage estimates by
#' converting detailed O&#42;NET-SOC codes such as `"15-1252.00"` to SOC codes such
#' as `"15-1252"`.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @param occupations A data frame containing an O&#42;NET occupation code column.
#' @param oews Optional OEWS tibble. If omitted, `onet_oews_national()` is called.
#' @param year Integer OEWS estimate year, used when `oews` is omitted.
#' @param by Name of the occupation code column in `occupations`.
#' @param cache_dir Directory used to cache the downloaded BLS ZIP file.
#' @param force Logical; if `TRUE`, re-download OEWS data when `oews` is omitted.
#' @param quiet Logical; if `FALSE`, show download progress.
#'
#' @return A tibble containing `occupations` plus a `soc_code` column and
#'   matching national OEWS employment and wage estimate columns.
#'
#' @section Lifecycle:
#' This helper is soft-deprecated in favor of [onet_weight_panel_oews()] plus
#' [onet_measure_aggregate()] for vintage-aware weighting.
#' @export
#'
#' @examples
#' occupations <- tibble::tibble(
#'   code = c("15-1252.00", "29-1141.00"),
#'   title = c("Software Developers", "Registered Nurses")
#' )
#'
#' oews <- tibble::tibble(
#'   occ_code = c("15-1252", "29-1141"),
#'   occ_title = c("Software Developers", "Registered Nurses"),
#'   tot_emp = c(1847900, 3175400),
#'   a_median = c(133080, 93070)
#' )
#'
#' suppressWarnings(onet_join_oews(occupations, oews = oews))
onet_join_oews <- function(
    occupations,
    oews = NULL,
    year = latest_oews_year(),
    by = "code",
    cache_dir = tools::R_user_dir("onet2r", "cache"),
    force = FALSE,
    quiet = TRUE) {
  lifecycle::deprecate_soft(
    "0.4.0",
    "onet_join_oews()",
    "onet_weight_panel_oews()",
    details = paste(
      "Use `onet_weight_panel_oews()` to build weights and",
      "`onet_measure_aggregate()` to aggregate user-supplied measures."
    )
  )
  join_oews_impl(
    occupations = occupations,
    oews = oews,
    year = year,
    by = by,
    cache_dir = cache_dir,
    force = force,
    quiet = quiet
  )
}

join_oews_impl <- function(
    occupations,
    oews = NULL,
    year = latest_oews_year(),
    by = "code",
    cache_dir = tools::R_user_dir("onet2r", "cache"),
    force = FALSE,
    quiet = TRUE) {
  if (!is.data.frame(occupations)) {
    cli::cli_abort("{.arg occupations} must be a data frame.")
  }
  if (!is.character(by) || length(by) != 1 || !by %in% names(occupations)) {
    cli::cli_abort("{.arg by} must name a column in {.arg occupations}.")
  }

  if (is.null(oews)) {
    oews <- onet_oews_national(
      year = year,
      cache_dir = cache_dir,
      force = force,
      quiet = quiet
    )
  }
  if (!is.data.frame(oews) || !"occ_code" %in% names(oews)) {
    cli::cli_abort("{.arg oews} must be a data frame with an {.var occ_code} column.")
  }

  occupations <- tibble::as_tibble(occupations)
  occupations$soc_code <- standardize_soc_code(occupations[[by]])

  dplyr::left_join(
    occupations,
    tibble::as_tibble(oews),
    by = dplyr::join_by(soc_code == occ_code),
    relationship = "many-to-one"
  )
}

latest_oews_year <- function(today = Sys.Date()) {
  parts <- as.POSIXlt(today)
  year <- parts$year + 1900
  month <- parts$mon + 1

  if (month >= 4) year - 1 else year - 2
}

validate_oews_year <- function(year) {
  if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
    cli::cli_abort("{.arg year} must be a single numeric year.")
  }

  year <- as.integer(year)
  if (year < 2003 || year > latest_oews_year() + 1) {
    cli::cli_abort(c(
      "{.arg year} must be a supported May OEWS estimate year, 2003 or later.",
      "i" = "The default assumes BLS publishes May estimates by early April of the following year; if the newest release is not out yet, pass an earlier {.arg year} explicitly."
    ))
  }

  year
}

download_oews_file <- function(type, year, cache_dir, force = FALSE, quiet = TRUE) {
  cache_dir <- file.path(cache_dir, "oews")
  url <- oews_url(type = type, year = year)
  destfile <- file.path(cache_dir, basename(url))
  onet_with_cache_transaction(
    destfile,
    download_oews_file_transaction(
      type = type,
      year = year,
      cache_dir = cache_dir,
      force = force,
      quiet = quiet,
      url = url,
      destfile = destfile
    )
  )
}

download_oews_file_transaction <- function(
    type,
    year,
    cache_dir,
    force,
    quiet,
    url,
    destfile) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destfile) && !isTRUE(force)) {
    validate_oews_zip(destfile)
    return(destfile)
  }

  manual_path <- find_oews_manual_download(type = type, year = year)
  if (!is.null(manual_path) && !isTRUE(force)) {
    return(cache_oews_manual_download(manual_path, destfile, quiet = quiet))
  }

  tmpfile <- tempfile("oews-", tmpdir = cache_dir, fileext = ".zip")
  on.exit(unlink(tmpfile, force = TRUE), add = TRUE)

  if (!file.exists(destfile) || isTRUE(force)) {
    download_oews_file_from_url(
      url = url,
      destfile = tmpfile,
      quiet = quiet,
      type = type,
      year = year
    )
    validate_oews_zip(tmpfile)
    onet_with_cache_lock(destfile, onet_atomic_replace(tmpfile, destfile))
  }

  destfile
}

download_oews_file_from_url <- function(
    url,
    destfile,
    quiet = TRUE,
    type,
    year) {
  tryCatch(
    download_oews_file_http(
      url = url,
      destfile = destfile,
      quiet = quiet
    ),
    error = function(cnd) {
      manual_path <- find_oews_manual_download(type = type, year = year)
      if (!is.null(manual_path)) {
        cache_oews_manual_download(
          manual_path,
          destfile,
          quiet = quiet
        )
        return(invisible(destfile))
      }

      manual_path <- request_oews_browser_download(
        url = url,
        type = type,
        year = year
      )
      if (!is.null(manual_path)) {
        cache_oews_manual_download(
          manual_path,
          destfile,
          quiet = quiet
        )
        return(invisible(destfile))
      }

      cli::cli_abort(
        c(
          "Failed to download OEWS estimates from BLS.",
          "i" = "URL: {.url {url}}",
          "i" = "BLS may reject automated downloads even when this URL works in a browser.",
          "i" = "Download the ZIP in your browser, leave it in Downloads, and rerun this function.",
          "i" = "If BLS changes URLs or file names, pass the current ZIP path directly with {.arg path}."
        ),
        parent = cnd
      )
    }
  )

  invisible(destfile)
}

request_oews_browser_download <- function(url, type, year) {
  if (!oews_browser_fallback_enabled()) {
    return(NULL)
  }

  file <- oews_file_name(type, year)
  cli::cli_inform(c(
    "BLS rejected the automated OEWS download. Opening the official URL in your browser.",
    "i" = "Leave the downloaded {.file {file}} file in your Downloads folder.",
    "i" = "onet2r will continue after the file appears."
  ))

  open_oews_browser_url(url)
  wait_for_oews_manual_download(
    type = type,
    year = year,
    timeout = oews_browser_wait_seconds()
  )
}

oews_browser_fallback_enabled <- function() {
  isTRUE(getOption("onet2r.oews_browser_fallback", interactive()))
}

oews_browser_wait_seconds <- function() {
  wait <- getOption("onet2r.oews_browser_wait", 120)
  wait <- suppressWarnings(as.numeric(wait))
  if (length(wait) != 1 || is.na(wait) || wait < 0) {
    return(120)
  }
  wait
}

wait_for_oews_manual_download <- function(type, year, timeout, interval = 1) {
  deadline <- Sys.time() + timeout

  repeat {
    path <- find_oews_manual_download(type = type, year = year)
    if (!is.null(path) && is_readable_oews_zip(path)) {
      return(path)
    }

    if (Sys.time() >= deadline) {
      return(NULL)
    }

    Sys.sleep(interval)
  }
}

open_oews_browser_url <- function(url) {
  utils::browseURL(url)
  invisible(url)
}

download_oews_file_http <- function(url, destfile, quiet = TRUE) {
  if (!isTRUE(quiet)) {
    cli::cli_inform("Downloading OEWS estimates from BLS: {.url {url}}")
  }

  httr2::request(url) |>
    httr2::req_user_agent(oews_user_agent()) |>
    httr2::req_headers(
      Accept = "application/zip,application/octet-stream,*/*"
    ) |>
    httr2::req_timeout(max(300, getOption("timeout"))) |>
    httr2::req_perform(path = destfile)

  invisible(destfile)
}

oews_user_agent <- function() {
  "onet2r (https://github.com/farach/onet2r)"
}

oews_national_url <- function(year) {
  oews_url("national", year)
}

oews_url <- function(type, year) {
  sprintf(
    "https://www.bls.gov/oes/special-requests/%s",
    oews_file_name(type, year)
  )
}

oews_file_name <- function(type, year) {
  yy <- year %% 100
  slug <- switch(type,
    national = "nat",
    state = "st",
    metro = "ma",
    industry = "in4"
  )

  sprintf("oesm%02d%s.zip", yy, slug)
}

find_oews_manual_download <- function(type, year) {
  file <- oews_file_name(type, year)

  for (dir in oews_manual_download_dirs()) {
    candidates <- oews_manual_download_candidates(dir, file)
    if (length(candidates) == 0) {
      next
    }

    info <- file.info(candidates)
    candidates <- candidates[order(info$mtime, decreasing = TRUE, na.last = TRUE)]
    readable <- vapply(candidates, is_readable_oews_zip, logical(1))
    if (any(readable)) {
      return(candidates[readable][[1]])
    }
  }

  NULL
}

oews_manual_download_candidates <- function(dir, file) {
  if (!dir.exists(dir)) {
    return(character())
  }

  stem <- tools::file_path_sans_ext(file)
  pattern <- sprintf("^%s( \\([0-9]+\\))?\\.zip$", stem)
  list.files(dir, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
}

oews_manual_download_dirs <- function() {
  dirs <- c(
    getOption("onet2r.oews_download_dir"),
    Sys.getenv("ONET2R_OEWS_DOWNLOAD_DIR", unset = NA_character_),
    file.path(path.expand("~"), "Downloads"),
    file.path(Sys.getenv("USERPROFILE"), "Downloads")
  )

  unique(dirs[!is.na(dirs) & nzchar(dirs)])
}

cache_oews_manual_download <- function(path, destfile, quiet = TRUE) {
  validate_oews_manual_zip(path)
  if (!isTRUE(quiet)) {
    cli::cli_inform(c(
      "Using manually downloaded OEWS ZIP.",
      "i" = "Path: {.path {path}}"
    ))
  }

  onet_atomic_write(destfile, function(tmp) {
    if (!file.copy(path, tmp, overwrite = FALSE)) {
      cli::cli_abort("Failed to copy manually downloaded OEWS ZIP into the cache.")
    }
    validate_oews_zip(tmp)
  })
  destfile
}

validate_oews_manual_zip <- function(path) {
  tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(cnd) {
      cli::cli_abort(
        "Manually downloaded OEWS file is not a readable ZIP archive: {.path {path}}",
        parent = cnd
      )
    }
  )
  invisible(path)
}

is_readable_oews_zip <- function(path) {
  tryCatch(
    {
      suppressWarnings(utils::unzip(path, list = TRUE))
      TRUE
    },
    error = function(cnd) FALSE
  )
}

read_oews_file <- function(path) {
  if (!grepl("\\.zip$", path, ignore.case = TRUE)) {
    return(read_oews_data_file(path))
  }

  files <- utils::unzip(path, list = TRUE)
  data_file <- first_oews_data_file(files$Name)

  tmpdir <- tempfile("onet2r-oews-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  utils::unzip(path, files = data_file, exdir = tmpdir)
  extracted <- file.path(tmpdir, data_file)

  read_oews_data_file(extracted)
}

read_oews_data_file <- function(path) {
  data <- if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    read_oews_excel(path)
  } else if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE)
  }

  clean_oews_data(data)
}

read_oews_excel <- function(path) {
  if (!rlang::is_installed("readxl")) {
    cli::cli_abort(
      c(
        "Package {.pkg readxl} is required to read OEWS Excel files.",
        "i" = "Install it with {.code install.packages(\"readxl\")}."
      )
    )
  }

  readxl::read_excel(path, guess_max = 100000, .name_repair = "minimal")
}

first_oews_data_file <- function(files) {
  matches <- files[grepl("\\.(xlsx?|csv|txt|dat)$", files, ignore.case = TRUE)]

  if (length(matches) == 0) {
    cli::cli_abort("The OEWS ZIP file does not contain a supported data file.")
  }

  preferred <- matches[grepl("national|_dl|data", matches, ignore.case = TRUE)]
  if (length(preferred) > 0) {
    if (length(preferred) > 1) {
      cli::cli_warn(
        c(
          "The OEWS ZIP contains multiple candidate data files; reading the first one.",
          "i" = "Candidates: {.path {preferred}}"
        )
      )
    }
    return(preferred[[1]])
  }

  if (length(matches) > 1) {
    cli::cli_warn(
      c(
        "The OEWS ZIP contains multiple data files; reading the first one.",
        "i" = "Candidates: {.path {matches}}"
      )
    )
  }
  matches[[1]]
}

clean_oews_data <- function(data) {
  out <- tibble::as_tibble(data)
  names(out) <- clean_oews_names(names(out))

  numeric_cols <- intersect(names(out), oews_numeric_columns())
  for (col in numeric_cols) {
    out <- add_oews_special_flags(out, col)
    out[[col]] <- parse_oews_number(out[[col]])
  }

  flag_cols <- intersect(names(out), c("annual", "hourly"))
  for (col in flag_cols) {
    out[[col]] <- toupper(trimws(as.character(out[[col]]))) %in% c("TRUE", "T", "YES", "Y")
  }

  if ("occ_code" %in% names(out)) {
    out$occ_code <- as.character(out$occ_code)
  }

  out
}

add_oews_special_flags <- function(data, col) {
  x <- trimws(as.character(data[[col]]))
  flag_values <- list(
    topcoded = "#",
    wage_suppressed = "*",
    employment_suppressed = "**",
    less_than_half = "~"
  )
  for (suffix in names(flag_values)) {
    marker <- flag_values[[suffix]]
    flag <- !is.na(x) & x == marker
    if (any(flag)) {
      data[[paste(col, suffix, sep = "_")]] <- flag
    }
  }
  data
}

clean_oews_names <- function(x) {
  x <- tolower(x)
  x <- gsub("%", "pct", x, fixed = TRUE)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x <- gsub("_+", "_", x)
  x
}

parse_oews_number <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  x <- gsub(",", "", as.character(x), fixed = TRUE)
  x <- gsub("$", "", x, fixed = TRUE)
  x <- trimws(x)
  x[x %in% c("", "*", "#", "**", "~", NA_character_)] <- NA_character_

  suppressWarnings(as.numeric(x))
}

validate_oews_zip <- function(path) {
  tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(cnd) {
      unlink(path, force = TRUE)
      cli::cli_abort("Cached OEWS file is not a readable ZIP archive.", parent = cnd)
    }
  )
  invisible(path)
}

warn_oews_path_year <- function(path, year) {
  file <- basename(path)
  match <- regexec("oesm([0-9]{2})", file, ignore.case = TRUE)
  parts <- regmatches(file, match)[[1]]
  if (length(parts) < 2) {
    return(invisible(NULL))
  }
  file_year <- 2000L + as.integer(parts[[2]])
  if (!is.na(file_year) && file_year != as.integer(year)) {
    cli::cli_warn(
      "The OEWS file name looks like year {file_year}, but {.arg year} is {year}."
    )
  }
  invisible(NULL)
}

oews_numeric_columns <- function() {
  c(
    "tot_emp",
    "emp_prse",
    "jobs_1000",
    "loc_quotient",
    "pct_total",
    "pct_rpt",
    "h_mean",
    "a_mean",
    "mean_prse",
    "h_pct10",
    "h_pct25",
    "h_median",
    "h_pct75",
    "h_pct90",
    "a_pct10",
    "a_pct25",
    "a_median",
    "a_pct75",
    "a_pct90"
  )
}

standardize_soc_code <- function(code) {
  code <- as.character(code)
  code <- trimws(code)
  code <- sub("\\.\\d{2}$", "", code)

  digits <- gsub("[^0-9]", "", code)
  six_digit <- !is.na(digits) & grepl("^\\d{6}$", digits)
  code[six_digit] <- paste0(
    substr(digits[six_digit], 1, 2),
    "-",
    substr(digits[six_digit], 3, 6)
  )

  code
}

standardize_onet_soc_code <- function(code) {
  code <- as.character(code)
  code <- trimws(code)

  clean <- gsub("[^0-9A-Za-z]", "", code)
  eight_digit <- !is.na(clean) & grepl("^\\d{8}$", clean)
  six_digit <- !is.na(clean) & grepl("^\\d{6}$", clean)

  code[eight_digit] <- paste0(
    substr(clean[eight_digit], 1, 2),
    "-",
    substr(clean[eight_digit], 3, 6),
    ".",
    substr(clean[eight_digit], 7, 8)
  )
  code[six_digit] <- paste0(
    substr(clean[six_digit], 1, 2),
    "-",
    substr(clean[six_digit], 3, 6),
    ".00"
  )

  code
}

validate_existing_path <- function(path) {
  validate_single_string(path, "path")
  if (!file.exists(path)) {
    cli::cli_abort("{.arg path} does not exist: {.path {path}}")
  }

  invisible(path)
}
