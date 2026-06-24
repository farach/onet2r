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
#' @param year Integer estimate year. Defaults to the latest year expected to be
#'   available from the BLS public OEWS downloads.
#' @param path Optional path to a manually downloaded OEWS ZIP, CSV, TXT, DAT, or
#'   Excel file. If supplied, no download is attempted.
#' @param cache_dir Directory used to cache the downloaded BLS ZIP file.
#' @param force Logical; if `TRUE`, re-download even when a cached file exists.
#' @param quiet Logical; if `FALSE`, show download progress.
#'
#' @return A tibble of OEWS estimates with `year`, `oews_type`, and snake_case
#'   OEWS columns.
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
#' onet_join_oews(occupations, oews = oews)
onet_join_oews <- function(
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
  if (year < 1999 || year > latest_oews_year() + 1) {
    cli::cli_abort("{.arg year} must be a plausible OEWS estimate year.")
  }

  year
}

download_oews_file <- function(type, year, cache_dir, force = FALSE, quiet = TRUE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  url <- oews_url(type = type, year = year)
  destfile <- file.path(cache_dir, basename(url))

  if (!file.exists(destfile) || isTRUE(force)) {
    old_options <- options(
      HTTPUserAgent = paste(
        "onet2r",
        as.character(utils::packageVersion("onet2r")),
        "(https://github.com/farach/onet2r)"
      ),
      timeout = max(300, getOption("timeout"))
    )
    on.exit(options(old_options), add = TRUE)

    tryCatch(
      utils::download.file(
        url = url,
        destfile = destfile,
        mode = "wb",
        quiet = quiet
      ),
      error = function(cnd) {
        cli::cli_abort(
          c(
            "Failed to download OEWS estimates from BLS.",
            "i" = "URL: {.url {url}}",
            "i" = "You can download the ZIP manually and pass it to {.arg path}."
          ),
          parent = cnd
        )
      }
    )
  }

  destfile
}

oews_national_url <- function(year) {
  oews_url("national", year)
}

oews_url <- function(type, year) {
  yy <- year %% 100
  slug <- switch(type,
    national = "nat",
    state = "st",
    metro = "msa",
    industry = "in"
  )

  sprintf("https://www.bls.gov/oes/special.requests/oesm%02d%s.zip", yy, slug)
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
    return(preferred[[1]])
  }

  matches[[1]]
}

clean_oews_data <- function(data) {
  out <- tibble::as_tibble(data)
  names(out) <- clean_oews_names(names(out))

  numeric_cols <- intersect(names(out), oews_numeric_columns())
  for (col in numeric_cols) {
    out[[col]] <- parse_oews_number(out[[col]])
  }

  if ("occ_code" %in% names(out)) {
    out$occ_code <- as.character(out$occ_code)
  }

  out
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
  x[x %in% c("", "*", "#", "**", NA_character_)] <- NA_character_

  suppressWarnings(as.numeric(x))
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
