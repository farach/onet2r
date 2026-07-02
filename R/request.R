# =============================================================================
# O*NET API Requests (API v2)
# =============================================================================

# O*NET API base URL
onet_base_url <- "https://api-v2.onetcenter.org"

#' Build an O&#42;NET API Request
#'
#' Creates an httr2 request object configured for the O&#42;NET API.
#'
#' @param .path Character string specifying the API endpoint path.
#' @param .path_segments Character vector of additional path segments to append
#'   to the URL path (optional).
#' @param .query Named list of query parameters (optional).
#'
#' @return An httr2 request object.
#' @keywords internal
#' @noRd
onet_request <- function(.path, .path_segments = NULL, .query = list()) {
  req <- httr2::request(onet_base_url) |>
    httr2::req_url_path_append(.path)

  # Append additional path segments if provided
  if (!is.null(.path_segments)) {
    for (segment in .path_segments) {
      req <- req |> httr2::req_url_path_append(as.character(segment))
    }
  }

  # Add query parameters if provided
  if (length(.query) > 0) {
    req <- do.call(httr2::req_url_query, c(list(req), .query))
  }

  # Default headers plus retry and error policy
  req |>
    httr2::req_user_agent("onet2r (https://github.com/farach/onet2r)") |>
    httr2::req_headers(
      `X-API-Key` = onet_api_key(),
      Accept = "application/json",
      .redact = "X-API-Key"
    ) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = is_transient_error,
      backoff = \(i) 0.2 * (2^i)
    ) |>
    httr2::req_error(
      body = onet_http_error_body
    )
}

#' Check if HTTP Response is a Transient Error
#'
#' Determines if an HTTP response represents a transient error that should be retried.
#'
#' @param resp An httr2 response object.
#'
#' @return Logical indicating if the error is transient.
#' @keywords internal
#' @noRd
is_transient_error <- function(resp) {
  status <- httr2::resp_status(resp)
  status %in% c(429, 500, 502, 503, 504)
}

onet_http_error_body <- function(resp) {
  status <- httr2::resp_status(resp)
  body <- tryCatch(
    httr2::resp_body_json(resp),
    error = function(cnd) NULL
  )
  message <- body$error %||% body$message %||% httr2::resp_status_desc(resp)
  guidance <- switch(as.character(status),
    "401" = "Check ONET_API_KEY or request a key at <https://services.onetcenter.org/developer/>.",
    "403" = "Check ONET_API_KEY and whether the endpoint is available for your account.",
    "404" = "Check the occupation code or endpoint path.",
    "422" = "Check the request arguments sent to the O*NET API.",
    "429" = "The O*NET API rate limit was reached; try again later or use onet_rate_limit().",
    NULL
  )
  c(
    sprintf("O*NET API request failed with HTTP %s.", status),
    as.character(message),
    guidance
  )
}

#' Perform an O&#42;NET API Request
#'
#' Executes a request and returns the parsed JSON body with error handling.
#'
#' @param req An httr2 request object from `onet_request()`.
#'
#' @return A list containing the parsed JSON response.
#' @keywords internal
#' @noRd
onet_perform <- function(req) {
  cache_file <- onet_cache_file(req)
  if (!is.null(cache_file) && file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  onet_rate_limit_pause()

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_json(resp)

  # If the API returns structured errors inside the JSON body
  if (!is.null(body$error)) {
    cli::cli_abort(c(
      "O*NET API returned an error:",
      "x" = as.character(body$error),
      "i" = if (!is.null(body$message)) as.character(body$message)
    ))
  }

  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    saveRDS(body, cache_file)
  }

  body
}

#' Configure O&#42;NET API Response Caching
#'
#' Enables or disables local caching for O&#42;NET API responses. Caching is off by
#' default because API results may change over time, but it is useful when
#' developing analyses that repeatedly request the same endpoints.
#'
#' @param enabled Logical; enable or disable response caching.
#' @param cache_dir Directory where cached API responses should be stored.
#'
#' @return Invisibly returns a list with the active cache settings.
#'
#' @details
#' Cached Web Services responses are reused until the caller disables caching
#' or clears them with [onet_cache_clear()]. The package does not attach a
#' time-to-live, so enable caching only when stale O&#42;NET API responses are
#' acceptable for the analysis.
#' @export
#'
#' @examples
#' onet_cache_use(enabled = FALSE)
onet_cache_use <- function(
    enabled = TRUE,
    cache_dir = tools::R_user_dir("onet2r", "cache")) {
  if (!is.logical(enabled) || length(enabled) != 1 || is.na(enabled)) {
    cli::cli_abort("{.arg enabled} must be `TRUE` or `FALSE`.")
  }
  validate_single_string(cache_dir, "cache_dir")

  options(
    onet2r.cache_enabled = enabled,
    onet2r.cache_dir = cache_dir
  )

  invisible(list(enabled = enabled, cache_dir = cache_dir))
}

#' Clear Cached O&#42;NET API Responses
#'
#' Deletes cached O&#42;NET API responses and downloaded source archives.
#'
#' @param cache_dir Directory containing cached API responses.
#' @param what Cache section to clear. Use `"api"` for Web Services responses,
#'   `"archives"` for O&#42;NET database ZIPs, `"crosswalks"` for O&#42;NET bridge
#'   CSVs, `"oews"` for BLS OEWS ZIPs, or `"all"` for every section.
#'
#' @return Invisibly returns the cache directory path.
#' @export
#'
#' @examples
#' tmp <- tempfile()
#' onet_cache_use(cache_dir = tmp)
#' onet_cache_clear(cache_dir = tmp)
onet_cache_clear <- function(
    cache_dir = getOption("onet2r.cache_dir", tools::R_user_dir("onet2r", "cache")),
    what = c("api", "archives", "crosswalks", "oews", "all")) {
  validate_single_string(cache_dir, "cache_dir")
  what <- match.arg(what)
  sections <- if (what == "all") c("api", "archives", "crosswalks", "oews") else what
  unlink(file.path(cache_dir, sections), recursive = TRUE, force = TRUE)
  invisible(cache_dir)
}

#' Configure Delay Between O&#42;NET API Requests
#'
#' Sets a minimum delay before each O&#42;NET API request. This can be useful for
#' polite bulk pulls or when working near rate limits.
#'
#' @param seconds Non-negative number of seconds to wait before each API request.
#'
#' @return Invisibly returns `seconds`.
#' @export
#'
#' @examples
#' onet_rate_limit(0)
onet_rate_limit <- function(seconds = 0) {
  if (!is.numeric(seconds) || length(seconds) != 1 || is.na(seconds) || seconds < 0) {
    cli::cli_abort("{.arg seconds} must be a single non-negative number.")
  }

  options(onet2r.request_delay = seconds)
  invisible(seconds)
}

onet_rate_limit_pause <- function() {
  seconds <- getOption("onet2r.request_delay", 0)
  if (is.numeric(seconds) && length(seconds) == 1 && seconds > 0) {
    Sys.sleep(seconds)
  }

  invisible(NULL)
}

onet_cache_file <- function(req) {
  if (!isTRUE(getOption("onet2r.cache_enabled", FALSE))) {
    return(NULL)
  }

  url <- req$url %||% NULL
  if (is.null(url)) {
    return(NULL)
  }

  cache_dir <- getOption("onet2r.cache_dir", tools::R_user_dir("onet2r", "cache"))
  file.path(cache_dir, "api", paste0(rlang::hash(url), ".rds"))
}

#' Convert API Response Record to Tibble
#'
#' Converts a single record (named list or data frame row-like object) to a tibble
#' and normalizes names to snake_case.
#'
#' @param x A list or data frame-like object to convert.
#'
#' @return A tibble (possibly 0-row).
#' @keywords internal
#' @noRd
as_onet_tibble <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(tibble::tibble())
  }

  # Replace NULL and nested values so tibble conversion keeps one row per record.
  if (is.list(x) && !is.data.frame(x)) {
    x <- lapply(x, function(col) {
      if (is.null(col)) {
        return(NA)
      }
      if (is.list(col) || length(col) != 1) {
        return(list(col))
      }
      col
    })
  }

  tbl <- tibble::as_tibble(x)
  names(tbl) <- to_snake_case(names(tbl))
  tbl
}

#' Convert to Snake Case
#'
#' @param x Character vector to convert.
#' @return Character vector in snake_case.
#' @keywords internal
#' @noRd
to_snake_case <- function(x) {
  # Convert CamelCase / PascalCase / ALLCAPS blocks to snake_case
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x) # split "HTTPResponse" -> "HTTP_Response"
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x) # split "myAPI" -> "my_API"
  x <- gsub("__+", "_", x)
  tolower(x)
}

#' Extract List Data from API Response
#'
#' Standardized extraction of list-based data from API responses,
#' handling empty results and converting to tibbles.
#'
#' @param resp A list containing the API response.
#' @param key The key containing the list data.
#' @param schema Optional tibble defining the expected schema for empty results.
#'
#' @return A tibble with the extracted data or empty tibble with correct schema.
#' @keywords internal
#' @noRd
extract_list_data <- function(resp, key, schema = NULL) {
  data <- resp[[key]]

  if (is.null(data) || length(data) == 0) {
    if (!is.null(schema)) {
      return(schema)
    }
    return(tibble::tibble())
  }

  result <- purrr::map(data, as_onet_tibble) |> purrr::list_rbind()

  if (!is.null(schema)) {
    if (nrow(result) == 0) {
      return(schema)
    }
    result <- enforce_schema(result, schema)
  }

  result
}

enforce_schema <- function(data, schema, select = FALSE) {
  out <- tibble::as_tibble(data)
  for (nm in names(schema)) {
    if (!nm %in% names(out)) {
      out[[nm]] <- schema[[nm]][NA_integer_]
    }
  }
  if (isTRUE(select)) {
    return(out[names(schema)])
  }
  out[c(names(schema), setdiff(names(out), names(schema)))]
}

#' Create Empty Tibble with Schema
#'
#' Helper to create an empty tibble with specific column types.
#'
#' @param ... Named arguments where names are column names and values are type constructors
#'   (e.g., character(), numeric(), logical()).
#'
#' @return An empty tibble with the specified schema.
#' @keywords internal
#' @noRd
empty_tibble <- function(...) {
  cols <- list(...)
  if (length(cols) == 0) {
    return(tibble::tibble())
  }
  do.call(tibble::tibble, cols)
}

validate_single_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single character string.")
  }

  invisible(x)
}

validate_range <- function(start, end) {
  if (!is.numeric(start) || length(start) != 1 || is.na(start) || start < 1) {
    cli::cli_abort("{.arg start} must be a positive number.")
  }
  if (!is.numeric(end) || length(end) != 1 || is.na(end) || end < 1) {
    cli::cli_abort("{.arg end} must be a positive number.")
  }
  if (end < start) {
    cli::cli_abort("{.arg end} must be greater than or equal to {.arg start}.")
  }
  if (end - start + 1 > 2000) {
    cli::cli_abort("O*NET API ranges must request no more than 2000 rows.")
  }
  invisible(NULL)
}

occupation_schema <- function() {
  empty_tibble(code = character(), title = character())
}

occupation_records_to_tbl <- function(records) {
  if (is.null(records) || length(records) == 0) {
    return(occupation_schema())
  }

  purrr::map(records, \(x) {
    tibble::tibble(
      code = x$code %||% NA_character_,
      title = x$title %||% NA_character_
    )
  }) |>
    purrr::list_rbind()
}

#' Paginate Through API Results
#'
#' Helper to paginate through API results with optional progress reporting.
#'
#' @param fetch_page Function that fetches a single page. Should accept
#'   `start` and `end` arguments and return a list with `data`, `start`,
#'   `end`, and `total` elements.
#' @param page_size Number of records per page.
#' @param show_progress Logical indicating whether to show progress messages.
#'
#' @return A tibble containing all paginated results.
#' @keywords internal
#' @noRd
paginate_api <- function(fetch_page, page_size = 2000, show_progress = TRUE) {
  all_rows <- list()
  start <- 1
  total <- NULL
  iterations <- 0L

  repeat {
    iterations <- iterations + 1L
    page <- fetch_page(start = start, end = start + page_size - 1)

    if (is.null(total)) {
      total <- page$total %||% NA_integer_
      max_iterations <- if (is.na(total) || total <= 0) {
        1L
      } else {
        ceiling(total / page_size) + 1L
      }
    }

    if (!is.null(page$data) && nrow(page$data) > 0) {
      all_rows <- c(all_rows, list(page$data))
    }

    if (is.na(total) || total == 0) {
      break
    }

    page_end <- page$end %||% NA_integer_
    if (is.na(page_end) || page_end < start) {
      cli::cli_abort(
        c(
          "O*NET pagination did not advance.",
          "i" = "The API returned page end {.val {page_end}} for start {.val {start}}."
        )
      )
    }
    if (page_end >= total) {
      break
    }
    if (iterations > max_iterations) {
      cli::cli_abort("O*NET pagination exceeded the expected number of pages.")
    }

    start <- page_end + 1

    if (isTRUE(show_progress) && total > 0) {
      cli::cli_inform("Fetching rows {start} to {min(start + page_size - 1, total)} of {total}...")
    }
  }

  if (length(all_rows) == 0) {
    return(tibble::tibble())
  }

  out <- purrr::list_rbind(all_rows)
  if (!is.na(total) && nrow(out) != total) {
    cli::cli_warn(
      "O*NET pagination returned {nrow(out)} rows but reported {total} total rows."
    )
  }
  out
}
