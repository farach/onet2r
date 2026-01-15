# =============================================================================
# O*NET API Requests (API v2)
# =============================================================================

# O*NET API base URL
onet_base_url <- "https://api-v2.onetcenter.org"

#' Build an O*NET API Request
#'
#' Creates an httr2 request object configured for the O*NET API.
#'
#' @param .path Character string specifying the API endpoint path.
#' @param .path_segments Character vector of additional path segments to append
#'   to the URL path (optional).
#' @param .query Named list of query parameters (optional).
#'
#' @return An httr2 request object.
#' @keywords internal
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
  
  # Default headers + retry policy
  req |>
    httr2::req_headers(
      `X-API-Key` = onet_api_key(),
      Accept = "application/json"
    ) |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = is_transient_error,
      backoff = \(i) 0.2 * (2 ^ i)
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
is_transient_error <- function(resp) {
  status <- httr2::resp_status(resp)
  status %in% c(429, 500, 502, 503, 504)
}

#' Perform an O*NET API Request
#'
#' Executes a request and returns the parsed JSON body with error handling.
#'
#' @param req An httr2 request object from [onet_request()].
#'
#' @return A list containing the parsed JSON response.
#' @keywords internal
onet_perform <- function(req) {
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
  
  body
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
as_onet_tibble <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(tibble::tibble())
  }
  
  # Replace NULL values with NA so tibble conversion doesn't error
  if (is.list(x) && !is.data.frame(x)) {
    x <- lapply(x, function(col) if (is.null(col)) NA else col)
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
to_snake_case <- function(x) {
  # Convert CamelCase / PascalCase / ALLCAPS blocks to snake_case
  # Examples:
  #   "HTTPResponse" -> "http_response"
  #   "API"          -> "api"
  #   "MyAPIClient"  -> "my_api_client"
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", x)  # split "HTTPResponse" -> "HTTP_Response"
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)     # split "myAPI" -> "my_API"
  x <- gsub("__+", "_", x)
  tolower(x)
}

#' Extract Data from Paged Response
#'
#' Extracts the data portion from a paged API response.
#'
#' @param resp A list containing the API response.
#' @param key The key containing the data (e.g., "occupation", "rows").
#'
#' @return A tibble of the extracted data.
#' @keywords internal
extract_paged_data <- function(resp, key) {
  data <- resp[[key]]
  if (is.null(data) || length(data) == 0) {
    return(tibble::tibble())
  }
  purrr::map(data, as_onet_tibble) |> purrr::list_rbind()
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
extract_list_data <- function(resp, key, schema = NULL) {
  data <- resp[[key]]
  
  if (is.null(data) || length(data) == 0) {
    if (!is.null(schema)) return(schema)
    return(tibble::tibble())
  }
  
  result <- purrr::map(data, as_onet_tibble) |> purrr::list_rbind()
  
  if (!is.null(schema) && nrow(result) == 0) {
    return(schema)
  }
  
  result
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
empty_tibble <- function(...) {
  cols <- list(...)
  if (length(cols) == 0) return(tibble::tibble())
  do.call(tibble::tibble, cols)
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
paginate_api <- function(fetch_page, page_size = 2000, show_progress = TRUE) {
  all_rows <- list()
  start <- 1
  total <- NULL
  
  repeat {
    page <- fetch_page(start = start, end = start + page_size - 1)
    
    if (is.null(total)) {
      total <- page$total %||% 0
    }
    
    if (!is.null(page$data) && nrow(page$data) > 0) {
      all_rows <- c(all_rows, list(page$data))
    }
    
    if (isTRUE(page$total == 0) || isTRUE(page$end >= page$total)) {
      break
    }
    
    start <- page$end + 1
    
    if (isTRUE(show_progress) && total > 0) {
      cli::cli_inform("Fetching rows {start} to {min(start + page_size - 1, total)} of {total}...")
    }
  }
  
  if (length(all_rows) == 0) {
    return(tibble::tibble())
  }
  
  purrr::list_rbind(all_rows)
}
