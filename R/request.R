# O*NET API base URL
onet_base_url <- "https://api-v2.onetcenter.org"

#' Build an O*NET API Request
#'
#' Creates an httr2 request object configured for the O*NET API.
#'
#' @param .path Character string specifying the API endpoint path.
#' @param .path_segments Additional path segments to append to the URL path (optional).
#' @param .query Named list or arguments for query parameters (optional).
#'
#' @return An httr2 request object.
#' @keywords internal
onet_request <- function(.path, .path_segments = NULL, .query = list()) {
  req <- request(onet_base_url) |>
    req_url_path_append(.path)
  
  # Append additional path segments if provided
  if (!is.null(.path_segments)) {
    for (segment in .path_segments) {
      req <- req |> req_url_path_append(segment)
    }
  }
  
  # Add query parameters if provided
  if (length(.query) > 0) {
    req <- req |> req_url_query(!!!.query)
  }
  
  req |>
    req_headers(`X-API-Key` = onet_api_key()) |>
    req_retry(
      max_tries = 3,
      is_transient = \(resp) {
        status <- resp_status(resp)
        # Retry on rate limits and transient server errors
        status %in% c(429, 500, 502, 503, 504)
      },
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
  status <- resp_status(resp)
  # Retry on rate limiting (429) and server errors (500-599)
  status == 429 || (status >= 500 && status < 600)
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
  resp <- req_perform(req)
  body <- resp_body_json(resp)
  
  # Check for OpenAPI-style errors in response body
  if (!is.null(body$error)) {
    cli_abort(c(
      "O*NET API returned an error:",
      "x" = body$error,
      "i" = if (!is.null(body$message)) body$message
    ))
  }
  
  body
}

#' Convert API Response to Tibble
#'
#' Converts a list from the API to a tibble with snake_case column names.
#'
#' @param x A list or data frame to convert.
#'
#' @return A tibble.
#' @keywords internal
as_onet_tibble <- function(x) {
 if (length(x) == 0) {
    return(tibble())
  }
  
  # Handle NULL columns by converting them to NA vectors
  # This prevents as_tibble() from erroring on NULL values
  if (is.list(x) && !is.data.frame(x)) {
    # For named lists, replace NULL values with NA
    x <- lapply(x, function(col) {
      if (is.null(col)) NA else col
    })
  }
  
  tbl <- as_tibble(x)
  names(tbl) <- to_snake_case(names(tbl))
  tbl
}

#' Convert to Snake Case
#'
#' @param x Character vector to convert.
#' @return Character vector in snake_case.
#' @keywords internal
to_snake_case <- function(x) {
  x <- gsub("([A-Z])", "_\\1", x)
  x <- gsub("^_", "", x)
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
    return(tibble())
  }
  map(data, as_onet_tibble) |> list_rbind()
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
  
  # Handle missing or empty data
  if (is.null(data) || length(data) == 0) {
    if (!is.null(schema)) {
      return(schema)
    }
    return(tibble())
  }
  
  # Convert list to tibble
  result <- map(data, as_onet_tibble) |> list_rbind()
  
  # If we have a schema and result is empty, return schema
  if (nrow(result) == 0 && !is.null(schema)) {
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
  if (length(cols) == 0) {
    return(tibble())
  }
  do.call(tibble, cols)
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
    
    # Store total from first page
    if (is.null(total)) {
      total <- page$total %||% 0
    }
    
    # Collect data if present
    if (length(page$data) > 0 && nrow(page$data) > 0) {
      all_rows <- c(all_rows, list(page$data))
    }
    
    # Check if we're done
    if (page$end >= page$total || page$total == 0) {
      break
    }
    
    # Show progress
    start <- page$end + 1
    if (show_progress && total > 0) {
      cli_inform("Fetching rows {start} to {min(start + page_size - 1, total)} of {total}...")
    }
  }
  
  # Combine all pages
  if (length(all_rows) == 0) {
    return(tibble())
  }
  
  list_rbind(all_rows)
}
