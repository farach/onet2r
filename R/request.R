# O*NET API base URL
onet_base_url <- "https://services.onetcenter.org/ws"

#' Build an O*NET API Request
#'
#' Creates an httr2 request object configured for the O*NET API.
#'
#' @param .path Character string specifying the API endpoint path.
#' @param ... Additional path segments and query parameters passed to the API.
#'
#' @return An httr2 request object.
#' @keywords internal
onet_request <- function(.path, ...) {
  request(onet_base_url) |>
    req_url_path_append(.path) |>
    req_url_query(...) |>
    req_headers(`X-API-Key` = onet_api_key()) |>
    req_retry(
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
  resp <- tryCatch(
    req_perform(req),
    error = function(e) {
      # Enhance error message with context
      cli_abort(c(
        "O*NET API request failed",
        "x" = conditionMessage(e),
        "i" = "Check your API key and internet connection"
      ))
    }
  )
  
  body <- resp_body_json(resp)
  
  # Check for API-level errors in response body
  if (!is.null(body$error)) {
    handle_api_error(body$error)
  }
  
  body
}

#' Handle API Error Responses
#'
#' Processes error information from the O*NET API response body.
#'
#' @param error The error object from the API response.
#'
#' @return Never returns; always throws an error.
#' @keywords internal
handle_api_error <- function(error) {
  error_msg <- error$message %||% "Unknown API error"
  error_code <- error$code %||% "UNKNOWN"
  
  cli_abort(c(
    "O*NET API returned an error",
    "x" = paste0("[", error_code, "] ", error_msg),
    "i" = "See {.url https://services.onetcenter.org/reference/} for API documentation"
  ))
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

#' Extract List Payload from API Response
#'
#' Standardizes extraction of list-based data from O*NET API responses.
#' Handles both simple lists and nested structures.
#'
#' @param resp API response list.
#' @param key Primary key containing the data array.
#' @param schema Optional named list specifying expected column types.
#'   Each element should be a function (e.g., character(), numeric()).
#'
#' @return A tibble with standardized column names and types.
#' @keywords internal
extract_list_payload <- function(resp, key, schema = NULL) {
  data <- resp[[key]]
  
  # Handle empty results
  if (is.null(data) || length(data) == 0) {
    return(create_empty_result(schema))
  }
  
  # Convert list items to tibbles and bind
  result <- map(data, as_onet_tibble) |> list_rbind()
  
  # Apply schema if provided
  if (!is.null(schema)) {
    result <- enforce_schema(result, schema)
  }
  
  result
}

#' Create Empty Result with Schema
#'
#' Creates an empty tibble with the specified column types.
#'
#' @param schema Named list of column types (e.g., list(code = character(), value = numeric())).
#'
#' @return An empty tibble with typed columns.
#' @keywords internal
create_empty_result <- function(schema = NULL) {
  if (is.null(schema)) {
    return(tibble())
  }
  
  # Create empty vectors with correct types
  cols <- map(schema, \(type_fn) type_fn())
  do.call(tibble, cols)
}

#' Enforce Schema on Tibble
#'
#' Ensures a tibble has the expected column types, coercing if necessary.
#'
#' @param data A tibble.
#' @param schema Named list of column types.
#'
#' @return Tibble with enforced types.
#' @keywords internal
enforce_schema <- function(data, schema) {
  for (col_name in names(schema)) {
    if (col_name %in% names(data)) {
      expected_type <- class(schema[[col_name]]())[1]
      actual_type <- class(data[[col_name]])[1]
      
      if (expected_type != actual_type) {
        # Attempt type coercion
        data[[col_name]] <- tryCatch(
          as(data[[col_name]], expected_type),
          error = function(e) {
            cli_warn(c(
              "Could not coerce column {.field {col_name}} to {.cls {expected_type}}",
              "i" = "Keeping as {.cls {actual_type}}"
            ))
            data[[col_name]]
          }
        )
      }
    }
  }
  data
}

#' Paginate Through API Results
#'
#' Generic pagination helper that fetches all pages of results.
#'
#' @param fetch_page Function that fetches a single page. Should accept
#'   `start` and `end` arguments and return a list with `data`, `start`,
#'   `end`, and `total` elements.
#' @param page_size Number of items to fetch per page.
#' @param show_progress Logical indicating whether to show progress messages.
#'
#' @return A tibble containing all paginated results.
#' @keywords internal
paginate_results <- function(fetch_page, page_size = 2000, show_progress = TRUE) {
  all_rows <- list()
  start <- 1
  total <- NULL
  
  repeat {
    page <- fetch_page(start = start, end = start + page_size - 1)
    
    if (length(page$data) > 0) {
      all_rows <- c(all_rows, list(page$data))
    }
    
    # Store total from first page
    if (is.null(total)) {
      total <- page$total
    }
    
    # Show progress if requested
    if (show_progress && total > page_size) {
      next_start <- page$end + 1
      cli_inform("Fetching rows {next_start} to {min(next_start + page_size - 1, total)} of {total}...")
    }
    
    # Check if we're done
    if (page$end >= page$total || page$total == 0) {
      break
    }
    
    start <- page$end + 1
  }
  
  if (length(all_rows) == 0) {
    return(tibble())
  }
  
  list_rbind(all_rows)
}
