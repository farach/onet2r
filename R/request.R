# O*NET API base URL
onet_base_url <- "https://api-v2.onetcenter.org"

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
  dots <- rlang::list2(...)
  nms <- names(dots)

  # Identify unnamed vs named components
  if (is.null(nms)) {
    is_unnamed <- rep(TRUE, length(dots))
  } else {
    is_unnamed <- is.na(nms) | nms == ""
  }

  path_parts <- dots[is_unnamed]
  query_parts <- dots[!is_unnamed]

  req <- httr2::request(onet_base_url) |>
    httr2::req_url_path_append(.path)

  # Unnamed args -> additional path segments
  if (length(path_parts) > 0) {
    for (p in path_parts) {
      req <- httr2::req_url_path_append(req, as.character(p))
    }
  }

  # Named args -> query parameters
  if (length(query_parts) > 0) {
    req <- do.call(httr2::req_url_query, c(list(req), query_parts))
  }

  req <- req |>
    httr2::req_headers(`X-API-Key` = onet_api_key(), Accept = "application/json") |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = \(resp) httr2::resp_status(resp) == 429,
      backoff = \(i) 0.2 * (2^i)
    )

  req
}

#' Perform an O*NET API Request
#'
#' Executes a request and returns the parsed JSON body.
#'
#' @param req An httr2 request object from [onet_request()].
#'
#' @return A list containing the parsed JSON response.
#' @keywords internal
onet_perform <- function(req) {
  resp <- httr2::req_perform(req)
  httr2::resp_body_json(resp)
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
