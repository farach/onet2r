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
      is_transient = \(resp) resp_status(resp) == 429,
      backoff = \(i) 0.2 * (2 ^ i)
    )
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
  resp <- req_perform(req)
  resp_body_json(resp)
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
