#' List O*NET Database Tables
#'
#' Retrieves a list of available database tables in O*NET.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{id}{Table identifier}
#'     \item{title}{Table title/description}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' tables <- onet_tables()
#' head(tables)
#' }
onet_tables <- function() {
  resp <- onet_request("database") |>
    onet_perform()

  schema <- list(id = character(), title = character())

  if (is.null(resp$table) || length(resp$table) == 0) {
    return(create_empty_result(schema))
  }

  map(resp$table, \(x) {
    tibble(
      id = x$id %||% NA_character_,
      title = x$title %||% NA_character_
    )
  }) |> list_rbind()
}

#' Get O*NET Table Column Information
#'
#' Retrieves metadata about columns in a specific database table.
#'
#' @param table_id Character string specifying the table identifier
#'   (e.g., "occupation_data", "skills").
#'
#' @return A tibble with column metadata including:
#'   \describe{
#'     \item{name}{Column name}
#'     \item{type}{Column data type}
#'     \item{description}{Column description}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' info <- onet_table_info("skills")
#' }
onet_table_info <- function(table_id) {
  if (!is.character(table_id) || length(table_id) != 1) {
    cli_abort("{.arg table_id} must be a single character string.")
  }

  resp <- onet_request("database/info", table_id) |>
    onet_perform()

  schema <- list(name = character(), type = character(), description = character())

  if (is.null(resp$column) || length(resp$column) == 0) {
    return(create_empty_result(schema))
  }

  map(resp$column, \(x) {
    tibble(
      name = x$name %||% NA_character_,
      type = x$type %||% NA_character_,
      description = x$description %||% NA_character_
    )
  }) |> list_rbind()
}

#' Get O*NET Table Data
#'
#' Retrieves all rows from a database table, automatically paginating
#' through results.
#'
#' @param table_id Character string specifying the table identifier
#'   (e.g., "occupation_data", "skills").
#' @param page_size Integer specifying how many rows to fetch per request
#'   (default 2000, which is the API maximum).
#' @param show_progress Logical indicating whether to show progress messages
#'   for large tables (default TRUE).
#'
#' @return A tibble containing all rows from the table.
#'
#' @details
#' This function automatically handles pagination to retrieve all rows
#' from large tables. For very large tables, this may take some time
#' and make multiple API requests.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get all skills data
#' skills_data <- onet_table("skills")
#'
#' # Get occupation data without progress messages
#' occ_data <- onet_table("occupation_data", show_progress = FALSE)
#' }
onet_table <- function(table_id, page_size = 2000, show_progress = TRUE) {
  if (!is.character(table_id) || length(table_id) != 1) {
    cli_abort("{.arg table_id} must be a single character string.")
  }
  if (!is.numeric(page_size) || page_size < 1 || page_size > 2000) {
    cli_abort("{.arg page_size} must be between 1 and 2000.")
  }
  
  # Use the reusable pagination helper
  fetch_page <- function(start, end) {
    onet_table_page(table_id, start = start, end = end)
  }
  
  paginate_results(fetch_page, page_size = page_size, show_progress = show_progress)
}

#' Get a Single Page of O*NET Table Data
#'
#' @param table_id Table identifier.
#' @param start First row to return.
#' @param end Last row to return.
#'
#' @return A list with `data` (tibble), `start`, `end`, and `total`.
#' @keywords internal
onet_table_page <- function(table_id, start = 1, end = 2000) {
  resp <- onet_request("database/rows", table_id, start = start, end = end) |>
    onet_perform()

  data <- if (is.null(resp$row) || length(resp$row) == 0) {
    tibble()
  } else {
    map(resp$row, as_onet_tibble) |> list_rbind()
  }

  list(
    data = data,
    start = resp$start %||% start,
    end = resp$end %||% 0,
    total = resp$total %||% 0
  )
}
