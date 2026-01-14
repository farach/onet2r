#' Search O*NET Occupations
#'
#' Searches O*NET occupations by keyword or O*NET-SOC code.
#'
#' @param keyword A character string containing the search term or O*NET-SOC code.
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{O*NET-SOC occupation code}
#'     \item{title}{Occupation title}
#'     \item{relevance_score}{Search relevance score (if keyword search)}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Search by keyword
#' onet_search("software developer")
#'
#' # Search by SOC code
#' onet_search("15-1252")
#' }
onet_search <- function(keyword, start = 1, end = 20) {
  if (!is.character(keyword) || length(keyword) != 1) {
    cli_abort("{.arg keyword} must be a single character string.")
  }

  resp <- onet_request("online/search", keyword = keyword, start = start, end = end) |>
    onet_perform()

  # Define expected schema for empty results
  schema <- empty_tibble(
    code = character(),
    title = character(),
    relevance_score = numeric()
  )

  # Extract data with schema
  if (is.null(resp$occupation) || length(resp$occupation) == 0) {
    cli_inform("No occupations found for keyword: {.val {keyword}}")
    return(schema)
  }

  results <- map(resp$occupation, \(x) {
    tibble(
      code = x$code %||% NA_character_,
      title = x$title %||% NA_character_,
      relevance_score = x$relevance_score %||% NA_real_
    )
  }) |> list_rbind()

  results
}
