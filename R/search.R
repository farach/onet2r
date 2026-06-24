#' Search O\*NET Occupations
#'
#' Searches O\*NET occupations by keyword or O\*NET-SOC code.
#'
#' @param keyword A character string containing the search term or O\*NET-SOC code.
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{O\*NET-SOC occupation code}
#'     \item{title}{Occupation title}
#'   }
#'
#' @export
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' # Search by keyword
#' onet_search("software developer")
#'
#' # Search by SOC code
#' onet_search("15-1252")
onet_search <- function(keyword, start = 1, end = 20) {
  validate_single_string(keyword, "keyword")

  resp <- onet_request("online/search", .query = list(keyword = keyword, start = start, end = end)) |>
    onet_perform()

  if (is.null(resp$occupation) || length(resp$occupation) == 0) {
    cli_inform("No occupations found for keyword: {.val {keyword}}")
    return(occupation_schema())
  }

  occupation_records_to_tbl(resp$occupation)
}
