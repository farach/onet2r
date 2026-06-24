#' Military to Civilian Occupation Crosswalk
#'
#' Searches for civilian occupations that match military job titles or codes.
#'
#' @param keyword A character string containing a military job title or code.
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble with matching civilian occupations:
#'   \describe{
#'     \item{code}{O&#42;NET-SOC occupation code}
#'     \item{title}{Civilian occupation title}
#'   }
#'
#' @export
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' # Search by military job title
#' onet_crosswalk_military("infantry")
#'
#' # Search by military code
#' onet_crosswalk_military("11B")
onet_crosswalk_military <- function(keyword, start = 1, end = 20) {
  validate_single_string(keyword, "keyword")

  resp <- onet_request("online/crosswalks/military",
    .query = list(keyword = keyword, start = start, end = end)
  ) |>
    onet_perform()

  if (is.null(resp$occupation) || length(resp$occupation) == 0) {
    cli_inform("No civilian occupations found for: {.val {keyword}}")
    return(occupation_schema())
  }

  occupation_records_to_tbl(resp$occupation)
}

#' Map O&#42;NET-SOC Codes Between Taxonomy Versions
#'
#' Converts occupation codes between the active O&#42;NET-SOC taxonomy
#' and the 2010 SOC taxonomy.
#'
#' @param code An occupation code to convert.
#' @param from Source taxonomy: "active" (current O&#42;NET-SOC) or "2010" (2010 SOC).
#' @param to Target taxonomy: "active" or "2010".
#'
#' @return A tibble with mapped occupation codes:
#'   \describe{
#'     \item{code}{Mapped occupation code in target taxonomy}
#'     \item{title}{Occupation title}
#'   }
#'
#' @export
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' # Map from active O*NET-SOC to 2010 SOC
#' onet_taxonomy_map("15-1252.00", from = "active", to = "2010")
#'
#' # Map from 2010 SOC to active O*NET-SOC
#' onet_taxonomy_map("15-1131.00", from = "2010", to = "active")
onet_taxonomy_map <- function(code, from = c("active", "2010"), to = c("2010", "active")) {
  from <- match.arg(from)
  to <- match.arg(to)

  if (from == to) {
    cli_abort("{.arg from} and {.arg to} must be different taxonomies.")
  }

  validate_single_string(code, "code")

  # Build the endpoint path based on direction
  if (from == "active") {
    resp <- onet_request("taxonomy/active/2010", .path_segments = code) |>
      onet_perform()
  } else {
    resp <- onet_request("taxonomy/2010/active", .path_segments = code) |>
      onet_perform()
  }

  if (is.null(resp$occupation) || length(resp$occupation) == 0) {
    cli_inform("No mapping found for code: {.val {code}}")
    return(occupation_schema())
  }

  occupation_records_to_tbl(resp$occupation)
}
