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
#'     \item{code}{O*NET-SOC occupation code}
#'     \item{title}{Civilian occupation title}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Search by military job title
#' onet_crosswalk_military("infantry")
#'
#' # Search by military code
#' onet_crosswalk_military("11B")
#' }
onet_crosswalk_military <- function(keyword, start = 1, end = 20) {
  if (!is.character(keyword) || length(keyword) != 1) {
    cli_abort("{.arg keyword} must be a single character string.")
  }

  resp <- onet_request("online/crosswalks/military",
                       keyword = keyword, start = start, end = end) |>
    onet_perform()

  if (is.null(resp$occupation) || length(resp$occupation) == 0) {
    cli_inform("No civilian occupations found for: {.val {keyword}}")
    return(tibble(code = character(), title = character()))
  }

  map(resp$occupation, \(x) {
    tibble(
      code = x$code %||% NA_character_,
      title = x$title %||% NA_character_
    )
  }) |> list_rbind()
}

#' Map O*NET-SOC Codes Between Taxonomy Versions
#'
#' Converts occupation codes between the active O*NET-SOC taxonomy
#' and the 2010 SOC taxonomy.
#'
#' @param code An occupation code to convert.
#' @param from Source taxonomy: "active" (current O*NET-SOC) or "2010" (2010 SOC).
#' @param to Target taxonomy: "active" or "2010".
#'
#' @return A tibble with mapped occupation codes:
#'   \describe{
#'     \item{code}{Mapped occupation code in target taxonomy}
#'     \item{title}{Occupation title}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' # Map from active O*NET-SOC to 2010 SOC
#' onet_taxonomy_map("15-1252.00", from = "active", to = "2010")
#'
#' # Map from 2010 SOC to active O*NET-SOC
#' onet_taxonomy_map("15-1131.00", from = "2010", to = "active")
#' }
onet_taxonomy_map <- function(code, from = c("active", "2010"), to = c("2010", "active")) {
  from <- match.arg(from)
  to <- match.arg(to)

  if (from == to) {
    cli_abort("{.arg from} and {.arg to} must be different taxonomies.")
  }

  if (!is.character(code) || length(code) != 1) {
    cli_abort("{.arg code} must be a single character string.")
  }

  # Build the endpoint path based on direction
  endpoint <- if (from == "active") {
    paste0("taxonomy/active/2010/", code)
  } else {
    paste0("taxonomy/2010/active/", code)
  }

  resp <- onet_request(endpoint) |>
    onet_perform()

  if (is.null(resp$occupation) || length(resp$occupation) == 0) {
    cli_inform("No mapping found for code: {.val {code}}")
    return(tibble(code = character(), title = character()))
  }

  map(resp$occupation, \(x) {
    tibble(
      code = x$code %||% NA_character_,
      title = x$title %||% NA_character_
    )
  }) |> list_rbind()
}
