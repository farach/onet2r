#' List All O*NET Occupations
#'
#' Retrieves a list of all occupations in the O*NET database.
#'
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 1000).
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{code}{O*NET-SOC occupation code}
#'     \item{title}{Occupation title}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' occupations <- onet_occupations()
#' head(occupations)
#' }
onet_occupations <- function(start = 1, end = 1000) {
  resp <- onet_request("online/occupations", .query = list(start = start, end = end)) |>
    onet_perform()

  # Define expected schema
  schema <- empty_tibble(code = character(), title = character())

  if (is.null(resp$occupation) || length(resp$occupation) == 0) {
    return(schema)
  }

  map(resp$occupation, \(x) {
    tibble(
      code = x$code %||% NA_character_,
      title = x$title %||% NA_character_
    )
  }) |> list_rbind()
}

#' Get O*NET Occupation Summary
#'
#' Retrieves summary information for a specific occupation.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#'
#' @return A list containing the occupation summary data.
#'
#' @export
#' @examples
#' \dontrun{
#' summary <- onet_occupation("15-1252.00")
#' }
onet_occupation <- function(code) {
  validate_onet_code(code)
  resp <- onet_request("online/occupations", .path_segments = c(code, "summary")) |>
    onet_perform()
  resp
}

#' Get Detailed O*NET Occupation Report
#'
#' Retrieves the full detailed report for a specific occupation.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#'
#' @return A list containing the full occupation details.
#'
#' @export
#' @examples
#' \dontrun{
#' details <- onet_occupation_details("15-1252.00")
#' }
onet_occupation_details <- function(code) {
  validate_onet_code(code)
  resp <- onet_request("online/occupations", .path_segments = c(code, "details")) |>
    onet_perform()
  resp
}

#' Get O*NET Occupation Skills
#'
#' Retrieves skills data for a specific occupation.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#'
#' @return A tibble with skill data including:
#'   \describe{
#'     \item{id}{Skill element ID}
#'     \item{name}{Skill name}
#'     \item{description}{Skill description}
#'     \item{value}{Importance or level value}
#'     \item{scale}{Scale type (importance/level)}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' skills <- onet_skills("15-1252.00")
#' }
onet_skills <- function(code) {
  onet_occupation_element(code, "skills")
}

#' Get O*NET Occupation Knowledge
#'
#' Retrieves knowledge areas for a specific occupation.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#'
#' @return A tibble with knowledge data.
#'
#' @export
#' @examples
#' \dontrun{
#' knowledge <- onet_knowledge("15-1252.00")
#' }
onet_knowledge <- function(code) {
  onet_occupation_element(code, "knowledge")
}

#' Get O*NET Occupation Abilities
#'
#' Retrieves abilities for a specific occupation.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#'
#' @return A tibble with abilities data.
#'
#' @export
#' @examples
#' \dontrun{
#' abilities <- onet_abilities("15-1252.00")
#' }
onet_abilities <- function(code) {
  onet_occupation_element(code, "abilities")
}

#' Get O*NET Occupation Technology Skills
#'
#' Retrieves hot technology skills for a specific occupation.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#'
#' @return A tibble with technology skills data:
#'   \describe{
#'     \item{category}{Technology category}
#'     \item{title}{Technology name}
#'     \item{hot_technology}{Logical indicating if it's a hot technology}
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' tech <- onet_technology("15-1252.00")
#' }
onet_technology <- function(code) {
  validate_onet_code(code)
  resp <- onet_request("online/occupations", .path_segments = c(code, "hot_technology")) |>
    onet_perform()

  # Define expected schema
  schema <- empty_tibble(
    category = character(),
    title = character(),
    hot_technology = logical()
  )

  if (is.null(resp$category) || length(resp$category) == 0) {
    return(schema)
  }

  # Flatten the nested category -> example structure
  results <- map(resp$category, \(cat) {
    if (is.null(cat$example) || length(cat$example) == 0) {
      return(NULL)
    }
    map(cat$example, \(ex) {
      tibble(
        category = cat$title$name %||% NA_character_,
        title = ex$name %||% NA_character_,
        hot_technology = ex$hot_technology %||% NA
      )
    }) |> list_rbind()
  }) |> list_rbind()

  if (is.null(results) || nrow(results) == 0) {
    return(schema)
  }
  
  results
}

# Internal helper to fetch occupation elements (skills, knowledge, abilities)
onet_occupation_element <- function(code, element) {
  validate_onet_code(code)

  # Get summary which contains all elements
  resp <- onet_request("online/occupations", .path_segments = c(code, "summary")) |>
    onet_perform()

  # Define expected schema
  schema <- empty_tibble(
    id = character(),
    name = character(),
    description = character(),
    value = numeric(),
    scale = character()
  )

  data <- resp[[element]]
  if (is.null(data) || length(data$element) == 0) {
    return(schema)
  }

  map(data$element, \(x) {
    tibble(
      id = x$id %||% NA_character_,
      name = x$name %||% NA_character_,
      description = x$description %||% NA_character_,
      value = x$score$value %||% NA_real_,
      scale = x$score$scale %||% NA_character_
    )
  }) |> list_rbind()
}

# Validate O*NET-SOC code format
validate_onet_code <- function(code) {
  if (!is.character(code) || length(code) != 1) {
    cli_abort("{.arg code} must be a single character string.")
  }
  if (!grepl("^\\d{2}-\\d{4}(\\.\\d{2})?$", code)) {
    cli_abort(c(
      "Invalid O*NET-SOC code format: {.val {code}}",
      "i" = "Expected format: XX-XXXX or XX-XXXX.XX (e.g., 15-1252 or 15-1252.00)"
    ))
  }
  invisible(code)
}
