# =============================================================================
# O*NET Occupations (API v2)
# =============================================================================

# ---- Public functions ---------------------------------------------------------

#' List All O*NET Occupations
#'
#' Retrieves a list of occupations in the O*NET database.
#'
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 1000).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{code}{O*NET-SOC occupation code}
#'   \item{title}{Occupation title}
#' }
#'
#' @export
onet_occupations <- function(start = 1, end = 1000) {
  resp <- onet_request("online/occupations", start = start, end = end) |>
    onet_perform()
  
  if (is.null(resp$occupation) || length(resp$occupation) == 0) {
    return(tibble::tibble(code = character(), title = character()))
  }
  
  purrr::map(resp$occupation, \(x) {
    tibble::tibble(
      code = x$code %||% NA_character_,
      title = x$title %||% NA_character_
    )
  }) |>
    purrr::list_rbind()
}

#' Get O*NET Occupation Overview
#'
#' Retrieves overview information for a specific occupation.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#' @return A list containing the occupation overview data.
#'
#' @export
onet_occupation <- function(code) {
  validate_onet_code(code)
  onet_request("online/occupations", code) |>
    onet_perform()
}

#' Get O*NET Occupation Details Index
#'
#' Retrieves the details index for a specific occupation. The response includes
#' links to available detailed sections.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#' @return A list containing the occupation details index.
#'
#' @export
onet_occupation_details <- function(code) {
  validate_onet_code(code)
  onet_request("online/occupations", code, "details") |>
    onet_perform()
}

# ---- Details: element-based sections (resp$element) ---------------------------

#' Get O*NET Occupation Skills (details)
#' @export
onet_skills <- function(code, start = 1, end = 20) {
  onet_details_element(code, "skills", start = start, end = end)
}

#' Get O*NET Occupation Knowledge (details)
#' @export
onet_knowledge <- function(code, start = 1, end = 20) {
  onet_details_element(code, "knowledge", start = start, end = end)
}

#' Get O*NET Occupation Abilities (details)
#' @export
onet_abilities <- function(code, start = 1, end = 20) {
  onet_details_element(code, "abilities", start = start, end = end)
}

#' Get O*NET Occupation Work Styles (details)
#' @export
onet_work_styles <- function(code, start = 1, end = 20) {
  onet_details_element(code, "work_styles", start = start, end = end)
}

#' Get O*NET Occupation Interests (details)
#' @export
onet_interests <- function(code, start = 1, end = 20) {
  onet_details_element(code, "interests", start = start, end = end)
}

#' Get O*NET Occupation Work Context (details)
#' @export
onet_work_context <- function(code, start = 1, end = 20) {
  onet_details_element(code, "work_context", start = start, end = end)
}

#' Get O*NET Occupation Work Activities (details)
#' @export
onet_work_activities <- function(code, start = 1, end = 20) {
  onet_details_element(code, "work_activities", start = start, end = end)
}

# ---- Details: tasks (resp$task) ----------------------------------------------

#' Get O*NET Occupation Tasks (details)
#' @export
onet_tasks <- function(code, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "tasks", start = start, end = end) |>
    onet_perform()
  
  onet_list_to_tbl(resp$task)
}

# ---- Details: detailed work activities (resp$activity) ------------------------

#' Get O*NET Detailed Work Activities (details)
#'
#' Note: this endpoint returns items under `activity` with fields like id/title/related.
#'
#' @export
onet_detailed_work_activities <- function(code, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "detailed_work_activities", start = start, end = end) |>
    onet_perform()
  
  onet_list_to_tbl(resp$activity)
}

# ---- Details: related occupations (resp$occupation) ---------------------------

#' Get O*NET Related Occupations (details)
#' @export
onet_related_occupations <- function(code, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "related_occupations", start = start, end = end) |>
    onet_perform()
  
  onet_list_to_tbl(resp$occupation)
}

# ---- Details: professional associations (resp$source) -------------------------

#' Get O*NET Professional Associations (details)
#' @export
onet_professional_associations <- function(code, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "professional_associations", start = start, end = end) |>
    onet_perform()
  
  onet_list_to_tbl(resp$source)
}

# ---- Details: apprenticeship (resp$example_title) -----------------------------

#' Get O*NET Apprenticeship Opportunities (details)
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble with one row per apprenticeship example title.
#'
#' @export
onet_apprenticeship <- function(code, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "apprenticeship", start = start, end = end) |>
    onet_perform()
  
  x <- resp$example_title
  if (is.null(x) || length(x) == 0) {
    return(tibble::tibble(example_title = character()))
  }
  
  # API returns list of strings
  tibble::tibble(example_title = unlist(x, use.names = FALSE))
}

# ---- Details: job zone (object response) --------------------------------------

#' Get O*NET Job Zone (details)
#'
#' Note: this endpoint returns a non-paged object with keys like:
#' code, title, education, related_experience, job_training, job_zone_examples, svp_range
#'
#' @export
onet_job_zone <- function(code) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "job_zone") |>
    onet_perform()
  
  # Return as a list (faithful to API), but provide convenience tibbles when possible
  resp
}

# ---- Details: education (shape currently observed as `response`) --------------

#' Get O*NET Education (details)
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{code}{Education category code}
#'   \item{title}{Education level}
#'   \item{percentage_of_respondents}{Percent of respondents reporting this level}
#' }
#'
#' @export
onet_education <- function(code, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "education", start = start, end = end) |>
    onet_perform()
  
  if (is.null(resp$response) || length(resp$response) == 0) {
    return(tibble::tibble(
      code = integer(),
      title = character(),
      percentage_of_respondents = integer()
    ))
  }
  
  onet_list_to_tbl(resp$response)
}

# ---- Hot Technologies ---------------------------------------------------------

#' Get O*NET Hot Technologies
#'
#' Retrieves hot technologies for a specific occupation from the hot technology endpoint.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble converted from the API `example` objects.
#' @rdname onet_hot_technology
#' @export
onet_technology <- function(code, start = 1, end = 20) {
  onet_hot_technology(code, start = start, end = end)
}
#' @inheritParams onet_hot_technology
#' @export
onet_technology <- function(code, start = 1, end = 20) {
  onet_hot_technology(code, start = start, end = end)
}

# ---- Technology Skills (details/technology_skills) ----------------------------

#' Get O*NET Technology Skills (details)
#'
#' Flattens the `category -> example` and optionally `example_more` structure.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#' @param start Integer specifying the first category to return (default 1).
#' @param end Integer specifying the last category to return (default 20).
#' @param include_more Logical; include `example_more` items (default FALSE).
#'
#' @return A tibble with one row per technology example, including category metadata.
#'
#' @export
onet_technology_skills <- function(code, start = 1, end = 20, include_more = FALSE) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "technology_skills", start = start, end = end) |>
    onet_perform()
  
  cats <- resp$category
  if (is.null(cats) || length(cats) == 0) {
    return(tibble::tibble())
  }
  
  purrr::map(cats, \(cat) {
    base <- tibble::tibble(
      category_code = cat$code %||% NA_integer_,
      category_title = cat$title %||% NA_character_,
      category_related = cat$related %||% NA_character_
    )
    
    ex_tbl <- NULL
    if (!is.null(cat$example) && length(cat$example) > 0) {
      ex_tbl <- onet_list_to_tbl(cat$example)
      ex_tbl$example_source <- "example"
      ex_tbl <- dplyr::bind_cols(base, ex_tbl)
    }
    
    if (!isTRUE(include_more)) return(ex_tbl)
    
    more_tbl <- NULL
    if (!is.null(cat$example_more) && length(cat$example_more) > 0) {
      more_tbl <- onet_list_to_tbl(cat$example_more)
      more_tbl$example_source <- "example_more"
      more_tbl <- dplyr::bind_cols(base, more_tbl)
    }
    
    dplyr::bind_rows(ex_tbl, more_tbl)
  }) |>
    purrr::list_rbind()
}

#' Get O*NET Hot Technologies
#'
#' Retrieves hot technologies for a specific occupation.
#'
#' @param code An O*NET-SOC occupation code (e.g., "15-1252.00").
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble with one row per technology.
#'
#' @export
onet_hot_technology <- function(code, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request(
    "online/occupations", code, "hot_technology",
    start = start, end = end
  ) |>
    onet_perform()
  
  if (is.null(resp$example) || length(resp$example) == 0) {
    return(tibble::tibble(
      title = character(),
      href = character(),
      hot_technology = logical(),
      in_demand = logical(),
      percentage = integer()
    ))
  }
  
  # Uses your existing helper that binds heterogeneous records safely
  out <- onet_list_to_tbl(resp$example)
  
  # Optional: enforce a stable column order + presence
  # (keeps your downstream consistent even if some rows omit fields)
  for (nm in c("title", "href", "hot_technology", "in_demand", "percentage")) {
    if (!nm %in% names(out)) out[[nm]] <- NA
  }
  
  out |>
    dplyr::select(title, href, hot_technology, in_demand, percentage)
}

# ---- In-demand skills (details/in_demand_skills) -----------------------------

#' Get O*NET In-Demand Skills (details)
#' @export
onet_in_demand_skills <- function(code, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", "in_demand_skills", start = start, end = end) |>
    onet_perform()
  
  key <- onet_first_list_key(resp, ignore = c("start", "end", "total", "next"))
  onet_list_to_tbl(resp[[key]])
}

# ---- Internal helpers ---------------------------------------------------------

# Internal: details element section -> tibble (fields vary by section)
onet_details_element <- function(code, section, start = 1, end = 20) {
  validate_onet_code(code)
  
  resp <- onet_request("online/occupations", code, "details", section, start = start, end = end) |>
    onet_perform()
  
  onet_list_to_tbl(resp$element)
}

# Internal: convert list of objects -> tibble with snake_case names
onet_list_to_tbl <- function(x) {
  if (is.null(x) || length(x) == 0) return(tibble::tibble())
  purrr::map(x, as_onet_tibble) |>
    purrr::list_rbind()
}

# Internal: pick the first key in a response that looks like a list of records
onet_first_list_key <- function(resp, ignore = character()) {
  keys <- setdiff(names(resp), ignore)
  for (k in keys) {
    v <- resp[[k]]
    if (is.list(v) && length(v) > 0 && is.list(v[[1]])) return(k)
  }
  keys[[1]] %||% ""
}

# Validate O*NET-SOC code format
validate_onet_code <- function(code) {
  if (!is.character(code) || length(code) != 1) {
    cli::cli_abort("{.arg code} must be a single character string.")
  }
  if (!grepl("^\\d{2}-\\d{4}(\\.\\d{2})?$", code)) {
    cli::cli_abort(c(
      "Invalid O*NET-SOC code format: {.val {code}}",
      "i" = "Expected format: XX-XXXX or XX-XXXX.XX (e.g., 15-1252 or 15-1252.00)"
    ))
  }
  invisible(code)
}
