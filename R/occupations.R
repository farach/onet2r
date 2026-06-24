# =============================================================================
# O*NET Occupations (API v2)
# =============================================================================

# ---- Public functions ---------------------------------------------------------

#' List a Page of O&#42;NET Occupations
#'
#' Retrieves a single page of occupations in the O&#42;NET database.
#'
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 1000).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{code}{O&#42;NET-SOC occupation code}
#'   \item{title}{Occupation title}
#' }
#'
#' @details
#' Use [onet_occupations_all()] to automatically paginate through the full
#' occupation list.
#'
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_occupations(start = 1, end = 5)
#'
#' @export
onet_occupations <- function(start = 1, end = 1000) {
  onet_occupations_page(start = start, end = end)$data
}

#' List All O&#42;NET Occupations with Auto-Pagination
#'
#' Retrieves all occupations in the O&#42;NET database by automatically paginating
#' through the results.
#'
#' @param page_size Integer specifying how many rows to fetch per request
#'   (default 2000, which is the API maximum).
#' @param show_progress Logical indicating whether to show progress messages
#'   for pagination (default TRUE).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{code}{O&#42;NET-SOC occupation code}
#'   \item{title}{Occupation title}
#' }
#'
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_occupations_all(show_progress = FALSE)
#'
#' @export
onet_occupations_all <- function(page_size = 2000, show_progress = TRUE) {
  validate_page_size(page_size)

  paginate_api(
    fetch_page = function(start, end) {
      onet_occupations_page(start = start, end = end)
    },
    page_size = page_size,
    show_progress = show_progress
  )
}

onet_occupations_page <- function(start = 1, end = 1000) {
  resp <- onet_request(
    "online/occupations",
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  list(
    data = occupation_records_to_tbl(resp$occupation),
    start = resp$start %||% start,
    end = resp$end %||% 0,
    total = resp$total %||% 0
  )
}

#' Get O&#42;NET Occupation Overview
#'
#' Retrieves overview information for a specific occupation.
#'
#' @param code An O&#42;NET-SOC occupation code (e.g., "15-1252.00").
#' @return A list containing the occupation overview data.
#'
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_occupation("15-1252.00")
#'
#' @export
onet_occupation <- function(code) {
  validate_onet_code(code)

  onet_request(
    "online/occupations",
    .path_segments = c(code)
  ) |>
    onet_perform()
}

#' Get O&#42;NET Occupation Details Index
#'
#' Retrieves the details index for a specific occupation. The response includes
#' links to available detailed sections from the occupation overview.
#'
#' @param code An O&#42;NET-SOC occupation code (e.g., "15-1252.00").
#' @return A list containing the available occupation details sections.
#'
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_occupation_details("15-1252.00")
#'
#' @export
onet_occupation_details <- function(code) {
  validate_onet_code(code)

  details <- onet_occupation(code)$details_contents
  if (is.null(details)) {
    return(list())
  }

  details
}

# ---- Details: element-based sections (resp$element) ---------------------------

#' Get O&#42;NET Occupation Skills (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of skills.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_skills("15-1252.00", end = 5)
#' @export
onet_skills <- function(code, start = 1, end = 20) {
  onet_details_element(code, "skills", start = start, end = end)
}

#' Get All O&#42;NET Occupation Skills (details)
#'
#' Retrieves all skill rows for a single occupation by automatically
#' paginating through the endpoint.
#'
#' @param code An O&#42;NET-SOC occupation code.
#' @param page_size Integer specifying how many rows to fetch per request
#'   (default 2000, which is the API maximum).
#' @param show_progress Logical indicating whether to show progress messages
#'   for pagination (default TRUE).
#'
#' @return A tibble of skills.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_skills_all("15-1252.00", show_progress = FALSE)
#' @export
onet_skills_all <- function(code, page_size = 2000, show_progress = TRUE) {
  validate_page_size(page_size)

  onet_details_element_all(
    code = code,
    section = "skills",
    page_size = page_size,
    show_progress = show_progress
  )
}

#' Get O&#42;NET Occupation Knowledge (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of knowledge elements.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_knowledge("15-1252.00", end = 5)
#' @export
onet_knowledge <- function(code, start = 1, end = 20) {
  onet_details_element(code, "knowledge", start = start, end = end)
}

#' Get O&#42;NET Occupation Abilities (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of ability elements.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_abilities("15-1252.00", end = 5)
#' @export
onet_abilities <- function(code, start = 1, end = 20) {
  onet_details_element(code, "abilities", start = start, end = end)
}

#' Get O&#42;NET Occupation Work Styles (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of work style elements.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_work_styles("15-1252.00", end = 5)
#' @export
onet_work_styles <- function(code, start = 1, end = 20) {
  onet_details_element(code, "work_styles", start = start, end = end)
}

#' Get O&#42;NET Occupation Interests (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of interest elements.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_interests("15-1252.00", end = 5)
#' @export
onet_interests <- function(code, start = 1, end = 20) {
  onet_details_element(code, "interests", start = start, end = end)
}

#' Get O&#42;NET Occupation Work Context (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of work context elements.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_work_context("15-1252.00", end = 5)
#' @export
onet_work_context <- function(code, start = 1, end = 20) {
  onet_details_element(code, "work_context", start = start, end = end)
}

#' Get All O&#42;NET Occupation Work Context (details)
#'
#' Retrieves all work context rows for a single occupation by automatically
#' paginating through the endpoint.
#'
#' @param code An O&#42;NET-SOC occupation code.
#' @param page_size Integer specifying how many rows to fetch per request
#'   (default 2000, which is the API maximum).
#' @param show_progress Logical indicating whether to show progress messages
#'   for pagination (default TRUE).
#'
#' @return A tibble of work context elements.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_work_context_all("15-1252.00", show_progress = FALSE)
#' @export
onet_work_context_all <- function(code, page_size = 2000, show_progress = TRUE) {
  validate_page_size(page_size)

  onet_details_element_all(
    code = code,
    section = "work_context",
    page_size = page_size,
    show_progress = show_progress
  )
}

#' Get O&#42;NET Occupation Work Activities (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of work activities.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_work_activities("15-1252.00", end = 5)
#' @export
onet_work_activities <- function(code, start = 1, end = 20) {
  onet_details_element(code, "work_activities", start = start, end = end)
}

#' Get All O&#42;NET Occupation Work Activities (details)
#'
#' Retrieves all work activity rows for a single occupation by automatically
#' paginating through the endpoint.
#'
#' @param code An O&#42;NET-SOC occupation code.
#' @param page_size Integer specifying how many rows to fetch per request
#'   (default 2000, which is the API maximum).
#' @param show_progress Logical indicating whether to show progress messages
#'   for pagination (default TRUE).
#'
#' @return A tibble of work activities.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_work_activities_all("15-1252.00", show_progress = FALSE)
#' @export
onet_work_activities_all <- function(code, page_size = 2000, show_progress = TRUE) {
  validate_page_size(page_size)

  onet_details_element_all(
    code = code,
    section = "work_activities",
    page_size = page_size,
    show_progress = show_progress
  )
}

# ---- Details: tasks (resp$task) ----------------------------------------------

#' Get O&#42;NET Occupation Tasks (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of tasks.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_tasks("15-1252.00", end = 5)
#' @export
onet_tasks <- function(code, start = 1, end = 20) {
  validate_onet_code(code)

  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "details", "tasks"),
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  onet_list_to_tbl(resp$task)
}

# ---- Details: detailed work activities (resp$activity) ------------------------

#' Get O&#42;NET Detailed Work Activities (details)
#'
#' Note: this endpoint returns items under `activity` with fields like id/title/related.
#'
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of detailed work activities.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_detailed_work_activities("15-1252.00", end = 5)
#' @export
onet_detailed_work_activities <- function(code, start = 1, end = 20) {
  validate_onet_code(code)

  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "details", "detailed_work_activities"),
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  onet_list_to_tbl(resp$activity)
}

# ---- Details: related occupations (resp$occupation) ---------------------------

#' Get O&#42;NET Related Occupations (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of related occupations.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_related_occupations("15-1252.00", end = 5)
#' @export
onet_related_occupations <- function(code, start = 1, end = 20) {
  validate_onet_code(code)

  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "details", "related_occupations"),
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  onet_list_to_tbl(resp$occupation)
}

# ---- Details: professional associations (resp$source) -------------------------

#' Get O&#42;NET Professional Associations (details)
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of professional associations.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_professional_associations("15-1252.00", end = 5)
#' @export
onet_professional_associations <- function(code, start = 1, end = 20) {
  validate_onet_code(code)

  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "details", "professional_associations"),
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  onet_list_to_tbl(resp$source)
}

# ---- Details: apprenticeship (resp$example_title) -----------------------------

#' Get O&#42;NET Apprenticeship Opportunities (details)
#'
#' @param code An O&#42;NET-SOC occupation code (e.g., "15-1252.00").
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble with one row per apprenticeship example title.
#'
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_apprenticeship("15-1252.00", end = 5)
#'
#' @export
onet_apprenticeship <- function(code, start = 1, end = 20) {
  validate_onet_code(code)

  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "details", "apprenticeship"),
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  x <- resp$example_title
  if (is.null(x) || length(x) == 0) {
    return(tibble::tibble(example_title = character()))
  }

  tibble::tibble(example_title = unlist(x, use.names = FALSE))
}

# ---- Details: job zone (object response) --------------------------------------

#' Get O&#42;NET Job Zone (details)
#'
#' Note: this endpoint returns a non-paged object with keys like:
#' code, title, education, related_experience, job_training, job_zone_examples, svp_range
#'
#' @param code An O&#42;NET-SOC occupation code.
#' @return A list (faithful to API response).
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_job_zone("15-1252.00")
#' @export
onet_job_zone <- function(code) {
  validate_onet_code(code)

  onet_request(
    "online/occupations",
    .path_segments = c(code, "details", "job_zone")
  ) |>
    onet_perform()
}

# ---- Details: education (resp$response) ---------------------------------------

#' Get O&#42;NET Education (details)
#'
#' @param code An O&#42;NET-SOC occupation code (e.g., "15-1252.00").
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
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_education("15-1252.00")
#'
#' @export
onet_education <- function(code, start = 1, end = 20) {
  validate_onet_code(code)

  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "details", "education"),
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  schema <- tibble::tibble(
    code = integer(),
    title = character(),
    percentage_of_respondents = numeric()
  )

  if (is.null(resp$response) || length(resp$response) == 0) {
    return(schema)
  }

  onet_list_to_tbl(resp$response)
}

# ---- Hot technology endpoint (/hot_technology) --------------------------------

#' Get O&#42;NET Hot Technologies
#'
#' Retrieves hot technologies for a specific occupation from the hot technology endpoint.
#'
#' @param code An O&#42;NET-SOC occupation code (e.g., "15-1252.00").
#' @param start Integer specifying the first result to return (default 1).
#' @param end Integer specifying the last result to return (default 20).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{title}{Technology name}
#'   \item{href}{Technology details link}
#'   \item{hot_technology}{Logical indicating hot technology}
#'   \item{in_demand}{Logical indicating in-demand}
#'   \item{percentage}{Percent of postings/mentions as defined by API}
#' }
#'
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_hot_technology("15-1252.00", end = 5)
#'
#' @export
onet_hot_technology <- function(code, start = 1, end = 20) {
  validate_onet_code(code)

  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "hot_technology"),
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  schema <- tibble::tibble(
    title = character(),
    href = character(),
    hot_technology = logical(),
    in_demand = logical(),
    percentage = numeric()
  )

  if (is.null(resp$example) || length(resp$example) == 0) {
    return(schema)
  }

  out <- onet_list_to_tbl(resp$example)

  # Enforce stable columns (API objects can be heterogeneous)
  for (nm in names(schema)) {
    if (!nm %in% names(out)) out[[nm]] <- NA
  }

  out |>
    dplyr::select(title, href, hot_technology, in_demand, percentage)
}

#' Get O&#42;NET Technology (alias of hot technologies)
#'
#' @rdname onet_hot_technology
#' @export
onet_technology <- function(code, start = 1, end = 20) {
  onet_hot_technology(code, start = start, end = end)
}

# ---- Technology Skills (details/technology_skills) ----------------------------

#' Get O&#42;NET Technology Skills (details)
#'
#' Flattens the `category -> example` and optionally `example_more` structure.
#'
#' @param code An O&#42;NET-SOC occupation code (e.g., "15-1252.00").
#' @param start Integer specifying the first category to return (default 1).
#' @param end Integer specifying the last category to return (default 20).
#' @param include_more Logical; include `example_more` items (default FALSE).
#'
#' @return A tibble with one row per technology example, including category metadata.
#'
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_technology_skills("15-1252.00", end = 3)
#'
#' @export
onet_technology_skills <- function(code, start = 1, end = 20, include_more = FALSE) {
  validate_onet_code(code)

  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "details", "technology_skills"),
    .query = list(start = start, end = end)
  ) |>
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

    if (!isTRUE(include_more)) {
      return(ex_tbl)
    }

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

# ---- In-demand skills (details/in_demand_skills) -----------------------------

#' Get O&#42;NET In-Demand Skills (details)
#'
#' Retrieves in-demand technology records for an occupation. The O&#42;NET details
#' index advertises an `in_demand` link for some occupations, but the endpoint
#' may return 404; this helper uses the working hot-technology endpoint and
#' filters to rows where `in_demand` is `TRUE`.
#'
#' @param code An O&#42;NET-SOC occupation code.
#' @param start Integer specifying the first result to return.
#' @param end Integer specifying the last result to return.
#' @return A tibble of in-demand skills records.
#' @examplesIf nzchar(Sys.getenv("ONET_API_KEY"))
#' onet_in_demand_skills("15-1252.00", end = 5)
#' @export
onet_in_demand_skills <- function(code, start = 1, end = 20) {
  out <- onet_hot_technology(code, start = start, end = end)
  if (!"in_demand" %in% names(out)) {
    return(out)
  }

  out |>
    dplyr::filter(isTRUE(.data$in_demand) | .data$in_demand %in% TRUE)
}

# ---- Internal helpers ---------------------------------------------------------

# Internal: details element section -> tibble (usually resp$element)
onet_details_element <- function(code, section, start = 1, end = 20) {
  validate_onet_code(code)

  onet_details_element_page(
    code = code,
    section = section,
    start = start,
    end = end
  )$data
}

onet_details_element_all <- function(code, section, page_size = 2000, show_progress = TRUE) {
  validate_onet_code(code)

  paginate_api(
    fetch_page = function(start, end) {
      onet_details_element_page(
        code = code,
        section = section,
        start = start,
        end = end
      )
    },
    page_size = page_size,
    show_progress = show_progress
  )
}

onet_details_element_page <- function(code, section, start = 1, end = 20) {
  resp <- onet_request(
    "online/occupations",
    .path_segments = c(code, "details", section),
    .query = list(start = start, end = end)
  ) |>
    onet_perform()

  list(
    data = onet_list_to_tbl(resp$element),
    start = resp$start %||% start,
    end = resp$end %||% 0,
    total = resp$total %||% 0
  )
}

validate_page_size <- function(page_size) {
  if (!is.numeric(page_size) || length(page_size) != 1 || is.na(page_size) ||
    page_size < 1 || page_size > 2000) {
    cli::cli_abort("{.arg page_size} must be between 1 and 2000.")
  }

  invisible(page_size)
}

# Internal: convert list of objects -> tibble with snake_case names
onet_list_to_tbl <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(tibble::tibble())
  }
  purrr::map(x, as_onet_tibble) |>
    purrr::list_rbind()
}

# Internal: pick the first key in a response that looks like a list of records
onet_first_list_key <- function(resp, ignore = character()) {
  keys <- setdiff(names(resp), ignore)
  for (k in keys) {
    v <- resp[[k]]
    if (is.list(v) && length(v) > 0 && is.list(v[[1]])) {
      return(k)
    }
  }
  keys[[1]] %||% ""
}

# Validate O*NET-SOC code format
validate_onet_code <- function(code) {
  validate_single_string(code, "code")
  if (!grepl("^\\d{2}-\\d{4}(\\.\\d{2})?$", code)) {
    cli::cli_abort(c(
      "Invalid O*NET-SOC code format: {.val {code}}",
      "i" = "Expected format: XX-XXXX or XX-XXXX.XX (e.g., 15-1252 or 15-1252.00)"
    ))
  }
  invisible(code)
}
