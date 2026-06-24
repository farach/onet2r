# =============================================================================
# Reference-SOC weights
# =============================================================================

#' Resolve Source Codes to a Reference SOC
#'
#' Maps a source occupation code column onto a reference SOC vintage using an
#' optional crosswalk. If no crosswalk is supplied, source and reference codes
#' are treated as the same SOC grain.
#'
#' @param data A data frame with source occupation codes.
#' @param code Source code column.
#' @param source_taxonomy Source taxonomy label.
#' @param reference_taxonomy Reference taxonomy label.
#' @param source_year Optional source year.
#' @param crosswalk Optional data frame with `source_code` and
#'   `reference_soc_code`, plus optional `crosswalk_weight` and `map_type`.
#'
#' @return A tibble with source and reference SOC mapping fields.
#' @export
onet_reference_soc_resolve <- function(
    data,
    code,
    source_taxonomy,
    reference_taxonomy = "2018 SOC",
    source_year = NA_integer_,
    crosswalk = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  validate_single_column(data, code, "code")
  validate_single_string(source_taxonomy, "source_taxonomy")
  validate_single_string(reference_taxonomy, "reference_taxonomy")

  sources <- tibble::tibble(source_code = unique(as.character(data[[code]])))
  if (is.null(crosswalk)) {
    mapped <- tibble::tibble(
      source_code = sources$source_code,
      reference_soc_code = standardize_soc_code(sources$source_code),
      map_type = "one_to_one",
      crosswalk_weight = 1
    )
  } else {
    mapped <- normalize_reference_crosswalk(crosswalk)
    mapped <- dplyr::inner_join(sources, mapped, by = "source_code")
  }

  mapped |>
    dplyr::mutate(
      source_taxonomy = source_taxonomy,
      source_year = as.integer(source_year),
      reference_taxonomy = reference_taxonomy,
      crosswalk_path = paste(source_taxonomy, reference_taxonomy, sep = " -> ")
    ) |>
    dplyr::select(
      "source_taxonomy",
      "source_year",
      "reference_taxonomy",
      "source_code",
      "reference_soc_code",
      "map_type",
      "crosswalk_weight",
      "crosswalk_path"
    )
}

#' Create an OEWS Weight Panel
#'
#' @param oews OEWS data with occupation and employment columns.
#' @param year OEWS estimate year.
#' @param code OEWS occupation code column.
#' @param employment Employment column.
#' @param reference_taxonomy Reference SOC label.
#' @param source_taxonomy Optional source taxonomy label.
#' @param crosswalk Optional source-to-reference crosswalk.
#'
#' @return A normalized employment-weight panel.
#' @export
onet_weight_panel_oews <- function(
    oews,
    year,
    code = "occ_code",
    employment = "tot_emp",
    reference_taxonomy = "2018 SOC",
    source_taxonomy = NULL,
    crosswalk = NULL) {
  if (!is.data.frame(oews)) {
    cli::cli_abort("{.arg oews} must be a data frame.")
  }
  validate_single_column(oews, code, "code")
  validate_single_column(oews, employment, "employment")
  if (is.null(source_taxonomy)) {
    source_taxonomy <- oews_soc_vintage(year)
  }
  resolved <- onet_reference_soc_resolve(
    oews,
    code = code,
    source_taxonomy = source_taxonomy,
    reference_taxonomy = reference_taxonomy,
    source_year = year,
    crosswalk = crosswalk
  )
  data <- tibble::as_tibble(oews)
  data$source_code <- as.character(data[[code]])
  data$.employment <- parse_oews_number(data[[employment]])

  out <- dplyr::inner_join(
    data,
    resolved,
    by = "source_code",
    relationship = "many-to-many"
  )
  out$employment <- out$.employment * out$crosswalk_weight
  weight_panel_summarise(out, year, "OEWS")
}

#' Create a PUMS Weight Panel
#'
#' @param pums PUMS microdata or already-filtered person records.
#' @param year ACS or CPS year.
#' @param socp Occupation column, commonly `SOCP`.
#' @param weight Person weight column, commonly `PWGTP`.
#' @param group Optional columns for demographic or geographic cells.
#' @param replicate_weights Optional replicate-weight columns.
#' @param reference_taxonomy Reference SOC label.
#' @param source_taxonomy Optional source taxonomy label.
#' @param crosswalk Optional source-to-reference crosswalk.
#'
#' @return A normalized employment-weight panel.
#' @export
onet_weight_panel_pums <- function(
    pums,
    year,
    socp = "SOCP",
    weight = "PWGTP",
    group = NULL,
    replicate_weights = NULL,
    reference_taxonomy = "2018 SOC",
    source_taxonomy = NULL,
    crosswalk = NULL) {
  if (!is.data.frame(pums)) {
    cli::cli_abort("{.arg pums} must be a data frame.")
  }
  validate_single_column(pums, socp, "socp")
  validate_single_column(pums, weight, "weight")
  if (!is.null(group)) {
    validate_character_columns(pums, group, "group")
  }
  if (!is.null(replicate_weights)) {
    validate_character_columns(pums, replicate_weights, "replicate_weights")
  }
  if (is.null(source_taxonomy)) {
    source_taxonomy <- pums_soc_vintage(year)
  }

  resolved <- onet_reference_soc_resolve(
    pums,
    code = socp,
    source_taxonomy = source_taxonomy,
    reference_taxonomy = reference_taxonomy,
    source_year = year,
    crosswalk = crosswalk
  )
  data <- tibble::as_tibble(pums)
  data$source_code <- as.character(data[[socp]])
  data$.employment <- parse_oews_number(data[[weight]])
  joined <- dplyr::inner_join(
    data,
    resolved,
    by = "source_code",
    relationship = "many-to-many"
  )
  joined$employment <- joined$.employment * joined$crosswalk_weight
  out <- weight_panel_summarise(joined, year, "PUMS", group = group)
  if (!is.null(replicate_weights)) {
    out <- add_pums_replicate_se(out, joined, replicate_weights, group)
  }
  out
}

validate_weight_panel <- function(weight_panel) {
  required <- c(
    "reference_soc_code", "year", "employment", "weight_share",
    "source", "source_taxonomy", "reference_taxonomy"
  )
  validate_columns_present(weight_panel, required, "weight_panel")
}

weight_panel_summarise <- function(data, year, source, group = NULL) {
  group_cols <- c("reference_soc_code", group)
  out <- data |>
    dplyr::summarise(
      employment = sum(.data$employment, na.rm = TRUE),
      source_taxonomy = dplyr::first(.data$source_taxonomy),
      reference_taxonomy = dplyr::first(.data$reference_taxonomy),
      .by = dplyr::all_of(group_cols)
    )
  total <- sum(out$employment, na.rm = TRUE)
  out$year <- as.integer(year)
  out$source <- source
  out$weight_share <- if (total > 0) out$employment / total else NA_real_
  out |>
    dplyr::select(
      "reference_soc_code",
      dplyr::all_of(group),
      "year",
      "employment",
      "weight_share",
      "source",
      "source_taxonomy",
      "reference_taxonomy"
    ) |>
    dplyr::arrange(.data$reference_soc_code)
}

normalize_reference_crosswalk <- function(crosswalk) {
  if (!is.data.frame(crosswalk)) {
    cli::cli_abort("{.arg crosswalk} must be a data frame.")
  }
  validate_single_column(crosswalk, "source_code", "source_code")
  validate_single_column(crosswalk, "reference_soc_code", "reference_soc_code")
  out <- tibble::as_tibble(crosswalk)
  if (!"crosswalk_weight" %in% names(out)) {
    out$crosswalk_weight <- 1
  }
  if (!"map_type" %in% names(out)) {
    out$map_type <- "one_to_one"
  }
  out$source_code <- as.character(out$source_code)
  out$reference_soc_code <- standardize_soc_code(out$reference_soc_code)
  out$crosswalk_weight <- parse_onet_number(out$crosswalk_weight)
  out[c("source_code", "reference_soc_code", "map_type", "crosswalk_weight")]
}

oews_soc_vintage <- function(year) {
  year <- as.integer(year)
  dplyr::case_when(
    year >= 2021 ~ "2018 SOC",
    year >= 2019 ~ "2010/2018 SOC hybrid",
    year >= 2012 ~ "2010 SOC",
    year >= 2010 ~ "2000/2010 SOC hybrid",
    TRUE ~ "2000 SOC"
  )
}

pums_soc_vintage <- function(year) {
  year <- as.integer(year)
  if (year >= 2018) "2018 SOC" else "2010 SOC"
}

add_pums_replicate_se <- function(out, joined, replicate_weights, group) {
  key_cols <- c("reference_soc_code", group)
  scale <- 4 / length(replicate_weights)
  rep_totals <- purrr::map(replicate_weights, function(rep_col) {
    data <- joined
    data$employment <- parse_oews_number(data[[rep_col]]) * data$crosswalk_weight
    data |>
      dplyr::summarise(
        replicate_employment = sum(.data$employment, na.rm = TRUE),
        .by = dplyr::all_of(key_cols)
      )
  })
  rep_data <- purrr::list_rbind(rep_totals, names_to = "replicate")
  se <- dplyr::left_join(
    rep_data,
    out[c(key_cols, "employment")],
    by = key_cols,
    relationship = "many-to-one"
  ) |>
    dplyr::summarise(
      employment_se = sqrt(scale * sum((.data$replicate_employment - .data$employment)^2)),
      .by = dplyr::all_of(key_cols)
    )
  dplyr::left_join(out, se, by = key_cols, relationship = "one-to-one")
}
