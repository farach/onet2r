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
#'
#' @examples
#' jobs <- tibble::tibble(occ_code = c("15-1252", "29-1141"))
#' onet_reference_soc_resolve(
#'   jobs,
#'   code = "occ_code",
#'   source_taxonomy = "2018 SOC"
#' )
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
    same_taxonomy <- identical(source_taxonomy, reference_taxonomy)
    if (!same_taxonomy) {
      cli::cli_warn(
        c(
          "No crosswalk was supplied between source and reference taxonomies.",
          "i" = "{.arg source_taxonomy} is {.val {source_taxonomy}} and {.arg reference_taxonomy} is {.val {reference_taxonomy}}.",
          "i" = "Reference codes will be standardized but marked as unbridged."
        )
      )
    }
    mapped <- tibble::tibble(
      source_code = sources$source_code,
      reference_soc_code = standardize_soc_code(sources$source_code),
      map_type = if (same_taxonomy) "one_to_one" else "unbridged",
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
#' @param o_group Optional OEWS aggregation-level column. When present, rows are
#'   filtered to detailed occupations by default.
#' @param reference_taxonomy Reference SOC label.
#' @param source_taxonomy Optional source taxonomy label.
#' @param crosswalk Optional source-to-reference crosswalk.
#'
#' @return A normalized employment-weight panel.
#'
#' @details
#' Real OEWS files include total, major, minor, broad, and detailed occupation
#' rows. When `o_group` is present, this function keeps detailed rows before
#' computing employment shares. If `o_group` is absent, obvious aggregate SOC
#' rows such as `00-0000` and codes ending in `-0000` are dropped with a
#' warning. `weight_share` is computed within the returned panel.
#' @export
#'
#' @examples
#' oews <- tibble::tibble(
#'   occ_code = c("15-1252", "29-1141"),
#'   tot_emp = c(100, 300)
#' )
#' onet_weight_panel_oews(oews, year = 2024)
onet_weight_panel_oews <- function(
    oews,
    year,
    code = "occ_code",
    employment = "tot_emp",
    o_group = "o_group",
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
  data <- tibble::as_tibble(oews)
  data <- filter_oews_detailed_rows(data, code = code, o_group = o_group)
  warn_missing_employment(data[[employment]], "OEWS")
  data$.employment <- parse_oews_number(data[[employment]])
  data <- data[!is.na(data$.employment), , drop = FALSE]

  resolved <- onet_reference_soc_resolve(
    data,
    code = code,
    source_taxonomy = source_taxonomy,
    reference_taxonomy = reference_taxonomy,
    source_year = year,
    crosswalk = crosswalk
  )
  data$source_code <- as.character(data[[code]])

  out <- dplyr::inner_join(
    data,
    resolved,
    by = "source_code",
    relationship = "many-to-many"
  )
  out$employment <- out$.employment * out$crosswalk_weight
  out <- drop_invalid_reference_soc(out, source = "OEWS")
  weight_panel_summarise(out, year, "OEWS")
}

#' Create a PUMS Weight Panel
#'
#' @param pums ACS PUMS microdata or already-filtered person records.
#' @param year ACS PUMS data year.
#' @param socp Occupation column, commonly `SOCP`. Do not pass Census `OCCP`;
#'   it is not an SOC field.
#' @param weight Person weight column, commonly `PWGTP`.
#' @param group Optional columns for demographic or geographic cells.
#' @param replicate_weights Optional replicate-weight columns.
#' @param reference_taxonomy Reference SOC label.
#' @param source_taxonomy Optional source taxonomy label.
#' @param crosswalk Optional source-to-reference crosswalk.
#'
#' @return A normalized employment-weight panel. When `group` is supplied,
#'   `weight_share` is computed within each group cell.
#'
#' @details
#' `onet_weight_panel_pums()` expects ACS PUMS records that have already been
#' filtered to the employment universe used by the analysis. For employment
#' weights, a common ACS starting point is employed civilians,
#' `ESR %in% c(1, 2)`, often restricted to age 16 or older. Use `SOCP` as the
#' occupation field. `OCCP` is a Census occupation recode, not an SOC field.
#'
#' ACS `SOCP` can include aggregate codes with trailing `X` characters. Those
#' rows cannot be matched directly to O&#42;NET or OEWS SOC codes, so they are
#' dropped with a warning unless the caller supplies a crosswalk that maps them
#' to valid reference SOC codes.
#'
#' Replicate-weight standard errors use `sqrt((4 / R) * sum((theta_r -
#' theta)^2))`. For ACS PUMS, pass the full 80 `PWGTP1` through `PWGTP80`
#' columns. Passing a smaller subset produces a warning because the resulting
#' standard error is not survey-valid.
#' @export
#'
#' @examples
#' pums <- tibble::tibble(
#'   SOCP = c("151252", "151252", "291141"),
#'   PWGTP = c(50, 50, 300),
#'   state = c("WA", "WA", "WA")
#' )
#' onet_weight_panel_pums(pums, year = 2024, group = "state")
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
  if (toupper(socp) == "OCCP") {
    cli::cli_warn(
      "{.arg socp} is {.val OCCP}; ACS OCCP is not an SOC code. Use SOCP or a Census occupation-to-SOC crosswalk."
    )
  }
  if ("ESR" %in% names(pums)) {
    employed <- as.character(pums$ESR) %in% c("1", "2")
    if (any(!employed, na.rm = TRUE)) {
      cli::cli_warn(
        "ACS PUMS employment weights usually require filtering to ESR %in% c(1, 2) before calling {.fun onet_weight_panel_pums}."
      )
    }
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
  warn_missing_employment(data[[weight]], "PUMS")
  data$.employment <- parse_oews_number(data[[weight]])
  data <- data[!is.na(data$.employment), , drop = FALSE]
  joined <- dplyr::inner_join(
    data,
    resolved,
    by = "source_code",
    relationship = "many-to-many"
  )
  joined$employment <- joined$.employment * joined$crosswalk_weight
  joined <- drop_invalid_reference_soc(joined, source = "PUMS")
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
  total_cols <- group
  if (length(total_cols) == 0) {
    out$.total_employment <- sum(out$employment, na.rm = TRUE)
  } else {
    out <- out |>
    dplyr::mutate(
      .total_employment = sum(.data$employment, na.rm = TRUE),
      .by = dplyr::all_of(total_cols)
    )
  }
  out$year <- as.integer(year)
  out$source <- source
  out$weight_share <- dplyr::if_else(
    out$.total_employment > 0,
    out$employment / out$.total_employment,
    NA_real_
  )
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
  if (year >= 2018) {
    "2018 SOC"
  } else if (year >= 2012) {
    "2010 SOC"
  } else {
    cli::cli_warn(
      "ACS PUMS SOCP vintage is only documented here for data years 2012 and later."
    )
    "unknown SOC vintage"
  }
}

add_pums_replicate_se <- function(out, joined, replicate_weights, group) {
  if (!(length(replicate_weights) %in% c(80L, 160L))) {
    cli::cli_warn(
      "Replicate-weight standard errors are designed for the full replicate set, usually 80 ACS columns or 160 CPS ASEC columns."
    )
  }
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

filter_oews_detailed_rows <- function(data, code, o_group) {
  if (!is.null(o_group) && o_group %in% names(data)) {
    keep <- tolower(trimws(as.character(data[[o_group]]))) == "detailed"
    dropped <- sum(!keep & !is.na(keep))
    if (dropped > 0) {
      cli::cli_inform(
        "Dropped {dropped} OEWS aggregate row{?s}; keeping {.val detailed} occupations."
      )
    }
    return(data[keep %in% TRUE, , drop = FALSE])
  }

  source_codes <- standardize_soc_code(data[[code]])
  aggregate <- source_codes == "00-0000" | grepl("-0000$", source_codes)
  aggregate[is.na(aggregate)] <- FALSE
  if (any(aggregate)) {
    cli::cli_warn(
      "Dropped {sum(aggregate)} OEWS aggregate occupation row{?s}; add {.var o_group} to keep this explicit."
    )
    data <- data[!aggregate, , drop = FALSE]
  }
  data
}

warn_missing_employment <- function(x, source) {
  missing <- is.na(parse_oews_number(x))
  if (any(missing)) {
    cli::cli_warn(
      "{.val {source}} weights dropped {sum(missing)} row{?s} with missing or unparseable employment."
    )
  }
  invisible(NULL)
}

drop_invalid_reference_soc <- function(data, source) {
  valid <- grepl("^\\d{2}-\\d{4}$", data$reference_soc_code)
  valid[is.na(valid)] <- FALSE
  if (all(valid)) {
    return(data)
  }
  dropped_employment <- sum(data$employment[!valid], na.rm = TRUE)
  total_employment <- sum(data$employment, na.rm = TRUE)
  dropped_share <- if (total_employment > 0) dropped_employment / total_employment else NA_real_
  cli::cli_warn(
    c(
      "{.val {source}} weights dropped {sum(!valid)} row{?s} with non-SOC reference codes.",
      "i" = "Dropped employment share: {round(dropped_share, 4)}."
    )
  )
  data[valid, , drop = FALSE]
}
