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

#' Bridge O&#42;NET Occupations to OEWS Reference Codes
#'
#' Builds a bridge from O&#42;NET-SOC codes to the reference SOC codes of an OEWS
#' weight panel, including the combined codes OEWS publishes in place of some
#' detailed SOC occupations. Pass the result to the `bridge` argument of
#' [onet_measure_aggregate()] or [onet_measure_sensitivity()].
#'
#' @param occupations O&#42;NET-SOC codes to bridge: a character vector, a data
#'   frame with an `occupation_code` column, or an occupation-level
#'   [onet_measure()]. Usually the occupation scores you are aggregating.
#' @param weight_panel A weight panel from [onet_weight_panel_oews()] built from
#'   May 2021 or later OEWS estimates, or any data frame with
#'   `reference_soc_code` and `year` columns. National, state, metropolitan, and
#'   industry panels all work.
#' @param occupation_code Occupation code column when `occupations` is a data
#'   frame.
#'
#' @return A tibble with one row per O&#42;NET-SOC code and columns
#'   `from_onet_soc_code`, `from_soc_code`, `reference_soc_code`, `map_type`,
#'   `crosswalk_weight`, and `crosswalk_path`. `map_type` is `"direct"` when
#'   the occupation's SOC is in the panel, `"oews_combination"` when OEWS
#'   publishes the occupation inside a combined code that is in the panel, and
#'   `"not_in_panel"` otherwise. Rows other than `"oews_combination"` keep the
#'   occupation's own SOC as `reference_soc_code`, so aggregation treats them as
#'   it would without a bridge. [onet_provenance()] reports `crosswalk_path` for
#'   aggregates that use the bridge.
#'
#' @details
#' Beginning with the May 2021 estimates, OEWS publishes most detailed 2018 SOC
#' occupations but combines some of them, either at the broad-occupation level
#' (for example `31-1120`, Home Health and Personal Care Aides, which holds
#' `31-1121` and `31-1122`) or as OEWS-specific codes (for example `25-9045`,
#' Teaching Assistants, Except Postsecondary). Without a bridge, O&#42;NET
#' occupations inside those codes cannot match the panel, and their employment
#' is left out of `covered_employment`.
#'
#' The bridge uses the 12 combined codes, and the SOC occupations each one
#' includes, listed in the BLS May 2021 OEWS occupation definitions. The May
#' 2023 through May 2025 national files publish the same 12 codes. Because the
#' list comes from those definitions rather than from the rows of
#' `weight_panel`, an occupation missing from a state, metropolitan, or industry
#' panel, for example because its estimate is suppressed, is never merged into a
#' neighboring combined code. It stays `"not_in_panel"`.
#'
#' Occupations inside one combination are averaged with equal weight, the same
#' way [onet_measure_aggregate()] averages several O&#42;NET detail codes that
#' share one SOC. OEWS publishes no employment split among them.
#'
#' May 2019 and May 2020 OEWS estimates use a hybrid of the 2010 and 2018 SOC
#' with different combined codes, and earlier estimates use older SOC versions,
#' so `weight_panel` must contain only years from 2021 on. Build a bridge by
#' hand for earlier panels.
#' @export
#'
#' @examples
#' weights <- tibble::tibble(
#'   reference_soc_code = c("29-1141", "31-1120"),
#'   year = 2024L,
#'   employment = c(3000, 4000),
#'   weight_share = c(3, 4) / 7,
#'   source = "OEWS",
#'   source_taxonomy = "2018 SOC",
#'   reference_taxonomy = "2018 SOC"
#' )
#' # Stylized scores for illustration only.
#' scores <- tibble::tibble(
#'   onet_soc_code = c("29-1141.00", "31-1121.00", "31-1122.00"),
#'   measure_score = c(0.2, 0.4, 0.6)
#' )
#' bridge <- onet_oews_bridge(scores, weights)
#' bridge
#' onet_measure_aggregate(scores, weights, bridge = bridge, measure_id = "stylized")
onet_oews_bridge <- function(
    occupations,
    weight_panel,
    occupation_code = "onet_soc_code") {
  codes <- oews_bridge_codes(occupations, occupation_code)
  validate_columns_present(weight_panel, c("reference_soc_code", "year"), "weight_panel")
  validate_oews_bridge_years(weight_panel$year)

  panel_codes <- unique(standardize_soc_code(weight_panel$reference_soc_code))
  panel_codes <- panel_codes[!is.na(panel_codes)]
  parts <- oews_combination_parts()
  parts <- parts[parts$reference_soc_code %in% panel_codes, , drop = FALSE]

  from_soc <- standardize_soc_code(codes)
  combined_into <- parts$reference_soc_code[match(from_soc, parts$part_soc_code)]
  map_type <- dplyr::case_when(
    from_soc %in% panel_codes ~ "direct",
    !is.na(combined_into) ~ "oews_combination",
    .default = "not_in_panel"
  )
  combined <- map_type == "oews_combination"
  reference_soc_code <- from_soc
  reference_soc_code[combined] <- combined_into[combined]

  if (any(combined)) {
    targets <- sort(unique(reference_soc_code[combined]))
    cli::cli_inform(
      "Mapped {sum(combined)} O*NET occupation{?s} into {length(targets)} OEWS combination code{?s}: {.val {targets}}."
    )
  }

  taxonomy <- if ("reference_taxonomy" %in% names(weight_panel)) {
    collapse_unique(weight_panel$reference_taxonomy)
  } else {
    NA_character_
  }
  if (is.na(taxonomy)) {
    taxonomy <- "OEWS SOC"
  }

  out <- tibble::tibble(
    from_onet_soc_code = codes,
    from_soc_code = from_soc,
    reference_soc_code = reference_soc_code,
    map_type = map_type,
    crosswalk_weight = rep(1, length(codes)),
    crosswalk_path = rep(
      paste0("O*NET-SOC -> ", taxonomy, " with OEWS combinations"),
      length(codes)
    )
  )
  out[order(out$from_onet_soc_code), , drop = FALSE]
}

oews_bridge_codes <- function(occupations, occupation_code) {
  if (inherits(occupations, "onet_measure")) {
    if (!identical(occupations$metadata$key_type, "occupation")) {
      cli::cli_abort(c(
        "{.arg occupations} must be an occupation-level {.cls onet_measure}.",
        "i" = "Roll task measures up with {.fun onet_task_to_occupation} first."
      ))
    }
    codes <- occupations$data$measure_key
  } else if (is.data.frame(occupations)) {
    validate_single_string(occupation_code, "occupation_code")
    if (!occupation_code %in% names(occupations)) {
      cli::cli_abort(
        "{.arg occupation_code} must name a column in {.arg occupations}: {.val {occupation_code}} was not found."
      )
    }
    codes <- occupations[[occupation_code]]
  } else if (is.character(occupations)) {
    codes <- occupations
  } else {
    cli::cli_abort(
      "{.arg occupations} must be a character vector of O*NET-SOC codes, a data frame, or an occupation-level {.cls onet_measure}."
    )
  }
  codes <- standardize_onet_soc_code(as.character(codes))
  unique(codes[!is.na(codes) & nzchar(codes)])
}

validate_oews_bridge_years <- function(year) {
  years <- suppressWarnings(as.integer(year))
  if (length(years) == 0) {
    cli::cli_abort("{.arg weight_panel} has no rows.")
  }
  if (anyNA(years) || any(years < 2021L)) {
    found <- unique(ifelse(is.na(years), "NA", as.character(years)))
    cli::cli_abort(c(
      "{.arg weight_panel} must contain only OEWS estimate years from 2021 on.",
      "x" = "Years found: {.val {found}}.",
      "i" = "OEWS has used the same combined codes since May 2021. May 2019 and May 2020 use hybrid 2010/2018 SOC combinations, and earlier years use older SOC versions.",
      "i" = "Build a bridge by hand for earlier years."
    ))
  }
  invisible(years)
}

# OEWS codes that combine detailed 2018 SOC occupations, with the occupations
# each one includes, from the BLS May 2021 OEWS occupation definitions
# (occupation_definitions_m2021.xlsx). The May 2023 through May 2025 national
# files publish the same 12 codes. See docs/DATA_NOTES.md.
oews_combination_parts <- function() {
  parts <- list(
    `13-1020` = c("13-1021", "13-1022", "13-1023"),
    `13-2020` = c("13-2022", "13-2023"),
    `21-1018` = c("21-1011", "21-1014"),
    `25-2052` = c("25-2055", "25-2056"),
    # BLS also lists the 2010 SOC occupation 25-9041 Teacher Assistants.
    `25-9045` = c("25-9041", "25-9042", "25-9043", "25-9049"),
    `29-2010` = c("29-2011", "29-2012"),
    `31-1120` = c("31-1121", "31-1122"),
    `39-7010` = c("39-7011", "39-7012"),
    `47-4090` = c("47-4091", "47-4099"),
    `51-2028` = c("51-2022", "51-2023"),
    `51-2090` = c("51-2092", "51-2099"),
    `53-1047` = c("53-1042", "53-1043", "53-1044", "53-1049")
  )
  tibble::tibble(
    reference_soc_code = rep(names(parts), lengths(parts)),
    part_soc_code = unlist(parts, use.names = FALSE)
  )
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
#'
#' The `year` argument means the 1-year ACS PUMS data year. 5-year PUMS files
#' pool responses across vintages and can mix SOCP code versions within one
#' file; SOC-vintage labeling is not reliable for them. Prefer 1-year files
#' for weight panels, or resolve codes explicitly with a crosswalk.
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
