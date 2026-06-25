# =============================================================================
# Weighted O*NET summaries
# =============================================================================

#' Summarise O&#42;NET Values with Employment or Wage Weights
#'
#' Computes employment-weighted summaries for O&#42;NET task, skill, ability, or
#' work-activity rows after joining occupation-level employment and wage
#' estimates. This is useful for moving from occupation-level O&#42;NET outputs to
#' labor-market-weighted measures.
#'
#' @param data A data frame containing one row per occupation-value pair.
#' @param group Character vector of columns to group by, such as task or skill
#'   identifiers.
#' @param value Name of the numeric O&#42;NET value column to average.
#' @param occupation_code Name of the occupation code column in `data`.
#' @param oews Optional OEWS tibble. If supplied and `weight` is not already in
#'   `data`, it is joined with [onet_join_oews()].
#' @param weight Name of the employment weight column. Defaults to `tot_emp`,
#'   the OEWS total-employment column.
#' @param wage Name of the wage column used for wage-weighted summaries.
#'
#' @return A tibble with `group` columns plus `n_records`, `n_occupations`,
#'   `total_weight`, `weighted_mean`, and `wage_weighted_mean`.
#'
#' @section Lifecycle:
#' This helper is soft-deprecated for measure work. Prefer
#' [onet_weight_panel_oews()], [onet_weight_panel_pums()], and
#' [onet_measure_aggregate()] for vintage-aware weighting.
#' @export
#'
#' @examples
#' skills <- tibble::tibble(
#'   code = c("15-1252.00", "15-1252.00", "29-1141.00", "29-1141.00"),
#'   element_id = c("2.A.1.a", "2.A.1.b", "2.A.1.a", "2.A.1.b"),
#'   element_name = c(
#'     "Reading Comprehension", "Active Listening",
#'     "Reading Comprehension", "Active Listening"
#'   ),
#'   data_value = c(4.12, 4.00, 3.88, 4.25)
#' )
#'
#' oews <- tibble::tibble(
#'   occ_code = c("15-1252", "29-1141"),
#'   tot_emp = c(1847900, 3175400),
#'   a_median = c(133080, 93070)
#' )
#'
#' suppressWarnings(
#'   onet_weighted_summary(
#'     skills,
#'     group = c("element_id", "element_name"),
#'     value = "data_value",
#'     oews = oews
#'   )
#' )
onet_weighted_summary <- function(
    data,
    group,
    value,
    occupation_code = "code",
    oews = NULL,
    weight = "tot_emp",
    wage = "a_median") {
  lifecycle::deprecate_soft(
    "0.4.0",
    "onet_weighted_summary()",
    "onet_measure_aggregate()",
    details = paste(
      "Build a weight panel with `onet_weight_panel_oews()` or",
      "`onet_weight_panel_pums()` before aggregating measures."
    )
  )
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  validate_character_columns(data, group, "group")
  validate_single_column(data, value, "value")
  validate_single_column(data, occupation_code, "occupation_code")

  data <- tibble::as_tibble(data)
  if (!weight %in% names(data)) {
    if (is.null(oews)) {
      cli::cli_abort(
        c(
          "{.arg data} does not contain the weight column {.var {weight}}.",
          "i" = "Supply {.arg oews} or add a weight column to {.arg data}."
        )
      )
    }
    data <- join_oews_impl(data, oews = oews, by = occupation_code)
  }
  validate_single_column(data, weight, "weight")

  data$.onet_value <- parse_oews_number(data[[value]])
  data$.onet_weight <- parse_oews_number(data[[weight]])
  data$.onet_wage <- if (wage %in% names(data)) {
    parse_oews_number(data[[wage]])
  } else {
    NA_real_
  }

  data |>
    dplyr::filter(!is.na(.data$.onet_value), !is.na(.data$.onet_weight)) |>
    dplyr::summarise(
      n_records = dplyr::n(),
      n_occupations = dplyr::n_distinct(.data[[occupation_code]]),
      total_weight = sum(.data$.onet_weight, na.rm = TRUE),
      weighted_mean = weighted_mean_na(.data$.onet_value, .data$.onet_weight),
      wage_weighted_mean = weighted_mean_na(
        .data$.onet_value,
        .data$.onet_weight * .data$.onet_wage
      ),
      .by = dplyr::all_of(group)
    ) |>
    dplyr::arrange(dplyr::desc(.data$total_weight))
}

weighted_mean_na <- function(x, w) {
  keep <- !is.na(x) & !is.na(w) & w > 0
  if (!any(keep)) {
    return(NA_real_)
  }

  stats::weighted.mean(x[keep], w[keep])
}

validate_single_column <- function(data, column, arg) {
  validate_single_string(column, arg)
  if (!column %in% names(data)) {
    cli::cli_abort("{.arg {arg}} must name a column in {.arg data}.")
  }

  invisible(column)
}

validate_character_columns <- function(data, columns, arg) {
  if (!is.character(columns) || length(columns) == 0 || anyNA(columns)) {
    cli::cli_abort("{.arg {arg}} must be a character vector of column names.")
  }

  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg {arg}} contains unknown columns: {.var {missing}}.")
  }

  invisible(columns)
}
