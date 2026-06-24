# =============================================================================
# Bring-your-own measure infrastructure
# =============================================================================

#' Create a Bring-Your-Own O&#42;NET Measure
#'
#' Validates a user-supplied occupation, task, or DWA score table and records
#' coverage against an optional universe. The package does not supply or alter
#' the substantive score.
#'
#' @param data A data frame containing the user-supplied measure.
#' @param key Name of the key column.
#' @param score Name of the numeric score column.
#' @param key_type Measure grain: occupation, task, or DWA.
#' @param universe Optional vector or data frame of valid keys.
#' @param measure_id Short identifier for the measure.
#' @param measure_name Human-readable measure name.
#' @param source Optional source label.
#' @param release_version Optional O&#42;NET release used to create the measure.
#' @param weight_panel Optional weight panel used to report employment coverage
#'   for occupation-level measures.
#'
#' @return An `onet_measure` object with `data`, `coverage`, `unmatched`, and
#'   `metadata` fields.
#' @export
#'
#' @examples
#' scores <- tibble::tibble(
#'   onet_soc_code = c("15-1252.00", "29-1141.00"),
#'   score = c(0.7, 0.2)
#' )
#' universe <- c("15-1252.00", "29-1141.00", "11-1011.00")
#' measure <- onet_measure(scores, "onet_soc_code", "score", universe = universe)
#' onet_measure_coverage(measure)
onet_measure <- function(
    data,
    key,
    score,
    key_type = c("occupation", "task", "dwa"),
    universe = NULL,
    measure_id = "user_measure",
    measure_name = measure_id,
    source = NA_character_,
    release_version = NA_character_,
    weight_panel = NULL) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  key_type <- match.arg(key_type)
  validate_single_column(data, key, "key")
  validate_single_column(data, score, "score")
  validate_single_string(measure_id, "measure_id")
  validate_single_string(measure_name, "measure_name")

  out <- tibble::as_tibble(data)
  out$measure_key <- as.character(out[[key]])
  out$measure_score <- parse_onet_number(out[[score]])
  if (any(is.na(out$measure_key) | !nzchar(out$measure_key))) {
    cli::cli_abort("{.arg key} must not contain missing or blank keys.")
  }
  if (any(is.na(out$measure_score))) {
    cli::cli_abort("{.arg score} must contain numeric values.")
  }

  universe_keys <- measure_universe_keys(universe, key)
  matched <- if (is.null(universe_keys)) {
    rep(TRUE, nrow(out))
  } else {
    out$measure_key %in% universe_keys
  }
  unmatched <- tibble::tibble(
    key_type = key_type,
    measure_key = out$measure_key[!matched]
  )
  coverage <- measure_coverage_table(
    data = out,
    matched = matched,
    universe_keys = universe_keys,
    key_type = key_type,
    weight_panel = weight_panel
  )

  structure(
    list(
      data = out,
      coverage = coverage,
      unmatched = unmatched,
      metadata = list(
        measure_id = measure_id,
        measure_name = measure_name,
        source = source,
        release_version = release_version,
        key = key,
        score = score,
        key_type = key_type
      )
    ),
    class = "onet_measure"
  )
}

#' Return Measure Coverage
#'
#' @param measure An object from [onet_measure()].
#'
#' @return A tibble with key and optional employment coverage.
#' @export
onet_measure_coverage <- function(measure) {
  validate_onet_measure(measure)
  measure$coverage
}

#' Aggregate Task Scores to Occupations
#'
#' Rolls a task-level measure up to occupations using O&#42;NET task relevance,
#' importance, or equal task weights.
#'
#' @param measure An `onet_measure` object with `key_type = "task"`.
#' @param task_ratings A data frame containing occupation task ratings.
#' @param task_metadata Optional data frame containing task type by task id.
#' @param occupation_code Column with O&#42;NET-SOC codes in `task_ratings`.
#' @param task_id Column with task ids in `task_ratings` and `task_metadata`.
#' @param task_type Column with Core or Supplemental labels.
#' @param scale_id Column with O&#42;NET task rating scale ids.
#' @param value Column with rating values.
#' @param weight_scale Scale used for task weights. Relevance (`"RT"`) is the
#'   default.
#' @param include_supplemental If `TRUE`, include Supplemental tasks.
#'
#' @return A tibble with occupation-level measure scores.
#' @export
onet_task_to_occupation <- function(
    measure,
    task_ratings,
    task_metadata = NULL,
    occupation_code = "onet_soc_code",
    task_id = "task_id",
    task_type = "task_type",
    scale_id = "scale_id",
    value = "data_value",
    weight_scale = "RT",
    include_supplemental = FALSE) {
  validate_onet_measure(measure)
  if (measure$metadata$key_type != "task") {
    cli::cli_abort("{.arg measure} must have {.code key_type = \"task\"}.")
  }
  if (!is.data.frame(task_ratings)) {
    cli::cli_abort("{.arg task_ratings} must be a data frame.")
  }
  validate_single_column(task_ratings, occupation_code, "occupation_code")
  validate_single_column(task_ratings, task_id, "task_id")
  validate_single_column(task_ratings, scale_id, "scale_id")
  validate_single_column(task_ratings, value, "value")

  ratings <- tibble::as_tibble(task_ratings)
  needs_task_metadata <- !task_type %in% names(ratings) ||
    all(is.na(ratings[[task_type]]) | !nzchar(as.character(ratings[[task_type]])))
  if (needs_task_metadata) {
    if (is.null(task_metadata)) {
      cli::cli_abort(
        c(
          "{.arg task_ratings} does not include {.var {task_type}}.",
          "i" = "Supply {.arg task_metadata} with task type labels."
        )
      )
    }
    validate_single_column(task_metadata, task_id, "task_id")
    validate_single_column(task_metadata, task_type, "task_type")
    if (task_type %in% names(ratings)) {
      ratings[[task_type]] <- NULL
    }
    ratings <- dplyr::left_join(
      ratings,
      tibble::as_tibble(task_metadata)[c(task_id, task_type)],
      by = task_id,
      relationship = "many-to-one"
    )
  }

  ratings <- ratings |>
    dplyr::filter(as.character(.data[[scale_id]]) == weight_scale)
  if (!include_supplemental) {
    ratings <- ratings |>
      dplyr::filter(tolower(as.character(.data[[task_type]])) == "core")
  }

  ratings$measure_key <- as.character(ratings[[task_id]])
  scored <- dplyr::inner_join(
    ratings,
    measure$data[c("measure_key", "measure_score")],
    by = "measure_key",
    relationship = "many-to-many"
  )
  scored$.task_weight <- parse_onet_number(scored[[value]])

  scored |>
    dplyr::summarise(
      n_tasks = dplyr::n_distinct(.data[[task_id]]),
      total_task_weight = sum(.data$.task_weight, na.rm = TRUE),
      measure_score = weighted_mean_na(.data$measure_score, .data$.task_weight),
      .by = dplyr::all_of(occupation_code)
    ) |>
    dplyr::mutate(soc_code = standardize_soc_code(.data[[occupation_code]])) |>
    dplyr::arrange(.data[[occupation_code]])
}

#' Aggregate Occupation Measures with Employment Weights
#'
#' Aggregates an occupation-level measure using a normalized weight panel.
#'
#' @param measure An `onet_measure` object or a data frame of occupation scores.
#' @param weight_panel A weight panel from [onet_weight_panel_oews()] or
#'   [onet_weight_panel_pums()].
#' @param occupation_code Occupation code column when `measure` is a data frame.
#' @param score Score column when `measure` is a data frame.
#' @param bridge Optional bridge from O&#42;NET-SOC to `reference_soc_code`.
#' @param measure_id Identifier used when `measure` is a data frame.
#'
#' @return A one-row tibble with the aggregate and coverage fields. Provenance
#'   is attached as a `provenance` attribute.
#' @export
onet_measure_aggregate <- function(
    measure,
    weight_panel,
    occupation_code = "onet_soc_code",
    score = "measure_score",
    bridge = NULL,
    measure_id = "user_measure") {
  scores <- occupation_scores_table(measure, occupation_code, score, measure_id)
  validate_weight_panel(weight_panel)
  weights <- tibble::as_tibble(weight_panel)

  mapped <- map_occupation_scores(scores, bridge)
  joined <- dplyr::left_join(
    mapped,
    weights,
    by = "reference_soc_code",
    relationship = "many-to-many"
  )
  joined$.effective_weight <- joined$employment * joined$crosswalk_weight
  aggregate <- weighted_mean_na(joined$measure_score, joined$.effective_weight)
  total_employment <- sum(weights$employment, na.rm = TRUE)
  covered_employment <- sum(joined$.effective_weight, na.rm = TRUE)

  result <- tibble::tibble(
    measure_id = scores$measure_id[[1]],
    aggregate = aggregate,
    total_employment = total_employment,
    covered_employment = covered_employment,
    employment_coverage_share = if (total_employment > 0) {
      covered_employment / total_employment
    } else {
      NA_real_
    },
    n_occupations = dplyr::n_distinct(joined$measure_key),
    n_reference_soc = dplyr::n_distinct(joined$reference_soc_code)
  )
  attr(result, "provenance") <- aggregation_provenance(scores, weights, bridge)
  result
}

#' Compare Aggregates Across Plumbing Choices
#'
#' @param results A data frame with one row per scenario and an aggregate column.
#' @param baseline Scenario name used as the baseline. If `NULL`, the first row
#'   is the baseline.
#' @param scenario Scenario column.
#' @param aggregate Aggregate value column.
#'
#' @return A tibble with baseline movement fields.
#' @export
onet_robustness_diagnostic <- function(
    results,
    baseline = NULL,
    scenario = "scenario",
    aggregate = "aggregate") {
  if (!is.data.frame(results)) {
    cli::cli_abort("{.arg results} must be a data frame.")
  }
  validate_single_column(results, scenario, "scenario")
  validate_single_column(results, aggregate, "aggregate")
  data <- tibble::as_tibble(results)
  if (is.null(baseline)) {
    baseline <- as.character(data[[scenario]][[1]])
  }
  baseline_rows <- data[as.character(data[[scenario]]) == baseline, , drop = FALSE]
  if (nrow(baseline_rows) != 1) {
    cli::cli_abort("{.arg baseline} must identify exactly one scenario.")
  }
  baseline_value <- parse_onet_number(baseline_rows[[aggregate]])[[1]]
  data$baseline_scenario <- baseline
  data$baseline_aggregate <- baseline_value
  data$movement <- parse_onet_number(data[[aggregate]]) - baseline_value
  data$movement_percent <- if (is.na(baseline_value) || baseline_value == 0) {
    NA_real_
  } else {
    data$movement / baseline_value
  }
  data
}

measure_universe_keys <- function(universe, key) {
  if (is.null(universe)) {
    return(NULL)
  }
  if (is.data.frame(universe)) {
    validate_single_column(universe, key, "key")
    return(unique(as.character(universe[[key]])))
  }
  unique(as.character(universe))
}

measure_coverage_table <- function(data, matched, universe_keys, key_type, weight_panel) {
  n_universe <- if (is.null(universe_keys)) NA_integer_ else length(unique(universe_keys))
  out <- tibble::tibble(
    key_type = key_type,
    n_input = dplyr::n_distinct(data$measure_key),
    n_universe = n_universe,
    n_matched = dplyr::n_distinct(data$measure_key[matched]),
    coverage_share = if (!is.na(n_universe) && n_universe > 0) {
      dplyr::n_distinct(data$measure_key[matched]) / n_universe
    } else {
      NA_real_
    },
    employment_coverage_share = NA_real_
  )
  if (!is.null(weight_panel) && key_type == "occupation") {
    weights <- tibble::as_tibble(weight_panel)
    if ("reference_soc_code" %in% names(weights) && "employment" %in% names(weights)) {
      covered <- standardize_soc_code(data$measure_key[matched])
      total <- sum(weights$employment, na.rm = TRUE)
      covered_total <- weights |>
        dplyr::filter(.data$reference_soc_code %in% covered) |>
        dplyr::summarise(total = sum(.data$employment, na.rm = TRUE)) |>
        dplyr::pull("total")
      out$employment_coverage_share <- if (total > 0) covered_total / total else NA_real_
    }
  }
  out
}

validate_onet_measure <- function(measure) {
  if (!inherits(measure, "onet_measure")) {
    cli::cli_abort("{.arg measure} must be an {.cls onet_measure} object.")
  }
  invisible(measure)
}

occupation_scores_table <- function(measure, occupation_code, score, measure_id) {
  if (inherits(measure, "onet_measure")) {
    if (measure$metadata$key_type != "occupation") {
      cli::cli_abort("{.arg measure} must have {.code key_type = \"occupation\"}.")
    }
    return(tibble::tibble(
      measure_id = measure$metadata$measure_id,
      measure_key = measure$data$measure_key,
      measure_score = measure$data$measure_score
    ))
  }
  if (!is.data.frame(measure)) {
    cli::cli_abort("{.arg measure} must be an onet_measure object or data frame.")
  }
  validate_single_column(measure, occupation_code, "occupation_code")
  validate_single_column(measure, score, "score")
  tibble::tibble(
    measure_id = measure_id,
    measure_key = as.character(measure[[occupation_code]]),
    measure_score = parse_onet_number(measure[[score]])
  )
}

map_occupation_scores <- function(scores, bridge) {
  if (is.null(bridge)) {
    return(tibble::tibble(
      measure_id = scores$measure_id,
      measure_key = scores$measure_key,
      measure_score = scores$measure_score,
      reference_soc_code = standardize_soc_code(scores$measure_key),
      crosswalk_weight = 1
    ))
  }
  bridge <- normalize_measure_bridge(bridge)
  dplyr::inner_join(
    scores,
    bridge,
    by = dplyr::join_by(measure_key == from_onet_soc_code),
    relationship = "many-to-many"
  )
}

normalize_measure_bridge <- function(bridge) {
  bridge <- normalize_bridge(bridge)
  if (!"reference_soc_code" %in% names(bridge)) {
    bridge$reference_soc_code <- bridge$to_soc_code
  }
  bridge |>
    dplyr::select(
      "from_onet_soc_code",
      "reference_soc_code",
      "crosswalk_weight"
    )
}

aggregation_provenance <- function(scores, weights, bridge) {
  list(
    measure_id = scores$measure_id[[1]],
    weight_source = unique(weights$source),
    weight_year = unique(weights$year),
    source_taxonomy = unique(weights$source_taxonomy),
    reference_taxonomy = unique(weights$reference_taxonomy),
    bridge_used = !is.null(bridge)
  )
}
