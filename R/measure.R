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
  duplicate_keys <- out$measure_key[duplicated(out$measure_key)]
  if (length(duplicate_keys) > 0) {
    cli::cli_abort(
      "{.arg key} must uniquely identify measure rows; duplicate keys include {.val {unique(duplicate_keys)}}."
    )
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
#'
#' @examples
#' measure <- onet_measure(
#'   tibble::tibble(onet_soc_code = "15-1252.00", score = 0.7),
#'   key = "onet_soc_code",
#'   score = "score",
#'   universe = c("15-1252.00", "29-1141.00")
#' )
#' onet_measure_coverage(measure)
onet_measure_coverage <- function(measure) {
  onet_coverage(measure)
}

#' Return Object Coverage
#'
#' Returns the coverage table recorded by an onet2r measure, aggregate, or
#' decomposition object. For sensitivity results, returns one coverage row per
#' scenario with scenario identifiers attached.
#'
#' @param x An onet2r object with coverage metadata.
#'
#' @return A tibble with coverage fields.
#' @export
#'
#' @examples
#' measure <- onet_measure(
#'   tibble::tibble(onet_soc_code = "15-1252.00", score = 0.7),
#'   key = "onet_soc_code",
#'   score = "score",
#'   universe = c("15-1252.00", "29-1141.00")
#' )
#' onet_coverage(measure)
onet_coverage <- function(x) {
  if (inherits(x, "onet_measure")) {
    validate_onet_measure(x)
    return(x$coverage)
  }
  if (inherits(x, "onet_sensitivity") && "coverage" %in% names(x) && length(x$coverage) > 0) {
    coverage <- purrr::list_rbind(x$coverage)
    scenario_cols <- intersect(
      c(
        "scenario",
        "task_release",
        "soc_vintage",
        "weight_panel",
        "bridge",
        "weight_scale",
        "include_supplemental"
      ),
      names(x)
    )
    scenario_cols <- setdiff(scenario_cols, names(coverage))
    return(dplyr::bind_cols(tibble::as_tibble(x[scenario_cols]), coverage))
  }
  if (is.data.frame(x) && "coverage" %in% names(x) && length(x$coverage) > 0) {
    return(x$coverage[[1]])
  }
  cli::cli_abort("{.arg x} does not contain onet2r coverage metadata.")
}

#' Return Aggregate Provenance
#'
#' Returns the provenance recorded by an aggregate or sensitivity table.
#'
#' @param x An object returned by [onet_measure_aggregate()] or
#'   [onet_measure_sensitivity()].
#'
#' @return A tibble with provenance fields.
#' @export
#'
#' @examples
#' measure <- onet_measure(
#'   tibble::tibble(onet_soc_code = c("15-1252.00", "29-1141.00"), score = c(0.7, 0.2)),
#'   key = "onet_soc_code",
#'   score = "score",
#'   measure_id = "stylized_score"
#' )
#' weights <- tibble::tibble(
#'   reference_soc_code = c("15-1252", "29-1141"),
#'   year = 2024L,
#'   employment = c(100, 300),
#'   weight_share = c(0.25, 0.75),
#'   source = "fixture",
#'   source_taxonomy = "2018 SOC",
#'   reference_taxonomy = "2018 SOC"
#' )
#' aggregate <- onet_measure_aggregate(measure, weights)
#' onet_provenance(aggregate)
onet_provenance <- function(x) {
  if (is.data.frame(x) && "provenance" %in% names(x)) {
    return(purrr::list_rbind(x$provenance))
  }
  cli::cli_abort("{.arg x} does not contain onet2r provenance metadata.")
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
#' @return A tibble with occupation-level measure scores, task-count metadata,
#'   and measure provenance columns.
#' @export
#'
#' @examples
#' task_measure <- onet_measure(
#'   tibble::tibble(task_id = c("1001", "1002"), score = c(0.8, 0.4)),
#'   key = "task_id",
#'   score = "score",
#'   key_type = "task"
#' )
#' task_ratings <- tibble::tibble(
#'   onet_soc_code = c("15-1252.00", "15-1252.00"),
#'   task_id = c("1001", "1002"),
#'   scale_id = "RT",
#'   data_value = c(80, 20),
#'   task_type = "Core"
#' )
#' onet_task_to_occupation(task_measure, task_ratings)
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
  if (!is.character(weight_scale) || length(weight_scale) != 1 || !weight_scale %in% c("RT", "IM")) {
    cli::cli_abort("{.arg weight_scale} must be either {.val RT} or {.val IM}.")
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
  task_type_values <- tolower(trimws(as.character(ratings[[task_type]])))
  missing_task_type <- is.na(task_type_values) | task_type_values %in% c("", "n/a")
  if (any(missing_task_type)) {
    cli::cli_inform(
      "Excluded {sum(missing_task_type)} task rating row{?s} with missing task type."
    )
    ratings <- ratings[!missing_task_type, , drop = FALSE]
  }

  ratings <- ratings |>
    dplyr::filter(as.character(.data[[scale_id]]) == weight_scale)
  category_col <- intersect(c("category", "Category"), names(ratings))
  if (length(category_col) == 1) {
    has_category <- !is.na(ratings[[category_col]]) & nzchar(as.character(ratings[[category_col]]))
    if (any(has_category)) {
      cli::cli_abort(
        "{.arg weight_scale} must refer to a task-level RT or IM row, not category rows."
      )
    }
  }
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
      measure_id = measure$metadata$measure_id,
      measure_release = measure$metadata$release_version,
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
#' @param year Optional single year used to filter a multi-year weight panel.
#' @param cell Optional named list or named vector used to filter a multi-cell
#'   weight panel, such as `list(state = "WA")`.
#'
#' @return A one-row tibble with the aggregate, coverage fields, and list-column
#'   metadata readable with [onet_provenance()] and [onet_coverage()].
#'
#' @details
#' When multiple O&#42;NET detail occupations map to the same reference SOC,
#' `onet_measure_aggregate()` first averages those detail scores within the SOC.
#' Employment coverage is then counted once per reference SOC, so coverage shares
#' cannot exceed 100 percent because of detail-code duplication.
#' @export
#'
#' @examples
#' measure <- onet_measure(
#'   tibble::tibble(onet_soc_code = c("15-1252.00", "29-1141.00"), score = c(0.7, 0.2)),
#'   key = "onet_soc_code",
#'   score = "score",
#'   measure_id = "stylized_score"
#' )
#' weights <- tibble::tibble(
#'   reference_soc_code = c("15-1252", "29-1141"),
#'   year = 2024L,
#'   employment = c(100, 300),
#'   weight_share = c(0.25, 0.75),
#'   source = "fixture",
#'   source_taxonomy = "2018 SOC",
#'   reference_taxonomy = "2018 SOC"
#' )
#' onet_measure_aggregate(measure, weights)
onet_measure_aggregate <- function(
    measure,
    weight_panel,
    occupation_code = "onet_soc_code",
    score = "measure_score",
    bridge = NULL,
    measure_id = "user_measure",
    year = NULL,
    cell = NULL) {
  scores <- occupation_scores_table(measure, occupation_code, score, measure_id)
  validate_weight_panel(weight_panel)
  weights <- filter_aggregate_weight_panel(weight_panel, year = year, cell = cell)

  mapped <- map_occupation_scores(scores, bridge)
  collapsed <- collapse_mapped_scores(mapped)
  joined <- dplyr::left_join(
    collapsed,
    weights,
    by = "reference_soc_code",
    relationship = "many-to-one"
  )
  joined$.effective_weight <- joined$employment * joined$coverage_weight
  aggregate <- weighted_mean_na(joined$measure_score, joined$.effective_weight)
  total_employment <- sum(weights$employment, na.rm = TRUE)
  covered_employment <- joined |>
    dplyr::filter(!is.na(.data$measure_score), !is.na(.data$employment)) |>
    dplyr::summarise(total = sum(.data$.effective_weight, na.rm = TRUE)) |>
    dplyr::pull("total")

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
    n_occupations = dplyr::n_distinct(mapped$measure_key),
    n_reference_soc = dplyr::n_distinct(joined$reference_soc_code)
  )
  new_onet_aggregate(
    result,
    provenance = aggregation_provenance(scores, weights, bridge)
  )
}

#' Stress Test a User-Supplied Measure
#'
#' Runs the same user-supplied measure through alternative weight panels,
#' bridges, and task-handling choices. The package does not create the
#' substantive measure. It only changes the plumbing around that measure.
#'
#' @param measure An `onet_measure` object.
#' @param weight_panels A weight-panel data frame or named list of weight-panel
#'   data frames.
#' @param bridges Optional bridge data frame, `NULL`, or named list of bridges.
#' @param task_ratings For task-level measures, a task-ratings data frame or
#'   named list of task-ratings data frames.
#' @param task_metadata Optional task metadata data frame or named list matching
#'   `task_ratings`.
#' @param include_supplemental Logical vector. For task-level measures, controls
#'   whether Supplemental tasks are included.
#' @param weight_scale Character vector of task rating scale ids. Defaults to
#'   `"RT"`, the Task Ratings scale for Relevance of Task.
#' @param year Optional single year passed to [onet_measure_aggregate()].
#' @param cell Optional cell filter passed to [onet_measure_aggregate()].
#' @param baseline Optional scenario label used as the movement baseline.
#'
#' @return A tibble with one row per scenario, aggregate results, movement
#'   fields, and provenance list-column metadata.
#' @export
#'
#' @examples
#' scores <- tibble::tibble(
#'   onet_soc_code = c("15-1252.00", "29-1141.00"),
#'   score = c(0.7, 0.2)
#' )
#' measure <- onet_measure(scores, "onet_soc_code", "score")
#' weights <- tibble::tibble(
#'   reference_soc_code = c("15-1252", "29-1141"),
#'   year = 2024L,
#'   employment = c(100, 300),
#'   weight_share = c(0.25, 0.75),
#'   source = "fixture",
#'   source_taxonomy = "2018 SOC",
#'   reference_taxonomy = "2018 SOC"
#' )
#' onet_measure_sensitivity(measure, weights)
onet_measure_sensitivity <- function(
    measure,
    weight_panels,
    bridges = list(no_bridge = NULL),
    task_ratings = NULL,
    task_metadata = NULL,
    include_supplemental = FALSE,
    weight_scale = "RT",
    year = NULL,
    cell = NULL,
    baseline = NULL) {
  validate_onet_measure(measure)
  weight_panels <- named_data_frame_list(weight_panels, "weight_panels", "weights")
  bridges <- named_bridge_list(bridges)

  if (!is.logical(include_supplemental) || anyNA(include_supplemental)) {
    cli::cli_abort("{.arg include_supplemental} must be a logical vector without missing values.")
  }
  if (!is.character(weight_scale) || length(weight_scale) == 0 || anyNA(weight_scale)) {
    cli::cli_abort("{.arg weight_scale} must be a character vector without missing values.")
  }

  task_sets <- sensitivity_task_sets(measure, task_ratings, task_metadata)
  rows <- list()
  row_index <- 1L

  for (task_name in names(task_sets)) {
    task_set <- task_sets[[task_name]]
    scales <- if (measure$metadata$key_type == "task") weight_scale else NA_character_
    supplemental_choices <- if (measure$metadata$key_type == "task") {
      include_supplemental
    } else {
      NA
    }
    for (scale_choice in scales) {
      for (supplemental_choice in supplemental_choices) {
        scores_input <- sensitivity_scores_input(
          measure = measure,
          task_set = task_set,
          weight_scale = scale_choice,
          include_supplemental = supplemental_choice
        )
        for (weight_name in names(weight_panels)) {
          for (bridge_name in names(bridges)) {
            aggregate <- onet_measure_aggregate(
              scores_input$scores,
              weight_panel = weight_panels[[weight_name]],
              bridge = bridges[[bridge_name]],
              measure_id = measure$metadata$measure_id,
              year = year,
              cell = cell
            )
            aggregate$scenario <- sensitivity_scenario_label(
              task_name = task_name,
              weight_name = weight_name,
              bridge_name = bridge_name,
              weight_scale = scale_choice,
              include_supplemental = supplemental_choice,
              is_task = measure$metadata$key_type == "task"
            )
            aggregate$task_release <- scores_input$release_version
            aggregate$soc_vintage <- scores_input$soc_vintage
            aggregate$weight_panel <- weight_name
            aggregate$bridge <- bridge_name
            aggregate$weight_scale <- scale_choice
            aggregate$include_supplemental <- supplemental_choice
            rows[[row_index]] <- aggregate
            row_index <- row_index + 1L
          }
        }
      }
    }
  }

  out <- purrr::list_rbind(rows) |>
    dplyr::relocate(
      "scenario",
      "measure_id",
      "task_release",
      "soc_vintage",
      "weight_panel",
      "bridge",
      "weight_scale",
      "include_supplemental"
    )

  out <- onet_robustness_diagnostic(out, baseline = baseline)
  class(out) <- unique(c("onet_sensitivity", class(out)))
  out
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
#'
#' @examples
#' scenarios <- tibble::tibble(
#'   scenario = c("baseline", "alternate_weights"),
#'   aggregate = c(0.40, 0.45)
#' )
#' onet_robustness_diagnostic(scenarios)
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
      measure_score = measure$data$measure_score,
      measure_release = measure$metadata$release_version
    ))
  }
  if (!is.data.frame(measure)) {
    cli::cli_abort("{.arg measure} must be an onet_measure object or data frame.")
  }
  validate_single_column(measure, occupation_code, "occupation_code")
  validate_single_column(measure, score, "score")
  measure_id_values <- if ("measure_id" %in% names(measure)) {
    as.character(measure$measure_id)
  } else {
    rep(measure_id, nrow(measure))
  }
  measure_release_values <- if ("measure_release" %in% names(measure)) {
    as.character(measure$measure_release)
  } else {
    rep(NA_character_, nrow(measure))
  }
  tibble::tibble(
    measure_id = measure_id_values,
    measure_key = as.character(measure[[occupation_code]]),
    measure_score = parse_onet_number(measure[[score]]),
    measure_release = measure_release_values
  )
}

map_occupation_scores <- function(scores, bridge) {
  if (is.null(bridge)) {
    return(tibble::tibble(
      measure_id = scores$measure_id,
      measure_key = scores$measure_key,
      measure_score = scores$measure_score,
      measure_release = scores$measure_release,
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

collapse_mapped_scores <- function(mapped) {
  mapped |>
    dplyr::summarise(
      measure_id = dplyr::first(.data$measure_id),
      measure_score = weighted_mean_na(.data$measure_score, .data$crosswalk_weight),
      measure_release = collapse_unique(.data$measure_release),
      n_measure_keys = dplyr::n_distinct(.data$measure_key),
      coverage_weight = min(sum(.data$crosswalk_weight, na.rm = TRUE), 1),
      .by = "reference_soc_code"
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
  tibble::tibble(
    measure_id = scores$measure_id[[1]],
    measure_release = collapse_unique(scores$measure_release),
    weight_source = collapse_unique(weights$source),
    weight_year = unique(stats::na.omit(as.integer(weights$year)))[[1]],
    source_taxonomy = collapse_unique(weights$source_taxonomy),
    reference_taxonomy = collapse_unique(weights$reference_taxonomy),
    bridge_used = !is.null(bridge),
    crosswalk_path = aggregate_crosswalk_path(weights, bridge)
  )
}

new_onet_aggregate <- function(result, provenance) {
  coverage <- result[c(
    "measure_id",
    "total_employment",
    "covered_employment",
    "employment_coverage_share",
    "n_occupations",
    "n_reference_soc"
  )]
  result$coverage <- list(coverage)
  result$provenance <- list(provenance)
  class(result) <- unique(c("onet_aggregate", class(result)))
  result
}

filter_aggregate_weight_panel <- function(weight_panel, year = NULL, cell = NULL) {
  weights <- tibble::as_tibble(weight_panel)
  weight_years <- as.integer(weights$year)
  if (!is.null(year)) {
    if (!is.numeric(year) || length(year) != 1 || is.na(year)) {
      cli::cli_abort("{.arg year} must be a single non-missing numeric year.")
    }
    year_value <- as.integer(year)
    weights <- weights[!is.na(weight_years) & weight_years == year_value, , drop = FALSE]
    weight_years <- as.integer(weights$year)
  }
  if (nrow(weights) == 0) {
    cli::cli_abort("The filtered {.arg weight_panel} has no rows.")
  }
  if (anyNA(weight_years)) {
    cli::cli_abort(
      c(
        "{.arg weight_panel} must not contain missing years for aggregation.",
        "i" = "Pass {.arg year} to filter a multi-period panel or remove rows with missing years."
      )
    )
  }

  years <- unique(weight_years)
  if (length(years) != 1) {
    cli::cli_abort(
      c(
        "{.arg weight_panel} must contain exactly one year for aggregation.",
        "i" = "Pass {.arg year} or pre-filter the panel."
      )
    )
  }

  cell_cols <- weight_panel_cell_columns(weights)
  if (!is.null(cell)) {
    cell <- normalize_cell_filter(cell, cell_cols)
    for (cell_name in names(cell)) {
      cell_value <- as.character(cell[[cell_name]])
      weights <- weights[as.character(weights[[cell_name]]) == cell_value, , drop = FALSE]
    }
  }
  if (nrow(weights) == 0) {
    cli::cli_abort("The filtered {.arg weight_panel} has no rows.")
  }
  if (length(cell_cols) > 0) {
    cell_count <- nrow(unique(weights[cell_cols]))
    if (cell_count != 1) {
      cli::cli_abort(
        c(
          "{.arg weight_panel} must contain exactly one cell for aggregation.",
          "i" = "Pass {.arg cell} or pre-filter the panel."
        )
      )
    }
  }

  duplicates <- weights |>
    dplyr::summarise(n = dplyr::n(), .by = "reference_soc_code") |>
    dplyr::filter(.data$n > 1)
  if (nrow(duplicates) > 0) {
    cli::cli_abort(
      "{.arg weight_panel} must have at most one row per {.var reference_soc_code} after filtering."
    )
  }

  weights
}

weight_panel_cell_columns <- function(weights) {
  standard <- c(
    "reference_soc_code",
    "year",
    "employment",
    "weight_share",
    "source",
    "source_taxonomy",
    "reference_taxonomy",
    "employment_se"
  )
  setdiff(names(weights), standard)
}

normalize_cell_filter <- function(cell, cell_cols) {
  if (is.atomic(cell)) {
    cell <- as.list(cell)
  }
  if (!is.list(cell) || is.null(names(cell)) || any(!nzchar(names(cell)))) {
    cli::cli_abort("{.arg cell} must be a named list or named vector.")
  }
  unknown <- setdiff(names(cell), cell_cols)
  if (length(unknown) > 0) {
    cli::cli_abort("{.arg cell} contains unknown cell columns: {.var {unknown}}.")
  }
  cell
}

collapse_unique <- function(x) {
  out <- unique(as.character(x))
  out <- out[!is.na(out) & nzchar(out)]
  if (length(out) == 0) {
    return(NA_character_)
  }
  paste(out, collapse = ", ")
}

aggregate_crosswalk_path <- function(weights, bridge) {
  if (!is.null(bridge)) {
    bridge <- tibble::as_tibble(bridge)
    if (all(c("from_vintage", "to_vintage") %in% names(bridge))) {
      from <- collapse_unique(bridge$from_vintage)
      to <- collapse_unique(bridge$to_vintage)
      return(paste(from, to, sep = " -> "))
    }
    return("custom bridge")
  }
  paste(
    collapse_unique(weights$source_taxonomy),
    collapse_unique(weights$reference_taxonomy),
    sep = " -> "
  )
}

named_data_frame_list <- function(x, arg, default_name) {
  if (is.data.frame(x)) {
    out <- list(x)
    names(out) <- default_name
    return(out)
  }
  if (!is.list(x) || length(x) == 0) {
    cli::cli_abort("{.arg {arg}} must be a data frame or non-empty list of data frames.")
  }
  ok <- vapply(x, is.data.frame, logical(1))
  if (!all(ok)) {
    cli::cli_abort("{.arg {arg}} must contain only data frames.")
  }
  names(x) <- scenario_names(names(x), default_name, length(x))
  x
}

named_bridge_list <- function(x) {
  if (is.null(x)) {
    return(list(no_bridge = NULL))
  }
  if (is.data.frame(x)) {
    return(list(bridge = x))
  }
  if (!is.list(x) || length(x) == 0) {
    cli::cli_abort("{.arg bridges} must be `NULL`, a data frame, or a non-empty list.")
  }
  ok <- vapply(x, \(item) is.null(item) || is.data.frame(item), logical(1))
  if (!all(ok)) {
    cli::cli_abort("{.arg bridges} must contain only data frames or `NULL` entries.")
  }
  names(x) <- scenario_names(names(x), "bridge", length(x))
  x
}

scenario_names <- function(names, prefix, n) {
  if (!is.null(names) && all(nzchar(names))) {
    return(names)
  }
  if (n == 1) {
    return(prefix)
  }
  paste0(prefix, "_", seq_len(n))
}

sensitivity_task_sets <- function(measure, task_ratings, task_metadata) {
  if (measure$metadata$key_type != "task") {
    return(list(measure_release = list(ratings = NULL, metadata = NULL)))
  }
  if (is.null(task_ratings)) {
    cli::cli_abort("{.arg task_ratings} is required when {.arg measure} has task keys.")
  }
  ratings <- named_data_frame_list(task_ratings, "task_ratings", "task_release")
  metadata <- sensitivity_metadata_list(task_metadata, ratings)
  purrr::map(names(ratings), \(name) {
    list(ratings = ratings[[name]], metadata = metadata[[name]])
  }) |>
    stats::setNames(names(ratings))
}

sensitivity_metadata_list <- function(task_metadata, ratings) {
  if (is.null(task_metadata)) {
    return(stats::setNames(rep(list(NULL), length(ratings)), names(ratings)))
  }
  if (is.data.frame(task_metadata)) {
    return(stats::setNames(rep(list(task_metadata), length(ratings)), names(ratings)))
  }
  if (!is.list(task_metadata) || length(task_metadata) == 0) {
    cli::cli_abort("{.arg task_metadata} must be a data frame, list, or `NULL`.")
  }
  ok <- vapply(task_metadata, \(item) is.null(item) || is.data.frame(item), logical(1))
  if (!all(ok)) {
    cli::cli_abort("{.arg task_metadata} must contain only data frames or `NULL` entries.")
  }
  if (!is.null(names(task_metadata)) && all(names(ratings) %in% names(task_metadata))) {
    return(task_metadata[names(ratings)])
  }
  if (length(task_metadata) != length(ratings)) {
    cli::cli_abort("{.arg task_metadata} must match {.arg task_ratings} by name or length.")
  }
  names(task_metadata) <- names(ratings)
  task_metadata
}

sensitivity_scores_input <- function(
    measure,
    task_set,
    weight_scale,
    include_supplemental) {
  if (measure$metadata$key_type != "task") {
    return(list(
      scores = measure,
      release_version = measure$metadata$release_version,
      soc_vintage = NA_character_
    ))
  }

  scores <- onet_task_to_occupation(
    measure,
    task_ratings = task_set$ratings,
    task_metadata = task_set$metadata,
    weight_scale = weight_scale,
    include_supplemental = include_supplemental
  )
  list(
    scores = scores,
    release_version = sensitivity_release_value(task_set$ratings, "release_version"),
    soc_vintage = sensitivity_release_value(task_set$ratings, "soc_vintage")
  )
}

sensitivity_release_value <- function(data, column) {
  if (!is.data.frame(data) || !column %in% names(data)) {
    return(NA_character_)
  }
  values <- unique(as.character(data[[column]]))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return(NA_character_)
  }
  paste(values, collapse = ", ")
}

sensitivity_scenario_label <- function(
    task_name,
    weight_name,
    bridge_name,
    weight_scale,
    include_supplemental,
    is_task) {
  parts <- c(task_name, weight_name, bridge_name)
  if (is_task) {
    task_part <- paste0(
      weight_scale,
      if (isTRUE(include_supplemental)) "_core_plus_supplemental" else "_core"
    )
    parts <- c(task_part, parts)
  }
  paste(parts, collapse = " / ")
}
