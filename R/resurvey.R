# =============================================================================
# O*NET resurvey panel and resurvey conditioning
# =============================================================================

#' Known O&#42;NET Longitudinal Seams
#'
#' Internal lookup of the two taxonomy/scale seams that must not be counted as
#' content change. Verified against O&#42;NET release documentation:
#' v21.0 (August 2016) retired the Task Relevance scale, and v25.1
#' (November 2020) moved from O&#42;NET-SOC 2010 to O&#42;NET-SOC 2019 / SOC 2018.
#'
#' @return A tibble with `seam_type` and the first-day-of-month `seam_date`.
#' @keywords internal
#' @noRd
onet_known_seams <- function() {
  tibble::tibble(
    seam_type = c("scale_seam", "soc_seam"),
    seam_date = as.Date(c("2016-08-01", "2020-11-01"))
  )
}

# Resolve the seam table used to flag date-based longitudinal seams. `NULL`
# keeps the default Task-Ratings-scoped table from onet_known_seams() so
# existing calls are unchanged. A supplied table lets non-Task-Ratings inputs
# (Work Activities, Work Context, Abilities) drop the v21.0 Task Relevance
# scale seam, which does not apply to them, or provide their own seam dates.
resolve_seams <- function(seams) {
  if (is.null(seams)) {
    return(onet_known_seams())
  }
  if (!is.data.frame(seams)) {
    cli::cli_abort("{.arg seams} must be a data frame or `NULL`.")
  }
  missing <- setdiff(c("seam_type", "seam_date"), names(seams))
  if (length(missing) > 0) {
    cli::cli_abort("{.arg seams} is missing columns: {.var {missing}}.")
  }
  out <- tibble::tibble(
    seam_type = as.character(seams$seam_type),
    seam_date = as.Date(seams$seam_date)
  )
  if (nrow(out) > 0 && anyNA(out$seam_date)) {
    cli::cli_abort("{.arg seams} column {.field seam_date} must contain valid dates.")
  }
  out
}

# Classify a Task Ratings Domain Source into the resurvey source vocabulary.
# Survey rotation = Incumbent + Occupational Expert (the Data Collection
# Program). Analyst - Transition rows are the SOC-2018 seam carry-forward and
# never advance the survey clock.
classify_survey_source <- function(domain_source,
                                   survey_sources = c("Incumbent", "Occupational Expert")) {
  ds <- as.character(domain_source)
  survey_pattern <- paste(survey_sources, collapse = "|")
  is_survey <- grepl(survey_pattern, ds, ignore.case = TRUE)
  is_survey[is.na(is_survey)] <- FALSE
  is_analyst <- grepl("analyst|transition", ds, ignore.case = TRUE)
  is_analyst[is.na(is_analyst)] <- FALSE

  out <- rep("other", length(ds))
  out[is_analyst] <- "analyst"
  out[is_survey] <- "survey"
  out[is.na(ds)] <- "unknown"
  out
}

max_date_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(as.Date(NA))
  }
  max(x)
}

# For each position, the maximum of all strictly-prior non-missing dates (NA
# when none). Used to carry an occupation's survey clock forward across releases
# that were not re-surveyed, so a gap year does not reset the clock.
running_prev_max_date <- function(x) {
  n <- length(x)
  out <- rep(as.Date(NA), n)
  best <- as.Date(NA)
  for (i in seq_len(n)) {
    out[i] <- best
    if (!is.na(x[i])) {
      best <- if (is.na(best)) x[i] else max(best, x[i])
    }
  }
  out
}

# Per-release incoming-transition seam flags. A release inherits a seam when the
# interval since the prior release crosses a known seam date, or when the SOC
# taxonomy vintage changes from the prior release.
resurvey_release_seams <- function(rel_meta, seams = onet_known_seams()) {
  rel_meta <- rel_meta |>
    dplyr::arrange(.data$release_date, .data$release_version)

  prev_date <- dplyr::lag(rel_meta$release_date)
  prev_vintage <- dplyr::lag(as.character(rel_meta$soc_vintage))
  this_vintage <- as.character(rel_meta$soc_vintage)
  n <- nrow(rel_meta)
  seam_type <- rep(NA_character_, n)

  for (i in seq_len(n)) {
    if (is.na(prev_date[i])) {
      next
    }
    crossed <- seams$seam_type[
      seams$seam_date > prev_date[i] & seams$seam_date <= rel_meta$release_date[i]
    ]
    if (length(crossed) > 0) {
      seam_type[i] <- crossed[[1]]
      next
    }
    if (!is.na(this_vintage[i]) && !is.na(prev_vintage[i]) &&
      this_vintage[i] != prev_vintage[i]) {
      seam_type[i] <- "soc_seam"
    }
  }

  tibble::tibble(
    release_version = rel_meta$release_version,
    seam_in = !is.na(seam_type),
    seam_type = seam_type
  )
}

empty_resurvey_panel <- function(item = "task_id") {
  cols <- list(
    onet_soc_code = character(),
    soc_code = character()
  )
  cols[[item]] <- character()
  cols <- c(cols, list(
    title = character(),
    task = character(),
    release_version = character(),
    release_date = as.Date(character()),
    soc_vintage = factor(character(), levels = onet_vintage_levels),
    scale_id = factor(character()),
    data_value = double(),
    source_date = as.Date(character()),
    domain_source = character(),
    survey_source = character(),
    recommend_suppress = character(),
    occ_survey_date = as.Date(character()),
    prev_survey_date = as.Date(character()),
    prev_release_version = character(),
    resurvey_event = logical(),
    cycle_index = integer(),
    age_resolved = double(),
    seam_in = logical(),
    seam_type = character()
  ))
  out <- tibble::as_tibble(cols)
  class(out) <- unique(c("onet_resurvey_panel", class(out)))
  out
}

#' Assemble a Task by Resurvey-Cycle Panel
#'
#' Restructures a longitudinal O&#42;NET panel into the task by resurvey-cycle
#' frame used to study when occupations are actually re-rated. O&#42;NET publishes
#' an occupation survey `source_date` on each Task Ratings row and rotates its
#' incumbent-worker survey across only part of the taxonomy each year, so a cell
#' is an observation of change only when the occupation was re-rated between two
#' releases. This function exposes that resurvey clock at the task level.
#'
#' @param panel A tibble from [onet_panel()] (a Task Ratings panel) or the same
#'   schema. Must contain `onet_soc_code`, `release_version`, `release_date`,
#'   `soc_vintage`, `source_date`, `domain_source`, `data_value`, and the `item`
#'   key column. `scale_id` is required when `scale` is not `NULL`.
#' @param scale Optional scale id to filter to a single rating, for example
#'   `"IM"` for Importance. Filtering keeps one row per occupation, item, and
#'   release. Use `NULL` to keep every scale.
#' @param item Column that identifies the content item within an occupation.
#'   Defaults to `"task_id"`.
#' @param survey_sources Character vector of `domain_source` values that count
#'   as an incumbent survey rotation. Defaults to Incumbent and Occupational
#'   Expert. Matching is case-insensitive and by substring.
#' @param min_importance Optional numeric floor applied to `data_value` (on the
#'   selected `scale`). Rows below the floor, or with missing `data_value`, are
#'   dropped. Use it to reproduce the Importance filter that removes the
#'   post-v21.0 Task Relevance artifact.
#' @param seams Optional data frame with `seam_type` and `seam_date` columns that
#'   overrides the default Task-Ratings seam table returned by
#'   `onet_known_seams()`. Use it for non-Task-Ratings inputs such as Work
#'   Activities, Work Context, or Abilities, where the v21.0 Task Relevance scale
#'   seam does not apply. A row is a date-based seam when its `seam_date` falls in
#'   the interval between a release and its prior release. Supply an empty table
#'   to disable date-based seams entirely. `NULL` keeps the default table, so
#'   Task Ratings output is unchanged. Cross-vintage SOC seams are always
#'   detected from `soc_vintage` regardless of this table.
#'
#' @return A tibble with one row per occupation, item, and release. Alongside
#'   the identifier and rating columns it carries the resurvey clock:
#'   `survey_source` (survey, analyst, other, or unknown), `occ_survey_date`
#'   (the occupation survey clock, the latest survey `source_date` in that
#'   release), `prev_survey_date`, `prev_release_version`, `resurvey_event`
#'   (the survey clock advanced versus the occupation's prior release within the
#'   same taxonomy era), `cycle_index` (cumulative resurvey count),
#'   `age_resolved` (years of staleness a resurvey resolved), and the incoming
#'   seam flags `seam_in` and `seam_type`.
#'
#' @details
#' The occupation survey clock is the newest `source_date` among rows whose
#' `domain_source` is a survey source; Analyst - Transition carry-forward rows
#' are excluded from the clock. Resurvey events and cycle indices are computed
#' within each `soc_vintage` era so the v25.1 SOC seam never registers as a
#' resurvey. Pair this frame with [onet_condition_on_resurvey()] to obtain the
#' at-risk set and its selection reasons.
#' @export
#'
#' @examples
#' panel <- tibble::tibble(
#'   release_version = rep(c("22.1", "23.1"), each = 2),
#'   release_date = rep(as.Date(c("2017-10-01", "2018-11-01")), each = 2),
#'   soc_vintage = "2010",
#'   onet_soc_code = rep(c("15-1132.00", "29-1141.00"), 2),
#'   soc_code = rep(c("15-1132", "29-1141"), 2),
#'   task_id = rep(c("1001", "1002"), 2),
#'   task = rep(c("Write code.", "Assess patients."), 2),
#'   scale_id = "IM",
#'   data_value = c(4.1, 4.6, 4.1, 4.8),
#'   source_date = as.Date(c("2016-07-01", "2016-07-01", "2016-07-01", "2018-07-01")),
#'   domain_source = "Incumbent"
#' )
#' onet_resurvey_panel(panel)
onet_resurvey_panel <- function(
    panel,
    scale = "IM",
    item = "task_id",
    survey_sources = c("Incumbent", "Occupational Expert"),
    min_importance = NULL,
    seams = NULL) {
  validate_single_string(item, "item")
  seam_table <- resolve_seams(seams)
  if (!is.null(scale)) {
    validate_single_string(scale, "scale")
  }
  if (!is.character(survey_sources) || length(survey_sources) == 0 ||
    anyNA(survey_sources)) {
    cli::cli_abort("{.arg survey_sources} must be a non-empty character vector.")
  }
  if (!is.null(min_importance)) {
    if (!is.numeric(min_importance) || length(min_importance) != 1 ||
      is.na(min_importance)) {
      cli::cli_abort("{.arg min_importance} must be a single number or `NULL`.")
    }
  }

  required <- c(
    "onet_soc_code", "release_version", "release_date", "soc_vintage",
    "source_date", "domain_source", "data_value", item
  )
  validate_columns_present(panel, required, "panel")

  data <- tibble::as_tibble(panel)
  if (!is.null(scale)) {
    validate_columns_present(data, "scale_id", "panel")
    data <- data |>
      dplyr::filter(as.character(.data$scale_id) == scale)
  }
  if (!"scale_id" %in% names(data)) {
    data$scale_id <- factor(NA_character_)
  }
  if (!"soc_code" %in% names(data)) {
    data$soc_code <- standardize_soc_code(data$onet_soc_code)
  }
  if (!"title" %in% names(data)) {
    data$title <- NA_character_
  }
  if (!"task" %in% names(data)) {
    data$task <- NA_character_
  }
  if (!"recommend_suppress" %in% names(data)) {
    data$recommend_suppress <- NA_character_
  }
  data$source_date <- as.Date(data$source_date)
  data$data_value <- parse_onet_number(data$data_value)

  if (!is.null(min_importance)) {
    data <- data |>
      dplyr::filter(!is.na(.data$data_value) & .data$data_value >= min_importance)
  }

  if (nrow(data) == 0) {
    return(empty_resurvey_panel(item))
  }

  data$survey_source <- classify_survey_source(data$domain_source, survey_sources)

  rel_meta <- data |>
    dplyr::distinct(.data$release_version, .data$release_date, .data$soc_vintage) |>
    dplyr::arrange(.data$release_date, .data$release_version)

  seams <- resurvey_release_seams(rel_meta, seam_table)

  occ_clock <- data |>
    dplyr::filter(.data$survey_source == "survey") |>
    dplyr::summarise(
      occ_survey_date = max_date_or_na(.data$source_date),
      .by = c("onet_soc_code", "release_version")
    )

  occ_rel <- data |>
    dplyr::distinct(
      .data$onet_soc_code, .data$release_version, .data$release_date
    ) |>
    dplyr::left_join(occ_clock, by = c("onet_soc_code", "release_version")) |>
    dplyr::left_join(seams, by = "release_version") |>
    dplyr::mutate(seam_in = dplyr::coalesce(.data$seam_in, FALSE)) |>
    dplyr::arrange(.data$onet_soc_code, .data$release_date, .data$release_version) |>
    dplyr::group_by(.data$onet_soc_code) |>
    dplyr::mutate(
      prev_survey_date = running_prev_max_date(.data$occ_survey_date),
      prev_release_version = dplyr::lag(.data$release_version),
      .has_prior = dplyr::row_number() > 1,
      .advanced = !is.na(.data$occ_survey_date) &
        !is.na(.data$prev_survey_date) &
        .data$occ_survey_date > .data$prev_survey_date,
      resurvey_event = dplyr::if_else(
        .data$.has_prior,
        .data$.advanced & !.data$seam_in,
        NA
      ),
      cycle_index = cumsum(dplyr::coalesce(.data$resurvey_event, FALSE)),
      age_resolved = dplyr::if_else(
        dplyr::coalesce(.data$resurvey_event, FALSE),
        as.numeric(.data$occ_survey_date - .data$prev_survey_date) / 365.25,
        NA_real_
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      "onet_soc_code", "release_version", "occ_survey_date",
      "prev_survey_date", "prev_release_version", "resurvey_event",
      "cycle_index", "age_resolved", "seam_in", "seam_type"
    )

  out <- data |>
    dplyr::left_join(occ_rel, by = c("onet_soc_code", "release_version")) |>
    dplyr::mutate(
      soc_vintage = factor(as.character(.data$soc_vintage), levels = onet_vintage_levels),
      cycle_index = as.integer(.data$cycle_index)
    )

  out <- out |>
    dplyr::select(
      "onet_soc_code",
      "soc_code",
      dplyr::all_of(item),
      "title",
      "task",
      "release_version",
      "release_date",
      "soc_vintage",
      "scale_id",
      "data_value",
      "source_date",
      "domain_source",
      "survey_source",
      "recommend_suppress",
      "occ_survey_date",
      "prev_survey_date",
      "prev_release_version",
      "resurvey_event",
      "cycle_index",
      "age_resolved",
      "seam_in",
      "seam_type"
    ) |>
    dplyr::arrange(
      .data$soc_vintage, .data$onet_soc_code, .data$release_date,
      .data[[item]]
    )

  class(out) <- unique(c("onet_resurvey_panel", class(out)))
  out
}

#' Condition an O&#42;NET Panel on Resurvey Events
#'
#' Labels each row of an [onet_resurvey_panel()] frame with the reason it is or
#' is not part of the at-risk set for change estimation. This is the resurvey
#' denominator: comparisons only carry information about task change when the
#' occupation was actually re-rated, so estimation should restrict to the rows
#' this function marks at risk.
#'
#' @param resurvey_panel A tibble from [onet_resurvey_panel()].
#' @param at_risk_only If `TRUE`, return only the resurveyed at-risk rows. The
#'   default returns every row with its label so the excluded denominator stays
#'   visible.
#'
#' @return The input tibble with two added columns: `selection_reason`, a factor
#'   with levels `resurveyed`, `unrevisited`, `taxonomy_seam`, and `suppressed`,
#'   and `at_risk`, `TRUE` only when `selection_reason` is `resurveyed`.
#'
#' @details
#' Labels are assigned by precedence so structural exclusions win over the
#' resurvey signal:
#' 1. `taxonomy_seam` when the incoming transition crosses a seam (`seam_in`) or
#'    the row is an Analyst - Transition carry-forward. The v25.1 SOC-2010 to
#'    SOC-2018 carry-forward is never treated as a resurvey.
#' 2. `suppressed` when `recommend_suppress` is `"Y"`.
#' 3. `resurveyed` when the occupation survey clock advanced.
#' 4. `unrevisited` otherwise, including a first appearance with no prior
#'    release to compare against.
#' @export
#'
#' @examples
#' panel <- tibble::tibble(
#'   release_version = rep(c("22.1", "23.1"), each = 2),
#'   release_date = rep(as.Date(c("2017-10-01", "2018-11-01")), each = 2),
#'   soc_vintage = "2010",
#'   onet_soc_code = rep(c("15-1132.00", "29-1141.00"), 2),
#'   soc_code = rep(c("15-1132", "29-1141"), 2),
#'   task_id = rep(c("1001", "1002"), 2),
#'   scale_id = "IM",
#'   data_value = c(4.1, 4.6, 4.1, 4.8),
#'   source_date = as.Date(c("2016-07-01", "2016-07-01", "2016-07-01", "2018-07-01")),
#'   domain_source = "Incumbent"
#' )
#' rp <- onet_resurvey_panel(panel)
#' onet_condition_on_resurvey(rp)
onet_condition_on_resurvey <- function(resurvey_panel, at_risk_only = FALSE) {
  if (!is.logical(at_risk_only) || length(at_risk_only) != 1 || is.na(at_risk_only)) {
    cli::cli_abort("{.arg at_risk_only} must be `TRUE` or `FALSE`.")
  }
  required <- c(
    "resurvey_event", "seam_in", "domain_source", "recommend_suppress"
  )
  validate_columns_present(resurvey_panel, required, "resurvey_panel")

  data <- tibble::as_tibble(resurvey_panel)

  is_seam <- dplyr::coalesce(data$seam_in, FALSE) |
    is_transition_source(data$domain_source)
  is_suppressed <- is_suppressed_estimate(data$recommend_suppress)
  is_resurveyed <- dplyr::coalesce(data$resurvey_event, FALSE)

  reason <- dplyr::case_when(
    is_seam ~ "taxonomy_seam",
    is_suppressed ~ "suppressed",
    is_resurveyed ~ "resurveyed",
    TRUE ~ "unrevisited"
  )

  data$selection_reason <- factor(
    reason,
    levels = c("resurveyed", "unrevisited", "taxonomy_seam", "suppressed")
  )
  data$at_risk <- reason == "resurveyed"

  if (isTRUE(at_risk_only)) {
    data <- data |> dplyr::filter(.data$at_risk)
  }

  class(data) <- unique(c("onet_resurvey_panel", setdiff(class(resurvey_panel), "onet_resurvey_panel")))
  data
}
