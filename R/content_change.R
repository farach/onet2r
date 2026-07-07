# =============================================================================
# O*NET content-change metrics
# =============================================================================

empty_content_change <- function() {
  tibble::tibble(
    onet_soc_code = character(),
    soc_code = character(),
    title = character(),
    from_release = character(),
    to_release = character(),
    from_release_date = as.Date(character()),
    to_release_date = as.Date(character()),
    soc_vintage_from = factor(character(), levels = onet_vintage_levels),
    soc_vintage_to = factor(character(), levels = onet_vintage_levels),
    n_from = integer(),
    n_to = integer(),
    n_added = integer(),
    n_dropped = integer(),
    n_retained = integer(),
    jaccard = double(),
    churn_rate = double(),
    rating_delta_l2 = double(),
    cosine = double(),
    seam = logical(),
    seam_type = character(),
    safely_comparable = logical()
  )
}

# Seam type for one from -> to release pair, or NA when the pair is seam-free.
pair_seam <- function(from_date, to_date, from_vintage, to_vintage) {
  seams <- onet_known_seams()
  crossed <- seams$seam_type[seams$seam_date > from_date & seams$seam_date <= to_date]
  if (length(crossed) > 0) {
    return(crossed[[1]])
  }
  if (!is.na(from_vintage) && !is.na(to_vintage) &&
    as.character(from_vintage) != as.character(to_vintage)) {
    return("soc_seam")
  }
  NA_character_
}

content_change_pair <- function(tasks, item, from_meta, to_meta) {
  a <- tasks |>
    dplyr::filter(.data$release_version == from_meta$release_version) |>
    dplyr::select("onet_soc_code", dplyr::all_of(item), from_value = "data_value")
  b <- tasks |>
    dplyr::filter(.data$release_version == to_meta$release_version) |>
    dplyr::select(
      "onet_soc_code", dplyr::all_of(item), to_value = "data_value",
      to_title = "title"
    )

  both <- intersect(unique(a$onet_soc_code), unique(b$onet_soc_code))
  if (length(both) == 0) {
    return(empty_content_change())
  }

  joined <- dplyr::full_join(a, b, by = c("onet_soc_code", item)) |>
    dplyr::filter(.data$onet_soc_code %in% both) |>
    dplyr::mutate(
      in_a = !is.na(.data$from_value),
      in_b = !is.na(.data$to_value),
      from_fill = dplyr::coalesce(.data$from_value, 0),
      to_fill = dplyr::coalesce(.data$to_value, 0),
      retained_delta = dplyr::if_else(
        .data$in_a & .data$in_b, .data$to_value - .data$from_value, NA_real_
      )
    )

  metrics <- joined |>
    dplyr::summarise(
      title = dplyr::first(.data$to_title[!is.na(.data$to_title)]),
      n_from = sum(.data$in_a),
      n_to = sum(.data$in_b),
      n_retained = sum(.data$in_a & .data$in_b),
      n_dropped = sum(.data$in_a & !.data$in_b),
      n_added = sum(!.data$in_a & .data$in_b),
      .l2 = sqrt(sum(.data$retained_delta^2, na.rm = TRUE)),
      .dot = sum(.data$from_fill * .data$to_fill),
      .norm_from = sqrt(sum(.data$from_fill^2)),
      .norm_to = sqrt(sum(.data$to_fill^2)),
      .by = "onet_soc_code"
    ) |>
    dplyr::mutate(
      n_union = .data$n_retained + .data$n_dropped + .data$n_added,
      jaccard = dplyr::if_else(.data$n_union > 0, .data$n_retained / .data$n_union, NA_real_),
      churn_rate = dplyr::if_else(
        .data$n_union > 0, (.data$n_dropped + .data$n_added) / .data$n_union, NA_real_
      ),
      rating_delta_l2 = .data$.l2,
      cosine = dplyr::if_else(
        .data$.norm_from > 0 & .data$.norm_to > 0,
        .data$.dot / (.data$.norm_from * .data$.norm_to),
        NA_real_
      )
    )

  seam_type <- pair_seam(
    from_meta$release_date, to_meta$release_date,
    from_meta$soc_vintage, to_meta$soc_vintage
  )

  tibble::tibble(
    onet_soc_code = metrics$onet_soc_code,
    soc_code = standardize_soc_code(metrics$onet_soc_code),
    title = metrics$title,
    from_release = from_meta$release_version,
    to_release = to_meta$release_version,
    from_release_date = from_meta$release_date,
    to_release_date = to_meta$release_date,
    soc_vintage_from = factor(as.character(from_meta$soc_vintage), levels = onet_vintage_levels),
    soc_vintage_to = factor(as.character(to_meta$soc_vintage), levels = onet_vintage_levels),
    n_from = as.integer(metrics$n_from),
    n_to = as.integer(metrics$n_to),
    n_added = as.integer(metrics$n_added),
    n_dropped = as.integer(metrics$n_dropped),
    n_retained = as.integer(metrics$n_retained),
    jaccard = metrics$jaccard,
    churn_rate = metrics$churn_rate,
    rating_delta_l2 = metrics$rating_delta_l2,
    cosine = metrics$cosine,
    seam = !is.na(seam_type),
    seam_type = seam_type,
    safely_comparable = is.na(seam_type)
  )
}

#' Measure O&#42;NET Content Change Between Releases
#'
#' Computes task-set and rating content-change metrics for each occupation
#' between O&#42;NET releases. This is the single source of content metrics for
#' longitudinal work, so robustness measures cannot silently diverge. It is
#' seam-aware: comparisons that cross the v21.0 Task Relevance retirement or the
#' v25.1 SOC-2010 to SOC-2018 change are flagged so taxonomy churn is not
#' counted as content churn.
#'
#' @param panel A tibble from [onet_panel()] (a Task Ratings panel) or the same
#'   schema. Must contain `onet_soc_code`, `release_version`, `release_date`,
#'   `soc_vintage`, `data_value`, and the `item` key column. `scale_id` is
#'   required when `scale` is not `NULL`.
#' @param scale Optional scale id used for the rating metrics and item
#'   membership, for example `"IM"` for Importance. Use `NULL` to keep every
#'   scale.
#' @param item Column that identifies the content item within an occupation.
#'   Defaults to `"task_id"`. Item membership drives the set metrics.
#' @param min_importance Optional numeric floor applied to `data_value` on the
#'   selected `scale`. Items below the floor, or with missing `data_value`, are
#'   dropped before set membership is computed. Use it to apply the Importance
#'   filter that removes the post-v21.0 Task Relevance artifact.
#' @param from,to Optional release versions selecting a single comparison. When
#'   both are supplied only that pair is returned; otherwise every adjacent
#'   release pair is compared in release-date order.
#'
#' @return A tibble with one row per occupation and release pair:
#'   `n_from`, `n_to`, `n_added`, `n_dropped`, `n_retained`; `jaccard`
#'   (set similarity, the retained share of the item union); `churn_rate`
#'   (`1 - jaccard`); `rating_delta_l2` (the Euclidean norm of `data_value`
#'   changes over retained items); `cosine` (cosine similarity of the
#'   `data_value` vectors over the item union, missing items filled with zero);
#'   and the seam flags `seam`, `seam_type`, and `safely_comparable`.
#'
#' @details
#' Set metrics compare the `item` keys an occupation carries in each release.
#' `rating_delta_l2` uses only retained items, so it measures rating drift among
#' tasks that persist; `cosine` uses the union with zero fill, so it responds to
#' both membership turnover and rating change. Seam-crossing pairs still receive
#' metrics but are marked `safely_comparable = FALSE`; include them only in a
#' clearly labeled seam-inclusive row. Occupations are matched on
#' `onet_soc_code`, so cross-taxonomy pairs generally share few codes and should
#' be bridged with [onet_crosswalk_bridge()] before interpretation.
#' @export
#'
#' @examples
#' panel <- tibble::tibble(
#'   release_version = c(rep("22.1", 3), rep("23.1", 3)),
#'   release_date = c(rep(as.Date("2017-10-01"), 3), rep(as.Date("2018-11-01"), 3)),
#'   soc_vintage = "2010",
#'   onet_soc_code = "15-1132.00",
#'   task_id = c("1", "2", "3", "1", "2", "4"),
#'   scale_id = "IM",
#'   data_value = c(4.0, 3.5, 2.0, 4.5, 3.5, 3.0)
#' )
#' onet_content_change(panel)
onet_content_change <- function(
    panel,
    scale = "IM",
    item = "task_id",
    min_importance = NULL,
    from = NULL,
    to = NULL) {
  validate_single_string(item, "item")
  if (!is.null(scale)) {
    validate_single_string(scale, "scale")
  }
  if (!is.null(min_importance)) {
    if (!is.numeric(min_importance) || length(min_importance) != 1 ||
      is.na(min_importance)) {
      cli::cli_abort("{.arg min_importance} must be a single number or `NULL`.")
    }
  }
  if (!is.null(from)) {
    validate_single_string(from, "from")
  }
  if (!is.null(to)) {
    validate_single_string(to, "to")
  }

  required <- c(
    "onet_soc_code", "release_version", "release_date", "soc_vintage",
    "data_value", item
  )
  validate_columns_present(panel, required, "panel")

  data <- tibble::as_tibble(panel)
  if (!is.null(scale)) {
    validate_columns_present(data, "scale_id", "panel")
    data <- data |>
      dplyr::filter(as.character(.data$scale_id) == scale)
  }
  if (!"title" %in% names(data)) {
    data$title <- NA_character_
  }
  data$data_value <- parse_onet_number(data$data_value)
  data$release_date <- as.Date(data$release_date)

  if (!is.null(min_importance)) {
    data <- data |>
      dplyr::filter(!is.na(.data$data_value) & .data$data_value >= min_importance)
  }

  if (nrow(data) == 0) {
    return(empty_content_change())
  }

  tasks <- data |>
    dplyr::distinct(
      .data$onet_soc_code, .data$release_version, .data[[item]],
      .keep_all = TRUE
    ) |>
    dplyr::select(
      "onet_soc_code", "release_version", dplyr::all_of(item),
      "data_value", "title"
    )

  rel_meta <- data |>
    dplyr::distinct(.data$release_version, .data$release_date, .data$soc_vintage) |>
    dplyr::arrange(.data$release_date, .data$release_version)

  pairs <- content_change_pairs(rel_meta, from, to)
  if (nrow(pairs) == 0) {
    return(empty_content_change())
  }

  purrr::map(seq_len(nrow(pairs)), \(i) {
    content_change_pair(
      tasks = tasks,
      item = item,
      from_meta = rel_meta[rel_meta$release_version == pairs$from[[i]], ][1, ],
      to_meta = rel_meta[rel_meta$release_version == pairs$to[[i]], ][1, ]
    )
  }) |>
    purrr::list_rbind()
}

# Resolve the from -> to release pairs to compare.
content_change_pairs <- function(rel_meta, from, to) {
  versions <- rel_meta$release_version

  if (!is.null(from) || !is.null(to)) {
    if (is.null(from) || is.null(to)) {
      cli::cli_abort("Supply both {.arg from} and {.arg to}, or neither.")
    }
    missing <- setdiff(c(from, to), versions)
    if (length(missing) > 0) {
      cli::cli_abort("Version{?s} not present in {.arg panel}: {.val {missing}}.")
    }
    return(tibble::tibble(from = from, to = to))
  }

  if (length(versions) < 2) {
    return(tibble::tibble(from = character(), to = character()))
  }

  tibble::tibble(
    from = versions[-length(versions)],
    to = versions[-1]
  )
}
