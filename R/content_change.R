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
pair_seam <- function(from_date, to_date, from_vintage, to_vintage,
                      seams = onet_known_seams()) {
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

content_change_pair <- function(from_tasks, to_tasks, item, from_meta, to_meta,
                                seams = onet_known_seams()) {
  join_keys <- c(
    "onet_soc_code",
    item,
    intersect("scale_id", names(from_tasks))
  )
  a <- from_tasks |>
    dplyr::select(
      dplyr::all_of(join_keys),
      from_value = "data_value"
    )
  b <- to_tasks |>
    dplyr::select(
      dplyr::all_of(join_keys), to_value = "data_value",
      to_title = "title"
    )

  both <- intersect(unique(a$onet_soc_code), unique(b$onet_soc_code))
  if (length(both) == 0) {
    return(empty_content_change())
  }

  joined <- dplyr::full_join(a, b, by = join_keys) |>
    dplyr::filter(.data$onet_soc_code %in% both)
  in_a <- !is.na(joined$from_value)
  in_b <- !is.na(joined$to_value)
  from_fill <- dplyr::coalesce(joined$from_value, 0)
  to_fill <- dplyr::coalesce(joined$to_value, 0)
  retained_delta <- dplyr::if_else(
    in_a & in_b, joined$to_value - joined$from_value, NA_real_
  )

  occupation <- group_index(joined["onet_soc_code"])
  n_from <- group_sum(in_a, occupation)
  n_to <- group_sum(in_b, occupation)
  n_retained <- group_sum(in_a & in_b, occupation)
  n_dropped <- group_sum(in_a & !in_b, occupation)
  n_added <- group_sum(!in_a & in_b, occupation)
  n_union <- n_retained + n_dropped + n_added
  dot <- group_sum(from_fill * to_fill, occupation)
  norm_from <- sqrt(group_sum(from_fill^2, occupation))
  norm_to <- sqrt(group_sum(to_fill^2, occupation))
  onet_soc_code <- joined$onet_soc_code[occupation$first]

  seam_type <- pair_seam(
    from_meta$release_date, to_meta$release_date,
    from_meta$soc_vintage, to_meta$soc_vintage,
    seams = seams
  )

  tibble::tibble(
    onet_soc_code = onet_soc_code,
    soc_code = standardize_soc_code(onet_soc_code),
    title = group_first_present(joined$to_title, occupation),
    from_release = from_meta$release_version,
    to_release = to_meta$release_version,
    from_release_date = from_meta$release_date,
    to_release_date = to_meta$release_date,
    soc_vintage_from = factor(as.character(from_meta$soc_vintage), levels = onet_vintage_levels),
    soc_vintage_to = factor(as.character(to_meta$soc_vintage), levels = onet_vintage_levels),
    n_from = as.integer(n_from),
    n_to = as.integer(n_to),
    n_added = as.integer(n_added),
    n_dropped = as.integer(n_dropped),
    n_retained = as.integer(n_retained),
    jaccard = dplyr::if_else(n_union > 0, n_retained / n_union, NA_real_),
    churn_rate = dplyr::if_else(
      n_union > 0, (n_dropped + n_added) / n_union, NA_real_
    ),
    rating_delta_l2 = sqrt(
      group_sum(retained_delta^2, occupation, na.rm = TRUE)
    ),
    cosine = dplyr::if_else(
      norm_from > 0 & norm_to > 0,
      dot / (norm_from * norm_to),
      NA_real_
    ),
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
#' seam-aware: comparisons that cross the v25.1 SOC-2010 to SOC-2018 change are
#' flagged by default so taxonomy churn is not counted as content churn. v21.0
#' is not a package-verified default seam; supply a custom `seams` row for a
#' channel-specific v21.0 comparison.
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
#'   dropped before set membership is computed. Use it to apply an Importance
#'   floor for any channel-specific artifact a caller has external evidence
#'   for, for example near a specific release boundary.
#' @param from,to Optional release versions selecting a single comparison. When
#'   both are supplied only that pair is returned; otherwise every adjacent
#'   release pair is compared in release-date order.
#' @param seams Optional data frame with `seam_type` and `seam_date` columns that
#'   overrides the default seam table returned by `onet_known_seams()`, which
#'   currently contains only the verified v25.1 SOC-2010 to SOC-2018 taxonomy
#'   seam. Use it to supply channel-specific or source-specific seam dates,
#'   such as a v21.0 row, when a caller has external evidence that a
#'   comparison spanning that date needs seam treatment; v21.0 is not a
#'   package-verified default seam. Supply an empty table to disable
#'   date-based seams entirely. `NULL` keeps the default table, so Task
#'   Ratings output is unchanged. Cross-vintage SOC seams are always detected
#'   from `soc_vintage` regardless of this table.
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
#' The filtered input must contain at most one row per occupation, item, release,
#' and scale. Duplicate keys are rejected rather than resolved by row order.
#' When `scale = NULL` and `scale_id` is present, item-scale combinations define
#' membership so ratings from different scales are not joined to each other.
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
    to = NULL,
    seams = NULL) {
  validate_single_string(item, "item")
  seam_table <- resolve_seams(seams)
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

  grain <- c("onet_soc_code", item, "release_version")
  if ("scale_id" %in% names(data)) {
    grain <- c(grain, "scale_id")
  }
  duplicates <- duplicate_key_rows(data, grain)
  if (nrow(duplicates) > 0) {
    cli::cli_abort(c(
      "{.arg panel} must contain at most one row per occupation, item, release, and scale after filtering.",
      "x" = "Duplicate key{?s}: {format_key_rows(duplicates, grain)}.",
      "i" = "Remove the duplicate rows or aggregate them explicitly before calling {.fun onet_content_change}."
    ))
  }

  tasks <- data |>
    dplyr::select(
      "onet_soc_code", "release_version", dplyr::all_of(item),
      dplyr::any_of("scale_id"), "data_value", "title"
    )

  rel_meta <- data |>
    dplyr::distinct(.data$release_version, .data$release_date, .data$soc_vintage) |>
    dplyr::arrange(.data$release_date, .data$release_version)

  pairs <- content_change_pairs(rel_meta, from, to)
  if (nrow(pairs) == 0) {
    return(empty_content_change())
  }

  release_rows <- split(
    seq_len(nrow(tasks)),
    as.character(tasks$release_version)
  )
  release_tasks <- function(release) {
    vctrs::vec_slice(tasks, release_rows[[as.character(release)]])
  }

  purrr::map(seq_len(nrow(pairs)), \(i) {
    content_change_pair(
      from_tasks = release_tasks(pairs$from[[i]]),
      to_tasks = release_tasks(pairs$to[[i]]),
      item = item,
      from_meta = rel_meta[rel_meta$release_version == pairs$from[[i]], ][1, ],
      to_meta = rel_meta[rel_meta$release_version == pairs$to[[i]], ][1, ],
      seams = seam_table
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
