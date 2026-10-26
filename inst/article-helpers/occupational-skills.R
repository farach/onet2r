# Research choices for the article, not an exported package measure.
# Rated O*NET skills: audit, a fixed common-support universe, rating
# distributions, nearest neighbors and sensitivity checks. Requires dplyr,
# tibble and cli. Source occupational-neighbors.R first for the neighbor
# functions. This file does not rank occupations on a combined skills score.
#
# Vocabulary
#   domain  essential or transferable. Domains are never pooled.
#   scale   IM (Importance, 1 to 5) or LV (Level, 0 to 7). Scales are never
#           pooled in a distance. Every block is one domain and one scale.
#   block   es_im, es_lv, ts_im or ts_lv.
#   gap     absolute rating difference on one element, divided by the scale
#           range from the Scales Reference table. A distance is the mean gap
#           over the elements of one block, so it lies in [0, 1].

skills_blocks <- function() {
  tibble::tibble(
    block = c("es_im", "es_lv", "ts_im", "ts_lv"),
    domain = c("essential", "essential", "transferable", "transferable"),
    scale = c("IM", "LV", "IM", "LV")
  )
}

# Category labels of the Importance scale, from the O*NET analyst skills
# ratings procedures update (HumRRO, revised March 2018,
# https://www.onetcenter.org/dl_files/AOSkills_ProcUpdate.pdf). The database
# holds means over eight analysts, so a mean at a label value is not a
# category assignment.
importance_labels <- function() {
  c(
    `1` = "Not Important", `2` = "Somewhat Important", `3` = "Important",
    `4` = "Very Important", `5` = "Extremely Important"
  )
}

# Thresholds for share-at-or-above tables. Integer Importance values carry the
# category label. Level values 2, 4 and 6 carry the element-specific example
# anchors. Every other value is a grid point with no anchor.
skills_threshold_grid <- function() {
  labels <- importance_labels()
  im <- c(2, 2.5, 3, 3.5, 4, 4.5)
  lv <- 1:6
  tibble::tibble(
    scale = c(rep("IM", length(im)), rep("LV", length(lv))),
    threshold = c(im, lv),
    threshold_basis = c(
      ifelse(im %in% 2:5, "scale_category_label", "grid_point"),
      ifelse(lv %in% c(2L, 4L, 6L), "level_example_anchor", "grid_point")
    ),
    category_label = c(unname(labels[as.character(im)]), rep(NA_character_, length(lv)))
  )
}

skills_long <- function(tables) {
  domains <- c(essential = "es", transferable = "ts")
  if (!is.list(tables) || !setequal(names(tables), names(domains))) {
    cli::cli_abort("Supply a list with essential and transferable skills tables.")
  }
  needed <- c(
    "onet_soc_code", "element_id", "element_name", "scale_id", "data_value",
    "n", "standard_error", "recommend_suppress", "not_relevant"
  )
  to_number <- function(x) {
    out <- suppressWarnings(as.numeric(x))
    if (any(is.na(out) & !is.na(x))) {
      cli::cli_abort("Ratings, rater counts and standard errors must be numeric.")
    }
    out
  }
  out <- lapply(names(domains), function(domain_name) {
    x <- tables[[domain_name]]
    if (!is.data.frame(x) || !all(needed %in% names(x))) {
      cli::cli_abort("The {domain_name} table is missing required columns.")
    }
    prefix <- domains[[domain_name]]
    tibble::tibble(
      occupation = as.character(x$onet_soc_code),
      domain = domain_name,
      scale = as.character(x$scale_id),
      block = paste0(prefix, "_", tolower(as.character(x$scale_id))),
      element_id = as.character(x$element_id),
      element_name = as.character(x$element_name),
      value = to_number(x$data_value),
      n_raters = to_number(x$n),
      se = to_number(x$standard_error),
      suppress = as.character(x$recommend_suppress),
      not_relevant = as.character(x$not_relevant)
    )
  })
  out <- dplyr::bind_rows(out)
  if (!all(out$scale %in% c("IM", "LV"))) {
    cli::cli_abort("Skills tables must hold IM and LV ratings only.")
  }
  if (!all(out$suppress %in% c("Y", "N", NA)) ||
      !all(out$not_relevant %in% c("Y", "N", NA))) {
    cli::cli_abort("Flag columns must hold Y, N or missing values.")
  }
  if (anyNA(out$occupation) || anyDuplicated(out[c("occupation", "block", "element_id")])) {
    cli::cli_abort("Each occupation needs one rating per block and element.")
  }
  out
}

skills_scale_ranges <- function(scales_reference, scales = c("IM", "LV")) {
  if (!is.data.frame(scales_reference) ||
      !all(c("scale_id", "minimum", "maximum") %in% names(scales_reference))) {
    cli::cli_abort("Supply the Scales Reference table.")
  }
  x <- scales_reference[match(scales, scales_reference$scale_id), ]
  if (anyNA(x$scale_id)) {
    cli::cli_abort("The Scales Reference table must list every requested scale.")
  }
  out <- tibble::tibble(
    scale = x$scale_id,
    minimum = as.numeric(x$minimum),
    maximum = as.numeric(x$maximum)
  )
  out$range <- out$maximum - out$minimum
  if (anyNA(out$range) || any(out$range <= 0)) {
    cli::cli_abort("Each scale needs a positive range.")
  }
  out
}

safe_extreme <- function(x, fun) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) NA_real_ else fun(x)
}

# Everything that could make a rating unfit for use, counted and not repaired.
# Level auto-fill: analysts who rate Importance as 1 do not rate Level, which
# is set to 0, so a Level mean of 0 is not an unrated cell. The paired columns
# audit that rule against the data.
skills_rating_audit <- function(long, ranges) {
  importance_floor <- ranges$minimum[match("IM", ranges$scale)]
  paired <- dplyr::inner_join(
    long[long$scale == "IM", c("occupation", "domain", "element_id", "value")] |>
      dplyr::rename(importance = "value"),
    long[long$scale == "LV", c("occupation", "domain", "element_id", "value", "not_relevant")] |>
      dplyr::rename(level = "value", level_not_relevant = "not_relevant"),
    by = c("occupation", "domain", "element_id"),
    relationship = "one-to-one"
  )
  blocks <- skills_blocks()
  rows <- lapply(seq_len(nrow(blocks)), function(i) {
    x <- long[long$block == blocks$block[[i]], ]
    rng <- ranges[ranges$scale == blocks$scale[[i]], ]
    pair <- paired[paired$domain == blocks$domain[[i]], ]
    is_lv <- blocks$scale[[i]] == "LV"
    tibble::tibble(
      block = blocks$block[[i]],
      domain = blocks$domain[[i]],
      scale = blocks$scale[[i]],
      n_rows = nrow(x),
      n_occupations = dplyr::n_distinct(x$occupation),
      n_elements = dplyr::n_distinct(x$element_id),
      n_missing_value = sum(is.na(x$value)),
      value_min = safe_extreme(x$value, min),
      value_max = safe_extreme(x$value, max),
      n_outside_scale = sum(x$value < rng$minimum | x$value > rng$maximum, na.rm = TRUE),
      n_not_two_decimals = sum(abs(x$value * 100 - round(x$value * 100)) > 1e-6, na.rm = TRUE),
      n_at_scale_minimum = sum(x$value == rng$minimum, na.rm = TRUE),
      n_raters_min = safe_extreme(x$n_raters, min),
      n_raters_max = safe_extreme(x$n_raters, max),
      se_min = safe_extreme(x$se, min),
      se_max = safe_extreme(x$se, max),
      n_recommend_suppress = sum(x$suppress == "Y", na.rm = TRUE),
      n_recommend_suppress_missing = sum(is.na(x$suppress)),
      se_min_recommend_suppress = safe_extreme(x$se[x$suppress %in% "Y"], min),
      se_max_not_suppressed = safe_extreme(x$se[x$suppress %in% "N"], max),
      n_not_relevant = sum(x$not_relevant == "Y", na.rm = TRUE),
      n_not_relevant_missing = sum(is.na(x$not_relevant)),
      n_level_zero = if (is_lv) sum(pair$level == 0) else NA_integer_,
      n_level_zero_importance_at_floor = if (is_lv) {
        sum(pair$level == 0 & pair$importance == importance_floor)
      } else {
        NA_integer_
      },
      n_importance_at_floor_level_positive = if (is_lv) {
        sum(pair$importance == importance_floor & pair$level > 0)
      } else {
        NA_integer_
      },
      importance_max_when_not_relevant = if (is_lv) {
        safe_extreme(pair$importance[pair$level_not_relevant %in% "Y"], max)
      } else {
        NA_real_
      },
      level_max_when_not_relevant = if (is_lv) {
        safe_extreme(pair$level[pair$level_not_relevant %in% "Y"], max)
      } else {
        NA_real_
      }
    )
  })
  dplyr::bind_rows(rows)
}

# One row per occupation with completeness and flag counts per block. A block
# is complete when every element of that block has a rating. Elements are the
# ones that appear anywhere in the long table.
skills_eligibility <- function(long, occupations) {
  blocks <- skills_blocks()
  elements <- unique(long[c("block", "element_id")])
  expected <- table(elements$block)
  if (!all(blocks$block %in% names(expected))) {
    cli::cli_abort("Every block needs at least one element.")
  }
  counts <- long |>
    dplyr::summarise(
      n_elements = sum(!is.na(.data$value)),
      n_recommend_suppress = sum(.data$suppress %in% "Y"),
      n_not_relevant = sum(.data$not_relevant %in% "Y"),
      .by = c("occupation", "block")
    )
  out <- tibble::tibble(occupation = occupations)
  for (block in blocks$block) {
    n_expected <- as.integer(expected[[block]])
    x <- counts[counts$block == block, ]
    x <- x[match(occupations, x$occupation), ]
    n_elements <- ifelse(is.na(x$n_elements), 0L, x$n_elements)
    out[[paste0(block, "_n_elements")]] <- as.integer(n_elements)
    out[[paste0(block, "_complete")]] <- n_elements == n_expected
    out[[paste0(block, "_n_recommend_suppress")]] <- as.integer(
      ifelse(is.na(x$n_recommend_suppress), 0L, x$n_recommend_suppress)
    )
    out[[paste0(block, "_n_not_relevant")]] <- as.integer(
      ifelse(is.na(x$n_not_relevant), 0L, x$n_not_relevant)
    )
  }
  complete_cols <- paste0(blocks$block, "_complete")
  suppress_cols <- paste0(blocks$block, "_n_recommend_suppress")
  nr_cols <- paste0(blocks$block, "_n_not_relevant")
  out$complete_all_blocks <- rowSums(out[complete_cols]) == length(complete_cols)
  out$n_recommend_suppress <- as.integer(rowSums(out[suppress_cols]))
  out$n_not_relevant <- as.integer(rowSums(out[nr_cols]))
  out
}

# Universes, all inside the occupations that have DWA profiles.
#   in_pool_complete       complete in all four blocks (sensitivity pool).
#   in_common_universe     complete and no Recommend Suppress flag in any block.
#                          This is the primary skills universe.
#   in_pool_no_not_relevant, in_pool_no_flags  stricter pools for disclosure.
# Not Relevant alone never removes an occupation from the primary universe.
skills_common_universe <- function(eligibility, titles) {
  if (!is.data.frame(titles) || !all(c("onet_soc_code", "title") %in% names(titles)) ||
      !identical(sort(titles$onet_soc_code), sort(eligibility$occupation))) {
    cli::cli_abort("Titles must cover exactly the occupations in the eligibility table.")
  }
  out <- eligibility
  out$title <- titles$title[match(out$occupation, titles$onet_soc_code)]
  out$in_pool_complete <- out$complete_all_blocks
  out$in_common_universe <- out$complete_all_blocks & out$n_recommend_suppress == 0L
  out$in_pool_no_not_relevant <- out$complete_all_blocks & out$n_not_relevant == 0L
  out$in_pool_no_flags <- out$in_common_universe & out$n_not_relevant == 0L
  out$exclusion_reason <- dplyr::case_when(
    out$in_common_universe ~ NA_character_,
    !out$complete_all_blocks ~ "incomplete_skills_profile",
    .default = "recommend_suppress_flag"
  )
  names(out)[names(out) == "occupation"] <- "onet_soc_code"
  out[order(out$onet_soc_code, method = "radix"), ]
}

# Occupation by element matrices, one per block. Rows follow universe. The
# nr_floor option sets Level to 0 where Not Relevant is Y. That is a coarse
# sensitivity and not the primary treatment.
skills_profile_matrices <- function(long, universe, nr_floor = FALSE) {
  blocks <- skills_blocks()
  if (!is.character(universe) || anyNA(universe) || anyDuplicated(universe) ||
      length(universe) < 3L) {
    cli::cli_abort("Supply a character universe of at least three unique occupations.")
  }
  out <- lapply(blocks$block, function(block) {
    x <- long[long$block == block & long$occupation %in% universe, ]
    elements <- sort(unique(long$element_id[long$block == block]), method = "radix")
    if (nrow(x) != length(universe) * length(elements) || anyNA(x$value)) {
      cli::cli_abort("Every universe occupation needs a complete {block} block.")
    }
    values <- x$value
    if (nr_floor && endsWith(block, "_lv")) {
      values[x$not_relevant %in% "Y"] <- 0
    }
    m <- matrix(
      NA_real_, nrow = length(universe), ncol = length(elements),
      dimnames = list(universe, elements)
    )
    m[cbind(match(x$occupation, universe), match(x$element_id, elements))] <- values
    m
  })
  stats::setNames(out, blocks$block)
}

# Sum over elements of absolute gaps in integer hundredths. Every sum is exact
# because the inputs are whole numbers stored as doubles.
absolute_gap_sums <- function(values) {
  hundredths <- round(values * 100)
  if (max(abs(values * 100 - hundredths)) > 1e-6) {
    cli::cli_abort("Ratings must have at most two decimals for exact sums.")
  }
  as.matrix(stats::dist(hundredths, method = "manhattan"))
}

# Sum over elements of absolute differences in doubled mid-ranks within the
# universe. Doubling keeps tied ranks whole numbers, so the sums are exact.
ordinal_gap_sums <- function(values) {
  doubled <- apply(values, 2L, function(v) 2 * rank(v, ties.method = "average"))
  dimnames(doubled) <- dimnames(values)
  as.matrix(stats::dist(doubled, method = "manhattan"))
}

# Total variation distance between row-normalized profiles, rounded so that
# floating point noise cannot split mathematical ties.
composition_distances <- function(values, digits = 12L) {
  totals <- rowSums(values)
  if (any(totals <= 0)) {
    cli::cli_abort("Every occupation needs a positive total to normalize a profile.")
  }
  shares <- values / totals
  dimnames(shares) <- dimnames(values)
  round(0.5 * as.matrix(stats::dist(shares, method = "manhattan")), digits)
}

# Distance sums plus the constant that scales them to [0, 1]. The distance is
# sums / denominator. Ranking uses the exact sums, never the scaled values.
skills_block_distance <- function(values, range,
                                  kind = c("absolute", "ordinal", "composition")) {
  kind <- match.arg(kind)
  if (!is.matrix(values) || is.null(rownames(values)) || !is.numeric(range) ||
      length(range) != 1L || range <= 0) {
    cli::cli_abort("Supply a named rating matrix and a positive scale range.")
  }
  switch(
    kind,
    absolute = list(
      sums = absolute_gap_sums(values),
      denominator = 100 * ncol(values) * range
    ),
    ordinal = list(
      sums = ordinal_gap_sums(values),
      denominator = 2 * nrow(values) * ncol(values)
    ),
    composition = list(sums = composition_distances(values), denominator = 1)
  )
}

skills_neighbors <- function(block_distance, k_max = 10L, digits = 8L) {
  out <- distance_neighbors(block_distance$sums, k_max = k_max)
  out$distance <- round(out$distance / block_distance$denominator, digits)
  attr(out, "k_max") <- as.integer(k_max)
  out
}

skills_distance_matrix <- function(block_distance) {
  block_distance$sums / block_distance$denominator
}

# Item-level gaps between two occupations, one row per block and element.
# gap_normalized is the gap divided by the scale range. The mean of
# gap_normalized within a block equals the block distance.
rating_pair_gaps <- function(matrices, ranges, a, b) {
  if (!is.list(matrices) || length(matrices) == 0L || is.null(names(matrices))) {
    cli::cli_abort("Supply the output of skills_profile_matrices().")
  }
  rows <- lapply(names(matrices), function(block) {
    m <- matrices[[block]]
    if (!all(c(a, b) %in% rownames(m)) || identical(a, b)) {
      cli::cli_abort("Supply two different occupations that are in the matrices.")
    }
    scale <- toupper(sub("^[a-z]+_", "", block))
    range <- ranges$range[match(scale, ranges$scale)]
    gap <- round(abs(m[a, ] - m[b, ]), 2)
    tibble::tibble(
      block = block,
      element_id = colnames(m),
      value_a = unname(m[a, ]),
      value_b = unname(m[b, ]),
      gap = unname(gap),
      gap_normalized = unname(gap) / range
    )
  })
  dplyr::bind_rows(rows)
}

# Pairs chosen by rule, with ties broken by occupation code. The nearest,
# median and farthest pair by exact sums, one set per block.
skills_pair_selection <- function(sums) {
  upper <- which(upper.tri(sums), arr.ind = TRUE)
  pairs <- tibble::tibble(
    occupation_a = rownames(sums)[upper[, 1]],
    occupation_b = colnames(sums)[upper[, 2]],
    exact_sum = sums[upper]
  )
  pairs <- pairs[
    order(pairs$exact_sum, pairs$occupation_a, pairs$occupation_b, method = "radix"),
  ]
  pick <- c(
    nearest_pair = 1L,
    median_pair = as.integer(ceiling(nrow(pairs) / 2)),
    farthest_pair = nrow(pairs)
  )
  dplyr::bind_cols(
    tibble::tibble(pair_kind = names(pick)),
    pairs[unname(pick), ]
  )
}

skills_rating_distribution <- function(long, universe, ranges) {
  blocks <- skills_blocks()
  rows <- lapply(seq_len(nrow(blocks)), function(i) {
    x <- long[long$block == blocks$block[[i]] & long$occupation %in% universe, ]
    floor_value <- ranges$minimum[match(blocks$scale[[i]], ranges$scale)]
    is_lv <- blocks$scale[[i]] == "LV"
    x |>
      dplyr::summarise(
        n_occupations = dplyr::n(),
        mean = mean(.data$value),
        sd = stats::sd(.data$value),
        min = min(.data$value),
        p10 = unname(stats::quantile(.data$value, 0.10, type = 7)),
        p25 = unname(stats::quantile(.data$value, 0.25, type = 7)),
        median = stats::median(.data$value),
        p75 = unname(stats::quantile(.data$value, 0.75, type = 7)),
        p90 = unname(stats::quantile(.data$value, 0.90, type = 7)),
        max = max(.data$value),
        share_at_scale_minimum = mean(.data$value == floor_value),
        n_not_relevant = if (is_lv) sum(.data$not_relevant %in% "Y") else NA_integer_,
        .by = c("element_id", "element_name")
      ) |>
      dplyr::mutate(
        block = blocks$block[[i]], domain = blocks$domain[[i]],
        scale = blocks$scale[[i]], .before = 1L
      ) |>
      dplyr::arrange(.data$element_id)
  })
  dplyr::bind_rows(rows)
}

# Share of occupations whose rating is at or above each threshold, with the
# threshold basis stated. The comparison is always >=. Not Relevant variants
# apply to Level only because Importance rows carry no flag.
skills_threshold_table <- function(long, universe, pool, anchors) {
  grid <- skills_threshold_grid()
  blocks <- skills_blocks()
  anchor_text <- stats::setNames(
    anchors$anchor_description, paste(anchors$element_id, anchors$anchor_value)
  )
  hundredths <- function(data) split(round(data$value * 100), data$element_id)
  rows <- lapply(seq_len(nrow(blocks)), function(i) {
    is_lv <- blocks$scale[[i]] == "LV"
    x <- long[long$block == blocks$block[[i]], ]
    primary <- x[x$occupation %in% universe, ]
    primary_values <- hundredths(primary)
    pool_values <- hundredths(x[x$occupation %in% pool, ])
    elements <- unique(primary[c("element_id", "element_name")])
    out <- dplyr::cross_join(elements, grid[grid$scale == blocks$scale[[i]], ])
    out <- out[order(out$element_id, out$threshold, method = "radix"), ]
    n_in <- function(values) {
      vapply(out$element_id, function(e) length(values[[e]]), integer(1), USE.NAMES = FALSE)
    }
    n_at <- function(values) {
      vapply(
        seq_len(nrow(out)),
        function(j) sum(values[[out$element_id[[j]]]] >= round(out$threshold[[j]] * 100)),
        integer(1)
      )
    }
    out$n_occupations <- n_in(primary_values)
    out$n_at_or_above <- n_at(primary_values)
    out$share_at_or_above <- out$n_at_or_above / out$n_occupations
    out$n_pool_complete <- n_in(pool_values)
    out$share_at_or_above_pool_complete <- n_at(pool_values) / out$n_pool_complete
    if (is_lv) {
      relevant_values <- hundredths(primary[!primary$not_relevant %in% "Y", ])
      floored <- primary
      floored$value[floored$not_relevant %in% "Y"] <- 0
      n_relevant <- n_in(relevant_values)
      out$n_not_relevant <- out$n_occupations - n_relevant
      out$n_at_or_above_excluding_not_relevant <- n_at(relevant_values)
      out$share_at_or_above_excluding_not_relevant <- ifelse(
        n_relevant > 0L, out$n_at_or_above_excluding_not_relevant / n_relevant, NA_real_
      )
      out$share_at_or_above_nr_floor <- n_at(hundredths(floored)) / out$n_occupations
    } else {
      out$n_not_relevant <- NA_integer_
      out$n_at_or_above_excluding_not_relevant <- NA_integer_
      out$share_at_or_above_excluding_not_relevant <- NA_real_
      out$share_at_or_above_nr_floor <- NA_real_
    }
    out$threshold_label <- dplyr::case_when(
      out$threshold_basis == "scale_category_label" ~ out$category_label,
      out$threshold_basis == "level_example_anchor" ~
        unname(anchor_text[paste(out$element_id, out$threshold)]),
      .default = NA_character_
    )
    out$block <- blocks$block[[i]]
    out$domain <- blocks$domain[[i]]
    out$comparison <- ">="
    out
  })
  out <- dplyr::bind_rows(rows)
  out[c(
    "block", "domain", "scale", "element_id", "element_name", "threshold",
    "comparison", "threshold_basis", "threshold_label", "n_occupations",
    "n_at_or_above", "share_at_or_above", "n_not_relevant",
    "n_at_or_above_excluding_not_relevant",
    "share_at_or_above_excluding_not_relevant",
    "share_at_or_above_nr_floor", "n_pool_complete",
    "share_at_or_above_pool_complete"
  )]
}

# Level anchors for the skills elements only. Every skill element has the
# example anchors at Level 2, 4 and 6.
skills_level_anchors <- function(anchors, elements) {
  needed <- c("element_id", "element_name", "scale_id", "anchor_value", "anchor_description")
  if (!is.data.frame(anchors) || !all(needed %in% names(anchors))) {
    cli::cli_abort("Supply the Level Scale Anchors table.")
  }
  x <- anchors[anchors$scale_id == "LV" & anchors$element_id %in% elements, ]
  out <- tibble::tibble(
    element_id = as.character(x$element_id),
    element_name = as.character(x$element_name),
    scale = as.character(x$scale_id),
    anchor_value = as.integer(x$anchor_value),
    anchor_description = as.character(x$anchor_description)
  )
  out <- out[order(out$element_id, out$anchor_value, method = "radix"), ]
  per_element <- table(out$element_id)
  if (!setequal(names(per_element), elements) || any(per_element != 3L) ||
      !all(out$anchor_value %in% c(2L, 4L, 6L))) {
    cli::cli_abort("Each skill element needs exactly the Level 2, 4 and 6 anchors.")
  }
  out
}

# Overlap of nearest sets at several k for one pair of neighbor tables, one row
# per occupation. Descriptive only. It carries no significance statement.
skills_neighbor_overlap <- function(a, b, ks = c(1L, 5L, 10L)) {
  out <- NULL
  for (k in ks) {
    x <- neighbor_set_overlap(a, b, k)[c("occupation", "jaccard")]
    names(x)[2] <- paste0("jaccard_k", k)
    out <- if (is.null(out)) x else dplyr::left_join(
      out, x, by = "occupation", relationship = "one-to-one"
    )
  }
  out
}

skills_overlap_summary <- function(overlap) {
  columns <- grep("^jaccard_k", names(overlap), value = TRUE)
  if (length(columns) == 0L) {
    cli::cli_abort("Supply the output of skills_neighbor_overlap().")
  }
  rows <- lapply(columns, function(column) {
    x <- overlap[[column]]
    tibble::tibble(
      k = as.integer(sub("^jaccard_k", "", column)),
      metric = c("n_query_occupations", "mean_jaccard", "share_identical", "share_disjoint"),
      value = c(length(x), mean(x), mean(x == 1), mean(x == 0))
    )
  })
  dplyr::bind_rows(rows)
}
