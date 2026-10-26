# Research choices for the article, not an exported package measure.
# Nearest-neighbor helpers that do not depend on the kind of profile. Use them
# for rated skills, DWA shares or software links. Requires dplyr and tibble.
# This file does not call other article helper files.

as_distance_matrix <- function(distance) {
  if (inherits(distance, "dist")) {
    distance <- as.matrix(distance)
  }
  if (!is.matrix(distance) || !is.numeric(distance) ||
      nrow(distance) != ncol(distance)) {
    cli::cli_abort("Supply a square numeric distance matrix or a dist object.")
  }
  ids <- rownames(distance)
  if (is.null(ids) || !identical(ids, colnames(distance)) ||
      anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    cli::cli_abort("Distance dimnames must be identical, unique and non-missing.")
  }
  if (nrow(distance) < 2L || any(!is.finite(distance)) || any(distance < 0)) {
    cli::cli_abort(
      "Distances must be finite and non-negative for at least two occupations."
    )
  }
  if (any(diag(distance) != 0) ||
      !isTRUE(all.equal(distance, t(distance), tolerance = 1e-12))) {
    cli::cli_abort("Distances must be symmetric with a zero diagonal.")
  }
  distance
}

# Directed nearest neighbors. Rank is 1 plus the number of strictly closer
# occupations, so rank <= k is exactly the nearest set at k with ties at the
# cutoff retained. Pass digits to round distances before ranking when the
# distance carries floating point noise.
distance_neighbors <- function(distance, k_max = 10L, digits = NULL) {
  distance <- as_distance_matrix(distance)
  if (!is.numeric(k_max) || length(k_max) != 1L || is.na(k_max) ||
      k_max < 1 || k_max != round(k_max) || k_max >= nrow(distance)) {
    cli::cli_abort(
      "k_max must be a whole number from 1 to one less than the number of occupations."
    )
  }
  if (!is.null(digits)) {
    distance <- round(distance, digits)
  }
  ids <- rownames(distance)
  rows <- lapply(seq_along(ids), function(i) {
    d <- distance[i, -i]
    ranks <- rank(d, ties.method = "min")
    keep <- which(ranks <= k_max)
    keep <- keep[order(d[keep], names(d)[keep], method = "radix")]
    tibble::tibble(
      occupation = ids[[i]],
      neighbor = names(d)[keep],
      distance = unname(d[keep]),
      rank = as.integer(ranks[keep])
    )
  })
  out <- dplyr::bind_rows(rows)
  attr(out, "k_max") <- as.integer(k_max)
  out
}

check_neighbor_table <- function(x, k) {
  if (!is.data.frame(x) ||
      !all(c("occupation", "neighbor", "rank") %in% names(x))) {
    cli::cli_abort(
      "Supply neighbor tables with occupation, neighbor and rank columns."
    )
  }
  if (anyNA(x$occupation) || anyNA(x$neighbor) || anyNA(x$rank)) {
    cli::cli_abort("Neighbor tables must not contain missing values.")
  }
  if (anyDuplicated(x[c("occupation", "neighbor")])) {
    cli::cli_abort("Neighbor tables need one row per occupation and neighbor.")
  }
  k_max <- attr(x, "k_max")
  if (!is.null(k_max) && k > k_max) {
    cli::cli_abort("A neighbor table was built with a smaller k_max than k.")
  }
  invisible(x)
}

# Tie-aware Jaccard overlap of two nearest sets per occupation. Both tables
# must describe the same query occupations. Neighbors may differ in pool.
neighbor_set_overlap <- function(a, b, k) {
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1 || k != round(k)) {
    cli::cli_abort("k must be a whole number of at least 1.")
  }
  check_neighbor_table(a, k)
  check_neighbor_table(b, k)
  occupations <- sort(unique(a$occupation), method = "radix")
  if (!setequal(occupations, unique(b$occupation))) {
    cli::cli_abort("Both neighbor tables need the same query occupations.")
  }
  set_a <- a[a$rank <= k, c("occupation", "neighbor")]
  set_b <- b[b$rank <= k, c("occupation", "neighbor")]
  size_a <- dplyr::count(set_a, .data$occupation, name = "n_a")
  size_b <- dplyr::count(set_b, .data$occupation, name = "n_b")
  both <- dplyr::inner_join(
    set_a, set_b, by = c("occupation", "neighbor"), relationship = "one-to-one"
  ) |>
    dplyr::count(.data$occupation, name = "n_both")
  tibble::tibble(occupation = occupations) |>
    dplyr::left_join(size_a, by = "occupation", relationship = "one-to-one") |>
    dplyr::left_join(size_b, by = "occupation", relationship = "one-to-one") |>
    dplyr::left_join(both, by = "occupation", relationship = "one-to-one") |>
    dplyr::mutate(
      n_a = dplyr::coalesce(.data$n_a, 0L),
      n_b = dplyr::coalesce(.data$n_b, 0L),
      n_both = dplyr::coalesce(.data$n_both, 0L),
      n_union = .data$n_a + .data$n_b - .data$n_both,
      jaccard = .data$n_both / .data$n_union
    )
}

# Keep the rows of chosen query occupations and the k_max attribute. Use it to
# compare a table built on a larger pool with one built on a smaller universe.
restrict_neighbor_queries <- function(x, occupations) {
  check_neighbor_table(x, 1L)
  if (!is.character(occupations) || anyNA(occupations) ||
      anyDuplicated(occupations) || !all(occupations %in% x$occupation)) {
    cli::cli_abort(
      "Choose unique query occupations that appear in the neighbor table."
    )
  }
  out <- x[x$occupation %in% occupations, ]
  attr(out, "k_max") <- attr(x, "k_max")
  out
}

neighbor_overlap_summary <- function(overlap) {
  if (!is.data.frame(overlap) || !"jaccard" %in% names(overlap) ||
      nrow(overlap) == 0L || anyNA(overlap$jaccard)) {
    cli::cli_abort("Supply the output of neighbor_set_overlap().")
  }
  tibble::tibble(
    n_occupations = nrow(overlap),
    mean_jaccard = mean(overlap$jaccard),
    share_identical = mean(overlap$jaccard == 1),
    share_disjoint = mean(overlap$jaccard == 0)
  )
}

# Spearman correlation of all pairwise distances between two distance
# matrices over the same occupations. Descriptive only. Pass digits to round
# distances before ranking, as distance_neighbors does, so that distances that
# are equal in exact arithmetic tie.
distance_rank_correlation <- function(x, y, digits = NULL) {
  x <- as_distance_matrix(x)
  y <- as_distance_matrix(y)
  if (!setequal(rownames(x), rownames(y))) {
    cli::cli_abort("Both distance matrices need the same occupations.")
  }
  y <- y[rownames(x), rownames(x), drop = FALSE]
  pairs <- upper.tri(x)
  x_pairs <- x[pairs]
  y_pairs <- y[pairs]
  if (!is.null(digits)) {
    x_pairs <- round(x_pairs, digits)
    y_pairs <- round(y_pairs, digits)
  }
  stats::cor(x_pairs, y_pairs, method = "spearman")
}
