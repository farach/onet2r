binary_profile_pairs <- function(data, universe) {
  shares <- profile_shares(data, universe)
  if (any(data$weight != 1)) {
    cli::cli_abort("Binary comparisons require one unit per occupation-item link.")
  }
  items <- sort(unique(shares$item))
  incidence <- matrix(0, length(universe), length(items))
  incidence[cbind(match(shares$occupation, universe),
                  match(shares$item, items))] <- 1
  overlap <- tcrossprod(incidence)
  sizes <- rowSums(incidence)
  indices <- which(upper.tri(overlap), arr.ind = TRUE)
  shared <- overlap[indices]
  size_a <- sizes[indices[, 1]]
  size_b <- sizes[indices[, 2]]
  tibble::tibble(
    occupation_a = universe[indices[, 1]],
    occupation_b = universe[indices[, 2]],
    n_a = as.integer(size_a), n_b = as.integer(size_b),
    shared = as.integer(shared),
    only_a = as.integer(size_a - shared),
    only_b = as.integer(size_b - shared),
    distance = 1 - shared / pmax(size_a, size_b),
    jaccard_distance = (size_a + size_b - 2 * shared) /
      (size_a + size_b - shared),
    containment_a_in_b = shared / size_a,
    containment_b_in_a = shared / size_b,
    sorensen_total = (size_a + size_b - 2 * shared) / (size_a + size_b),
    replacement_component = pmin(size_a - shared, size_b - shared) /
      (shared + pmin(size_a - shared, size_b - shared)),
    nestedness_resultant_component = sorensen_total - replacement_component
  )
}

profile_neighbors <- function(pairs, k = 1L) {
  if (length(k) != 1L || !is.numeric(k) || is.na(k) || !is.finite(k) ||
      k < 1 || k != as.integer(k)) {
    cli::cli_abort("Supply one positive integer neighborhood size.")
  }
  required <- c("occupation_a", "occupation_b", "distance")
  if (!is.data.frame(pairs) || !all(required %in% names(pairs)) ||
      anyNA(pairs[required]) || any(!is.finite(pairs$distance)) ||
      any(pairs$distance < 0) ||
      any(pairs$occupation_a == pairs$occupation_b) ||
      anyDuplicated(pairs[c("occupation_a", "occupation_b")])) {
    cli::cli_abort("Supply unique, finite, non-self occupation pairs.")
  }
  directed <- dplyr::bind_rows(
    dplyr::transmute(pairs, occupation = .data$occupation_a,
                     neighbor = .data$occupation_b, distance = .data$distance),
    dplyr::transmute(pairs, occupation = .data$occupation_b,
                     neighbor = .data$occupation_a, distance = .data$distance)
  )
  directed |>
    dplyr::mutate(
      distance = round(.data$distance, 12),
      .by = "occupation"
    ) |>
    dplyr::mutate(
      neighbor_rank = dplyr::min_rank(.data$distance), .by = "occupation"
    ) |>
    dplyr::filter(.data$neighbor_rank <= k) |>
    dplyr::arrange(.data$occupation, .data$distance, .data$neighbor)
}

neighbor_overlap <- function(pairs_a, pairs_b, k = 5L) {
  a <- profile_neighbors(pairs_a, k) |>
    dplyr::rename(rank = "neighbor_rank")
  b <- profile_neighbors(pairs_b, k) |>
    dplyr::rename(rank = "neighbor_rank")
  neighbor_set_overlap(a, b, k) |>
    dplyr::transmute(
      occupation = .data$occupation, n_a = .data$n_a, n_b = .data$n_b,
      n_shared = .data$n_both, neighbor_jaccard = .data$jaccard,
      k = as.integer(k)
    )
}
