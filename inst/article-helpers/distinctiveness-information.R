# Statistical information under an equal-occupation, within-profile draw.
profile_information_budget <- function(data, mapping, universe) {
  profiles <- profile_shares(data, universe)
  needed <- c("item", "iwa_element_id", "gwa_element_id")
  if (!all(needed %in% names(mapping)) ||
      anyNA(mapping[needed]) || anyDuplicated(mapping$item) ||
      anyDuplicated(unique(mapping[c("iwa_element_id", "gwa_element_id")])[
        "iwa_element_id"
      ]) ||
      !all(profiles$item %in% mapping$item)) {
    stop("Each activity must have one complete nested hierarchy route.",
         call. = FALSE)
  }
  profiles <- dplyr::left_join(
    profiles, mapping[needed], by = "item", relationship = "many-to-one"
  )
  n <- length(universe)
  grains <- c("item", "iwa_element_id", "gwa_element_id")
  details <- lapply(grains, function(grain) {
    shares <- profiles |>
      dplyr::summarise(
        tf = sum(.data$share), .by = c("occupation", dplyr::all_of(grain))
      ) |>
      dplyr::rename(label_id = dplyr::all_of(grain))
    mixture <- shares |>
      dplyr::summarise(
        mixture_mass = sum(.data$tf) / n, .by = "label_id"
      )
    shares |>
      dplyr::left_join(mixture, by = "label_id", relationship = "many-to-one") |>
      dplyr::mutate(bits = .data$tf * log2(.data$tf / .data$mixture_mass))
  })
  bits <- vapply(details, function(x) sum(x$bits) / n, numeric(1))
  if (any(diff(bits) > 1e-10) || any(bits < -1e-10) ||
      any(bits > log2(n) + 1e-10)) {
    stop("Information or hierarchy conservation check failed.", call. = FALSE)
  }
  summary <- tibble::tibble(
    grain = c("dwa_element_id", grains[-1]),
    n_occupations = n,
    n_labels_with_positive_mass = vapply(
      details, function(x) dplyr::n_distinct(x$label_id), integer(1)
    ),
    information_bits = bits,
    conditional_entropy_bits = log2(n) - bits,
    lost_from_dwa_bits = bits[[1]] - bits,
    retained_dwa_percent = if (bits[[1]] > 0) 100 * bits / bits[[1]] else NA_real_
  )
  fine_loss <- details[[1]] |>
    dplyr::left_join(
      mapping[needed], by = c("label_id" = "item"),
      relationship = "many-to-one"
    ) |>
    dplyr::summarise(fine_bits = sum(.data$bits) / n, .by = "gwa_element_id")
  parent_loss <- details[[3]] |>
    dplyr::summarise(
      broad_bits = sum(.data$bits) / n,
      mixture_mass = dplyr::first(.data$mixture_mass),
      n_occupations_with_positive_mass = dplyr::n_distinct(.data$occupation),
      .by = "label_id"
    ) |>
    dplyr::rename(gwa_element_id = "label_id") |>
    dplyr::left_join(
      fine_loss, by = "gwa_element_id", relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      lost_bits = .data$fine_bits - .data$broad_bits,
      conditional_lost_bits = .data$lost_bits / .data$mixture_mass
    )
  counts <- profiles |>
    dplyr::summarise(n_dwas = dplyr::n_distinct(.data$item),
                     .by = "gwa_element_id")
  parent_loss <- parent_loss |>
    dplyr::left_join(
      counts, by = "gwa_element_id", relationship = "one-to-one"
    ) |>
    dplyr::select(-"broad_bits", -"fine_bits") |>
    dplyr::arrange(dplyr::desc(.data$lost_bits), .data$gwa_element_id)
  stopifnot(
    min(parent_loss$lost_bits) >= -1e-10,
    abs(sum(parent_loss$lost_bits) - (bits[[1]] - bits[[3]])) < 1e-10
  )
  diagnostics <- lapply(seq_along(details), function(i) {
    details[[i]] |>
      dplyr::summarise(information_bits = sum(.data$bits), .by = "occupation") |>
      dplyr::mutate(grain = summary$grain[[i]])
  }) |>
    dplyr::bind_rows()
  list(summary = summary, parent_loss = parent_loss, diagnostics = diagnostics)
}


profile_tfidf_equal_group_weights <- function(data, universe, groups) {
  if (!is.character(groups) || length(groups) != length(universe) ||
      anyNA(groups) || any(!nzchar(trimws(groups)))) {
    stop("Supply one non-missing group label per occupation.", call. = FALSE)
  }
  group_lookup <- tibble::tibble(
    occupation = universe, group = groups
  )
  group_sizes <- group_lookup |>
    dplyr::count(.data$group, name = "n_occupations") |>
    dplyr::mutate(occupation_weight = 1 / .data$n_occupations)
  shares <- profile_shares(data, universe) |>
    dplyr::left_join(group_lookup, by = "occupation", relationship = "many-to-one") |>
    dplyr::left_join(group_sizes, by = "group", relationship = "many-to-one")
  n_groups <- dplyr::n_distinct(groups)
  frequencies <- shares |>
    dplyr::summarise(
      weighted_occupation_mass = sum(.data$occupation_weight), .by = "item"
    ) |>
    dplyr::mutate(
      prevalence = .data$weighted_occupation_mass / n_groups,
      idf = log(1 / .data$prevalence)
    )
  shares |>
    dplyr::left_join(frequencies, by = "item", relationship = "many-to-one") |>
    dplyr::mutate(tf_idf = .data$share * .data$idf)
}

profile_partition_information <- function(data, mapping, universe) {
  profiles <- profile_shares(data, universe)
  if (!all(c("item", "group") %in% names(mapping)) ||
      anyNA(mapping[c("item", "group")]) || anyDuplicated(mapping$item) ||
      !all(profiles$item %in% mapping$item)) {
    stop("Each activity must have one non-missing partition label.",
         call. = FALSE)
  }
  grouped <- profiles |>
    dplyr::left_join(
      mapping[c("item", "group")], by = "item", relationship = "many-to-one"
    ) |>
    dplyr::summarise(
      profile_share = sum(.data$share), .by = c("occupation", "group")
    )
  mixture <- grouped |>
    dplyr::summarise(
      mixture_mass = sum(.data$profile_share) / length(universe), .by = "group"
    )
  grouped |>
    dplyr::left_join(mixture, by = "group", relationship = "many-to-one") |>
    dplyr::summarise(
      information_bits = sum(
        .data$profile_share * log2(.data$profile_share / .data$mixture_mass)
      ) / length(universe)
    ) |>
    dplyr::pull("information_bits")
}
