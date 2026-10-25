# Research choices for the article, not an exported package measure.
write_snapshot_csv <- function(data, path, na = "") {
  connection <- base::file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  utils::write.csv(
    data, connection, row.names = FALSE, na = na, eol = "\n"
  )
  invisible(path)
}

profile_shares <- function(data, universe) {
  required <- c("occupation", "item", "weight")
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    cli::cli_abort("Supply a data frame with occupation, item, and weight columns.")
  }
  if (!is.character(universe) || length(universe) < 2L ||
      anyNA(universe) || any(!nzchar(trimws(universe))) ||
      anyDuplicated(universe)) {
    cli::cli_abort("Supply at least two unique, non-missing occupation codes.")
  }
  if (!is.character(data$occupation) || !is.character(data$item) ||
      anyNA(data$occupation) || anyNA(data$item) ||
      any(!nzchar(trimws(data$occupation))) ||
      any(!nzchar(trimws(data$item)))) {
    cli::cli_abort("Occupation and item keys must be non-missing character values.")
  }
  if (!is.numeric(data$weight) || any(!is.finite(data$weight)) ||
      any(data$weight < 0)) {
    cli::cli_abort("Weights must be finite, non-negative numbers.")
  }
  if (anyDuplicated(data[c("occupation", "item")])) {
    cli::cli_abort("Resolve duplicate occupation-item keys before scoring.")
  }
  if (any(!data$occupation %in% universe)) {
    cli::cli_abort("Every input occupation must belong to the declared universe.")
  }
  totals <- data |>
    dplyr::summarise(total = sum(.data$weight), .by = "occupation")
  if (any(!universe %in% totals$occupation) ||
      any(!is.finite(totals$total)) || any(totals$total <= 0)) {
    cli::cli_abort("Every universe occupation needs a positive, observed profile.")
  }
  data |>
    dplyr::select(dplyr::all_of(required)) |>
    dplyr::filter(.data$weight > 0) |>
    dplyr::left_join(totals, by = "occupation", relationship = "many-to-one") |>
    dplyr::mutate(share = .data$weight / .data$total) |>
    dplyr::select(-"total")
}

profile_tfidf <- function(data, universe) {
  shares <- profile_shares(data, universe)
  frequencies <- shares |>
    dplyr::count(.data$item, name = "n_occupations") |>
    dplyr::mutate(
      prevalence = .data$n_occupations / length(universe),
      idf = log(length(universe) / .data$n_occupations)
    )
  shares |>
    dplyr::left_join(frequencies, by = "item", relationship = "many-to-one") |>
    dplyr::mutate(tf_idf = .data$share * .data$idf)
}

profile_distances <- function(data, universe) {
  shares <- profile_shares(data, universe)
  items <- sort(unique(shares$item))
  values <- matrix(
    0,
    nrow = length(universe),
    ncol = length(items),
    dimnames = list(universe, items)
  )
  values[cbind(
    match(shares$occupation, universe),
    match(shares$item, items)
  )] <- shares$share
  distances <- as.matrix(stats::dist(values, method = "manhattan")) / 2
  indices <- which(upper.tri(distances), arr.ind = TRUE)
  tibble::tibble(
    occupation_a = universe[indices[, 1]],
    occupation_b = universe[indices[, 2]],
    distance = distances[indices]
  )
}
