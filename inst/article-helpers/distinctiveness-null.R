# Curveball trades preserve both occupation sizes and activity frequencies.
trade_activity_sets <- function(sets, n_trades) {
  if (!is.list(sets) || length(sets) < 2L ||
      any(lengths(sets) == 0L) ||
      !all(vapply(sets, function(x) {
        is.character(x) && !anyNA(x) && !anyDuplicated(x)
      }, logical(1))) ||
      length(n_trades) != 1L || !is.finite(n_trades) ||
      n_trades < 0 || n_trades != floor(n_trades)) {
    stop("Supply nonempty unique character sets and a nonnegative trade count.",
         call. = FALSE)
  }
  for (iteration in seq_len(n_trades)) {
    pair <- sample.int(length(sets), 2L)
    a <- sets[[pair[[1]]]]
    b <- sets[[pair[[2]]]]
    common <- intersect(a, b)
    only_a <- setdiff(a, common)
    only_b <- setdiff(b, common)
    pool <- c(only_a, only_b)
    if (!length(only_a) || !length(only_b)) next
    selected <- sample.int(length(pool), length(only_a))
    sets[[pair[[1]]]] <- c(common, pool[selected])
    sets[[pair[[2]]]] <- c(common, pool[-selected])
  }
  sets
}

binary_activity_distances <- function(sets) {
  occupations <- names(sets)
  shared <- matrix(0, length(sets), length(sets))
  occurrences <- split(
    rep(seq_along(sets), lengths(sets)), unlist(sets, use.names = FALSE)
  )
  for (indices in occurrences) {
    shared[indices, indices] <- shared[indices, indices, drop = FALSE] + 1
  }
  sizes <- lengths(sets)
  distance <- 1 - shared / outer(sizes, sizes, pmax)
  dimnames(distance) <- list(occupations, occupations)
  distance
}
