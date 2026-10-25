# Regenerate offline with the pinned archive; no API request is made.
library(onet2r)
library(dplyr)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[[1]])) {
  stop("Supply one existing local db_30_3_text.zip path.", call. = FALSE)
}
if (digest::digest(file = args[[1]], algo = "sha256") !=
    "7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780") {
  stop("The input does not match the pinned O*NET 30.3 archive.", call. = FALSE)
}
source(file.path("inst", "article-helpers", "occupational-distinctiveness.R"))
source(file.path("inst", "article-helpers", "distinctiveness-null.R"))
links <- onet_archive_read("30.3", "Tasks to DWAs", path = args[[1]])
hierarchy <- onet_archive_reference(
  "30.3", "GWAs to IWAs to DWAs", path = args[[1]]
) |>
  distinct(dwa_element_id, iwa_element_id, gwa_element_id)
profiles <- links |>
  distinct(onet_soc_code, dwa_element_id) |>
  transmute(occupation = onet_soc_code, item = dwa_element_id, weight = 1)
sets <- split(profiles$item, profiles$occupation)
universe <- names(sets)
frequencies <- table(unlist(sets, use.names = FALSE))
idf <- log(length(sets) / frequencies)
summarise_draw <- function(sets, draw) {
  profiles <- tibble::tibble(
    occupation = rep(names(sets), lengths(sets)),
    item = unlist(sets, use.names = FALSE), weight = 1
  ) |>
    left_join(
      hierarchy, by = c("item" = "dwa_element_id"),
      relationship = "many-to-one"
    )
  stopifnot(!anyNA(profiles))
  bind_rows(lapply(c("item", "iwa_element_id", "gwa_element_id"), function(grain) {
    if (grain == "item") {
      distances <- binary_activity_distances(sets)
    } else {
      grain_profiles <- profiles |>
        summarise(weight = sum(weight),
                  .by = c("occupation", dplyr::all_of(grain))) |>
        rename(item = dplyr::all_of(grain))
      pairs <- profile_distances(grain_profiles, universe)
      distances <- matrix(0, length(universe), length(universe))
      i <- match(pairs$occupation_a, universe)
      j <- match(pairs$occupation_b, universe)
      distances[cbind(i, j)] <- pairs$distance
      distances[cbind(j, i)] <- pairs$distance
    }
    mean_pair <- mean(distances[upper.tri(distances)])
    diag(distances) <- Inf
    tibble::tibble(
      draw = draw, grain = if (grain == "item") "dwa_element_id" else grain,
      mean_pair_distance = mean_pair,
      median_nearest_distance = median(apply(distances, 1, min)),
      mean_half_overlap_neighbors = mean(rowSums(distances <= 0.5 + 1e-12))
    )
  }))
}
set.seed(20261025)
results <- list(summarise_draw(sets, 0L))
size_checks <- list(tibble::tibble(
  draw = 0L,
  rarity_size_spearman = stats::cor(
    lengths(sets), vapply(sets, function(x) mean(idf[x]), numeric(1)),
    method = "spearman"
  )
))
for (draw in seq_len(20L)) {
  shuffled <- trade_activity_sets(sets, 20L * length(sets))
  stopifnot(
    identical(lengths(shuffled), lengths(sets)),
    identical(table(unlist(shuffled, use.names = FALSE)), frequencies)
  )
  results[[draw + 1L]] <- summarise_draw(shuffled, draw)
  size_checks[[draw + 1L]] <- tibble::tibble(
    draw = draw,
    rarity_size_spearman = stats::cor(
      lengths(shuffled),
      vapply(shuffled, function(x) mean(idf[x]), numeric(1)),
      method = "spearman"
    )
  )
}
output <- file.path("inst", "extdata", "occupational-distinctiveness")
utils::write.csv(bind_rows(results), file.path(output, "null-summary.csv"),
                 row.names = FALSE)
utils::write.csv(bind_rows(size_checks), file.path(output, "null-size-summary.csv"),
                 row.names = FALSE)
print(bind_rows(results), n = 63L)
