# Rscript data-raw/occupational-software.R path/to/db_30_3_text.zip
library(dplyr)
options(cli.progress_show_after = Inf)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[[1]])) {
  stop("Supply one existing local db_30_3_text.zip.", call. = FALSE)
}
archive <- args[[1]]
expected <- "7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780"
stopifnot(identical(digest::digest(file = archive, algo = "sha256"), expected))
source(file.path("inst", "article-helpers", "occupational-distinctiveness.R"))
source(file.path("inst", "article-helpers", "distinctiveness-information.R"))
source(file.path("inst", "article-helpers", "software-distinctiveness.R"))
source(file.path("inst", "article-helpers", "occupational-neighbors.R"))
out <- file.path("inst", "extdata", "occupational-software")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
entries <- utils::unzip(archive, list = TRUE)$Name
read_raw <- function(name) {
  entry <- entries[basename(entries) == paste0(name, ".txt")]
  stopifnot(length(entry) == 1L)
  connection <- unz(archive, entry, open = "r")
  on.exit(close(connection), add = TRUE)
  tibble::as_tibble(utils::read.delim(
    connection, quote = "", check.names = FALSE, stringsAsFactors = FALSE
  ))
}
save_table <- function(data, name) {
  suffix <- if (name %in% c(
    "neighbors", "prevalence", "representation-sensitivity", "occupations"
  )) {
    ".csv.gz"
  } else {
    ".csv"
  }
  write_snapshot_csv(data, file.path(out, paste0(name, suffix)))
}
raw <- read_raw("Software Skills")
occupations <- read_raw("Occupation Data") |>
  transmute(occupation = .data[["O*NET-SOC Code"]], title = .data$Title)
content <- read_raw("Content Model Reference") |>
  transmute(item = .data[["Element ID"]], label = .data[["Element Name"]])
software <- raw |>
  transmute(
    occupation = .data[["O*NET-SOC Code"]],
    item = .data[["Workplace Example"]], category = .data[["Element ID"]],
    hot = .data[["Hot Technology"]], in_demand = .data[["In Demand"]]
  )
stopifnot(!anyNA(software), !anyDuplicated(occupations$occupation),
          !anyDuplicated(content$item),
          !anyDuplicated(software[c("occupation", "item", "category")]))
profiles <- software |>
  distinct(.data$occupation, .data$item) |>
  mutate(weight = 1)
universe <- sort(unique(profiles$occupation))
scores <- profile_tfidf(profiles, universe)
prevalence <- scores |>
  distinct(.data$item, .data$n_occupations, .data$prevalence, .data$idf) |>
  arrange(desc(.data$n_occupations), .data$item)
pairs <- binary_profile_pairs(profiles, universe)
neighbors <- profile_neighbors(pairs)
summary <- scores |>
  summarise(n_examples = n(), mean_idf = sum(.data$tf_idf),
            .by = "occupation") |>
  left_join(
    neighbors |>
      summarise(nearest_distance = min(.data$distance),
                nearest_occupations = paste(.data$neighbor, collapse = ";"),
                n_nearest_ties = n(), .by = "occupation"),
    by = "occupation", relationship = "one-to-one"
  ) |>
  left_join(occupations, by = "occupation", relationship = "one-to-one") |>
  mutate(rarity_rank = min_rank(-round(.data$mean_idf, 12))) |>
  arrange(.data$occupation)
save_table(prevalence, "prevalence")
save_table(summary, "occupations")
save_table(profile_neighbors(pairs, 10L), "neighbors")
selected_pairs <- tibble::tribble(
  ~occupation_a, ~occupation_b,
  "13-2011.00", "43-3031.00",
  "15-1252.00", "15-1253.00",
  "27-1021.00", "27-1024.00",
  "29-1141.00", "29-2061.00"
) |>
  left_join(pairs, by = c("occupation_a", "occupation_b"),
            relationship = "one-to-one") |>
  left_join(occupations, by = c("occupation_a" = "occupation"),
            relationship = "many-to-one") |>
  rename(title_a = "title") |>
  left_join(occupations, by = c("occupation_b" = "occupation"),
            relationship = "many-to-one") |>
  rename(title_b = "title")
stopifnot(!anyNA(selected_pairs))
save_table(selected_pairs, "selected-pairs")
save_table(occupations |>
             mutate(has_software = .data$occupation %in% universe), "coverage")

routes <- software |>
  distinct(.data$item, .data$category) |>
  arrange(.data$item, .data$category) |>
  mutate(route_id = sprintf("route_%05d", row_number()),
         family = sub("\\.[^.]+$", "", .data$category))
route_profiles <- software |>
  left_join(routes, by = c("item", "category"), relationship = "many-to-one") |>
  transmute(occupation = .data$occupation, item = .data$route_id, weight = 1)
route_pairs <- binary_profile_pairs(route_profiles, universe)
resolution <- bind_rows(lapply(c("route_id", "category", "family"), function(grain) {
  if (grain == "route_id") {
    grouped <- route_profiles
    distances <- route_pairs$distance
  } else {
    grouped <- route_profiles |>
      left_join(routes, by = c("item" = "route_id"),
                relationship = "many-to-one", suffix = c("", "_example")) |>
      summarise(weight = sum(.data$weight),
                .by = c("occupation", all_of(grain))) |>
      rename(item = all_of(grain))
    distances <- profile_distances(grouped, universe)$distance
  }
  mapping <- routes |>
    transmute(item = .data$route_id, group = .data[[grain]])
  tibble::tibble(
    grain = grain, n_occupations = length(universe),
    n_labels = n_distinct(grouped$item), mean_distance = mean(distances),
    information_bits = profile_partition_information(
      route_profiles, mapping, universe
    )
  )
}))
stopifnot(all(diff(resolution$mean_distance) <= 1e-10),
          all(diff(resolution$information_bits) <= 1e-10))
resolution <- resolution |>
  mutate(retained_percent = 100 * .data$information_bits /
           .data$information_bits[[1]])
save_table(resolution, "resolution")
name_mapping <- routes |>
  transmute(group = .data$item, item = .data$route_id) |>
  select(item, group)
name_information <- profile_partition_information(
  route_profiles, name_mapping, universe
)
stopifnot(name_information <= resolution$information_bits[[1]] + 1e-10)
save_table(tibble::tibble(
  name_information_bits = name_information,
  pair_information_bits = resolution$information_bits[[1]],
  category_annotation_bits = resolution$information_bits[[1]] - name_information
), "identity-information")
category_prevalence <- software |>
  distinct(.data$occupation, .data$category) |>
  count(.data$category, name = "n_occupations") |>
  left_join(content, by = c("category" = "item"), relationship = "many-to-one") |>
  mutate(prevalence = .data$n_occupations / length(universe)) |>
  arrange(desc(.data$n_occupations), .data$category)
save_table(category_prevalence, "category-prevalence")
category_profiles <- software |>
  distinct(.data$occupation, .data$category) |>
  transmute(occupation = .data$occupation, item = .data$category, weight = 1)
category_pairs <- binary_profile_pairs(category_profiles, universe)
category_scores <- profile_tfidf(category_profiles, universe) |>
  summarise(category_mean_idf = sum(.data$tf_idf), .by = "occupation") |>
  left_join(summary, by = "occupation", relationship = "one-to-one")
save_table(category_scores |>
             select(occupation, title, mean_idf, category_mean_idf) |>
             mutate(name_rank = min_rank(-round(.data$mean_idf, 12)),
                    category_rank = min_rank(-round(.data$category_mean_idf, 12))),
           "category-rarity")
sensitivity <- bind_rows(lapply(c(1L, 5L, 10L), function(k) {
  bind_rows(
    neighbor_overlap(
      pairs, pairs |> mutate(distance = .data$jaccard_distance), k
    ) |>
      mutate(comparison = "Exact-name TV versus exact-name Jaccard"),
    neighbor_overlap(pairs, category_pairs, k) |>
      mutate(comparison = "Exact-name TV versus category-presence TV")
  )
}))
save_table(sensitivity, "representation-sensitivity")
identity <- routes |>
  count(.data$item, name = "n_categories") |>
  filter(.data$n_categories > 1L) |>
  arrange(desc(.data$n_categories), .data$item)
save_table(identity, "multiple-routes")
normalization <- profiles |>
  mutate(normalized = tolower(trimws(gsub("[[:space:]]+", " ", .data$item)))) |>
  distinct(.data$occupation, .data$normalized)
save_table(tibble::tibble(
  release = "30.3", source_sha256 = expected,
  source_url = "https://www.onetcenter.org/dl_files/database/db_30_3_text.zip",
  n_rows = nrow(raw), n_occupations = length(universe),
  n_examples = nrow(prevalence), n_routes = nrow(routes),
  n_multiple_route_examples = nrow(identity),
  n_within_occupation_multiple_routes = nrow(software |>
    count(.data$occupation, .data$item) |> filter(.data$n > 1L)),
  n_name_links = nrow(profiles),
  n_normalized_examples = n_distinct(normalization$normalized),
  n_normalized_links = nrow(normalization),
  n_singleton_examples = sum(prevalence$n_occupations == 1L),
  min_inventory = min(summary$n_examples), max_inventory = max(summary$n_examples),
  weighting = "Binary exact workplace-example presence; equal occupation weights",
  resolution_identity = "Exact workplace-example and category pair; no alias merging"
), "provenance")
save_table(tibble::tibble(
  draw = "observed", n_pairs = nrow(pairs),
  mean_distance = mean(pairs$distance),
  median_nearest_distance = median(summary$nearest_distance),
  inventory_rarity_spearman = cor(summary$n_examples, summary$mean_idf,
                                 method = "spearman"),
  inventory_isolation_spearman = cor(summary$n_examples,
                                    summary$nearest_distance, method = "spearman"),
  name_category_rarity_spearman = cor(category_scores$mean_idf,
                                    category_scores$category_mean_idf,
                                    method = "spearman")
), "diagnostics")
print(utils::head(prevalence, 10))
print(resolution)
print(utils::head(summary |> arrange(.data$rarity_rank), 6))
