# Regenerate the article's derived public-data snapshot without an API key.
# Run from the repository root after installing the current package:
# Rscript data-raw\occupational-distinctiveness.R path\to\db_30_3_text.zip
library(onet2r)
library(dplyr)
options(cli.progress_show_after = Inf)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[[1]])) {
  stop("Supply one existing local db_30_3_text.zip path.", call. = FALSE)
}
archive_path <- args[[1]]
source(file.path("inst", "article-helpers", "occupational-distinctiveness.R"))
output_dir <- file.path("inst", "extdata", "occupational-distinctiveness")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

read_table <- function(table) {
  onet_archive_read("30.3", table, path = archive_path)
}
tasks <- read_table("Task Statements")
links <- read_table("Tasks to DWAs")
occupations <- read_table("Occupation Data") |>
  distinct(onet_soc_code, title)
hierarchy <- onet_archive_reference(
  "30.3", "GWAs to IWAs to DWAs", path = archive_path
)
lookup <- hierarchy |>
  distinct(dwa_element_id, dwa_element_name, iwa_element_id, gwa_element_id)
stopifnot(
  !anyDuplicated(lookup$dwa_element_id),
  !anyNA(lookup),
  !anyNA(links$dwa_element_id),
  !anyNA(links$onet_soc_code),
  !anyDuplicated(occupations$onet_soc_code)
)
profiles <- links |>
  distinct(onet_soc_code, dwa_element_id) |>
  transmute(occupation = onet_soc_code, item = dwa_element_id, weight = 1)
universe <- sort(unique(profiles$occupation))
scores <- profile_tfidf(profiles, universe)
prevalence <- scores |>
  distinct(item, n_occupations, prevalence, idf) |>
  left_join(
    lookup, by = c("item" = "dwa_element_id"),
    relationship = "one-to-one"
  ) |>
  arrange(desc(prevalence), item)
stopifnot(!anyNA(prevalence$dwa_element_name))

distances <- profile_distances(profiles, universe)
resolution_summary <- bind_rows(lapply(
  c("dwa_element_id", "iwa_element_id", "gwa_element_id"),
  function(grain) {
    if (grain == "dwa_element_id") {
      grain_distances <- distances
    } else {
      broad_profiles <- profiles |>
        left_join(
          lookup, by = c("item" = "dwa_element_id"),
          relationship = "many-to-one"
        ) |>
        summarise(weight = sum(weight),
                  .by = c("occupation", dplyr::all_of(grain))) |>
        rename(item = dplyr::all_of(grain))
      grain_distances <- profile_distances(broad_profiles, universe)
    }
    stopifnot(all(grain_distances$distance <= distances$distance + 1e-12))
    tibble::tibble(
      grain = grain, n_pairs = nrow(grain_distances),
      mean_distance = mean(grain_distances$distance),
      median_distance = stats::median(grain_distances$distance),
      n_indistinguishable_pairs = sum(grain_distances$distance < 1e-12),
      mean_distance_lost = mean(distances$distance - grain_distances$distance)
    )
  }
))
directed <- bind_rows(
  distances |>
    transmute(occupation = occupation_a, neighbor = occupation_b, distance),
  distances |>
    transmute(occupation = occupation_b, neighbor = occupation_a, distance)
) |>
  mutate(distance = round(distance, 12))
neighbors <- directed |>
  group_by(occupation) |>
  slice_min(distance, with_ties = TRUE) |>
  summarise(
    nearest_distance = min(distance),
    nearest_occupations = paste(sort(neighbor), collapse = ";"),
    n_nearest_ties = n(),
    .groups = "drop"
  )
rarity <- scores |>
  summarise(
    n_activities = n(), mean_idf = round(sum(tf_idf), 12),
    .by = occupation
  ) |>
  left_join(
    occupations, by = c("occupation" = "onet_soc_code"),
    relationship = "one-to-one"
  ) |>
  left_join(neighbors, by = "occupation", relationship = "one-to-one") |>
  left_join(
    directed |>
      summarise(mean_distance = mean(distance), .by = occupation),
    by = "occupation", relationship = "one-to-one"
  ) |>
  mutate(rarity_rank = min_rank(desc(mean_idf))) |>
  arrange(rarity_rank, occupation)
stopifnot(!anyNA(rarity))

coverage <- tasks |>
  distinct(onet_soc_code, task_id) |>
  left_join(
    links |>
      distinct(onet_soc_code, task_id) |>
      mutate(linked = TRUE),
    by = c("onet_soc_code", "task_id"), relationship = "one-to-one"
  ) |>
  summarise(
    n_tasks = n(), n_linked_tasks = sum(!is.na(linked)),
    .by = onet_soc_code
  ) |>
  full_join(
    occupations, by = "onet_soc_code", relationship = "one-to-one"
  ) |>
  mutate(in_universe = onet_soc_code %in% universe) |>
  arrange(onet_soc_code)
stopifnot(
  nrow(anti_join(
    links |> distinct(onet_soc_code, task_id),
    tasks |> distinct(onet_soc_code, task_id),
    by = c("onet_soc_code", "task_id")
  )) == 0L
)
provenance <- tibble::tibble(
  release_version = "30.3",
  source_url = "https://www.onetcenter.org/dl_files/database/db_30_3_text.zip",
  source_sha256 = digest::digest(file = archive_path, algo = "sha256"),
  snapshot_date = as.character(Sys.Date()),
  inclusion = "Any linked task, including Supplemental tasks",
  weighting = "Binary occupation-DWA presence; equal occupation weights",
  idf = "Natural log(N/df); within-occupation L1-normalized TF",
  n_taxonomy_occupations = nrow(occupations),
  n_task_occupations = n_distinct(tasks$onet_soc_code),
  n_linked_occupations = length(universe),
  n_activities = nrow(prevalence),
  n_tasks = nrow(distinct(tasks, onet_soc_code, task_id)),
  n_linked_tasks = nrow(distinct(links, onet_soc_code, task_id)),
  n_shared_task_ids = tasks |>
    distinct(onet_soc_code, task_id) |>
    count(task_id) |>
    filter(n > 1L) |>
    nrow()
)
utils::write.csv(prevalence, file.path(output_dir, "activity-prevalence.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(rarity, file.path(output_dir, "occupation-profiles.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(coverage, file.path(output_dir, "coverage.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(provenance, file.path(output_dir, "provenance.csv"),
                 row.names = FALSE, na = "")
utils::write.csv(resolution_summary, file.path(output_dir, "resolution-summary.csv"),
                 row.names = FALSE, na = "")
print(provenance)
print(head(prevalence, 8))
print(head(rarity, 8))
