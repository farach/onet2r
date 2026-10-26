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
source(file.path("inst", "article-helpers", "distinctiveness-information.R"))
links <- onet_archive_read("30.3", "Tasks to DWAs", path = args[[1]])
hierarchy <- onet_archive_reference(
  "30.3", "GWAs to IWAs to DWAs", path = args[[1]]
)
mapping <- hierarchy |>
  distinct(item = dwa_element_id, iwa_element_id, gwa_element_id)
profiles <- links |>
  distinct(onet_soc_code, dwa_element_id) |>
  transmute(occupation = onet_soc_code, item = dwa_element_id, weight = 1)
universe <- sort(unique(profiles$occupation))
budget <- profile_information_budget(profiles, mapping, universe)
linked_mapping <- mapping |>
  semi_join(profiles |> distinct(item), by = "item")
group_sizes <- linked_mapping |>
  count(gwa_element_id, name = "n_dwas") |>
  arrange(gwa_element_id)
random_group_template <- rep(group_sizes$gwa_element_id, group_sizes$n_dwas)
random_draws <- 200L
random_seed <- 20261026L
set.seed(random_seed)
random_hierarchy <- bind_rows(lapply(seq_len(random_draws), function(draw) {
  assigned_groups <- sample(random_group_template)
  group_counts <- table(factor(
    assigned_groups, levels = group_sizes$gwa_element_id
  ))
  stopifnot(identical(as.integer(group_counts), group_sizes$n_dwas))
  random_mapping <- tibble::tibble(
    item = sort(linked_mapping$item), group = assigned_groups
  )
  tibble::tibble(
    draw = draw,
    information_bits = profile_partition_information(
      profiles, random_mapping, universe
    )
  )
})) |>
  mutate(
    seed = random_seed,
    n_groups = nrow(group_sizes),
    n_dwas = nrow(linked_mapping),
    group_size_rule = "preserve observed DWA counts; random assignment"
  )
budget$summary <- budget$summary |>
  mutate(
    release = "30.3", universe_id = "tasks-and-dwa-links-923",
    weighting_rule = "uniform occupation; binary DWA shares; fixed original mass",
    .before = 1
  )
budget$parent_loss <- budget$parent_loss |>
  left_join(
    onet_archive_reference(
      "30.3", "Content Model Reference", path = args[[1]]
    ) |>
      transmute(gwa_element_id = element_id, gwa_element_name = element_name),
    by = "gwa_element_id", relationship = "many-to-one"
  )
stopifnot(!anyNA(budget$parent_loss$gwa_element_name))
output <- file.path("inst", "extdata", "occupational-distinctiveness")
write_snapshot_csv(
  budget$summary, file.path(output, "information-summary.csv")
)
write_snapshot_csv(
  random_hierarchy, file.path(output, "information-random-hierarchy.csv")
)
write_snapshot_csv(
  budget$parent_loss, file.path(output, "information-loss.csv")
)
write_snapshot_csv(
  budget$diagnostics,
  file.path(output, "information-occupation-diagnostics.csv")
)
print(budget$summary)
print(budget$parent_loss)
tasks <- onet_archive_read("30.3", "Task Statements", path = args[[1]])
inventory <- tasks |>
  summarise(
    n_tasks = n(),
    n_core = sum(task_type == "Core", na.rm = TRUE),
    n_supplemental = sum(task_type == "Supplemental", na.rm = TRUE),
    min_source_date = min(source_date, na.rm = TRUE),
    max_source_date = max(source_date, na.rm = TRUE),
    .by = "onet_soc_code"
  ) |>
  mutate(n_unclassified = n_tasks - n_core - n_supplemental) |>
  arrange(onet_soc_code)
stopifnot(sum(inventory$n_core > 0) == 893L)
write_snapshot_csv(inventory, file.path(output, "task-inventory.csv"))
