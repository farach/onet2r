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
utils::write.csv(budget$summary, file.path(output, "information-summary.csv"),
                 row.names = FALSE)
utils::write.csv(budget$parent_loss, file.path(output, "information-loss.csv"),
                 row.names = FALSE)
utils::write.csv(budget$diagnostics,
                 file.path(output, "information-occupation-diagnostics.csv"),
                 row.names = FALSE)
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
utils::write.csv(inventory, file.path(output, "task-inventory.csv"),
                 row.names = FALSE)
