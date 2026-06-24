# Practical output validation for onet2r.
#
# Run from an installed package or from the source tree with:
# Rscript inst/examples/validate-outputs.R
#
# Live O*NET API calls require ONET_API_KEY. Local archive and OEWS examples use
# bundled sample files so they are deterministic and do not depend on downloads.

options(cli.num_colors = 1, crayon.enabled = FALSE, pillar.bold = FALSE)

inspect_tbl <- function(x, required = character(), min_rows = 1) {
  stopifnot(inherits(x, "tbl_df"))
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop("Missing expected columns: ", paste(missing, collapse = ", "))
  }
  if (nrow(x) < min_rows) {
    stop("Expected at least ", min_rows, " row(s), got ", nrow(x))
  }
  print(utils::head(x, 3), width = Inf)
  invisible(x)
}

inspect_list <- function(x, required = character()) {
  stopifnot(is.list(x))
  missing <- setdiff(required, names(x))
  if (length(missing) > 0) {
    stop("Missing expected elements: ", paste(missing, collapse = ", "))
  }
  print(utils::str(x, max.level = 1))
  invisible(x)
}

if (
  file.exists("DESCRIPTION") &&
    dir.exists("R") &&
    any(grepl("^Package:\\s+onet2r\\s*$", readLines("DESCRIPTION", warn = FALSE)))
) {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Install devtools to run this script from the source tree.")
  }
  devtools::load_all(quiet = TRUE)
} else {
  library(onet2r)
}

sample_oews <- system.file("extdata", "oews-national-sample.csv", package = "onet2r")
if (sample_oews == "") {
  sample_oews <- file.path("inst", "extdata", "oews-national-sample.csv")
}
sample_archive <- system.file("extdata", "onet-mini", "db_30_3_text", package = "onet2r")
if (sample_archive == "") {
  sample_archive <- file.path("inst", "extdata", "onet-mini", "db_30_3_text")
}

oews <- onet_oews_national(year = 2024, path = sample_oews)
inspect_tbl(oews, c("year", "oews_type", "occ_code", "tot_emp", "a_median"))
inspect_tbl(onet_oews("national", year = 2024, path = sample_oews), c("oews_type", "occ_code"))
inspect_tbl(onet_oews_state(year = 2024, path = sample_oews), c("oews_type", "occ_code"))
inspect_tbl(onet_oews_metro(year = 2024, path = sample_oews), c("oews_type", "occ_code"))
inspect_tbl(onet_oews_industry(year = 2024, path = sample_oews), c("oews_type", "occ_code"))

abilities <- onet_archive_read(
  "30.3",
  "Abilities",
  path = sample_archive,
  release_date = "2026-05-01"
)
inspect_tbl(abilities, c("onet_soc_code", "element_id", "data_value", "source_date"))

occupations <- abilities |>
  dplyr::distinct(code = onet_soc_code, title)
wage_context <- onet_join_oews(occupations, oews = oews)
inspect_tbl(wage_context, c("code", "soc_code", "tot_emp", "a_median"))

weighted <- onet_weighted_summary(
  abilities,
  group = c("element_id", "element_name"),
  value = "data_value",
  occupation_code = "onet_soc_code",
  oews = oews
)
inspect_tbl(
  weighted,
  c("element_id", "element_name", "total_weight", "weighted_mean")
)

pums <- tibble::tibble(
  SOCP = c("151252", "151252", "291141"),
  PWGTP = c(120, 80, 200)
)
pums_weights <- onet_pums_employment_weights(pums)
inspect_tbl(pums_weights, c("soc_code", "employment", "records"))

tasks <- onet_archive_read(
  "30.3",
  "Task Statements",
  path = sample_archive,
  release_date = "2026-05-01"
)
task_ratings <- onet_archive_read(
  "30.3",
  "Task Ratings",
  path = sample_archive,
  release_date = "2026-05-01"
)
inspect_tbl(tasks, c("onet_soc_code", "task_id", "task", "task_type"))
inspect_tbl(task_ratings, c("onet_soc_code", "task_id", "scale_id", "data_value"))

weight_panel <- onet_weight_panel_oews(oews, year = 2024)
inspect_tbl(weight_panel, c("reference_soc_code", "employment", "weight_share"))

task_scores <- tibble::tibble(
  task_id = c("1001", "1002", "2001"),
  score = c(0.8, 0.4, 0.2)
)
measure <- onet_measure(
  task_scores,
  key = "task_id",
  score = "score",
  key_type = "task",
  universe = tasks$task_id,
  measure_id = "validation_task_score"
)
inspect_tbl(onet_measure_coverage(measure), c("key_type", "coverage_share"))

occupation_scores <- onet_task_to_occupation(
  measure,
  task_ratings = task_ratings,
  task_metadata = tasks
)
inspect_tbl(occupation_scores, c("onet_soc_code", "measure_score", "n_tasks"))

aggregate <- onet_measure_aggregate(
  occupation_scores,
  weight_panel,
  measure_id = "validation_task_score"
)
inspect_tbl(aggregate, c("measure_id", "aggregate", "covered_employment"))

diagnostic <- onet_robustness_diagnostic(tibble::tibble(
  scenario = c("baseline", "alternative"),
  aggregate = c(aggregate$aggregate, aggregate$aggregate + 0.01)
))
inspect_tbl(diagnostic, c("scenario", "movement", "movement_percent"))

decomposition <- onet_decompose_change(
  tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    measure_score = c(1, 2),
    safely_comparable = c(TRUE, FALSE)
  ),
  tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    measure_score = c(2, 2.5),
    safely_comparable = c(TRUE, FALSE)
  ),
  tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    employment = c(100, 100)
  ),
  tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    employment = c(150, 50)
  )
)
inspect_tbl(decomposition, c("component", "value"))
stopifnot(isTRUE(all.equal(attr(decomposition, "coverage")$leakage, 0)))

onet_cache_use(enabled = FALSE)
onet_rate_limit(0)

if (nzchar(Sys.getenv("ONET_API_KEY"))) {
  code <- "15-1252.00"

  inspect_tbl(onet_search("software developer", end = 3), c("code", "title"))
  inspect_tbl(onet_occupations(start = 1, end = 3), c("code", "title"))
  inspect_tbl(onet_occupations_all(page_size = 2000, show_progress = FALSE), c("code", "title"))
  inspect_list(onet_occupation(code), c("code", "title"))
  inspect_list(onet_occupation_details(code))
  inspect_tbl(onet_skills(code, end = 5), min_rows = 1)
  inspect_tbl(onet_skills_all(code, show_progress = FALSE), min_rows = 1)
  inspect_tbl(onet_knowledge(code, end = 5), min_rows = 1)
  inspect_tbl(onet_abilities(code, end = 5), min_rows = 1)
  inspect_tbl(onet_work_styles(code, end = 5), min_rows = 1)
  inspect_tbl(onet_interests(code, end = 5), min_rows = 1)
  inspect_tbl(onet_work_context(code, end = 5), min_rows = 1)
  inspect_tbl(onet_work_context_all(code, show_progress = FALSE), min_rows = 1)
  inspect_tbl(onet_work_activities(code, end = 5), min_rows = 1)
  inspect_tbl(onet_work_activities_all(code, show_progress = FALSE), min_rows = 1)
  inspect_tbl(onet_tasks(code, end = 5), min_rows = 1)
  inspect_tbl(onet_detailed_work_activities(code, end = 5), min_rows = 1)
  inspect_tbl(onet_related_occupations(code, end = 5), min_rows = 1)
  inspect_tbl(onet_professional_associations(code, end = 5), min_rows = 0)
  inspect_tbl(onet_apprenticeship(code, end = 5), "example_title", min_rows = 0)
  inspect_tbl(onet_education(code), min_rows = 1)
  inspect_list(onet_job_zone(code))
  inspect_tbl(onet_hot_technology(code, end = 5), min_rows = 0)
  inspect_tbl(onet_technology(code, end = 5), min_rows = 0)
  inspect_tbl(onet_technology_skills(code, end = 3), min_rows = 1)
  inspect_tbl(onet_in_demand_skills(code, end = 5), min_rows = 0)
  inspect_tbl(onet_tables(), c("id", "title"), min_rows = 1)
  table_info <- inspect_tbl(onet_table_info("occupation_data"), c("name", "type", "description"), min_rows = 1)
  stopifnot(!anyNA(table_info$name))
  inspect_tbl(onet_table("occupation_data", show_progress = FALSE), min_rows = 1)
  inspect_tbl(onet_crosswalk_military("11B", end = 5), c("code", "title"), min_rows = 0)
  inspect_tbl(onet_taxonomy_map(code, from = "active", to = "2010"), c("code", "title"), min_rows = 0)
} else {
  message("Skipping live O*NET checks because ONET_API_KEY is not set.")
}
