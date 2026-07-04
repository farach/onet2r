# Practical output validation for onet2r.
#
# Run from an installed package or from the source tree with:
# Rscript inst/examples/validate-outputs.R
#
# Live O&#42;NET API calls require ONET_API_KEY. Local archive and OEWS examples use
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

validated_exports <- character()
mark_validated <- function(...) {
  validated_exports <<- union(validated_exports, c(...))
  invisible(NULL)
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
sample_archive_base <- dirname(sample_archive)

old_key <- Sys.getenv("ONET_API_KEY", unset = NA_character_)
Sys.setenv(ONET_API_KEY = "validation-placeholder-key")
stopifnot(identical(onet_api_key(), "validation-placeholder-key"))
if (is.na(old_key)) {
  Sys.unsetenv("ONET_API_KEY")
} else {
  Sys.setenv(ONET_API_KEY = old_key)
}
mark_validated("onet_api_key")

cache_dir <- tempfile("onet2r-validation-cache-")
dir.create(file.path(cache_dir, "api"), recursive = TRUE)
file.create(file.path(cache_dir, "api", "placeholder.rds"))
onet_cache_use(enabled = TRUE, cache_dir = cache_dir)
stopifnot(isTRUE(getOption("onet2r.cache_enabled")))
stopifnot(identical(onet_cache_clear(cache_dir = cache_dir, what = "api"), cache_dir))
stopifnot(!dir.exists(file.path(cache_dir, "api")))
onet_cache_use(enabled = FALSE)
mark_validated("onet_cache_use", "onet_cache_clear")

oews <- onet_oews_national(year = 2024, path = sample_oews)
inspect_tbl(oews, c("year", "oews_type", "occ_code", "tot_emp", "a_median"))
inspect_tbl(onet_oews("national", year = 2024, path = sample_oews), c("oews_type", "occ_code"))
inspect_tbl(onet_oews_state(year = 2024, path = sample_oews), c("oews_type", "occ_code"))
inspect_tbl(onet_oews_metro(year = 2024, path = sample_oews), c("oews_type", "occ_code"))
inspect_tbl(onet_oews_industry(year = 2024, path = sample_oews), c("oews_type", "occ_code"))
mark_validated(
  "onet_oews", "onet_oews_national", "onet_oews_state", "onet_oews_metro",
  "onet_oews_industry"
)

occupations <- tibble::tibble(
  code = c("15-1252.00", "29-1141.00"),
  title = c("Software Developers", "Registered Nurses")
)
joined_oews <- suppressWarnings(onet_join_oews(occupations, oews = oews))
inspect_tbl(joined_oews, c("soc_code", "tot_emp", "a_median"))
mark_validated("onet_join_oews")

abilities <- onet_archive_read(
  "30.3",
  "Abilities",
  path = sample_archive,
  release_date = "2026-05-01"
)
inspect_tbl(abilities, c("onet_soc_code", "element_id", "data_value", "source_date"))
mark_validated("onet_archive_read")

panel_archives <- c(
  `30.2` = file.path(sample_archive_base, "db_30_2_text"),
  `30.3` = file.path(sample_archive_base, "db_30_3_text")
)
panel <- onet_panel(
  "Abilities",
  versions = c("30.2", "30.3"),
  scale = "IM",
  archives = panel_archives,
  release_dates = c(`30.2` = "2026-02-01", `30.3` = "2026-05-01")
)
inspect_tbl(panel, c("release_version", "onet_soc_code", "element_id", "data_value"))
same_vintage_bridge <- onet_crosswalk_bridge("2019", "2019")
inspect_tbl(same_vintage_bridge, c("from_onet_soc_code", "to_onet_soc_code"), min_rows = 0)
manual_bridge <- panel |>
  dplyr::filter(!is.na(.data$onet_soc_code)) |>
  dplyr::summarise(
    title = {
      non_missing_titles <- .data$title[!is.na(.data$title)]
      if (length(non_missing_titles) > 0) {
        non_missing_titles[[1]]
      } else {
        NA_character_
      }
    },
    .by = c("onet_soc_code", "soc_code")
  ) |>
  dplyr::transmute(
    from_vintage = "2019",
    to_vintage = "2019",
    from_onet_soc_code = .data$onet_soc_code,
    to_onet_soc_code = .data$onet_soc_code,
    from_soc_code = .data$soc_code,
    to_soc_code = .data$soc_code,
    from_title = .data$title,
    to_title = .data$title,
    step_count = 0L,
    map_type = "one_to_one",
    crosswalk_weight = 1
  )
reconciled <- onet_panel_reconcile(panel, manual_bridge)
inspect_tbl(reconciled, c("from_release", "to_release", "change_type", "safely_comparable"))
change_summary <- onet_change_summary(reconciled)
inspect_tbl(change_summary, c("summary_level", "change_type", "share_weighted"))
mark_validated(
  "onet_panel", "onet_crosswalk_bridge", "onet_panel_reconcile",
  "onet_change_summary"
)

pums <- tibble::tibble(
  SOCP = c("151252", "151252", "291141"),
  PWGTP = c(120, 80, 200)
)
pums_weights <- onet_weight_panel_pums(pums, year = 2022)
inspect_tbl(pums_weights, c("reference_soc_code", "employment", "weight_share"))
legacy_pums <- suppressWarnings(onet_pums_employment_weights(pums))
inspect_tbl(legacy_pums, c("soc_code", "employment", "records"))
mark_validated("onet_weight_panel_pums", "onet_pums_employment_weights")

resolved_soc <- onet_reference_soc_resolve(
  tibble::tibble(code = c("151252", "291141")),
  code = "code",
  source_taxonomy = "2010 SOC",
  reference_taxonomy = "2018 SOC",
  source_year = 2017,
  crosswalk = tibble::tibble(
    source_code = c("151252", "291141"),
    reference_soc_code = c("15-1252", "29-1141"),
    crosswalk_weight = c(1, 1),
    map_type = c("one_to_one", "one_to_one")
  )
)
inspect_tbl(resolved_soc, c("reference_soc_code", "crosswalk_weight", "source_year"))
mark_validated("onet_reference_soc_resolve")

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
mark_validated("onet_weight_panel_oews")

oral_scores <- abilities |>
  dplyr::filter(element_id == "1.A.1.a.1") |>
  dplyr::transmute(onet_soc_code, measure_score = data_value)
oral_aggregate <- onet_measure_aggregate(
  oral_scores,
  weight_panel,
  measure_id = "validation_oral_comprehension"
)
inspect_tbl(oral_aggregate, c("measure_id", "aggregate", "covered_employment"))
inspect_tbl(onet_provenance(oral_aggregate), c("measure_id", "weight_source"))
inspect_tbl(onet_coverage(oral_aggregate), c("measure_id", "employment_coverage_share"))
mark_validated("onet_measure_aggregate", "onet_provenance", "onet_coverage")

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
mark_validated("onet_measure", "onet_measure_coverage")

occupation_scores <- onet_task_to_occupation(
  measure,
  task_ratings = task_ratings,
  task_metadata = tasks
)
inspect_tbl(occupation_scores, c("onet_soc_code", "measure_score", "n_tasks"))
mark_validated("onet_task_to_occupation")

aggregate <- onet_measure_aggregate(
  occupation_scores,
  weight_panel,
  measure_id = "validation_task_score"
)
inspect_tbl(aggregate, c("measure_id", "aggregate", "covered_employment"))

sensitivity <- onet_measure_sensitivity(
  measure,
  weight_panels = weight_panel,
  task_ratings = task_ratings,
  task_metadata = tasks,
  include_supplemental = c(FALSE, TRUE)
)
inspect_tbl(sensitivity, c("scenario", "aggregate", "movement"))
mark_validated("onet_measure_sensitivity")

diagnostic <- onet_robustness_diagnostic(tibble::tibble(
  scenario = c("baseline", "alternative"),
  aggregate = c(aggregate$aggregate, aggregate$aggregate + 0.01)
))
inspect_tbl(diagnostic, c("scenario", "movement", "movement_percent"))
mark_validated("onet_robustness_diagnostic")

weighted_summary <- suppressWarnings(onet_weighted_summary(
  tibble::tibble(
    code = c("15-1252.00", "29-1141.00"),
    element_id = "2.A.1.a",
    data_value = c(4, 2)
  ),
  group = "element_id",
  value = "data_value",
  oews = tibble::tibble(
    occ_code = c("15-1252", "29-1141"),
    tot_emp = c(100, 300),
    a_median = c(100, 200)
  )
))
inspect_tbl(weighted_summary, c("element_id", "weighted_mean", "wage_weighted_mean"))
mark_validated("onet_weighted_summary")

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
stopifnot(isTRUE(all.equal(onet_coverage(decomposition)$leakage, 0)))
mark_validated("onet_decompose_change")

onet_cache_use(enabled = FALSE)
onet_rate_limit(0)
mark_validated("onet_rate_limit")

if (identical(Sys.getenv("ONET2R_VALIDATE_LIVE", unset = ""), "true")) {
  releases <- onet_releases(refresh = TRUE)
  inspect_tbl(releases, c("version", "release_date", "text_url"))
  mark_validated("onet_releases")

  archive_path <- onet_archive_download("30.3", dir = tempfile("onet2r-archive-cache-"))
  stopifnot(file.exists(archive_path), grepl("\\.zip$", archive_path))
  mark_validated("onet_archive_download")

  bridge <- onet_crosswalk_bridge("2010", "2019")
  inspect_tbl(bridge, c("from_onet_soc_code", "to_onet_soc_code", "map_type"))

  data_updates_path <- Sys.getenv("ONET2R_DATA_UPDATES_PATH", unset = "")
  if (nzchar(data_updates_path)) {
    updates <- onet_data_updates(path = data_updates_path)
  } else {
    updates <- onet_data_updates(force = TRUE)
  }
  inspect_tbl(updates, c("data_update_type", "onet_soc_code", "title"))
  mark_validated("onet_data_updates")
} else {
  message("Skipping network-dependent release/archive checks; set ONET2R_VALIDATE_LIVE=true to run them.")
}

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
  mark_validated(
    "onet_search", "onet_occupations", "onet_occupations_all",
    "onet_occupation", "onet_occupation_details", "onet_skills",
    "onet_skills_all", "onet_knowledge", "onet_abilities",
    "onet_work_styles", "onet_interests", "onet_work_context",
    "onet_work_context_all", "onet_work_activities",
    "onet_work_activities_all", "onet_tasks", "onet_detailed_work_activities",
    "onet_related_occupations", "onet_professional_associations",
    "onet_apprenticeship", "onet_education", "onet_job_zone",
    "onet_hot_technology", "onet_technology", "onet_technology_skills",
    "onet_in_demand_skills", "onet_tables", "onet_table_info",
    "onet_table", "onet_crosswalk_military", "onet_taxonomy_map"
  )
} else {
  message("Skipping live O&#42;NET checks because ONET_API_KEY is not set.")
}

network_or_external_exports <- c(
  "onet_archive_download", "onet_data_updates", "onet_releases",
  "onet_search", "onet_occupations", "onet_occupations_all",
  "onet_occupation", "onet_occupation_details", "onet_skills",
  "onet_skills_all", "onet_knowledge", "onet_abilities",
  "onet_work_styles", "onet_interests", "onet_work_context",
  "onet_work_context_all", "onet_work_activities",
  "onet_work_activities_all", "onet_tasks", "onet_detailed_work_activities",
  "onet_related_occupations", "onet_professional_associations",
  "onet_apprenticeship", "onet_education", "onet_job_zone",
  "onet_hot_technology", "onet_technology", "onet_technology_skills",
  "onet_in_demand_skills", "onet_tables", "onet_table_info",
  "onet_table", "onet_crosswalk_military", "onet_taxonomy_map"
)

exports <- getNamespaceExports("onet2r")
unvalidated <- setdiff(sort(exports), sort(c(validated_exports, network_or_external_exports)))
if (length(unvalidated) > 0) {
  stop("Validation script does not cover exported functions: ", paste(unvalidated, collapse = ", "))
}
message("Validated exports: ", paste(sort(validated_exports), collapse = ", "))
message(
  "Deferred network/API exports: ",
  paste(sort(setdiff(network_or_external_exports, validated_exports)), collapse = ", ")
)
