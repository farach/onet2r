# Regenerate the article's employment-weighted activity snapshots without an
# API key or network access. Run from the repository root after installing the
# current package:
# Rscript data-raw/occupational-employment.R path/to/db_30_3_text.zip
#   path/to/oesm25nat.zip path/to/occupation_definitions_m2025.xlsx
# The script reads only three pinned local files and checks their SHA-256
# values. It never downloads anything. BLS returns HTTP 403 to scripted
# clients, so obtain the two BLS files through a browser.
library(onet2r)
library(dplyr)
options(cli.progress_show_after = Inf)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L || !all(file.exists(args))) {
  stop(
    "Supply three existing local files: the O*NET 30.3 text ZIP, the May 2025 ",
    "OEWS national ZIP and the May 2025 OEWS occupation definitions workbook.",
    call. = FALSE
  )
}
inputs <- tibble::tibble(
  input = c(
    "onet_text_archive", "oews_national_may_2025",
    "bls_occupation_definitions_may_2025"
  ),
  release = c("O*NET 30.3", "May 2025", "May 2025"),
  file_name = c(
    "db_30_3_text.zip", "oesm25nat.zip", "occupation_definitions_m2025.xlsx"
  ),
  source_url = c(
    "https://www.onetcenter.org/dl_files/database/db_30_3_text.zip",
    "https://www.bls.gov/oes/special-requests/oesm25nat.zip",
    "https://www.bls.gov/oes/occupation_definitions_m2025.xlsx"
  ),
  pinned_sha256 = c(
    "7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780",
    "b5855a37f3e03e779f6fbf173d3bbc94aeeff33426aef32fa95ce6d025bab1af",
    "d927416c1165e2043e8626f380f3bcaaa2f98377bfa5ef5c08209e573d95bb24"
  ),
  access_note = c(
    "Official O*NET 30.3 text archive download",
    paste(
      "BLS returns HTTP 403 to scripted clients such as curl. A fetch of the",
      "source URL from inside a browser page on 2026-10-26 returned HTTP 200",
      "and bytes that match the pinned SHA-256. The OEWS tables page",
      "https://www.bls.gov/oes/tables.htm links to this URL."
    ),
    paste(
      "BLS returns HTTP 403 to scripted clients such as curl. A fetch of the",
      "source URL from inside a browser page on 2026-10-26 returned HTTP 200",
      "and bytes that match the pinned SHA-256. The OEWS tables, structure and",
      "technical notes pages do not link to this URL."
    )
  )
)
inputs$sha256 <- unname(vapply(
  args, function(path) digest::digest(file = path, algo = "sha256"), character(1)
))
if (!identical(inputs$sha256, inputs$pinned_sha256)) {
  stop("An input file is not the pinned file.", call. = FALSE)
}
archive_path <- args[[1]]
oews_path <- args[[2]]
definitions_path <- args[[3]]

for (helper in c(
  "occupational-distinctiveness.R", "distinctiveness-information.R",
  "occupational-employment.R"
)) {
  source(file.path("inst", "article-helpers", helper))
}
output_dir <- file.path("inst", "extdata", "occupational-extensions")
snapshot_dir <- file.path("inst", "extdata", "occupational-distinctiveness")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
digits <- 8L
bound_types <- c(soc = "disjoint_allocation_bound", code = "profile_envelope")
whole <- function(x) {
  stopifnot(all(is.na(x) | (x == round(x) & abs(x) < 2^31 - 1)))
  as.integer(x)
}

# O*NET roster and activity links. Occupation Data lists all 1,016 codes.
# Only 923 codes have activity links, so they are the profiled codes.
occupation_data <- onet_archive_read(
  "30.3", "Occupation Data", path = archive_path
) |>
  distinct(onet_soc_code, title)
link_table <- onet_archive_read("30.3", "Tasks to DWAs", path = archive_path) |>
  distinct(unit = onet_soc_code, item = dwa_element_id)
items <- sort(unique(link_table$item), method = "radix")
profiled_codes <- sort(unique(link_table$unit), method = "radix")
stopifnot(
  nrow(occupation_data) == 1016L,
  length(profiled_codes) == 923L,
  length(items) == 2083L,
  !anyNA(link_table)
)
roster <- classify_onet_roster(occupation_data, profiled_codes)
taxonomy_accounting <- roster_accounting(roster)
print(as.data.frame(taxonomy_accounting))
stopifnot(all(
  taxonomy_accounting$status[
    taxonomy_accounting$quantity %in% c(
      "n_titles", "n_socs", "n_socs_with_detailed", "n_data_level",
      "n_soc_level_without_detailed", "n_soc_level_with_detailed",
      "n_detailed", "n_non_data_level", "n_military"
    )
  ] == "match"
))

# Raw May 2025 national OEWS rows, read through the package reader.
oews <- onet_oews("national", 2025, path = oews_path)
stopifnot(is.numeric(oews$tot_emp), is.numeric(oews$emp_prse))
# The package reader drops the geography, industry and ownership columns, so
# read them from the workbook itself. The scope has to be one national,
# cross-industry universe, otherwise the rows could not be one panel.
scope_files <- utils::unzip(
  oews_path, exdir = file.path(tempdir(), "oews-national-scope")
)
stopifnot(length(scope_files) == 1L, grepl("[.]xlsx$", scope_files))
oews_scope <- readxl::read_excel(
  scope_files, col_types = "text", .name_repair = "minimal"
)
stopifnot(
  nrow(oews_scope) == nrow(oews),
  all(c(
    "AREA", "AREA_TITLE", "AREA_TYPE", "NAICS", "NAICS_TITLE", "I_GROUP",
    "OWN_CODE"
  ) %in% names(oews_scope))
)
scope_rows <- c(
  rows_area_99_us_national = sum(
    oews_scope$AREA == "99" & oews_scope$AREA_TITLE == "U.S." &
      oews_scope$AREA_TYPE == "1"
  ),
  rows_naics_000000_cross_industry = sum(
    oews_scope$NAICS == "000000" & oews_scope$NAICS_TITLE == "Cross-industry" &
      oews_scope$I_GROUP == "cross-industry"
  ),
  rows_own_code_1235 = sum(oews_scope$OWN_CODE == "1235")
)
stopifnot(all(scope_rows == nrow(oews_scope)))
group_counts <- table(oews$o_group)
stopifnot(identical(
  as.integer(group_counts[c("detailed", "broad", "minor", "major", "total")]),
  c(830L, 454L, 94L, 22L, 1L)
))
total_row <- oews[oews$o_group == "total", ]
official_total <- total_row$tot_emp
detailed <- oews[oews$o_group == "detailed", ]
stopifnot(
  identical(total_row$occ_code, "00-0000"),
  official_total == 155495730,
  !anyDuplicated(detailed$occ_code),
  !anyNA(detailed$tot_emp),
  all(detailed$tot_emp > 0 & detailed$tot_emp == round(detailed$tot_emp)),
  sum(detailed$tot_emp) == 155481840
)

# The public panel builder keeps only detailed rows. In the raw rows, every
# published combination code is a detailed row. Some of them also appear as a
# broad hierarchy row with the same employment, so keeping every row would
# count that mass twice. Check both facts on the raw rows.
combination_codes <- c(
  "13-1020", "13-2020", "21-1018", "25-2052", "25-9045", "29-2010",
  "31-1120", "39-7010", "47-4090", "51-2028", "51-2090", "53-1047"
)
raw_combination_rows <- oews[oews$occ_code %in% combination_codes, ]
raw_detailed_combinations <- raw_combination_rows[
  raw_combination_rows$o_group == "detailed",
]
raw_broad_combinations <- raw_combination_rows[
  raw_combination_rows$o_group == "broad",
]
broad_codes_also_detailed <- intersect(
  oews$occ_code[oews$o_group == "broad"], detailed$occ_code
)
panel <- suppressMessages(onet_weight_panel_oews(oews, 2025))
stopifnot(
  all(raw_combination_rows$o_group %in% c("detailed", "broad")),
  nrow(raw_detailed_combinations) == 12L,
  setequal(raw_detailed_combinations$occ_code, combination_codes),
  !anyDuplicated(raw_broad_combinations$occ_code),
  all(raw_broad_combinations$tot_emp ==
        raw_detailed_combinations$tot_emp[
          match(raw_broad_combinations$occ_code, raw_detailed_combinations$occ_code)
        ]),
  setequal(broad_codes_also_detailed, raw_broad_combinations$occ_code),
  nrow(panel) == 830L,
  !anyDuplicated(panel$reference_soc_code),
  setequal(panel$reference_soc_code, detailed$occ_code),
  all(panel$employment ==
        detailed$tot_emp[match(panel$reference_soc_code, detailed$occ_code)]),
  abs(sum(panel$weight_share) - 1) < 1e-12,
  all(combination_codes %in% panel$reference_soc_code)
)

# BLS states which 2018 SOC occupations each combination code includes.
definitions <- readxl::read_excel(
  definitions_path, sheet = 1, col_types = "text", .name_repair = "minimal"
)
stopifnot(
  ncol(definitions) == 3L, nrow(definitions) == 832L,
  grepl("Code$", names(definitions)[[1]]),
  grepl("Title$", names(definitions)[[2]]),
  grepl("Definition$", names(definitions)[[3]])
)
names(definitions) <- c("code", "title", "definition")
bls_parts <- bls_combination_parts(definitions$code, definitions$definition)
expected_parts <- list(
  "13-1020" = c("13-1021", "13-1022", "13-1023"),
  "13-2020" = c("13-2022", "13-2023"),
  "21-1018" = c("21-1011", "21-1014"),
  "25-2052" = c("25-2055", "25-2056"),
  "25-9045" = c("25-9042", "25-9043", "25-9049"),
  "29-2010" = c("29-2011", "29-2012"),
  "31-1120" = c("31-1121", "31-1122"),
  "39-7010" = c("39-7011", "39-7012"),
  "47-4090" = c("47-4091", "47-4099"),
  "51-2028" = c("51-2022", "51-2023"),
  "51-2090" = c("51-2092", "51-2099"),
  "53-1047" = c("53-1042", "53-1043", "53-1044", "53-1049")
)
expected_pairs <- paste(
  rep(names(expected_parts), lengths(expected_parts)),
  unlist(expected_parts, use.names = FALSE)
)
stopifnot(
  nrow(bls_parts) == length(expected_pairs),
  setequal(paste(bls_parts$oews_code, bls_parts$part_soc_code), expected_pairs),
  !any(bls_parts$part_soc_code %in% detailed$occ_code),
  all(detailed$occ_code %in% definitions$code),
  setequal(setdiff(definitions$code, detailed$occ_code), c("11-1031", "45-3031"))
)
# The package keeps its own constituent list for the bridge. Compare it with
# the BLS definitions instead of assuming that the two agree.
stopifnot(exists(
  "oews_combination_parts", envir = asNamespace("onet2r"), inherits = FALSE
))
package_parts <- get("oews_combination_parts", envir = asNamespace("onet2r"))()
stopifnot(
  all(c("reference_soc_code", "part_soc_code") %in% names(package_parts)),
  all(combination_codes %in% package_parts$reference_soc_code)
)
package_only_codes <- setdiff(
  unique(package_parts$reference_soc_code), combination_codes
)

# One row per SOC occupation, with the OEWS cell that publishes its employment.
single_cells <- setdiff(detailed$occ_code, combination_codes)
parts <- bind_rows(
  tibble::tibble(cell = single_cells, soc_code = single_cells),
  bls_parts |> transmute(cell = oews_code, soc_code = part_soc_code)
)
stopifnot(!anyDuplicated(parts$soc_code), setequal(parts$cell, detailed$occ_code))

# The package bridge maps each roster code to a cell. It has to agree with the
# map that follows from the BLS definitions.
bridge <- suppressMessages(onet_oews_bridge(roster$onet_soc_code, panel))
stopifnot(
  nrow(bridge) == nrow(roster),
  setequal(bridge$from_onet_soc_code, roster$onet_soc_code),
  !anyDuplicated(bridge$from_onet_soc_code)
)
bridge <- bridge[match(roster$onet_soc_code, bridge$from_onet_soc_code), ]
definition_cell <- parts$cell[match(roster$soc_code, parts$soc_code)]
bridge_cell <- ifelse(
  bridge$map_type == "not_in_panel", NA_character_, bridge$reference_soc_code
)
stopifnot(identical(definition_cell, bridge_cell))

cells <- detailed |>
  transmute(
    cell = occ_code, employment = tot_emp, title = occ_title,
    raw_o_group = o_group, emp_prse = emp_prse,
    n_raw_rows_same_code = as.integer(
      table(factor(oews$occ_code, levels = occ_code))[occ_code]
    )
  )
inventory <- cell_inventory(cells, parts, roster)
stopifnot(
  nrow(inventory) == 830L,
  sum(inventory$employment) == sum(detailed$tot_emp)
)
coverage <- bind_rows(lapply(c("soc", "code"), function(rule) {
  out <- employment_coverage(inventory, official_total, rule)
  all_other_socs <- roster$soc_code[
    roster$taxonomy_class == "all_other_with_detailed"
  ]
  rows <- inventory$cell %in% parts$cell[parts$soc_code %in% all_other_socs]
  extra <- tibble::tibble(
    scope = "cells_with_all_other_soc_with_nested_detail",
    n_cells = sum(rows), employment = sum(inventory$employment[rows])
  )
  extra$share_official_total <- extra$employment / official_total
  extra$share_returned_panel <- extra$employment / sum(inventory$employment)
  bind_rows(out, extra) |>
    mutate(unit_rule = rule, bound_type = bound_types[[rule]], .before = 1L)
}))

# Equal-code baseline over all 923 profiled codes. It has to reproduce the
# published snapshot of the article.
original_profiles <- link_table |>
  transmute(occupation = unit, item = item, weight = 1)
original <- profile_tfidf(original_profiles, profiled_codes)
original_scores_table <- original |>
  summarise(mean_idf = round(sum(tf_idf), 12), .by = occupation)
original_scores <- stats::setNames(
  original_scores_table$mean_idf, original_scores_table$occupation
)
original_prevalence <- original |>
  distinct(item, prevalence)
original_prevalence <- stats::setNames(
  original_prevalence$prevalence, original_prevalence$item
)[items]
published_profiles <- utils::read.csv(
  file.path(snapshot_dir, "occupation-profiles.csv"), stringsAsFactors = FALSE
)
published_prevalence <- utils::read.csv(
  file.path(snapshot_dir, "activity-prevalence.csv"), stringsAsFactors = FALSE
)
stopifnot(
  nrow(published_profiles) == 923L,
  setequal(published_profiles$occupation, profiled_codes),
  max(abs(
    published_profiles$mean_idf -
      original_scores[published_profiles$occupation]
  )) < 1e-9,
  identical(
    published_profiles$rarity_rank,
    as.integer(rank(-original_scores[published_profiles$occupation],
                    ties.method = "min"))
  ),
  max(abs(
    published_prevalence$prevalence - original_prevalence[published_prevalence$item]
  )) < 1e-12
)

comparison_pairs <- list(
  c("original_923", "equal_unit"), c("equal_unit", "equal_minor_group"),
  c("equal_unit", "equal_prefix5_group"), c("equal_unit", "employment"),
  c("equal_minor_group", "employment")
)
round_idf <- function(p) ifelse(p > 0, round(log(1 / p), digits), NA_real_)
metric_rows <- function(comparison, values) {
  tibble::tibble(
    comparison = comparison, metric = names(values),
    value = unlist(values, use.names = FALSE)
  )
}
agreement_rows <- function(comparison, x_prevalence, y_prevalence, x, y) {
  prevalence_stats <- as.list(
    prevalence_agreement(x_prevalence, y_prevalence, digits = digits)
  )
  names(prevalence_stats) <- paste0("prevalence_", names(prevalence_stats))
  bind_rows(
    metric_rows(comparison, prevalence_stats),
    metric_rows(comparison, list(
      unit_rank_spearman = stats::cor(x, y, method = "spearman"),
      top10_jaccard = top_set_overlap(x, y, 10L)$jaccard,
      top20_jaccard = top_set_overlap(x, y, 20L)$jaccard
    ))
  )
}

# Every weighting scheme runs over one unit set: the profiled units in complete
# cells. Only the weights change. The equal-code and equal-group schemes are
# also recomputed with the article's own functions.
rule_results <- function(rule) {
  units <- build_cell_units(parts, roster, rule)
  result <- employment_prevalence(link_table, units, cells, items)
  ids <- result$units$unit
  z <- link_matrix(link_table, ids, items)
  minor <- substr(ids, 1, 4)
  prefix5 <- substr(ids, 1, 5)
  weights <- list(
    equal_unit = rep(1, length(ids)),
    equal_minor_group = equal_group_weights(minor),
    equal_prefix5_group = equal_group_weights(prefix5),
    employment = result$units$employment_weight
  )
  if (rule == "code") {
    two_stage <- two_stage_unit_weights(units[units$unit %in% ids, ], cells)
    weights$employment_two_stage <- unname(two_stage[ids])
  }
  prevalence <- lapply(weights, function(w) {
    unname(weighted_link_prevalence(z, w))
  })
  scores <- lapply(prevalence, function(p) unit_mean_idf(z, p))
  complete_cells <- unique(result$units$cell)
  cell_weight <- tapply(result$units$employment_weight, result$units$cell, sum)
  stopifnot(
    !anyDuplicated(ids),
    all(abs(
      cell_weight[complete_cells] -
        cells$employment[match(complete_cells, cells$cell)]
    ) < 1e-6),
    abs(sum(weights$employment) - result$employment_complete) < 1e-6,
    max(abs(prevalence$employment - result$items$prevalence)) < 1e-12,
    all(prevalence$employment >= result$items$lower - 1e-12),
    all(prevalence$employment <= result$items$upper + 1e-12),
    all(unlist(prevalence) >= 0 & unlist(prevalence) <= 1)
  )
  if (rule == "code") {
    stopifnot(abs(
      sum(weights$employment_two_stage) - result$employment_complete
    ) < 1e-6)
  }
  profiles <- link_table |>
    filter(unit %in% ids) |>
    transmute(occupation = unit, item = item, weight = 1)
  baseline <- profile_tfidf(profiles, ids)
  baseline_scores <- baseline |>
    summarise(mean_idf = round(sum(tf_idf), 12), .by = occupation)
  baseline_prevalence <- baseline |> distinct(item, prevalence)
  stopifnot(
    max(abs(
      baseline_prevalence$prevalence -
        prevalence$equal_unit[match(baseline_prevalence$item, items)]
    )) < 1e-12,
    max(abs(
      baseline_scores$mean_idf - scores$equal_unit[baseline_scores$occupation]
    )) < 1e-11
  )
  for (grouping in list(
    list(label = "equal_minor_group", groups = minor),
    list(label = "equal_prefix5_group", groups = prefix5)
  )) {
    grouped <- profile_tfidf_equal_group_weights(profiles, ids, grouping$groups)
    grouped_scores <- grouped |>
      summarise(mean_idf = round(sum(tf_idf), 12), .by = occupation)
    grouped_prevalence <- grouped |> distinct(item, prevalence)
    stopifnot(
      max(abs(
        grouped_prevalence$prevalence -
          prevalence[[grouping$label]][match(grouped_prevalence$item, items)]
      )) < 1e-12,
      max(abs(
        grouped_scores$mean_idf -
          scores[[grouping$label]][grouped_scores$occupation]
      )) < 1e-11
    )
  }

  # original_923 is the article's own rank over all 923 profiled codes. The
  # in-universe series re-ranks the same scores within the matched universe, so
  # it compares like with like with the other schemes.
  series <- c(
    list(
      original_923 = original_scores,
      original_923_in_universe = original_scores[ids]
    ),
    scores
  )
  rank_columns <- as.vector(rbind(
    paste0("mean_idf_", names(series)), paste0("rank_", names(series))
  ))
  ranks <- rank_sensitivity(series, k = 10L) |>
    mutate(
      title = occupation_data$title[match(unit, occupation_data$onet_soc_code)],
      unit_rule = rule, bound_type = bound_types[[rule]]
    ) |>
    select(
      "unit_rule", "bound_type", onet_soc_code = "unit", "title",
      "selected_reason", all_of(rank_columns)
    ) |>
    arrange(onet_soc_code)

  prevalence_series <- c(
    list(original_923 = unname(original_prevalence)), prevalence
  )
  score_series <- c(list(original_923 = original_scores[ids]), scores)
  agreement <- bind_rows(lapply(comparison_pairs, function(pair) {
    agreement_rows(
      paste(pair, collapse = "_vs_"),
      prevalence_series[[pair[[1]]]], prevalence_series[[pair[[2]]]],
      score_series[[pair[[1]]]], score_series[[pair[[2]]]]
    )
  }))
  if (rule == "code") {
    agreement <- bind_rows(
      agreement,
      agreement_rows(
        "employment_vs_employment_two_stage",
        prevalence$employment, prevalence$employment_two_stage,
        scores$employment, scores$employment_two_stage
      )
    )
  }
  bounds <- result$items
  width <- bounds$upper - bounds$lower
  width_wide <- bounds$upper_wide - bounds$lower_wide
  positive <- bounds$prevalence > 0
  outside_bounds <- function(p) {
    mean(p[positive] < bounds$lower[positive] - 1e-12 |
           p[positive] > bounds$upper[positive] + 1e-12)
  }
  universe_summary <- metric_rows("matched_universe", list(
    n_units = length(ids),
    n_cells = length(complete_cells),
    n_minor_groups = length(unique(minor)),
    n_prefix5_groups = length(unique(prefix5)),
    n_items_linked = sum(positive),
    n_items_unlinked = sum(!positive),
    employment_complete = result$employment_complete,
    employment_returned_panel = result$employment_total
  ))
  bound_summary <- metric_rows("employment_bounds", list(
    within_bounds_share = mean(
      bounds$prevalence >= bounds$lower - 1e-12 &
        bounds$prevalence <= bounds$upper + 1e-12
    ),
    zero_width_share = mean(width[positive] < 1e-12),
    mean_width = mean(width[positive]),
    median_width = stats::median(width[positive]),
    max_width = max(width[positive]),
    mean_width_wide = mean(width_wide[positive]),
    max_width_wide = max(width_wide[positive]),
    share_equal_unit_outside_bounds = outside_bounds(prevalence$equal_unit),
    share_equal_minor_group_outside_bounds = outside_bounds(
      prevalence$equal_minor_group
    )
  ))
  prevalence_table <- tibble::tibble(
    unit_rule = rule, bound_type = bound_types[[rule]], dwa_element_id = items,
    prevalence_equal_unit = round(prevalence$equal_unit, digits),
    prevalence_equal_minor_group = round(prevalence$equal_minor_group, digits),
    prevalence_equal_prefix5_group = round(prevalence$equal_prefix5_group, digits),
    prevalence_employment = round(prevalence$employment, digits),
    prevalence_employment_two_stage = if (rule == "code") {
      round(prevalence$employment_two_stage, digits)
    } else {
      NA_real_
    },
    employment_lower = round(bounds$lower, digits),
    employment_upper = round(bounds$upper, digits),
    employment_lower_wide = round(bounds$lower_wide, digits),
    employment_upper_wide = round(bounds$upper_wide, digits),
    idf_equal_unit = round_idf(prevalence$equal_unit),
    idf_employment = round_idf(prevalence$employment)
  )
  list(
    rule = rule, ids = ids, result = result,
    prevalence = prevalence_table, ranks = ranks,
    summary = bind_rows(universe_summary, agreement, bound_summary) |>
      mutate(unit_rule = rule, bound_type = bound_types[[rule]], .before = 1L)
  )
}
results <- lapply(c("soc", "code"), rule_results)
names(results) <- c("soc", "code")
complete_cells_by_rule <- lapply(results, function(x) {
  x$result$status$cell[x$result$status$status == "complete"]
})
# Every cell that is complete under the code rule has a profiled .00 code for
# each SOC occupation, so it is also complete under the soc rule.
stopifnot(
  all(complete_cells_by_rule$code %in% complete_cells_by_rule$soc),
  identical(results$soc$result$status$cell, results$code$result$status$cell)
)

# Roster table: where every O*NET-SOC code goes under both rules.
soc_units <- build_cell_units(parts, roster, "soc")
code_units <- build_cell_units(parts, roster, "code")
status_soc <- stats::setNames(inventory$status_soc, inventory$cell)
status_code <- stats::setNames(inventory$status_code, inventory$cell)
roster_table <- roster |>
  mutate(
    cell_code = bridge_cell,
    map_type = bridge$map_type,
    soc_rule_unit = onet_soc_code %in% soc_units$unit[soc_units$in_roster],
    code_rule_unit = onet_soc_code %in% code_units$unit[code_units$in_roster],
    cell_status_soc = unname(status_soc[cell_code]),
    cell_status_code = unname(status_code[cell_code]),
    soc_rule_in_universe = onet_soc_code %in% results$soc$ids,
    code_rule_in_universe = onet_soc_code %in% results$code$ids
  ) |>
  select(
    onet_soc_code, soc_code, title, profiled, taxonomy_class, n_codes_in_soc,
    soc_minor_group, soc_prefix5, cell_code, map_type, soc_rule_unit,
    code_rule_unit, cell_status_soc, cell_status_code, soc_rule_in_universe,
    code_rule_in_universe
  )
stopifnot(
  nrow(roster_table) == 1016L,
  all(!roster_table$soc_rule_in_universe | roster_table$profiled),
  all(!roster_table$code_rule_in_universe | roster_table$profiled),
  sum(roster_table$soc_rule_in_universe) == length(results$soc$ids),
  sum(roster_table$code_rule_in_universe) == length(results$code$ids)
)

inventory_table <- inventory |>
  mutate(
    cell_type = ifelse(cell %in% combination_codes, "combination", "direct")
  ) |>
  select(
    oews_code = "cell", "title", "raw_o_group", "n_raw_rows_same_code",
    "cell_type", "employment",
    "emp_prse", "n_parts", "n_roster_codes", "n_profiled_roster_codes",
    "n_socs_without_00_profile", "n_socs_without_00_profile_with_nested",
    "n_nested_profiled_unused_soc_rule", "n_units_soc", "n_profiled_units_soc",
    "status_soc", "n_units_code", "n_profiled_units_code", "status_code"
  ) |>
  mutate(employment = whole(employment))

# Combination audit: BLS definitions, the package's own constituent list, and
# what the O*NET roster can fill.
combination_audit <- bind_rows(lapply(combination_codes, function(code) {
  bls <- sort(bls_parts$part_soc_code[bls_parts$oews_code == code])
  pkg <- sort(package_parts$part_soc_code[package_parts$reference_soc_code == code])
  row <- inventory[inventory$cell == code, ]
  raw_rows <- raw_combination_rows[raw_combination_rows$occ_code == code, ]
  broad_row <- raw_rows[raw_rows$o_group == "broad", ]
  tibble::tibble(
    oews_code = code,
    oews_title = row$title,
    raw_o_groups = paste(sort(raw_rows$o_group), collapse = ";"),
    n_raw_rows = nrow(raw_rows),
    n_raw_detailed_rows = sum(raw_rows$o_group == "detailed"),
    raw_broad_row_same_employment = if (nrow(broad_row) == 0L) {
      NA
    } else {
      broad_row$tot_emp == row$employment
    },
    n_panel_rows = sum(panel$reference_soc_code == code),
    employment = whole(row$employment),
    bls_parts = paste(bls, collapse = ";"),
    package_parts = paste(pkg, collapse = ";"),
    parts_agree = setequal(bls, pkg),
    package_extra_parts = paste(setdiff(pkg, bls), collapse = ";"),
    bls_missing_from_package = paste(setdiff(bls, pkg), collapse = ";"),
    n_parts_in_roster = sum(bls %in% roster$soc_code),
    n_parts_with_profiled_00 = sum(
      paste0(bls, ".00") %in% roster$onet_soc_code[roster$profiled]
    ),
    status_soc = row$status_soc,
    status_code = row$status_code
  )
}))
stopifnot(
  nrow(combination_audit) == 12L,
  all(combination_audit$n_panel_rows == 1L),
  all(combination_audit$n_raw_detailed_rows == 1L),
  sum(combination_audit$n_raw_rows) == nrow(raw_combination_rows)
)

group_info <- function(rule) {
  ids <- results[[rule]]$ids
  c(
    n_units = length(ids),
    n_minor_groups = length(unique(substr(ids, 1, 4))),
    n_prefix5_groups = length(unique(substr(ids, 1, 5)))
  )
}
accounting_row <- function(source, quantity, observed, official = NA) {
  tibble::tibble(
    source = source, quantity = quantity, official = as.numeric(official),
    observed = as.numeric(observed),
    status = ifelse(
      is.na(official), "observed_only",
      ifelse(official == observed, "match", "mismatch")
    )
  )
}
bridge_counts <- table(bridge$map_type)
accounting <- bind_rows(
  taxonomy_accounting |>
    transmute(source = "onet_taxonomy", quantity, official, observed, status) |>
    mutate(official = as.numeric(official), observed = as.numeric(observed)),
  accounting_row("oews_raw", "rows_in_file", nrow(oews_scope)),
  accounting_row(
    "oews_raw", "rows_area_99_us_national",
    scope_rows[["rows_area_99_us_national"]], official = nrow(oews_scope)
  ),
  accounting_row(
    "oews_raw", "rows_naics_000000_cross_industry",
    scope_rows[["rows_naics_000000_cross_industry"]], official = nrow(oews_scope)
  ),
  accounting_row(
    "oews_raw", "rows_own_code_1235", scope_rows[["rows_own_code_1235"]],
    official = nrow(oews_scope)
  ),
  accounting_row("oews_raw", "detailed_rows", group_counts[["detailed"]]),
  accounting_row("oews_raw", "broad_rows", group_counts[["broad"]]),
  accounting_row("oews_raw", "minor_rows", group_counts[["minor"]]),
  accounting_row("oews_raw", "major_rows", group_counts[["major"]]),
  accounting_row("oews_raw", "total_rows", group_counts[["total"]]),
  accounting_row("oews_raw", "total_employment", official_total),
  accounting_row("oews_raw", "detailed_employment", sum(detailed$tot_emp)),
  accounting_row(
    "oews_raw", "total_minus_detailed_employment",
    official_total - sum(detailed$tot_emp)
  ),
  accounting_row(
    "oews_raw", "raw_rows_with_a_combination_code", nrow(raw_combination_rows)
  ),
  accounting_row(
    "oews_raw", "combination_codes_with_detailed_row",
    nrow(raw_detailed_combinations)
  ),
  accounting_row(
    "oews_raw", "combination_codes_also_broad_row",
    nrow(raw_broad_combinations)
  ),
  accounting_row(
    "oews_raw", "broad_codes_that_repeat_a_detailed_code",
    length(broad_codes_also_detailed)
  ),
  accounting_row(
    "oews_raw", "combination_employment_in_repeated_broad_rows",
    sum(raw_broad_combinations$tot_emp)
  ),
  accounting_row("oews_panel", "panel_cells", nrow(panel)),
  accounting_row(
    "oews_panel", "combination_codes_kept_in_panel",
    sum(panel$reference_soc_code %in% combination_codes)
  ),
  accounting_row("bls_definitions", "definition_rows", nrow(definitions)),
  accounting_row("bls_definitions", "combination_codes", length(combination_codes)),
  accounting_row("bls_definitions", "combination_parts", nrow(bls_parts)),
  accounting_row(
    "bls_definitions", "definition_codes_without_oews_cell",
    length(setdiff(definitions$code, detailed$occ_code))
  ),
  accounting_row(
    "package_constituent_list", "combination_codes_listed",
    dplyr::n_distinct(package_parts$reference_soc_code)
  ),
  accounting_row(
    "package_constituent_list", "listed_codes_not_in_2025_cells",
    length(package_only_codes)
  ),
  accounting_row(
    "package_constituent_list", "parts_not_in_bls_definitions",
    sum(!paste(package_parts$reference_soc_code, package_parts$part_soc_code) %in%
          expected_pairs & package_parts$reference_soc_code %in% combination_codes)
  ),
  accounting_row(
    "package_constituent_list", "bls_parts_missing_from_package",
    sum(!expected_pairs %in%
          paste(package_parts$reference_soc_code, package_parts$part_soc_code))
  ),
  accounting_row("bridge", "direct", bridge_counts[["direct"]]),
  accounting_row("bridge", "oews_combination", bridge_counts[["oews_combination"]]),
  accounting_row("bridge", "not_in_panel", bridge_counts[["not_in_panel"]]),
  accounting_row(
    "bridge", "profiled_codes_not_in_panel",
    sum(roster$profiled & bridge$map_type == "not_in_panel")
  ),
  accounting_row(
    "bridge", "codes_where_bridge_and_definitions_disagree",
    sum(!mapply(identical, definition_cell, bridge_cell))
  ),
  accounting_row("soc_rule_universe", "units", group_info("soc")[["n_units"]]),
  accounting_row(
    "soc_rule_universe", "minor_groups_four_characters",
    group_info("soc")[["n_minor_groups"]]
  ),
  accounting_row(
    "soc_rule_universe", "prefix_groups_five_characters",
    group_info("soc")[["n_prefix5_groups"]]
  ),
  accounting_row("code_rule_universe", "units", group_info("code")[["n_units"]]),
  accounting_row(
    "code_rule_universe", "minor_groups_four_characters",
    group_info("code")[["n_minor_groups"]]
  ),
  accounting_row(
    "code_rule_universe", "prefix_groups_five_characters",
    group_info("code")[["n_prefix5_groups"]]
  )
) |>
  mutate(official = whole(official), observed = whole(observed))

provenance <- inputs |>
  select(input, release, file_name, source_url, sha256, access_note)

coverage_table <- coverage |>
  mutate(n_cells = whole(n_cells), employment = whole(employment)) |>
  select(
    unit_rule, bound_type, scope, n_cells, employment, share_official_total,
    share_returned_panel
  ) |>
  mutate(
    share_official_total = round(share_official_total, 10),
    share_returned_panel = round(share_returned_panel, 10)
  )
prevalence_table <- bind_rows(lapply(results, `[[`, "prevalence"))
summary_table <- bind_rows(lapply(results, `[[`, "summary")) |>
  mutate(value = round(value, 10))
rank_table <- bind_rows(lapply(results, `[[`, "ranks")) |>
  mutate(across(starts_with("mean_idf_"), \(x) round(x, 10)))
stopifnot(
  nrow(prevalence_table) == 2L * length(items),
  all(summary_table$value[summary_table$metric == "within_bounds_share"] == 1)
)

write_snapshot_csv(provenance, file.path(output_dir, "employment-provenance.csv"))
write_snapshot_csv(accounting, file.path(output_dir, "employment-accounting.csv"))
write_snapshot_csv(roster_table, file.path(output_dir, "employment-roster.csv"))
write_snapshot_csv(
  inventory_table, file.path(output_dir, "employment-cell-inventory.csv")
)
write_snapshot_csv(
  combination_audit, file.path(output_dir, "employment-combination-audit.csv")
)
write_snapshot_csv(coverage_table, file.path(output_dir, "employment-coverage.csv"))
write_snapshot_csv(
  prevalence_table, file.path(output_dir, "activity-employment-prevalence.csv")
)
write_snapshot_csv(
  summary_table, file.path(output_dir, "activity-employment-summary.csv")
)
write_snapshot_csv(
  rank_table, file.path(output_dir, "activity-employment-rank-sensitivity.csv")
)

print(as.data.frame(accounting), max = 2000)
print(as.data.frame(coverage_table), digits = 6)
print(as.data.frame(combination_audit))
print(as.data.frame(summary_table), digits = 5, max = 2000)
print(as.data.frame(rank_table |> select(unit_rule, onet_soc_code, title, selected_reason)))
print(file.info(list.files(
  output_dir, pattern = "^(employment|activity-employment)-", full.names = TRUE
))["size"])
