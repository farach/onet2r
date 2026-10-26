# Regenerate the article's rated-skills snapshots without an API key or network
# access. Run from the repository root after installing the current package:
# Rscript data-raw/occupational-skills.R path/to/db_30_3_text.zip
# The script reads only the pinned local O*NET 30.3 text archive and checks its
# SHA-256 value. It never downloads anything.
library(onet2r)
library(dplyr)
options(cli.progress_show_after = Inf)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !file.exists(args[[1]])) {
  stop("Supply one existing local db_30_3_text.zip path.", call. = FALSE)
}
archive_path <- args[[1]]
archive_sha256 <- digest::digest(file = archive_path, algo = "sha256")
pinned_sha256 <- "7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780"
if (!identical(archive_sha256, pinned_sha256)) {
  stop("The archive is not the pinned O*NET 30.3 text database.", call. = FALSE)
}
for (helper in c(
  "occupational-distinctiveness.R", "occupational-neighbors.R",
  "occupational-skills.R"
)) {
  source(file.path("inst", "article-helpers", helper))
}
output_dir <- file.path("inst", "extdata", "occupational-extensions")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

read_reference <- function(table) {
  onet_archive_reference("30.3", table, path = archive_path)
}
occupations <- onet_archive_read("30.3", "Occupation Data", path = archive_path) |>
  distinct(onet_soc_code, title)
links <- onet_archive_read("30.3", "Tasks to DWAs", path = archive_path)
dwa_occupations <- sort(unique(links$onet_soc_code), method = "radix")
long <- skills_long(list(
  essential = read_reference("Essential Skills"),
  transferable = read_reference("Transferable Skills")
))
ranges <- skills_scale_ranges(read_reference("Scales Reference"))
elements <- long |>
  distinct(domain, element_id, element_name) |>
  arrange(domain, element_id)
anchors <- skills_level_anchors(
  read_reference("Level Scale Anchors"), elements$element_id
)
blocks <- skills_blocks()
stopifnot(
  nrow(occupations) == 1016L,
  !anyDuplicated(occupations$onet_soc_code),
  length(dwa_occupations) == 923L,
  all(long$occupation %in% dwa_occupations),
  setequal(unique(long$block), blocks$block),
  sum(elements$domain == "essential") == 10L,
  sum(elements$domain == "transferable") == 25L,
  nrow(anchors) == 105L
)

# Audit of the raw ratings, flags and the Level auto-fill rule.
audit <- skills_rating_audit(long, ranges)
stopifnot(
  all(audit$n_missing_value == 0L),
  all(audit$n_outside_scale == 0L),
  all(audit$n_not_two_decimals == 0L),
  all(audit$n_raters_min == 8 & audit$n_raters_max == 8),
  all(audit$n_recommend_suppress_missing == 0L),
  identical(audit$n_recommend_suppress, c(0L, 16L, 0L, 131L)),
  identical(audit$n_not_relevant, c(0L, 285L, 0L, 3098L)),
  all(audit$n_not_relevant_missing[audit$scale == "IM"] ==
        audit$n_rows[audit$scale == "IM"]),
  all(audit$n_not_relevant_missing[audit$scale == "LV"] == 0L),
  all(audit$se_min_recommend_suppress[audit$n_recommend_suppress > 0L] >
        audit$se_max_not_suppressed[audit$n_recommend_suppress > 0L])
)

# Fixed common-support universe. The primary universe is the set of occupations
# with DWA profiles that are complete in all four blocks and carry no Recommend
# Suppress flag. Not Relevant is audited and never removes an occupation.
eligibility <- skills_eligibility(long, dwa_occupations)
common <- skills_common_universe(
  eligibility, occupations[occupations$onet_soc_code %in% dwa_occupations, ]
)
universe <- common$onet_soc_code[common$in_common_universe]
pool <- common$onet_soc_code[common$in_pool_complete]
stopifnot(
  nrow(common) == 923L,
  length(pool) == 894L,
  length(universe) == 823L,
  sum(common$in_pool_no_not_relevant) == 93L,
  sum(common$in_pool_no_flags) == 84L,
  sum(common$exclusion_reason %in% "incomplete_skills_profile") == 29L,
  sum(common$exclusion_reason %in% "recommend_suppress_flag") == 71L,
  all(universe %in% pool)
)
matrices <- skills_profile_matrices(long, universe)
pool_matrices <- skills_profile_matrices(long, pool)
floored_matrices <- skills_profile_matrices(long, universe, nr_floor = TRUE)

block_range <- function(block) {
  scale <- blocks$scale[match(block, blocks$block)]
  ranges$range[match(scale, ranges$scale)]
}
k_max <- 10L
distance_digits <- 8L
block_ids <- stats::setNames(blocks$block, blocks$block)
primary_distance <- lapply(block_ids, function(b) {
  skills_block_distance(matrices[[b]], block_range(b))
})
primary_neighbors <- lapply(
  primary_distance, skills_neighbors, k_max = k_max, digits = distance_digits
)
# Ties are decided on exact integer sums. Rounding the published distance must
# not change any neighbor set.
for (b in blocks$block) {
  rounded <- distance_neighbors(
    skills_distance_matrix(primary_distance[[b]]),
    k_max = k_max, digits = distance_digits
  )
  keys <- c("occupation", "neighbor", "rank")
  stopifnot(identical(
    as.data.frame(primary_neighbors[[b]][keys]), as.data.frame(rounded[keys])
  ))
}
neighbors <- bind_rows(lapply(block_ids, function(b) {
  primary_neighbors[[b]] |>
    mutate(block = b) |>
    select(block, occupation, rank, neighbor, distance)
})) |>
  arrange(block, occupation, rank, neighbor)
stopifnot(
  !anyNA(neighbors),
  all(neighbors$rank >= 1L & neighbors$rank <= k_max),
  all(neighbors$occupation %in% universe),
  all(neighbors$neighbor %in% universe),
  all(neighbors$occupation != neighbors$neighbor),
  all(neighbors$distance >= 0 & neighbors$distance <= 1)
)

# Sensitivity of the nearest sets. These are overlaps, not significance tests.
# The first rows describe how many neighbors the primary tables keep because
# of ties at the cutoff.
sensitivity <- list()
ordinal_overlap <- list()
for (b in blocks$block) {
  sensitivity[[length(sensitivity) + 1L]] <- bind_rows(lapply(
    c(1L, 5L, 10L), function(k) {
      set_sizes <- primary_neighbors[[b]] |>
        filter(rank <= k) |>
        count(occupation) |>
        pull(n)
      stopifnot(length(set_sizes) == length(universe), all(set_sizes >= k))
      average_size <- mean(set_sizes)
      share_larger <- mean(set_sizes > k)
      tibble::tibble(
        k = k, metric = c("mean_set_size", "share_set_larger_than_k"),
        value = c(average_size, share_larger)
      )
    }
  )) |>
    mutate(alternative = "primary_tie_retention", block = b, .before = 1L)
}
for (b in blocks$block) {
  scale <- blocks$scale[blocks$block == b]
  primary <- primary_neighbors[[b]]
  primary_matrix <- skills_distance_matrix(primary_distance[[b]])
  alternatives <- list(
    ordinal_percentile = skills_block_distance(
      matrices[[b]], block_range(b), "ordinal"
    ),
    composition_secondary = skills_block_distance(
      matrices[[b]], block_range(b), "composition"
    )
  )
  if (scale == "LV") {
    alternatives$nr_floor <- skills_block_distance(
      floored_matrices[[b]], block_range(b)
    )
  }
  for (a in names(alternatives)) {
    alternative <- skills_neighbors(alternatives[[a]], k_max, distance_digits)
    overlap <- skills_neighbor_overlap(primary, alternative, c(1L, 5L, 10L))
    correlation <- distance_rank_correlation(
      primary_matrix, skills_distance_matrix(alternatives[[a]]),
      digits = distance_digits
    )
    sensitivity[[length(sensitivity) + 1L]] <- bind_rows(
      skills_overlap_summary(overlap),
      tibble::tibble(k = NA_integer_, metric = "distance_spearman", value = correlation)
    ) |>
      mutate(alternative = a, block = b, .before = 1L)
    if (a == "ordinal_percentile") {
      ordinal_overlap[[length(ordinal_overlap) + 1L]] <- overlap |>
        mutate(alternative = a, block = b, .before = 1L)
    }
  }
  pool_neighbors <- skills_neighbors(
    skills_block_distance(pool_matrices[[b]], block_range(b)),
    k_max, distance_digits
  ) |>
    restrict_neighbor_queries(universe)
  overlap <- skills_neighbor_overlap(primary, pool_neighbors, c(1L, 5L, 10L))
  outside <- vapply(
    c(1L, 5L, 10L),
    function(k) {
      mean(!pool_neighbors$neighbor[pool_neighbors$rank <= k] %in% universe)
    },
    numeric(1)
  )
  sensitivity[[length(sensitivity) + 1L]] <- bind_rows(
    skills_overlap_summary(overlap),
    tibble::tibble(
      k = c(1L, 5L, 10L), metric = "share_neighbors_outside_primary_universe",
      value = outside
    )
  ) |>
    mutate(alternative = "pool_894", block = b, .before = 1L)
}
sensitivity <- bind_rows(sensitivity) |>
  mutate(value = round(value, 8))
ordinal_overlap <- bind_rows(ordinal_overlap) |>
  mutate(across(starts_with("jaccard_"), \(x) round(x, 8))) |>
  arrange(block, occupation)

distribution <- skills_rating_distribution(long, universe, ranges) |>
  mutate(across(where(is.double), \(x) round(x, 8)))
thresholds <- skills_threshold_table(long, universe, pool, anchors) |>
  mutate(across(starts_with("share_"), \(x) round(x, 8)))

# Item-level gaps for pairs chosen by rule: nearest, median and farthest pair
# in each block, with ties broken by occupation code.
titles <- occupations |> distinct(onet_soc_code, title)
pair_examples <- bind_rows(lapply(block_ids, function(b) {
  chosen <- skills_pair_selection(primary_distance[[b]]$sums)
  bind_rows(lapply(seq_len(nrow(chosen)), function(i) {
    gaps <- rating_pair_gaps(
      matrices[b], ranges, chosen$occupation_a[[i]], chosen$occupation_b[[i]]
    )
    block_distance <- chosen$exact_sum[[i]] / primary_distance[[b]]$denominator
    stopifnot(abs(mean(gaps$gap_normalized) - block_distance) < 1e-12)
    gaps |>
      mutate(
        pair_kind = chosen$pair_kind[[i]],
        occupation_a = chosen$occupation_a[[i]],
        occupation_b = chosen$occupation_b[[i]],
        block_distance = round(block_distance, 8),
        gap_normalized = round(gap_normalized, 8)
      )
  })) |>
    left_join(
      elements |> select(element_id, element_name),
      by = "element_id", relationship = "many-to-one"
    ) |>
    left_join(
      titles |> transmute(occupation_a = onet_soc_code, title_a = title),
      by = "occupation_a", relationship = "many-to-one"
    ) |>
    left_join(
      titles |> transmute(occupation_b = onet_soc_code, title_b = title),
      by = "occupation_b", relationship = "many-to-one"
    )
})) |>
  select(
    block, pair_kind, occupation_a, title_a, occupation_b, title_b,
    block_distance, element_id, element_name, value_a, value_b, gap,
    gap_normalized
  )
stopifnot(nrow(pair_examples) == 3L * (2L * 10L + 2L * 25L), !anyNA(pair_examples))

block_columns <- function(suffix) paste0(blocks$block, "_", suffix)
common_out <- common |>
  select(
    onet_soc_code, title,
    all_of(c(
      block_columns("n_elements"), block_columns("complete"),
      block_columns("n_recommend_suppress"), block_columns("n_not_relevant")
    )),
    complete_all_blocks, n_recommend_suppress, n_not_relevant, in_pool_complete,
    in_common_universe, in_pool_no_not_relevant, in_pool_no_flags,
    exclusion_reason
  )
provenance <- tibble::tibble(
  release_version = "30.3",
  source_url = "https://www.onetcenter.org/dl_files/database/db_30_3_text.zip",
  source_sha256 = archive_sha256,
  rating_procedures_report = "O*NET Analyst Occupational Skills Ratings: Procedures Update (revised March 2018)",
  n_dwa_occupations = length(dwa_occupations),
  n_pool_complete = length(pool),
  n_primary_universe = length(universe),
  n_excluded_incomplete_skills_profile = sum(common$exclusion_reason %in% "incomplete_skills_profile"),
  n_excluded_recommend_suppress = sum(common$exclusion_reason %in% "recommend_suppress_flag"),
  n_pool_no_not_relevant = sum(common$in_pool_no_not_relevant),
  n_pool_no_flags = sum(common$in_pool_no_flags),
  n_elements_essential = sum(elements$domain == "essential"),
  n_elements_transferable = sum(elements$domain == "transferable"),
  range_importance = ranges$range[ranges$scale == "IM"],
  range_level = ranges$range[ranges$scale == "LV"],
  k_max = k_max,
  distance_digits = distance_digits,
  primary_distance = "Mean over elements of absolute rating gap divided by scale range, one block at a time",
  primary_universe = "DWA occupations complete in all four blocks with no Recommend Suppress flag",
  not_relevant_treatment = "Audited and kept; recorded values are used in the primary distance"
)

write_snapshot_csv(provenance, file.path(output_dir, "skills-provenance.csv"))
write_snapshot_csv(audit, file.path(output_dir, "skills-rating-audit.csv"))
write_snapshot_csv(common_out, file.path(output_dir, "skills-common-universe.csv"))
write_snapshot_csv(
  distribution, file.path(output_dir, "skills-rating-distributions.csv")
)
write_snapshot_csv(thresholds, file.path(output_dir, "skills-thresholds.csv"))
write_snapshot_csv(anchors, file.path(output_dir, "skills-level-anchors.csv"))
write_snapshot_csv(neighbors, file.path(output_dir, "skills-nearest-neighbors.csv"))
write_snapshot_csv(
  sensitivity, file.path(output_dir, "skills-neighbor-sensitivity.csv")
)
write_snapshot_csv(
  ordinal_overlap, file.path(output_dir, "skills-ordinal-neighbor-overlap.csv")
)
write_snapshot_csv(
  pair_examples, file.path(output_dir, "skills-pair-gap-examples.csv")
)
print(as.data.frame(t(provenance)))
print(as.data.frame(t(audit)))
cat(
  "sensitivity rows:", nrow(sensitivity),
  " ordinal overlap rows:", nrow(ordinal_overlap),
  " neighbor rows:", nrow(neighbors), "\n"
)
print(file.info(list.files(output_dir, pattern = "^skills-", full.names = TRUE))["size"])
