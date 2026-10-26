# Research choices for the article, not an exported package measure.
# Employment-weighted activity prevalence under named allocation rules. These
# helpers need dplyr, tibble and cli. They do not call other article helper
# files. Source occupational-distinctiveness.R to get write_snapshot_csv().
#
# Vocabulary
#   cell   One published OEWS detailed row. Cells are disjoint and counted once.
#   unit   One occupational description that shares the cell employment.
#   rule   How units are defined inside a cell.
#          soc:  one unit per BLS-defined SOC occupation, represented by its
#                .00 O*NET-SOC profile. Units in a cell are distinct SOC
#                occupations, so they are disjoint worker groups. Nested
#                detailed codes are never substituted for a missing .00 profile.
#          code: one unit per O*NET-SOC code in the cell, including nested
#                detailed codes that overlap their .00 parent. This is a
#                synthetic profile reweighting, not a disjoint worker
#                allocation. Its min/max envelope is a profile-allocation
#                sensitivity, not a sharp bound over mutually exclusive worker
#                groups.

# Counts published in the O*NET-SOC 2019 taxonomy summary
# (https://www.onetcenter.org/dl_files/Taxonomy2019_Summary.pdf, pages 7 to 9).
official_taxonomy_counts <- function() {
  tibble::tribble(
    ~quantity, ~official,
    "n_titles", 1016L,
    "n_socs", 867L,
    "n_socs_with_detailed", 76L,
    "n_data_level", 923L,
    "n_soc_level_without_detailed", 722L,
    "n_soc_level_with_detailed", 52L,
    "n_detailed", 149L,
    "n_non_data_level", 93L,
    "n_military", 19L,
    "n_all_other_with_detailed", 24L,
    "n_all_other_without_detailed", 50L
  )
}

# The BLS occupation definitions state which 2018 SOC occupations each
# combined OEWS code includes. Wording varies slightly, for example the word
# "the" is sometimes missing.
bls_combination_pattern <- paste0(
  "^\\s*This occupation\\s+includes\\s+(the\\s+)?2018 SOC occupations\\s"
)

bls_combination_parts <- function(code, definition) {
  if (!is.character(code) || !is.character(definition) ||
      length(code) != length(definition) || anyNA(code) ||
      anyDuplicated(code)) {
    cli::cli_abort("Supply one unique OEWS code and one definition per row.")
  }
  hits <- which(!is.na(definition) & grepl(bls_combination_pattern, definition))
  found <- regmatches(
    definition[hits], gregexpr("[0-9]{2}-[0-9]{4}", definition[hits])
  )
  parts <- tibble::tibble(
    oews_code = rep(code[hits], lengths(found)),
    part_soc_code = as.character(unlist(found, use.names = FALSE))
  )
  if (any(table(parts$oews_code) < 2L) || anyDuplicated(parts$part_soc_code) ||
      any(parts$part_soc_code == parts$oews_code)) {
    cli::cli_abort(
      "Each combination needs at least two distinct parts that are not the combined code."
    )
  }
  parts
}

# Classify every row of Occupation Data. A code counts as profiled when it has
# at least one activity link, which is how the article defines data-level
# occupations.
classify_onet_roster <- function(occupation_data, profiled_codes) {
  if (!is.data.frame(occupation_data) ||
      !all(c("onet_soc_code", "title") %in% names(occupation_data))) {
    cli::cli_abort("Supply Occupation Data with onet_soc_code and title columns.")
  }
  codes <- occupation_data$onet_soc_code
  if (!is.character(codes) || anyNA(codes) || anyDuplicated(codes) ||
      any(!grepl("^[0-9]{2}-[0-9]{4}\\.[0-9]{2}$", codes))) {
    cli::cli_abort("Occupation Data needs unique, well-formed O*NET-SOC codes.")
  }
  if (!is.character(profiled_codes) || !all(profiled_codes %in% codes)) {
    cli::cli_abort("Every profiled code must appear in Occupation Data.")
  }
  roster <- tibble::tibble(
    onet_soc_code = codes,
    soc_code = substr(codes, 1, 7),
    detail_suffix = substr(codes, 9, 10),
    title = as.character(occupation_data$title),
    profiled = codes %in% profiled_codes
  )
  is_00 <- roster$detail_suffix == "00"
  if (anyDuplicated(roster$soc_code[is_00]) ||
      !all(roster$soc_code %in% roster$soc_code[is_00])) {
    cli::cli_abort("Every SOC needs exactly one .00 O*NET-SOC code.")
  }
  codes_per_soc <- table(roster$soc_code)
  roster$n_codes_in_soc <- as.integer(codes_per_soc[roster$soc_code])
  profiled_00 <- stats::setNames(roster$profiled[is_00], roster$soc_code[is_00])
  roster$soc_00_profiled <- unname(profiled_00[roster$soc_code])
  has_nested <- is_00 & roster$n_codes_in_soc > 1L
  is_all_other <- grepl("All Other", roster$title, fixed = TRUE)
  roster$taxonomy_class <- dplyr::case_when(
    startsWith(roster$onet_soc_code, "55-") ~ "military",
    !is_00 & roster$profiled ~ "detailed",
    !is_00 ~ "detailed_unprofiled",
    roster$profiled & has_nested ~ "soc_level_with_detailed",
    roster$profiled ~ "soc_level",
    is_all_other & has_nested ~ "all_other_with_detailed",
    is_all_other ~ "all_other",
    .default = "other_unprofiled"
  )
  # A SOC minor group is the first four characters, for example 11-1. The
  # article's five-character prefix, for example 11-10, is a different grouping.
  roster$soc_minor_group <- substr(roster$soc_code, 1, 4)
  roster$soc_prefix5 <- substr(roster$soc_code, 1, 5)
  roster
}

roster_accounting <- function(roster) {
  observed <- c(
    n_titles = nrow(roster),
    n_socs = dplyr::n_distinct(roster$soc_code),
    n_socs_with_detailed = dplyr::n_distinct(
      roster$soc_code[roster$n_codes_in_soc > 1L]
    ),
    n_data_level = sum(roster$profiled),
    n_soc_level_without_detailed = sum(roster$taxonomy_class == "soc_level"),
    n_soc_level_with_detailed = sum(
      roster$taxonomy_class == "soc_level_with_detailed"
    ),
    n_detailed = sum(roster$taxonomy_class == "detailed"),
    n_non_data_level = sum(!roster$profiled),
    n_military = sum(roster$taxonomy_class == "military"),
    n_all_other_with_detailed = sum(
      roster$taxonomy_class == "all_other_with_detailed"
    ),
    n_all_other_without_detailed = sum(roster$taxonomy_class == "all_other")
  )
  expected <- official_taxonomy_counts()
  out <- tibble::tibble(
    quantity = expected$quantity,
    official = expected$official,
    observed = as.integer(observed[expected$quantity])
  )
  out$status <- ifelse(out$official == out$observed, "match", "mismatch")
  extra <- tibble::tibble(
    quantity = c("n_other_unprofiled", "n_detailed_unprofiled"),
    official = NA_integer_,
    observed = c(
      sum(roster$taxonomy_class == "other_unprofiled"),
      sum(roster$taxonomy_class == "detailed_unprofiled")
    ),
    status = "observed_only"
  )
  dplyr::bind_rows(out, extra)
}

# One row per unit that shares a cell's employment. parts has one row per SOC
# occupation in a cell. Unit identifiers must be unique across cells so that no
# cell mass is counted twice.
build_cell_units <- function(parts, roster, rule = c("soc", "code")) {
  rule <- match.arg(rule)
  if (!is.data.frame(parts) || !all(c("cell", "soc_code") %in% names(parts)) ||
      anyNA(parts[c("cell", "soc_code")]) || anyDuplicated(parts$soc_code)) {
    cli::cli_abort("Each SOC occupation must belong to exactly one OEWS cell.")
  }
  if (rule == "soc") {
    roster_00 <- roster[roster$detail_suffix == "00", ]
    unit <- paste0(parts$soc_code, ".00")
    idx <- match(unit, roster_00$onet_soc_code)
    units <- tibble::tibble(
      cell = parts$cell,
      soc_code = parts$soc_code,
      unit = unit,
      in_roster = !is.na(idx),
      profiled = !is.na(idx) & roster_00$profiled[idx]
    )
  } else {
    units <- parts[c("cell", "soc_code")] |>
      dplyr::left_join(
        roster[c("soc_code", "onet_soc_code", "profiled")],
        by = "soc_code", relationship = "one-to-many"
      ) |>
      dplyr::mutate(
        in_roster = !is.na(.data$onet_soc_code),
        unit = dplyr::coalesce(
          .data$onet_soc_code, paste0(.data$soc_code, ".--")
        ),
        profiled = dplyr::coalesce(.data$profiled, FALSE)
      ) |>
      dplyr::select("cell", "soc_code", "unit", "in_roster", "profiled")
  }
  if (anyDuplicated(units$unit)) {
    cli::cli_abort("Unit identifiers must be unique across cells.")
  }
  units
}

# Status of each cell. unmatched: no O*NET-SOC code exists for any SOC in the
# cell. complete: every unit has an activity profile. no_profile: no unit has
# one. partial: some units have one.
cell_completeness <- function(units, cells) {
  if (!is.data.frame(cells) || !all(c("cell", "employment") %in% names(cells)) ||
      anyNA(cells$cell) || anyDuplicated(cells$cell) ||
      !is.numeric(cells$employment) || any(!is.finite(cells$employment)) ||
      any(cells$employment <= 0)) {
    cli::cli_abort("Cells need unique codes and positive, finite employment.")
  }
  needed <- c("cell", "unit", "in_roster", "profiled")
  if (!is.data.frame(units) || !all(needed %in% names(units)) ||
      anyDuplicated(units$unit) || any(!units$cell %in% cells$cell)) {
    cli::cli_abort("Units need unique identifiers and belong to a listed cell.")
  }
  counts <- units |>
    dplyr::summarise(
      n_units = dplyr::n(),
      n_in_roster = sum(.data$in_roster),
      n_profiled = sum(.data$profiled),
      .by = "cell"
    )
  cells |>
    dplyr::left_join(counts, by = "cell", relationship = "one-to-one") |>
    dplyr::mutate(
      n_units = dplyr::coalesce(.data$n_units, 0L),
      n_in_roster = dplyr::coalesce(.data$n_in_roster, 0L),
      n_profiled = dplyr::coalesce(.data$n_profiled, 0L),
      status = dplyr::case_when(
        .data$n_in_roster == 0L ~ "unmatched",
        .data$n_profiled == .data$n_units ~ "complete",
        .data$n_profiled == 0L ~ "no_profile",
        .default = "partial"
      )
    )
}

# Cell inventory under both rules. cells has cell and employment, plus any
# descriptive columns to carry along. parts maps each SOC occupation to one cell.
cell_inventory <- function(cells, parts, roster) {
  status_soc <- cell_completeness(
    build_cell_units(parts, roster, "soc"), cells
  )
  status_code <- cell_completeness(
    build_cell_units(parts, roster, "code"), cells
  )
  per_soc <- roster |>
    dplyr::summarise(
      soc_00_profiled = any(.data$profiled & .data$detail_suffix == "00"),
      n_nested_profiled = sum(.data$profiled & .data$detail_suffix != "00"),
      n_roster_codes = dplyr::n(),
      n_profiled_roster_codes = sum(.data$profiled),
      .by = "soc_code"
    )
  per_cell <- parts |>
    dplyr::left_join(per_soc, by = "soc_code", relationship = "many-to-one") |>
    dplyr::mutate(
      soc_00_profiled = dplyr::coalesce(.data$soc_00_profiled, FALSE),
      n_nested_profiled = dplyr::coalesce(.data$n_nested_profiled, 0L),
      n_roster_codes = dplyr::coalesce(.data$n_roster_codes, 0L),
      n_profiled_roster_codes = dplyr::coalesce(
        .data$n_profiled_roster_codes, 0L
      )
    ) |>
    dplyr::summarise(
      n_parts = dplyr::n(),
      n_roster_codes = sum(.data$n_roster_codes),
      n_profiled_roster_codes = sum(.data$n_profiled_roster_codes),
      n_socs_without_00_profile = sum(!.data$soc_00_profiled),
      n_socs_without_00_profile_with_nested = sum(
        !.data$soc_00_profiled & .data$n_nested_profiled > 0L
      ),
      n_nested_profiled_unused_soc_rule = sum(.data$n_nested_profiled),
      .by = "cell"
    )
  keep <- c("cell", "n_units", "n_profiled", "status")
  soc <- status_soc[keep]
  names(soc)[-1] <- paste0(c("n_units", "n_profiled_units", "status"), "_soc")
  code <- status_code[keep]
  names(code)[-1] <- paste0(c("n_units", "n_profiled_units", "status"), "_code")
  cells |>
    dplyr::left_join(per_cell, by = "cell", relationship = "one-to-one") |>
    dplyr::left_join(soc, by = "cell", relationship = "one-to-one") |>
    dplyr::left_join(code, by = "cell", relationship = "one-to-one")
}

employment_coverage <- function(inventory, official_total,
                                rule = c("soc", "code")) {
  rule <- match.arg(rule)
  status_name <- paste0("status_", rule)
  needed <- c("employment", status_name, "n_socs_without_00_profile_with_nested")
  if (!is.data.frame(inventory) || !all(needed %in% names(inventory))) {
    cli::cli_abort("Supply the output of cell_inventory().")
  }
  status <- inventory[[status_name]]
  employment <- inventory$employment
  returned <- sum(employment)
  if (!is.numeric(official_total) || length(official_total) != 1L ||
      !is.finite(official_total) || official_total < returned) {
    cli::cli_abort("The official total cannot be smaller than the returned panel.")
  }
  scope <- function(name, rows) {
    tibble::tibble(
      scope = name, n_cells = sum(rows), employment = sum(employment[rows])
    )
  }
  rows <- dplyr::bind_rows(
    tibble::tibble(
      scope = "official_total", n_cells = NA_integer_, employment = official_total
    ),
    scope("returned_panel", rep(TRUE, length(status))),
    tibble::tibble(
      scope = "official_not_returned", n_cells = NA_integer_,
      employment = official_total - returned
    ),
    scope("matched", status != "unmatched"),
    scope("complete", status == "complete"),
    scope("incomplete", status != "complete"),
    scope("partial", status == "partial"),
    scope("no_profile", status == "no_profile"),
    scope("unmatched", status == "unmatched")
  )
  if (rule == "soc") {
    # These cells hold a SOC occupation whose .00 code has no activity profile
    # while nested detailed codes do. The soc rule leaves them out on purpose.
    rows <- dplyr::bind_rows(
      rows,
      scope(
        "incomplete_with_unused_nested_profiles",
        status != "complete" & inventory$n_socs_without_00_profile_with_nested > 0L
      )
    )
  }
  rows$share_official_total <- rows$employment / official_total
  rows$share_returned_panel <- rows$employment / returned
  rows$share_returned_panel[
    rows$scope %in% c("official_total", "official_not_returned")
  ] <- NA_real_
  rows
}

link_matrix <- function(links, unit_ids, items) {
  if (!is.data.frame(links) || !all(c("unit", "item") %in% names(links))) {
    cli::cli_abort("Supply links with unit and item columns.")
  }
  if (anyDuplicated(unit_ids) || anyDuplicated(items)) {
    cli::cli_abort("Unit and item identifiers must be unique.")
  }
  links <- links[links$unit %in% unit_ids, c("unit", "item")]
  links <- links[!duplicated(links), ]
  columns <- match(links$item, items)
  if (anyNA(columns)) {
    cli::cli_abort("Links refer to items that are not in the item list.")
  }
  z <- matrix(
    0, nrow = length(unit_ids), ncol = length(items),
    dimnames = list(unit_ids, items)
  )
  z[cbind(match(links$unit, unit_ids), columns)] <- 1
  z
}

# Weighted share of the weight that sits on units linked to each item. Every
# weighting scheme in the article is a unit weight vector passed to this
# function over one common unit set.
weighted_link_prevalence <- function(links, weights) {
  if (!is.matrix(links) || is.null(rownames(links)) || is.null(colnames(links))) {
    cli::cli_abort("Supply a unit by item link matrix with dimnames.")
  }
  if (!is.numeric(weights) || length(weights) != nrow(links) ||
      any(!is.finite(weights)) || any(weights < 0) || sum(weights) <= 0) {
    cli::cli_abort("Supply one finite, non-negative weight per unit with a positive sum.")
  }
  out <- as.vector(crossprod(weights, links)) / sum(weights)
  # A weighted share lies in [0, 1]. Remove floating-point overshoot only.
  stats::setNames(pmin(pmax(out, 0), 1), colnames(links))
}

equal_group_weights <- function(groups) {
  if (!is.character(groups) || anyNA(groups) || any(!nzchar(trimws(groups)))) {
    cli::cli_abort("Supply one non-missing group label per unit.")
  }
  1 / as.numeric(table(groups)[groups])
}

# Employment split equally across SOC occupations in a cell, then equally
# across the codes of each SOC occupation.
two_stage_unit_weights <- function(units, cells) {
  weights <- units |>
    dplyr::left_join(
      cells[c("cell", "employment")], by = "cell", relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      n_socs = dplyr::n_distinct(.data$soc_code), .by = "cell"
    ) |>
    dplyr::mutate(n_codes = dplyr::n(), .by = "soc_code") |>
    dplyr::mutate(
      weight = .data$employment / .data$n_socs / .data$n_codes
    )
  stats::setNames(weights$weight, weights$unit)
}

# Employment-weighted prevalence over complete cells with equal-detail
# allocation, plus the allocation bounds. lower and upper hold for any
# nonnegative split of each cell across its units. They are not confidence
# intervals. The wide bounds also let all employment outside the complete cells
# take any link status, so they cover partial, no-profile and unmatched cells.
employment_prevalence <- function(links, units, cells, items,
                                  total_employment = sum(cells$employment)) {
  if (!is.character(items) || length(items) < 1L || anyNA(items) ||
      anyDuplicated(items)) {
    cli::cli_abort("Supply a unique, non-missing item list.")
  }
  status <- cell_completeness(units, cells)
  if (!is.numeric(total_employment) || length(total_employment) != 1L ||
      !is.finite(total_employment) || total_employment < sum(cells$employment)) {
    cli::cli_abort("The total employment cannot be smaller than the listed cells.")
  }
  complete_cells <- status$cell[status$status == "complete"]
  if (length(complete_cells) == 0L) {
    cli::cli_abort("No cell has a complete set of profiles.")
  }
  universe <- units[units$cell %in% complete_cells, c("cell", "unit")]
  if (!all(universe$unit %in% links$unit)) {
    cli::cli_abort("Every unit in a complete cell needs at least one link.")
  }
  z <- link_matrix(links, universe$unit, items)
  cell_position <- match(universe$cell, status$cell)
  n_units <- status$n_units[cell_position]
  unit_weight <- status$employment[cell_position] / n_units
  employment_complete <- sum(status$employment[status$status == "complete"])
  by_cell <- rowsum(z, universe$cell, reorder = FALSE)
  cell_n <- status$n_units[match(rownames(by_cell), status$cell)]
  cell_employment <- status$employment[match(rownames(by_cell), status$cell)]
  lower <- as.vector(crossprod(cell_employment, (by_cell == cell_n) * 1)) /
    employment_complete
  upper <- as.vector(crossprod(cell_employment, (by_cell > 0) * 1)) /
    employment_complete
  prevalence <- unname(weighted_link_prevalence(z, unit_weight))
  tolerance <- 1e-12
  if (any(prevalence < lower - tolerance) || any(prevalence > upper + tolerance) ||
      any(lower < -tolerance) || any(upper > 1 + tolerance)) {
    cli::cli_abort("Prevalence left the allocation bounds, which should not happen.")
  }
  unknown <- total_employment - employment_complete
  list(
    items = tibble::tibble(
      item = items,
      prevalence = prevalence,
      lower = lower,
      upper = upper,
      lower_wide = lower * employment_complete / total_employment,
      upper_wide = (upper * employment_complete + unknown) / total_employment
    ),
    units = tibble::tibble(
      unit = universe$unit, cell = universe$cell, employment_weight = unit_weight
    ),
    status = status,
    employment_complete = employment_complete,
    employment_total = total_employment
  )
}

# Mean IDF over each unit's own linked items, with IDF = log(1 / prevalence).
# With equal unit weights this equals the article's rarity score.
unit_mean_idf <- function(links, prevalence, digits = 12) {
  if (!is.matrix(links) || length(prevalence) != ncol(links) ||
      any(!is.finite(prevalence)) || any(prevalence < 0 | prevalence > 1)) {
    cli::cli_abort("Supply one prevalence in [0, 1] per item.")
  }
  usable <- prevalence > 0
  if (any(links[, !usable, drop = FALSE] != 0)) {
    cli::cli_abort("A linked item has zero prevalence, so its IDF is undefined.")
  }
  n_items <- rowSums(links)
  if (any(n_items == 0)) {
    cli::cli_abort("Every unit needs at least one linked item.")
  }
  scores <- as.vector(
    links[, usable, drop = FALSE] %*% log(1 / prevalence[usable])
  ) / n_items
  stats::setNames(round(scores, digits), rownames(links))
}

# Agreement of two prevalence vectors over the items that both link. Ranks use
# values rounded to digits, so weighted sums that are equal in exact arithmetic
# tie instead of being ordered by floating point noise. Pass the published
# precision to reproduce the statistic from a published table.
prevalence_agreement <- function(x, y, digits = 12L) {
  if (length(x) != length(y) || any(is.na(x)) || any(is.na(y))) {
    cli::cli_abort("Supply two complete prevalence vectors of equal length.")
  }
  if (!is.numeric(digits) || length(digits) != 1L || is.na(digits) ||
      digits < 1 || digits != round(digits)) {
    cli::cli_abort("digits must be a positive whole number.")
  }
  keep <- x > 0 & y > 0
  if (!any(keep)) {
    cli::cli_abort("No item has a positive prevalence in both vectors.")
  }
  tibble::tibble(
    n_items = sum(keep),
    spearman = if (sum(keep) < 2L) {
      NA_real_
    } else {
      stats::cor(
        round(x[keep], digits), round(y[keep], digits), method = "spearman"
      )
    },
    mean_abs_difference = mean(abs(x[keep] - y[keep])),
    max_abs_difference = max(abs(x[keep] - y[keep]))
  )
}

# Overlap of the top-k sets of two score vectors over the same units, retaining
# ties at the cutoff.
top_set_overlap <- function(x, y, k) {
  if (length(x) != length(y) || is.null(names(x)) ||
      !identical(names(x), names(y)) || anyNA(x) || anyNA(y)) {
    cli::cli_abort("Supply complete score vectors with identical unit names.")
  }
  in_x <- rank(-x, ties.method = "min") <= k
  in_y <- rank(-y, ties.method = "min") <= k
  tibble::tibble(
    n_a = sum(in_x), n_b = sum(in_y), n_both = sum(in_x & in_y),
    jaccard = sum(in_x & in_y) / sum(in_x | in_y)
  )
}

# Ranks of each score series, with the units that reach the top k in any series
# listed once. Series may cover different units. A unit missing from a series
# gets NA for that series.
rank_sensitivity <- function(scores, k = 10L) {
  if (!is.list(scores) || length(scores) < 2L || is.null(names(scores)) ||
      anyDuplicated(names(scores)) || any(!nzchar(names(scores))) ||
      any(!vapply(scores, function(x) is.numeric(x) && !is.null(names(x)), logical(1)))) {
    cli::cli_abort("Supply at least two named score vectors with unit names.")
  }
  units <- sort(unique(unlist(lapply(scores, names), use.names = FALSE)))
  out <- tibble::tibble(unit = units)
  in_top <- matrix(FALSE, nrow = length(units), ncol = length(scores))
  for (i in seq_along(scores)) {
    series <- scores[[i]]
    ranks <- stats::setNames(rank(-series, ties.method = "min"), names(series))
    out[[paste0("mean_idf_", names(scores)[[i]])]] <- unname(series[units])
    out[[paste0("rank_", names(scores)[[i]])]] <- as.integer(unname(ranks[units]))
    in_top[, i] <- !is.na(ranks[units]) & ranks[units] <= k
  }
  out$selected_reason <- apply(
    in_top, 1L, function(row) paste(names(scores)[row], collapse = ";")
  )
  out[out$selected_reason != "", ]
}
