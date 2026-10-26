# Tests for the nonexported article helpers under inst/article-helpers that
# support the employment-weighted activity and rated-skills extensions, and for
# the offline snapshots under inst/extdata/occupational-extensions. Nothing here
# needs a network connection, an O*NET API key or the raw O*NET and BLS inputs.

load_extension_helpers <- function(files) {
  env <- new.env(parent = globalenv())
  for (file in files) {
    path <- system.file("article-helpers", file, package = "onet2r")
    if (!nzchar(path)) {
      path <- testthat::test_path("..", "..", "inst", "article-helpers", file)
    }
    sys.source(path, envir = env)
  }
  env
}

ext <- load_extension_helpers(c(
  "occupational-distinctiveness.R",
  "distinctiveness-information.R",
  "occupational-neighbors.R",
  "occupational-skills.R",
  "occupational-employment.R"
))

extension_path <- function(name, dir = "occupational-extensions") {
  path <- system.file("extdata", dir, name, package = "onet2r")
  if (!nzchar(path)) {
    path <- testthat::test_path("..", "..", "inst", "extdata", dir, name)
  }
  path
}

read_extension <- function(name, dir = "occupational-extensions") {
  utils::read.csv(
    extension_path(name, dir),
    stringsAsFactors = FALSE,
    na.strings = c("NA", "")
  )
}

extension_dir <- function() {
  dirname(extension_path("README.txt"))
}

toy_distances <- function() {
  matrix(
    c(0, 1, 1, 3,
      1, 0, 2, 3,
      1, 2, 0, 3,
      3, 3, 3, 0),
    nrow = 4,
    dimnames = list(letters[1:4], letters[1:4])
  )
}

toy_distances_alt <- function() {
  matrix(
    c(0, 2, 1, 3,
      2, 0, 1, 3,
      1, 1, 0, 3,
      3, 3, 3, 0),
    nrow = 4,
    dimnames = list(letters[1:4], letters[1:4])
  )
}

# Five OEWS cells. 13-1020 is a combination of 13-1021 and 13-1022, where only
# 13-1021 has an O*NET-SOC code. 15-1299 is an All Other SOC without a profiled
# .00 code but with two profiled detailed codes. 99-9999 has no O*NET code.
# With split_cell = TRUE 13-1022 gets a profiled .00 code as well.
toy_employment <- function(split_cell = FALSE) {
  occ <- tibble::tibble(
    onet_soc_code = c(
      "11-1011.00", "11-1011.03", "11-1021.00", "13-1021.00",
      "15-1299.00", "15-1299.08", "15-1299.09", "55-1011.00"
    ),
    title = c(
      "Chief Executives", "Chief Sustainability Officers",
      "General and Operations Managers", "Buyers and Purchasing Agents",
      "Computer Occupations, All Other",
      "Geospatial Information Scientists", "Cybersecurity Specialists",
      "Military Officers"
    )
  )
  profiled <- c(
    "11-1011.00", "11-1011.03", "11-1021.00", "13-1021.00",
    "15-1299.08", "15-1299.09"
  )
  links <- tibble::tribble(
    ~unit, ~item,
    "11-1011.00", "d1",
    "11-1011.00", "d2",
    "11-1011.03", "d2",
    "11-1011.03", "d3",
    "11-1021.00", "d1",
    "13-1021.00", "d4",
    "15-1299.08", "d1",
    "15-1299.08", "d3",
    "15-1299.09", "d3"
  )
  if (split_cell) {
    occ <- dplyr::bind_rows(
      occ,
      tibble::tibble(
        onet_soc_code = "13-1022.00", title = "Wholesale and Retail Buyers"
      )
    )
    profiled <- c(profiled, "13-1022.00")
    links <- dplyr::bind_rows(
      links, tibble::tibble(unit = "13-1022.00", item = c("d1", "d2"))
    )
  }
  list(
    roster = ext$classify_onet_roster(occ, profiled),
    occ = occ,
    profiled = profiled,
    cells = tibble::tibble(
      cell = c("11-1011", "11-1021", "13-1020", "15-1299", "99-9999"),
      employment = c(100, 200, 300, 400, 50)
    ),
    parts = tibble::tibble(
      cell = c("11-1011", "11-1021", "13-1020", "13-1020", "15-1299", "99-9999"),
      soc_code = c(
        "11-1011", "11-1021", "13-1021", "13-1022", "15-1299", "99-9999"
      )
    ),
    links = links,
    items = paste0("d", 1:4)
  )
}

# Neighbor helpers ----------------------------------------------------------

test_that("distance_neighbors keeps ties at the cutoff and records k_max", {
  m <- toy_distances()
  nb <- ext$distance_neighbors(m, 2L)
  expect_named(nb, c("occupation", "neighbor", "distance", "rank"))
  expect_identical(nrow(nb), 9L)
  expect_identical(attr(nb, "k_max"), 2L)
  of <- function(x, who, col) x[[col]][x$occupation == who]
  expect_identical(of(nb, "a", "neighbor"), c("b", "c"))
  expect_equal(of(nb, "a", "rank"), c(1, 1))
  expect_equal(of(nb, "a", "distance"), c(1, 1))
  expect_identical(of(nb, "b", "neighbor"), c("a", "c"))
  expect_equal(of(nb, "b", "rank"), c(1, 2))
  expect_equal(of(nb, "b", "distance"), c(1, 2))
  expect_identical(of(nb, "c", "neighbor"), c("a", "b"))
  expect_equal(of(nb, "c", "rank"), c(1, 2))
  expect_identical(of(nb, "d", "neighbor"), c("a", "b", "c"))
  expect_equal(of(nb, "d", "rank"), c(1, 1, 1))
  expect_equal(of(nb, "d", "distance"), c(3, 3, 3))
  nb1 <- ext$distance_neighbors(m, 1L)
  expect_identical(nrow(nb1), 7L)
  expect_equal(as.integer(table(nb1$occupation)), c(2L, 1L, 1L, 3L))
  expect_equal(ext$distance_neighbors(stats::as.dist(m), 2L), nb)
})

test_that("distance_neighbors can round away numerical noise before ranking", {
  noisy <- toy_distances()
  noisy["a", "b"] <- noisy["b", "a"] <- 1 + 1e-12
  strict <- ext$distance_neighbors(noisy, 1L)
  rounded <- ext$distance_neighbors(noisy, 1L, digits = 8L)
  expect_identical(strict$neighbor[strict$occupation == "a"], "c")
  expect_identical(rounded$neighbor[rounded$occupation == "a"], c("b", "c"))
})

test_that("distance_neighbors rejects invalid k_max and malformed distances", {
  m <- toy_distances()
  for (bad_k in list(4L, 0L, 1.5, NA_integer_, "2")) {
    expect_error(
      ext$distance_neighbors(m, bad_k), "k_max must be a whole number"
    )
  }
  expect_error(
    ext$distance_neighbors(matrix(1:6, 2), 1L), "square numeric distance matrix"
  )
  expect_error(
    ext$distance_neighbors(c(0, 1, 1, 0), 1L), "square numeric distance matrix"
  )
  expect_error(
    ext$distance_neighbors(unname(m), 1L), "unique and non-missing"
  )
  mismatch <- m
  colnames(mismatch) <- letters[2:5]
  expect_error(ext$distance_neighbors(mismatch, 1L), "unique and non-missing")
  repeated <- m
  dimnames(repeated) <- list(c("a", "a", "c", "d"), c("a", "a", "c", "d"))
  expect_error(ext$distance_neighbors(repeated, 1L), "unique and non-missing")
  negative <- m
  negative["a", "b"] <- negative["b", "a"] <- -1
  expect_error(ext$distance_neighbors(negative, 1L), "finite and non-negative")
  infinite <- m
  infinite["a", "b"] <- infinite["b", "a"] <- Inf
  expect_error(ext$distance_neighbors(infinite, 1L), "finite and non-negative")
  absent <- m
  absent["a", "b"] <- absent["b", "a"] <- NA_real_
  expect_error(ext$distance_neighbors(absent, 1L), "finite and non-negative")
  asymmetric <- m
  asymmetric["a", "b"] <- 5
  expect_error(
    ext$distance_neighbors(asymmetric, 1L), "symmetric with a zero diagonal"
  )
  offset <- m
  offset["a", "a"] <- 1
  expect_error(
    ext$distance_neighbors(offset, 1L), "symmetric with a zero diagonal"
  )
})

test_that("neighbor_set_overlap and its summary match hand-computed sets", {
  nb1 <- ext$distance_neighbors(toy_distances(), 1L)
  nb2 <- ext$distance_neighbors(toy_distances_alt(), 1L)
  overlap <- ext$neighbor_set_overlap(nb1, nb2, 1L)
  expect_named(
    overlap, c("occupation", "n_a", "n_b", "n_both", "n_union", "jaccard")
  )
  expect_identical(overlap$occupation, c("a", "b", "c", "d"))
  expect_equal(overlap$n_a, c(2, 1, 1, 3))
  expect_equal(overlap$n_b, c(1, 1, 2, 3))
  expect_equal(overlap$n_both, c(1, 0, 1, 3))
  expect_equal(overlap$n_union, c(2, 2, 2, 3))
  expect_equal(overlap$jaccard, c(0.5, 0, 0.5, 1))
  summary <- ext$neighbor_overlap_summary(overlap)
  expect_named(
    summary,
    c("n_occupations", "mean_jaccard", "share_identical", "share_disjoint")
  )
  expect_equal(summary$n_occupations, 4)
  expect_equal(summary$mean_jaccard, 0.5)
  expect_equal(summary$share_identical, 0.25)
  expect_equal(summary$share_disjoint, 0.25)
  nb <- ext$distance_neighbors(toy_distances(), 2L)
  same <- ext$neighbor_set_overlap(nb, nb, 2L)
  expect_equal(same$n_a, c(2, 2, 2, 3))
  expect_equal(same$jaccard, rep(1, 4))
})

test_that("neighbor overlap and query restriction reject unusable tables", {
  nb <- ext$distance_neighbors(toy_distances(), 2L)
  nb1 <- ext$distance_neighbors(toy_distances(), 1L)
  expect_error(ext$neighbor_set_overlap(nb, nb, 0L), "k must be a whole number")
  expect_error(
    ext$neighbor_set_overlap(
      ext$restrict_neighbor_queries(nb, c("a", "b", "c")), nb, 1L
    ),
    "same query occupations"
  )
  expect_error(ext$neighbor_set_overlap(nb1, nb1, 2L), "smaller k_max than k")
  expect_error(
    ext$neighbor_set_overlap(nb[c("occupation", "neighbor")], nb, 1L),
    "occupation, neighbor and rank columns"
  )
  holed <- nb
  holed$neighbor[1] <- NA_character_
  expect_error(ext$neighbor_set_overlap(holed, nb, 1L), "missing values")
  repeated <- dplyr::bind_rows(nb, nb[1, ])
  expect_error(
    ext$neighbor_set_overlap(repeated, nb, 1L),
    "one row per occupation and neighbor"
  )
  restricted <- ext$restrict_neighbor_queries(nb, c("a", "d"))
  expect_identical(nrow(restricted), 5L)
  expect_identical(attr(restricted, "k_max"), 2L)
  expect_error(
    ext$restrict_neighbor_queries(nb, "zzz"), "Choose unique query occupations"
  )
  expect_error(
    ext$restrict_neighbor_queries(nb, c("a", "a")),
    "Choose unique query occupations"
  )
  expect_error(
    ext$neighbor_overlap_summary(data.frame(x = 1)),
    "Supply the output of neighbor_set_overlap"
  )
})

test_that("distance_rank_correlation is a Spearman correlation over pairs", {
  m <- toy_distances()
  expect_equal(ext$distance_rank_correlation(m, m), 1)
  expect_equal(ext$distance_rank_correlation(m, m^2), 1)
  expect_equal(ext$distance_rank_correlation(m, m[4:1, 4:1]), 1)
  reversed <- 4 - m
  diag(reversed) <- 0
  expect_equal(ext$distance_rank_correlation(m, reversed), -1)
  expect_error(
    ext$distance_rank_correlation(m, m[1:3, 1:3]), "same occupations"
  )
  # 0.1 + 0.2 and 0.3 differ only by floating point noise.
  from_pairs <- function(values) {
    out <- matrix(0, 4, 4, dimnames = dimnames(m))
    out[upper.tri(out)] <- values
    out + t(out)
  }
  noisy <- from_pairs(c(0.1 + 0.2, 0.3, 0.5, 0.6, 0.7, 0.8))
  reference <- from_pairs(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
  expect_equal(ext$distance_rank_correlation(noisy, reference), 16.5 / 17.5)
  expect_equal(
    ext$distance_rank_correlation(noisy, reference, digits = 12),
    17 / sqrt(297.5)
  )
})

# Employment helpers --------------------------------------------------------

test_that("bls_combination_parts reads constituents from definition text", {
  parts <- ext$bls_combination_parts(
    c("13-1020", "11-1011", "25-9045"),
    c(
      "This occupation includes 2018 SOC occupations 13-1021, 13-1022, and 13-1023.",
      "Chief executives plan.",
      "This occupation  includes the 2018 SOC occupations 25-9042, 25-9043 and 25-9049."
    )
  )
  expect_named(parts, c("oews_code", "part_soc_code"))
  expect_identical(parts$oews_code, rep(c("13-1020", "25-9045"), each = 3))
  expect_identical(
    parts$part_soc_code,
    c("13-1021", "13-1022", "13-1023", "25-9042", "25-9043", "25-9049")
  )
  expect_error(
    ext$bls_combination_parts(c("a", "a"), c("x", "y")),
    "one unique OEWS code and one definition per row"
  )
  expect_error(
    ext$bls_combination_parts("a", c("x", "y")),
    "one unique OEWS code and one definition per row"
  )
  expect_error(
    ext$bls_combination_parts(
      "13-1020", "This occupation includes 2018 SOC occupations 13-1021."
    ),
    "at least two distinct parts"
  )
  expect_error(
    ext$bls_combination_parts(
      c("13-1020", "13-2020"),
      c(
        "This occupation includes 2018 SOC occupations 13-1021 and 13-1022.",
        "This occupation includes 2018 SOC occupations 13-1022 and 13-2023."
      )
    ),
    "at least two distinct parts"
  )
  expect_error(
    ext$bls_combination_parts(
      "13-1020",
      "This occupation includes 2018 SOC occupations 13-1020 and 13-1021."
    ),
    "at least two distinct parts"
  )
})

test_that("classify_onet_roster separates SOC-level, detailed and All Other codes", {
  toy <- toy_employment()
  roster <- toy$roster
  expect_identical(
    roster$taxonomy_class,
    c(
      "soc_level_with_detailed", "detailed", "soc_level", "soc_level",
      "all_other_with_detailed", "detailed", "detailed", "military"
    )
  )
  expect_identical(
    roster$soc_00_profiled,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_equal(roster$n_codes_in_soc, c(2, 2, 1, 1, 3, 3, 3, 1))
  expect_identical(roster$profiled, toy$occ$onet_soc_code %in% toy$profiled)
  expect_identical(
    roster$soc_code,
    c(rep("11-1011", 2), "11-1021", "13-1021", rep("15-1299", 3), "55-1011")
  )
  expect_identical(
    roster$soc_minor_group,
    c("11-1", "11-1", "11-1", "13-1", "15-1", "15-1", "15-1", "55-1")
  )
  expect_identical(
    roster$soc_prefix5,
    c("11-10", "11-10", "11-10", "13-10", "15-12", "15-12", "15-12", "55-10")
  )
  expect_error(
    ext$classify_onet_roster(data.frame(x = 1), "a"),
    "onet_soc_code and title columns"
  )
  duplicated_code <- toy$occ
  duplicated_code$onet_soc_code[2] <- duplicated_code$onet_soc_code[1]
  expect_error(
    ext$classify_onet_roster(duplicated_code, toy$profiled),
    "unique, well-formed"
  )
  expect_error(
    ext$classify_onet_roster(toy$occ, c(toy$profiled, "99-9999.00")),
    "Every profiled code must appear"
  )
  expect_error(
    ext$classify_onet_roster(
      toy$occ[-1, ], setdiff(toy$profiled, "11-1011.00")
    ),
    "exactly one .00"
  )
})

test_that("roster_accounting compares observed counts with the official taxonomy", {
  official <- ext$official_taxonomy_counts()
  expect_identical(nrow(official), 11L)
  expect_equal(official$official[official$quantity == "n_titles"], 1016)
  expect_equal(official$official[official$quantity == "n_data_level"], 923)
  accounting <- ext$roster_accounting(toy_employment()$roster)
  expect_named(accounting, c("quantity", "official", "observed", "status"))
  expect_identical(nrow(accounting), 13L)
  observed <- stats::setNames(accounting$observed, accounting$quantity)
  expect_equal(observed[["n_titles"]], 8)
  expect_equal(observed[["n_socs"]], 5)
  expect_equal(observed[["n_socs_with_detailed"]], 2)
  expect_equal(observed[["n_data_level"]], 6)
  expect_equal(observed[["n_soc_level_without_detailed"]], 2)
  expect_equal(observed[["n_soc_level_with_detailed"]], 1)
  expect_equal(observed[["n_detailed"]], 3)
  expect_equal(observed[["n_non_data_level"]], 2)
  expect_equal(observed[["n_military"]], 1)
  expect_equal(observed[["n_all_other_with_detailed"]], 1)
  expect_equal(observed[["n_all_other_without_detailed"]], 0)
  expect_true(all(accounting$status[1:11] == "mismatch"))
  expect_identical(
    accounting$quantity[12:13], c("n_other_unprofiled", "n_detailed_unprofiled")
  )
  expect_identical(accounting$status[12:13], rep("observed_only", 2))
  expect_true(all(is.na(accounting$official[12:13])))
  expect_equal(observed[["n_other_unprofiled"]], 0)
  expect_equal(observed[["n_detailed_unprofiled"]], 0)
})

test_that("build_cell_units makes SOC units and overlapping code units", {
  toy <- toy_employment()
  soc <- ext$build_cell_units(toy$parts, toy$roster, "soc")
  expect_named(soc, c("cell", "soc_code", "unit", "in_roster", "profiled"))
  expect_setequal(
    soc$unit,
    c(
      "11-1011.00", "11-1021.00", "13-1021.00", "13-1022.00", "15-1299.00",
      "99-9999.00"
    )
  )
  flag <- function(x, unit, col) x[[col]][x$unit == unit]
  expect_true(flag(soc, "11-1011.00", "profiled"))
  expect_true(flag(soc, "15-1299.00", "in_roster"))
  expect_false(flag(soc, "15-1299.00", "profiled"))
  expect_false(flag(soc, "13-1022.00", "in_roster"))
  expect_false(flag(soc, "99-9999.00", "in_roster"))
  expect_identical(flag(soc, "11-1011.00", "cell"), "11-1011")
  code <- ext$build_cell_units(toy$parts, toy$roster, "code")
  expect_setequal(
    code$unit,
    c(
      "11-1011.00", "11-1011.03", "11-1021.00", "13-1021.00", "13-1022.--",
      "15-1299.00", "15-1299.08", "15-1299.09", "99-9999.--"
    )
  )
  expect_identical(flag(code, "11-1011.03", "cell"), "11-1011")
  expect_true(flag(code, "15-1299.00", "in_roster"))
  expect_false(flag(code, "15-1299.00", "profiled"))
  expect_true(flag(code, "15-1299.08", "profiled"))
  expect_false(flag(code, "13-1022.--", "in_roster"))
  expect_false(flag(code, "13-1022.--", "profiled"))
  expect_identical(anyDuplicated(soc$unit), 0L)
  expect_identical(anyDuplicated(code$unit), 0L)
  expect_error(
    ext$build_cell_units(
      dplyr::bind_rows(toy$parts, toy$parts[1, ]), toy$roster, "soc"
    ),
    "exactly one OEWS cell"
  )
  expect_error(
    ext$build_cell_units(toy$parts["cell"], toy$roster, "soc"),
    "exactly one OEWS cell"
  )
  missing_soc <- toy$parts
  missing_soc$soc_code[1] <- NA_character_
  expect_error(
    ext$build_cell_units(missing_soc, toy$roster, "soc"),
    "exactly one OEWS cell"
  )
  expect_error(
    ext$build_cell_units(
      toy$parts, dplyr::bind_rows(toy$roster, toy$roster[1, ]), "code"
    ),
    "unique across cells"
  )
})

test_that("cell_completeness labels cells complete, partial, no_profile or unmatched", {
  toy <- toy_employment()
  soc_status <- ext$cell_completeness(
    ext$build_cell_units(toy$parts, toy$roster, "soc"), toy$cells
  )
  expect_named(
    soc_status,
    c("cell", "employment", "n_units", "n_in_roster", "n_profiled", "status")
  )
  expect_identical(soc_status$cell, toy$cells$cell)
  expect_identical(
    soc_status$status,
    c("complete", "complete", "partial", "no_profile", "unmatched")
  )
  expect_equal(soc_status$n_units, c(1, 1, 2, 1, 1))
  expect_equal(soc_status$n_in_roster, c(1, 1, 1, 1, 0))
  expect_equal(soc_status$n_profiled, c(1, 1, 1, 0, 0))
  code_status <- ext$cell_completeness(
    ext$build_cell_units(toy$parts, toy$roster, "code"), toy$cells
  )
  expect_identical(
    code_status$status,
    c("complete", "complete", "partial", "partial", "unmatched")
  )
  expect_equal(code_status$n_units, c(2, 1, 2, 3, 1))
  expect_equal(code_status$n_in_roster, c(2, 1, 1, 3, 0))
  expect_equal(code_status$n_profiled, c(2, 1, 1, 2, 0))
  units <- ext$build_cell_units(toy$parts, toy$roster, "soc")
  expect_error(
    ext$cell_completeness(units, dplyr::bind_rows(toy$cells, toy$cells[1, ])),
    "unique codes and positive, finite employment"
  )
  nonpositive <- toy$cells
  nonpositive$employment[1] <- 0
  expect_error(
    ext$cell_completeness(units, nonpositive),
    "unique codes and positive, finite employment"
  )
  expect_error(
    ext$cell_completeness(units, toy$cells[-1, ]),
    "belong to a listed cell"
  )
})

test_that("cell_inventory counts cells without a profiled .00 code", {
  toy <- toy_employment()
  inventory <- ext$cell_inventory(toy$cells, toy$parts, toy$roster)
  expect_named(
    inventory,
    c(
      "cell", "employment", "n_parts", "n_roster_codes",
      "n_profiled_roster_codes", "n_socs_without_00_profile",
      "n_socs_without_00_profile_with_nested",
      "n_nested_profiled_unused_soc_rule", "n_units_soc",
      "n_profiled_units_soc", "status_soc", "n_units_code",
      "n_profiled_units_code", "status_code"
    )
  )
  expect_identical(inventory$cell, toy$cells$cell)
  expect_equal(inventory$n_parts, c(1, 1, 2, 1, 1))
  expect_equal(inventory$n_roster_codes, c(2, 1, 1, 3, 0))
  expect_equal(inventory$n_profiled_roster_codes, c(2, 1, 1, 2, 0))
  expect_equal(inventory$n_socs_without_00_profile, c(0, 0, 1, 1, 1))
  expect_equal(
    inventory$n_socs_without_00_profile_with_nested, c(0, 0, 0, 1, 0)
  )
  expect_equal(inventory$n_nested_profiled_unused_soc_rule, c(1, 0, 0, 2, 0))
  expect_equal(inventory$n_units_soc, c(1, 1, 2, 1, 1))
  expect_equal(inventory$n_profiled_units_soc, c(1, 1, 1, 0, 0))
  expect_identical(
    inventory$status_soc,
    c("complete", "complete", "partial", "no_profile", "unmatched")
  )
  expect_equal(inventory$n_units_code, c(2, 1, 2, 3, 1))
  expect_equal(inventory$n_profiled_units_code, c(2, 1, 1, 2, 0))
  expect_identical(
    inventory$status_code,
    c("complete", "complete", "partial", "partial", "unmatched")
  )
})

test_that("employment_coverage reports shares of the official and returned totals", {
  toy <- toy_employment()
  inventory <- ext$cell_inventory(toy$cells, toy$parts, toy$roster)
  soc <- ext$employment_coverage(inventory, 1200, "soc")
  expect_named(
    soc,
    c(
      "scope", "n_cells", "employment", "share_official_total",
      "share_returned_panel"
    )
  )
  expect_identical(
    soc$scope,
    c(
      "official_total", "returned_panel", "official_not_returned", "matched",
      "complete", "incomplete", "partial", "no_profile", "unmatched",
      "incomplete_with_unused_nested_profiles"
    )
  )
  expect_equal(soc$n_cells, c(NA, 5, NA, 4, 2, 3, 1, 1, 1, 1))
  expect_equal(
    soc$employment, c(1200, 1050, 150, 1000, 300, 750, 300, 400, 50, 400)
  )
  expect_equal(
    soc$share_official_total,
    c(1200, 1050, 150, 1000, 300, 750, 300, 400, 50, 400) / 1200
  )
  expect_equal(
    soc$share_returned_panel,
    c(NA, 1050, NA, 1000, 300, 750, 300, 400, 50, 400) / 1050
  )
  expect_true(is.na(soc$share_returned_panel[1]))
  expect_equal(soc$share_returned_panel[2], 1)
  expect_true(is.na(soc$share_returned_panel[3]))
  code <- ext$employment_coverage(inventory, 1200, "code")
  expect_identical(code$scope, soc$scope[1:9])
  expect_equal(code$n_cells, c(NA, 5, NA, 4, 2, 3, 2, 0, 1))
  expect_equal(code$employment, c(1200, 1050, 150, 1000, 300, 750, 700, 0, 50))
  expect_error(
    ext$employment_coverage(list(), 1200, "soc"),
    "output of cell_inventory"
  )
  expect_error(
    ext$employment_coverage(inventory, 1000, "soc"),
    "official total cannot be smaller than the returned panel"
  )
})

test_that("link_matrix, weights and group weights behave as documented", {
  toy <- toy_employment()
  units <- c("11-1011.00", "11-1011.03", "11-1021.00")
  z <- ext$link_matrix(toy$links, units, toy$items)
  expect_identical(dim(z), c(3L, 4L))
  expect_identical(rownames(z), units)
  expect_identical(colnames(z), toy$items)
  expect_equal(as.numeric(z["11-1011.00", ]), c(1, 1, 0, 0))
  expect_equal(as.numeric(z["11-1011.03", ]), c(0, 1, 1, 0))
  expect_equal(as.numeric(z["11-1021.00", ]), c(1, 0, 0, 0))
  doubled <- dplyr::bind_rows(toy$links, toy$links)
  expect_equal(as.numeric(ext$link_matrix(doubled, units, toy$items)), as.numeric(z))
  expect_error(
    ext$link_matrix(data.frame(a = 1), "u", "i"), "unit and item columns"
  )
  expect_error(
    ext$link_matrix(toy$links, c("u", "u"), toy$items),
    "identifiers must be unique"
  )
  expect_error(
    ext$link_matrix(toy$links, units, c("d1", "d2")),
    "not in the item list"
  )
  expect_equal(
    unname(ext$weighted_link_prevalence(z, rep(1, 3))), c(2 / 3, 2 / 3, 1 / 3, 0)
  )
  expect_equal(
    unname(ext$weighted_link_prevalence(z, c(50, 50, 200))),
    c(5 / 6, 1 / 3, 1 / 6, 0)
  )
  set.seed(171)
  within_range <- logical(200)
  for (i in seq_along(within_range)) {
    p <- ext$weighted_link_prevalence(z, stats::runif(3, 0, 1e6))
    within_range[i] <- all(p >= 0 & p <= 1)
  }
  expect_true(all(within_range))
  expect_error(
    ext$weighted_link_prevalence(matrix(1, 2, 2), c(1, 1)), "with dimnames"
  )
  for (bad in list(c(0, 0, 0), c(-1, 1, 1), c(1, 1), c(1, NA, 1), c(1, Inf, 1))) {
    expect_error(
      ext$weighted_link_prevalence(z, bad),
      "finite, non-negative weight per unit"
    )
  }
  expect_equal(unname(ext$equal_group_weights(c("a", "a", "b"))), c(0.5, 0.5, 1))
  expect_error(
    ext$equal_group_weights(c("a", NA_character_)), "non-missing group label"
  )
  expect_error(
    ext$equal_group_weights(c("a", "")), "non-missing group label"
  )
})

test_that("two_stage_unit_weights splits cell employment equally across units", {
  toy <- toy_employment()
  units <- ext$build_cell_units(toy$parts, toy$roster, "code")
  status <- ext$cell_completeness(units, toy$cells)
  complete <- units[units$cell %in% status$cell[status$status == "complete"], ]
  weights <- ext$two_stage_unit_weights(complete, toy$cells)
  expect_equal(
    weights[c("11-1011.00", "11-1011.03", "11-1021.00")],
    c("11-1011.00" = 50, "11-1011.03" = 50, "11-1021.00" = 200)
  )
  expect_length(weights, 3)
})

test_that("employment_prevalence gives exact disjoint bounds for the soc rule", {
  toy <- toy_employment()
  units <- ext$build_cell_units(toy$parts, toy$roster, "soc")
  res <- ext$employment_prevalence(toy$links, units, toy$cells, toy$items)
  expect_setequal(
    names(res),
    c("items", "units", "status", "employment_complete", "employment_total")
  )
  expect_named(
    res$items,
    c("item", "prevalence", "lower", "upper", "lower_wide", "upper_wide")
  )
  expect_identical(res$items$item, toy$items)
  expect_identical(res$units$unit, c("11-1011.00", "11-1021.00"))
  expect_equal(res$units$employment_weight, c(100, 200))
  expect_equal(res$employment_complete, 300)
  expect_equal(res$employment_total, 1050)
  expect_identical(
    res$status$status, c("complete", "complete", "partial", "no_profile", "unmatched")
  )
  expect_equal(res$items$prevalence, c(1, 1 / 3, 0, 0))
  expect_equal(res$items$lower, c(1, 1 / 3, 0, 0))
  expect_equal(res$items$upper, c(1, 1 / 3, 0, 0))
  expect_equal(res$items$lower_wide, c(300, 100, 0, 0) / 1050)
  expect_equal(res$items$upper_wide, c(1050, 850, 750, 750) / 1050)
  wider <- ext$employment_prevalence(
    toy$links, units, toy$cells, toy$items, total_employment = 1200
  )
  expect_equal(wider$employment_total, 1200)
  expect_equal(wider$items$lower_wide, c(300, 100, 0, 0) / 1200)
  expect_equal(wider$items$upper_wide, c(1200, 1000, 900, 900) / 1200)
})

test_that("employment_prevalence code rule reweights profiles within cells", {
  toy <- toy_employment()
  units <- ext$build_cell_units(toy$parts, toy$roster, "code")
  res <- ext$employment_prevalence(toy$links, units, toy$cells, toy$items)
  expect_identical(
    res$units$unit, c("11-1011.00", "11-1011.03", "11-1021.00")
  )
  expect_equal(res$units$employment_weight, c(50, 50, 200))
  expect_equal(res$items$prevalence, c(5 / 6, 1 / 3, 1 / 6, 0))
  expect_equal(res$items$lower, c(2 / 3, 1 / 3, 0, 0))
  expect_equal(res$items$upper, c(1, 1 / 3, 1 / 3, 0))
  expect_equal(res$items$lower_wide, c(200, 100, 0, 0) / 1050)
  expect_equal(res$items$upper_wide, c(1050, 850, 850, 750) / 1050)
  expect_true(all(res$items$lower <= res$items$prevalence + 1e-12))
  expect_true(all(res$items$prevalence <= res$items$upper + 1e-12))
})

test_that("soc bounds hold for every nonnegative split of a cell and are attained", {
  toy <- toy_employment(split_cell = TRUE)
  units <- ext$build_cell_units(toy$parts, toy$roster, "soc")
  res <- ext$employment_prevalence(toy$links, units, toy$cells, toy$items)
  expect_equal(res$employment_complete, 600)
  lower <- stats::setNames(res$items$lower, res$items$item)[toy$items]
  upper <- stats::setNames(res$items$upper, res$items$item)[toy$items]
  expect_equal(unname(lower[c("d1", "d4")]), c(0.5, 0))
  expect_equal(unname(upper[c("d1", "d4")]), c(1, 0.5))
  z <- ext$link_matrix(toy$links, res$units$unit, toy$items)
  allocated <- function(weights) {
    ext$weighted_link_prevalence(z, weights[rownames(z)])
  }
  base <- c("11-1011.00" = 100, "11-1021.00" = 200)
  all_to_first <- allocated(c(base, "13-1021.00" = 300, "13-1022.00" = 0))
  all_to_second <- allocated(c(base, "13-1021.00" = 0, "13-1022.00" = 300))
  expect_equal(pmin(all_to_first, all_to_second), lower)
  expect_equal(pmax(all_to_first, all_to_second), upper)
  set.seed(171)
  inside <- logical(300)
  for (i in seq_along(inside)) {
    share <- stats::runif(1)
    p <- allocated(
      c(base, "13-1021.00" = 300 * share, "13-1022.00" = 300 * (1 - share))
    )
    inside[i] <- all(p >= lower - 1e-12 & p <= upper + 1e-12)
  }
  expect_true(all(inside))
})

test_that("employment_prevalence rejects incomplete inputs", {
  toy <- toy_employment()
  units <- ext$build_cell_units(toy$parts, toy$roster, "soc")
  expect_error(
    ext$employment_prevalence(toy$links, units, toy$cells, c("d1", "d1")),
    "unique, non-missing item list"
  )
  expect_error(
    ext$employment_prevalence(toy$links, units, toy$cells, c("d1", NA)),
    "unique, non-missing item list"
  )
  expect_error(
    ext$employment_prevalence(
      toy$links, units, toy$cells, toy$items, total_employment = 10
    ),
    "cannot be smaller than the listed cells"
  )
  only_partial <- toy$parts[toy$parts$cell == "13-1020", ]
  expect_error(
    ext$employment_prevalence(
      toy$links,
      ext$build_cell_units(only_partial, toy$roster, "soc"),
      toy$cells[toy$cells$cell == "13-1020", ],
      toy$items
    ),
    "No cell has a complete set of profiles"
  )
  expect_error(
    ext$employment_prevalence(
      toy$links[toy$links$unit != "11-1021.00", ], units, toy$cells, toy$items
    ),
    "needs at least one link"
  )
})

test_that("equal-unit prevalence and IDF reproduce the article's TF-IDF", {
  toy <- toy_employment()
  units <- ext$build_cell_units(toy$parts, toy$roster, "code")
  status <- ext$cell_completeness(units, toy$cells)
  complete <- units[units$cell %in% status$cell[status$status == "complete"], ]
  z <- ext$link_matrix(toy$links, complete$unit, toy$items)
  p <- ext$weighted_link_prevalence(z, rep(1, nrow(z)))
  idf <- ext$unit_mean_idf(z[, p > 0, drop = FALSE], p[p > 0])
  expect_equal(
    unname(idf[complete$unit]), c(0.4054651, 0.7520387, 0.4054651),
    tolerance = 1e-6
  )
  profiles <- tibble::tibble(
    occupation = toy$links$unit, item = toy$links$item, weight = 1
  ) |>
    dplyr::filter(.data$occupation %in% complete$unit)
  article <- ext$profile_tfidf(profiles, complete$unit)
  article_prevalence <- stats::setNames(
    article$prevalence[match(toy$items, article$item)], toy$items
  )
  expect_equal(
    unname(p[!is.na(article_prevalence)]),
    unname(article_prevalence[!is.na(article_prevalence)])
  )
  article_idf <- article |>
    dplyr::summarise(mean_idf = round(sum(.data$tf_idf), 12), .by = "occupation")
  expect_equal(
    unname(idf[article_idf$occupation]), article_idf$mean_idf
  )
  groups <- c("g1", "g1", "g2")
  weights <- ext$equal_group_weights(groups)
  p_group <- ext$weighted_link_prevalence(z, weights)
  expect_equal(unname(p_group), c(0.75, 0.5, 0.25, 0))
  article_group <- ext$profile_tfidf_equal_group_weights(
    profiles, complete$unit, groups
  )
  article_group_prevalence <- article_group$prevalence[match(toy$items, article_group$item)]
  expect_equal(
    unname(p_group[!is.na(article_group_prevalence)]),
    article_group_prevalence[!is.na(article_group_prevalence)]
  )
  idf_group <- ext$unit_mean_idf(z[, p_group > 0, drop = FALSE], p_group[p_group > 0])
  article_group_idf <- article_group |>
    dplyr::summarise(mean_idf = round(sum(.data$tf_idf), 12), .by = "occupation")
  expect_equal(unname(idf_group[article_group_idf$occupation]), article_group_idf$mean_idf)
  expect_error(
    ext$unit_mean_idf(z, c(0.5, 0.5, 0.5, 2)), "one prevalence in \\[0, 1\\] per item"
  )
  zero_linked <- p
  zero_linked[1] <- 0
  expect_error(ext$unit_mean_idf(z, zero_linked), "zero prevalence")
  expect_error(
    ext$unit_mean_idf(z[, 4, drop = FALSE], 0.5), "at least one linked item"
  )
})

test_that("prevalence_agreement, top_set_overlap and rank_sensitivity agree with hand checks", {
  agreement <- ext$prevalence_agreement(
    c(0.5, 0.25, 0, 0.1), c(0.4, 0.3, 0.2, 0.1)
  )
  expect_named(
    agreement,
    c("n_items", "spearman", "mean_abs_difference", "max_abs_difference")
  )
  expect_equal(agreement$n_items, 3)
  expect_equal(agreement$spearman, 1)
  expect_equal(agreement$mean_abs_difference, 0.05)
  expect_equal(agreement$max_abs_difference, 0.1)
  expect_error(ext$prevalence_agreement(c(1, 2), 1), "equal length")
  # 0.1 + 0.2 and 0.3 differ only by floating point noise, so they tie.
  tied <- ext$prevalence_agreement(c(0.1 + 0.2, 0.3, 0.9), c(0.1, 0.2, 0.3))
  expect_equal(tied$spearman, sqrt(3) / 2)
  expect_equal(
    ext$prevalence_agreement(c(0.5, 0.52, 0.9), c(0.1, 0.2, 0.3))$spearman, 1
  )
  expect_equal(
    ext$prevalence_agreement(
      c(0.5, 0.52, 0.9), c(0.1, 0.2, 0.3), digits = 1
    )$spearman,
    sqrt(3) / 2
  )
  one_shared <- ext$prevalence_agreement(c(0.5, 0), c(0.25, 0.5))
  expect_equal(one_shared$n_items, 1)
  expect_true(is.na(one_shared$spearman))
  expect_equal(one_shared$max_abs_difference, 0.25)
  expect_error(
    ext$prevalence_agreement(c(0, 0), c(0.5, 0.5)), "positive prevalence"
  )
  expect_error(
    ext$prevalence_agreement(c(0.5, 0.2), c(0.3, 0.4), digits = 0),
    "positive whole number"
  )
  x <- c(a = 3, b = 2, c = 2, d = 1)
  y <- c(a = 1, b = 2, c = 3, d = 4)
  overlap <- ext$top_set_overlap(x, y, 2)
  expect_named(overlap, c("n_a", "n_b", "n_both", "jaccard"))
  expect_equal(overlap$n_a, 3)
  expect_equal(overlap$n_b, 2)
  expect_equal(overlap$n_both, 1)
  expect_equal(overlap$jaccard, 0.25)
  expect_error(
    ext$top_set_overlap(x, unname(y), 2), "identical unit names"
  )
  sensitivity <- ext$rank_sensitivity(list(one = x, two = y[c("a", "b", "c")]), k = 2)
  expect_named(
    sensitivity,
    c("unit", "mean_idf_one", "rank_one", "mean_idf_two", "rank_two", "selected_reason")
  )
  expect_setequal(sensitivity$unit, c("a", "b", "c"))
  row <- function(unit) sensitivity[sensitivity$unit == unit, ]
  expect_equal(row("a")$rank_one, 1)
  expect_equal(row("a")$rank_two, 3)
  expect_identical(row("a")$selected_reason, "one")
  expect_equal(c(row("b")$rank_one, row("b")$rank_two), c(2, 2))
  expect_identical(row("b")$selected_reason, "one;two")
  expect_equal(c(row("c")$mean_idf_one, row("c")$mean_idf_two), c(2, 3))
  expect_equal(c(row("c")$rank_one, row("c")$rank_two), c(2, 1))
  expect_identical(row("c")$selected_reason, "one;two")
  expect_error(ext$rank_sensitivity(list(one = x), k = 2), "at least two named score vectors")
})

# Rated skills helpers ------------------------------------------------------

# Seven occupations with two Essential and three Transferable elements.
# 11-0002.00 has a Recommend Suppress flag on one Level rating, 11-0003.00 has
# Not Relevant Level ratings, and 11-0007.00 has Essential ratings only. Every
# expected number in the tests below was worked out by hand from these values.
toy_skills <- function() {
  codes <- paste0("11-000", 1:6, ".00")
  mk <- function(rows, elements) {
    m <- do.call(rbind, rows)
    dimnames(m) <- list(codes, elements)
    m
  }
  matrices <- list(
    es_im = mk(
      list(c(3, 4), c(3.5, 4), c(1, 2), c(5, 4.5), c(3, 4.25), c(3, 4)),
      c("e1", "e2")
    ),
    es_lv = mk(
      list(c(4, 2), c(4, 4), c(1, 0), c(6, 5), c(4, 2.5), c(4, 2)),
      c("e1", "e2")
    ),
    ts_im = mk(
      list(
        c(2, 3, 4), c(2, 3, 5), c(1, 1, 2), c(4, 4, 4), c(2, 3, 4), c(2, 3, 4)
      ),
      c("t1", "t2", "t3")
    ),
    ts_lv = mk(
      list(
        c(2, 3, 4), c(2, 3, 5), c(0, 0, 1), c(4, 4, 4), c(2.5, 3, 4), c(2, 3, 4)
      ),
      c("t1", "t2", "t3")
    )
  )
  raw <- function(im, lv, suppress = NULL, not_relevant = NULL) {
    one <- function(values, scale) {
      data.frame(
        onet_soc_code = rep(rownames(values), times = ncol(values)),
        element_id = rep(colnames(values), each = nrow(values)),
        element_name = paste("Name", rep(colnames(values), each = nrow(values))),
        scale_id = scale,
        data_value = as.character(c(values)),
        n = "8",
        standard_error = "0.25",
        recommend_suppress = "N",
        not_relevant = if (scale == "IM") NA_character_ else "N",
        stringsAsFactors = FALSE
      )
    }
    out <- rbind(one(im, "IM"), one(lv, "LV"))
    key <- paste(out$onet_soc_code, out$element_id)
    out$recommend_suppress[out$scale_id == "LV" & key %in% suppress] <- "Y"
    out$not_relevant[out$scale_id == "LV" & key %in% not_relevant] <- "Y"
    out
  }
  essential <- raw(
    matrices$es_im, matrices$es_lv,
    suppress = "11-0002.00 e2",
    not_relevant = c("11-0003.00 e1", "11-0003.00 e2")
  )
  transferable <- raw(
    matrices$ts_im, matrices$ts_lv,
    not_relevant = "11-0003.00 t1"
  )
  extra <- essential[essential$onet_soc_code == "11-0001.00", ]
  extra$onet_soc_code <- "11-0007.00"
  essential <- rbind(essential, extra)
  occs <- c(codes, "11-0007.00")
  anchor_elements <- c("e1", "e2", "t1", "t2", "t3")
  list(
    codes = codes,
    occs = occs,
    matrices = matrices,
    essential = essential,
    transferable = transferable,
    long = ext$skills_long(
      list(essential = essential, transferable = transferable)
    ),
    scales_reference = data.frame(
      scale_id = c("IM", "LV"),
      scale_name = c("Importance", "Level"),
      minimum = c(1, 0),
      maximum = c(5, 7),
      stringsAsFactors = FALSE
    ),
    titles = tibble::tibble(
      onet_soc_code = occs, title = paste("Title", seq_along(occs))
    ),
    anchors = data.frame(
      element_id = rep(anchor_elements, each = 3),
      element_name = paste("Name", rep(anchor_elements, each = 3)),
      scale_id = "LV",
      anchor_value = rep(c(2L, 4L, 6L), times = 5),
      anchor_description = paste(
        "Anchor", rep(anchor_elements, each = 3), rep(c(2, 4, 6), times = 5)
      ),
      stringsAsFactors = FALSE
    )
  )
}

test_that("skills blocks, Importance labels and the threshold grid are fixed", {
  blocks <- ext$skills_blocks()
  expect_identical(blocks$block, c("es_im", "es_lv", "ts_im", "ts_lv"))
  expect_identical(blocks$domain, rep(c("essential", "transferable"), each = 2))
  expect_identical(blocks$scale, rep(c("IM", "LV"), times = 2))
  labels <- ext$importance_labels()
  expect_identical(names(labels), as.character(1:5))
  expect_identical(
    unname(labels),
    c(
      "Not Important", "Somewhat Important", "Important", "Very Important",
      "Extremely Important"
    )
  )
  grid <- ext$skills_threshold_grid()
  expect_equal(nrow(grid), 12)
  im <- grid[grid$scale == "IM", ]
  lv <- grid[grid$scale == "LV", ]
  expect_equal(im$threshold, c(2, 2.5, 3, 3.5, 4, 4.5))
  expect_identical(
    im$threshold_basis,
    rep(c("scale_category_label", "grid_point"), times = 3)
  )
  expect_identical(
    im$category_label,
    c("Somewhat Important", NA, "Important", NA, "Very Important", NA)
  )
  expect_equal(lv$threshold, 1:6)
  expect_identical(
    lv$threshold_basis,
    rep(c("grid_point", "level_example_anchor"), times = 3)
  )
  expect_true(all(is.na(lv$category_label)))
})

test_that("skills_long stacks both domains and rejects malformed tables", {
  toy <- toy_skills()
  long <- toy$long
  expect_equal(nrow(long), 64)
  expect_named(
    long,
    c(
      "occupation", "domain", "scale", "block", "element_id", "element_name",
      "value", "n_raters", "se", "suppress", "not_relevant"
    )
  )
  counts <- vapply(
    c("es_im", "es_lv", "ts_im", "ts_lv"),
    function(block) sum(long$block == block),
    integer(1)
  )
  expect_equal(unname(counts), c(14L, 14L, 18L, 18L))
  one <- long[
    long$occupation == "11-0002.00" & long$block == "es_lv" &
      long$element_id == "e2",
  ]
  expect_equal(nrow(one), 1)
  expect_equal(one$value, 4)
  expect_equal(c(one$n_raters, one$se), c(8, 0.25))
  expect_identical(one$suppress, "Y")
  expect_identical(one$domain, "essential")
  expect_identical(one$scale, "LV")
  expect_true(all(is.na(long$not_relevant[long$scale == "IM"])))

  tables <- list(essential = toy$essential, transferable = toy$transferable)
  expect_error(ext$skills_long(tables["essential"]), "essential and transferable")
  expect_error(ext$skills_long(list(a = 1, b = 2)), "essential and transferable")
  expect_error(ext$skills_long(1), "essential and transferable")
  broken <- tables
  broken$essential <- broken$essential[setdiff(names(broken$essential), "n")]
  expect_error(ext$skills_long(broken), "essential table is missing required columns")
  broken <- tables
  broken$transferable$data_value[1] <- "high"
  expect_error(ext$skills_long(broken), "must be numeric")
  broken <- tables
  broken$essential$scale_id[1] <- "OJ"
  expect_error(ext$skills_long(broken), "IM and LV ratings only")
  broken <- tables
  broken$essential$recommend_suppress[1] <- "maybe"
  expect_error(ext$skills_long(broken), "Y, N or missing")
  broken <- tables
  broken$essential <- rbind(broken$essential, broken$essential[1, ])
  expect_error(ext$skills_long(broken), "one rating per block and element")
  broken <- tables
  broken$essential$onet_soc_code[1] <- NA
  expect_error(ext$skills_long(broken), "one rating per block and element")
})

test_that("skills_scale_ranges returns the positive range of each requested scale", {
  scales <- toy_skills()$scales_reference
  ranges <- ext$skills_scale_ranges(scales)
  expect_identical(ranges$scale, c("IM", "LV"))
  expect_equal(ranges$minimum, c(1, 0))
  expect_equal(ranges$maximum, c(5, 7))
  expect_equal(ranges$range, c(4, 7))
  expect_equal(ext$skills_scale_ranges(scales, scales = "LV")$range, 7)
  expect_error(ext$skills_scale_ranges(list()), "Scales Reference table")
  expect_error(ext$skills_scale_ranges(scales[1, ]), "list every requested scale")
  flat <- scales
  flat$maximum[2] <- 0
  expect_error(ext$skills_scale_ranges(flat), "positive range")
})

test_that("skills_rating_audit counts flags, Not Relevant ratings and the Level auto-fill rule", {
  toy <- toy_skills()
  ranges <- ext$skills_scale_ranges(toy$scales_reference)
  audit <- ext$skills_rating_audit(toy$long, ranges)
  expect_identical(audit$block, c("es_im", "es_lv", "ts_im", "ts_lv"))
  expect_identical(audit$domain, rep(c("essential", "transferable"), each = 2))
  expect_identical(audit$scale, rep(c("IM", "LV"), times = 2))
  expect_equal(audit$n_rows, c(14, 14, 18, 18))
  expect_equal(audit$n_occupations, c(7, 7, 6, 6))
  expect_equal(audit$n_elements, c(2, 2, 3, 3))
  expect_equal(audit$value_min, c(1, 0, 1, 0))
  expect_equal(audit$value_max, c(5, 6, 5, 5))
  expect_equal(audit$n_at_scale_minimum, c(1, 1, 2, 2))
  expect_equal(audit$n_raters_min, rep(8, 4))
  expect_equal(audit$n_raters_max, rep(8, 4))
  expect_equal(audit$se_min, rep(0.25, 4))
  expect_equal(audit$se_max, rep(0.25, 4))
  for (column in c(
    "n_missing_value", "n_outside_scale", "n_not_two_decimals",
    "n_recommend_suppress_missing"
  )) {
    expect_equal(audit[[column]], rep(0, 4), info = column)
  }
  expect_equal(audit$n_recommend_suppress, c(0, 1, 0, 0))
  expect_equal(audit$se_min_recommend_suppress, c(NA, 0.25, NA, NA))
  expect_equal(audit$se_max_not_suppressed, rep(0.25, 4))
  expect_equal(audit$n_not_relevant, c(0, 2, 0, 1))
  expect_equal(audit$n_not_relevant_missing, c(14, 0, 18, 0))
  expect_equal(audit$n_level_zero, c(NA, 1, NA, 2))
  expect_equal(audit$n_level_zero_importance_at_floor, c(NA, 0, NA, 2))
  expect_equal(audit$n_importance_at_floor_level_positive, c(NA, 1, NA, 0))
  expect_equal(audit$importance_max_when_not_relevant, c(NA, 2, NA, 1))
  expect_equal(audit$level_max_when_not_relevant, c(NA, 1, NA, 0))

  bent <- toy$long
  bent$value[bent$block == "es_im"][1] <- 5.5
  bent$value[bent$block == "ts_im"][1] <- 2.005
  bent$value[bent$block == "ts_lv"][2] <- NA
  bent_audit <- ext$skills_rating_audit(bent, ranges)
  expect_equal(bent_audit$n_outside_scale, c(1, 0, 0, 0))
  expect_equal(bent_audit$n_not_two_decimals, c(0, 0, 1, 0))
  expect_equal(bent_audit$n_missing_value, c(0, 0, 0, 1))
})

test_that("skills_eligibility and skills_common_universe define the primary universe", {
  toy <- toy_skills()
  elig <- ext$skills_eligibility(toy$long, toy$occs)
  expect_equal(nrow(elig), 7)
  expect_identical(elig$occupation, toy$occs)
  expect_equal(elig$es_im_n_elements, rep(2, 7))
  expect_equal(elig$ts_im_n_elements, c(rep(3, 6), 0))
  expect_identical(elig$ts_lv_complete, c(rep(TRUE, 6), FALSE))
  expect_identical(elig$complete_all_blocks, c(rep(TRUE, 6), FALSE))
  expect_equal(elig$n_recommend_suppress, c(0, 1, 0, 0, 0, 0, 0))
  expect_equal(elig$es_lv_n_recommend_suppress, c(0, 1, 0, 0, 0, 0, 0))
  expect_equal(elig$n_not_relevant, c(0, 0, 3, 0, 0, 0, 0))
  expect_equal(elig$es_lv_n_not_relevant, c(0, 0, 2, 0, 0, 0, 0))
  expect_equal(elig$ts_lv_n_not_relevant, c(0, 0, 1, 0, 0, 0, 0))
  expect_error(
    ext$skills_eligibility(toy$long[toy$long$block != "ts_lv", ], toy$occs),
    "at least one element"
  )

  universe <- ext$skills_common_universe(elig, toy$titles)
  expect_identical(universe$onet_soc_code, toy$occs)
  expect_identical(universe$title, toy$titles$title)
  expect_identical(universe$in_pool_complete, c(rep(TRUE, 6), FALSE))
  expect_identical(
    universe$in_common_universe, c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    universe$in_pool_no_not_relevant,
    c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    universe$in_pool_no_flags, c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(
    universe$exclusion_reason,
    c(NA, "recommend_suppress_flag", NA, NA, NA, NA, "incomplete_skills_profile")
  )
  not_relevant_only <- universe$onet_soc_code == "11-0003.00"
  expect_true(universe$in_common_universe[not_relevant_only])
  expect_equal(universe$n_not_relevant[not_relevant_only], 3)
  expect_error(
    ext$skills_common_universe(elig, toy$titles[-1, ]),
    "cover exactly the occupations"
  )
  expect_error(
    ext$skills_common_universe(elig, toy$titles["title"]),
    "cover exactly the occupations"
  )
})

test_that("skills_profile_matrices builds complete matrices and floors Not Relevant Level only on request", {
  toy <- toy_skills()
  m6 <- ext$skills_profile_matrices(toy$long, toy$codes)
  expect_named(m6, c("es_im", "es_lv", "ts_im", "ts_lv"))
  for (block in names(m6)) {
    expect_equal(m6[[block]], toy$matrices[[block]], info = block)
  }
  expect_identical(colnames(m6$ts_lv), c("t1", "t2", "t3"))
  reversed <- ext$skills_profile_matrices(toy$long, rev(toy$codes))
  expect_equal(reversed$es_im, m6$es_im[6:1, ])

  floored <- ext$skills_profile_matrices(toy$long, toy$codes, nr_floor = TRUE)
  expect_equal(floored$es_im, m6$es_im)
  expect_equal(floored$ts_im, m6$ts_im)
  expect_equal(unname(m6$es_lv["11-0003.00", ]), c(1, 0))
  expect_equal(unname(floored$es_lv["11-0003.00", ]), c(0, 0))
  expect_equal(floored$es_lv[-3, ], m6$es_lv[-3, ])
  expect_equal(floored$ts_lv, m6$ts_lv)

  expect_error(
    ext$skills_profile_matrices(toy$long, toy$codes[1:2]), "at least three unique"
  )
  expect_error(
    ext$skills_profile_matrices(toy$long, c(toy$codes[1:3], toy$codes[1])),
    "at least three unique"
  )
  expect_error(ext$skills_profile_matrices(toy$long, 1:5), "at least three unique")
  expect_error(
    ext$skills_profile_matrices(toy$long, c(toy$codes, "11-0007.00")),
    "complete ts_im block"
  )
})

test_that("absolute distances are exact sums over elements, scaled by the scale range", {
  toy <- toy_skills()
  m6 <- toy$matrices
  bd <- ext$skills_block_distance(m6$es_im, 4)
  expect_equal(bd$denominator, 800)
  expected <- rbind(
    c(0, 50, 400, 250, 25, 0),
    c(50, 0, 450, 200, 75, 50),
    c(400, 450, 0, 650, 425, 400),
    c(250, 200, 650, 0, 225, 250),
    c(25, 75, 425, 225, 0, 25),
    c(0, 50, 400, 250, 25, 0)
  )
  expect_equal(unname(bd$sums), expected)
  expect_identical(dimnames(bd$sums), list(toy$codes, toy$codes))
  distance <- ext$skills_distance_matrix(bd)
  expect_equal(distance["11-0001.00", "11-0002.00"], 0.0625)
  expect_equal(distance["11-0001.00", "11-0003.00"], 0.5)
  expect_equal(distance["11-0003.00", "11-0004.00"], 0.8125)
  expect_true(all(distance >= 0 & distance <= 1))
  expect_equal(
    distance["11-0001.00", "11-0002.00"],
    mean(abs(m6$es_im[1, ] - m6$es_im[2, ]) / 4)
  )

  nn <- ext$skills_neighbors(bd, k_max = 1)
  expect_named(nn, c("occupation", "neighbor", "distance", "rank"))
  expect_equal(attr(nn, "k_max"), 1)
  expect_equal(nrow(nn), 9)
  expect_true(all(nn$rank == 1))
  sets <- split(nn$neighbor, nn$occupation)
  expect_identical(sets[["11-0001.00"]], "11-0006.00")
  expect_setequal(sets[["11-0002.00"]], c("11-0001.00", "11-0006.00"))
  expect_setequal(sets[["11-0003.00"]], c("11-0001.00", "11-0006.00"))
  expect_identical(sets[["11-0004.00"]], "11-0002.00")
  expect_setequal(sets[["11-0005.00"]], c("11-0001.00", "11-0006.00"))
  expect_identical(sets[["11-0006.00"]], "11-0001.00")
  by_query <- split(nn$distance, nn$occupation)
  smallest <- unname(vapply(by_query, min, numeric(1)))
  expect_equal(smallest, c(0, 0.0625, 0.5, 0.25, 0.03125, 0))
  expect_equal(unname(vapply(by_query, max, numeric(1))), smallest)

  nn5 <- ext$skills_neighbors(bd, k_max = 5, digits = 1)
  first <- nn5[nn5$occupation == "11-0001.00", ]
  first <- first[order(first$rank), ]
  expect_identical(
    first$neighbor,
    c("11-0006.00", "11-0005.00", "11-0002.00", "11-0004.00", "11-0003.00")
  )
  expect_equal(first$rank, 1:5)
  expect_equal(first$distance, c(0, 0, 0.1, 0.3, 0.5))

  expect_error(ext$absolute_gap_sums(m6$es_im + 0.001), "at most two decimals")
  expect_error(ext$skills_block_distance(unname(m6$es_im), 4), "named rating matrix")
  expect_error(ext$skills_block_distance(m6$es_im[, 1], 4), "named rating matrix")
  expect_error(ext$skills_block_distance(m6$es_im, 0), "positive scale range")
})

test_that("ordinal distances use within-universe mid-ranks and ignore monotone transformations", {
  toy <- toy_skills()
  values <- toy$matrices$es_im
  od <- ext$skills_block_distance(values, 4, "ordinal")
  expect_equal(od$denominator, 24)
  expected <- rbind(
    c(0, 4, 8, 12, 4, 0),
    c(4, 0, 12, 8, 8, 4),
    c(8, 12, 0, 20, 12, 8),
    c(12, 8, 20, 0, 8, 12),
    c(4, 8, 12, 8, 0, 4),
    c(0, 4, 8, 12, 4, 0)
  )
  expect_equal(unname(od$sums), expected)
  distance <- ext$skills_distance_matrix(od)
  expect_equal(distance["11-0001.00", "11-0002.00"], 1 / 6)
  expect_equal(distance["11-0003.00", "11-0004.00"], 20 / 24)
  ranks <- apply(values, 2, rank, ties.method = "average")
  expect_equal(
    distance["11-0001.00", "11-0002.00"],
    mean(abs(ranks[1, ] - ranks[2, ])) / 6
  )
  expect_equal(ext$ordinal_gap_sums(exp(values)), od$sums)
  expect_equal(ext$ordinal_gap_sums(values * 10 - 3), od$sums)
  smaller <- ext$skills_block_distance(values[-4, ], 4, "ordinal")
  expect_equal(smaller$denominator, 20)
})

test_that("composition distances are total variation between row-normalized profiles", {
  toy <- toy_skills()
  values <- toy$matrices$es_lv
  composition <- ext$skills_block_distance(values, 7, "composition")
  expect_equal(composition$denominator, 1)
  expect_equal(
    unname(round(composition$sums["11-0001.00", ], 4)),
    c(0, 0.1667, 0.3333, 0.1212, 0.0513, 0)
  )
  expect_equal(
    unname(round(composition$sums["11-0003.00", ], 4)),
    c(0.3333, 0.5, 0, 0.4545, 0.3846, 0.3333)
  )
  expect_equal(ext$composition_distances(2 * values), composition$sums)
  zero_total <- values
  zero_total[3, ] <- 0
  expect_error(
    ext$skills_block_distance(zero_total, 7, "composition"), "positive total"
  )
})

test_that("rating_pair_gaps reports item gaps whose mean is the block distance", {
  toy <- toy_skills()
  ranges <- ext$skills_scale_ranges(toy$scales_reference)
  a <- toy$codes[1]
  b <- toy$codes[2]
  gaps <- ext$rating_pair_gaps(toy$matrices, ranges, a, b)
  expect_named(
    gaps, c("block", "element_id", "value_a", "value_b", "gap", "gap_normalized")
  )
  expect_equal(nrow(gaps), 10)
  expect_identical(unique(gaps$block), c("es_im", "es_lv", "ts_im", "ts_lv"))
  row <- function(block, element) {
    gaps[gaps$block == block & gaps$element_id == element, ]
  }
  e1 <- row("es_im", "e1")
  expect_equal(c(e1$value_a, e1$value_b, e1$gap, e1$gap_normalized), c(3, 3.5, 0.5, 0.125))
  e2 <- row("es_lv", "e2")
  expect_equal(c(e2$value_a, e2$value_b, e2$gap), c(2, 4, 2))
  expect_equal(e2$gap_normalized, 2 / 7)
  expect_equal(row("ts_im", "t3")$gap_normalized, 0.25)
  expect_equal(row("ts_lv", "t3")$gap_normalized, 1 / 7)
  expect_equal(sum(gaps$gap == 0), 6)
  for (block in names(toy$matrices)) {
    scale <- toupper(sub("^[a-z]+_", "", block))
    range <- ranges$range[ranges$scale == scale]
    distance <- ext$skills_distance_matrix(
      ext$skills_block_distance(toy$matrices[[block]], range)
    )
    expect_equal(
      mean(gaps$gap_normalized[gaps$block == block]), distance[a, b], info = block
    )
  }
  expect_error(
    ext$rating_pair_gaps(toy$matrices, ranges, a, a), "two different occupations"
  )
  expect_error(
    ext$rating_pair_gaps(toy$matrices, ranges, a, "11-9999.00"),
    "two different occupations"
  )
  expect_error(
    ext$rating_pair_gaps(list(), ranges, a, b), "output of skills_profile_matrices"
  )
  expect_error(
    ext$rating_pair_gaps(unname(toy$matrices), ranges, a, b),
    "output of skills_profile_matrices"
  )
})

test_that("skills_pair_selection picks the nearest, median and farthest pair by exact sums", {
  toy <- toy_skills()
  sums <- ext$skills_block_distance(toy$matrices$es_im, 4)$sums
  selected <- ext$skills_pair_selection(sums)
  expect_identical(selected$pair_kind, c("nearest_pair", "median_pair", "farthest_pair"))
  expect_identical(selected$occupation_a, c("11-0001.00", "11-0004.00", "11-0003.00"))
  expect_identical(selected$occupation_b, c("11-0006.00", "11-0005.00", "11-0004.00"))
  expect_equal(selected$exact_sum, c(0, 225, 650))
  # Ties are broken by occupation code, so the median and farthest of three
  # pairs with sums 0, 25 and 25 are the two tied pairs in code order.
  small <- sums[toy$codes[c(1, 5, 6)], toy$codes[c(1, 5, 6)]]
  tied <- ext$skills_pair_selection(small)
  expect_identical(tied$occupation_a, c("11-0001.00", "11-0001.00", "11-0005.00"))
  expect_identical(tied$occupation_b, c("11-0006.00", "11-0005.00", "11-0006.00"))
  expect_equal(tied$exact_sum, c(0, 25, 25))
})

test_that("skills_rating_distribution matches base R summaries by element", {
  toy <- toy_skills()
  ranges <- ext$skills_scale_ranges(toy$scales_reference)
  distribution <- ext$skills_rating_distribution(toy$long, toy$codes, ranges)
  expect_named(
    distribution,
    c(
      "block", "domain", "scale", "element_id", "element_name", "n_occupations",
      "mean", "sd", "min", "p10", "p25", "median", "p75", "p90", "max",
      "share_at_scale_minimum", "n_not_relevant"
    )
  )
  expect_equal(nrow(distribution), 10)
  expect_identical(
    paste(distribution$block, distribution$element_id),
    c(
      "es_im e1", "es_im e2", "es_lv e1", "es_lv e2",
      paste("ts_im", c("t1", "t2", "t3")), paste("ts_lv", c("t1", "t2", "t3"))
    )
  )
  for (i in seq_len(nrow(distribution))) {
    block <- distribution$block[i]
    v <- toy$matrices[[block]][, distribution$element_id[i]]
    floor_value <- if (grepl("_im$", block)) 1 else 0
    info <- paste(block, distribution$element_id[i])
    expect_equal(distribution$n_occupations[i], length(v), info = info)
    expect_equal(distribution$mean[i], mean(v), info = info)
    expect_equal(distribution$sd[i], stats::sd(v), info = info)
    expect_equal(distribution$min[i], min(v), info = info)
    expect_equal(distribution$max[i], max(v), info = info)
    expect_equal(
      c(
        distribution$p10[i], distribution$p25[i], distribution$median[i],
        distribution$p75[i], distribution$p90[i]
      ),
      unname(stats::quantile(v, c(0.1, 0.25, 0.5, 0.75, 0.9), type = 7)),
      info = info
    )
    expect_equal(
      distribution$share_at_scale_minimum[i], mean(v == floor_value), info = info
    )
  }
  expect_true(all(is.na(distribution$n_not_relevant[grepl("_im$", distribution$block)])))
  expect_equal(distribution$n_not_relevant[distribution$block == "es_lv"], c(1, 1))
  expect_equal(distribution$n_not_relevant[distribution$block == "ts_lv"], c(1, 0, 0))

  restricted <- ext$skills_rating_distribution(toy$long, toy$codes[-3], ranges)
  expect_equal(restricted$n_occupations, rep(5, 10))
  expect_equal(restricted$n_not_relevant[restricted$block == "es_lv"], c(0, 0))
})

test_that("skills_threshold_table reports shares at or above each threshold with its basis", {
  toy <- toy_skills()
  thr <- ext$skills_threshold_table(toy$long, toy$codes, toy$codes, toy$anchors)
  expect_equal(dim(thr), c(60, 18))
  expect_named(
    thr,
    c(
      "block", "domain", "scale", "element_id", "element_name", "threshold",
      "comparison", "threshold_basis", "threshold_label", "n_occupations",
      "n_at_or_above", "share_at_or_above", "n_not_relevant",
      "n_at_or_above_excluding_not_relevant",
      "share_at_or_above_excluding_not_relevant", "share_at_or_above_nr_floor",
      "n_pool_complete", "share_at_or_above_pool_complete"
    )
  )
  expect_true(all(thr$comparison == ">="))
  expect_equal(thr$share_at_or_above, thr$n_at_or_above / thr$n_occupations)
  groups <- paste(thr$block, thr$element_id)
  for (column in c("share_at_or_above", "share_at_or_above_pool_complete")) {
    steps <- split(thr[[column]], groups)
    expect_true(all(vapply(steps, function(x) all(diff(x) <= 0), logical(1))), info = column)
  }

  e1 <- thr[thr$block == "es_im" & thr$element_id == "e1", ]
  expect_equal(e1$threshold, c(2, 2.5, 3, 3.5, 4, 4.5))
  expect_equal(e1$n_at_or_above, c(5, 5, 5, 2, 1, 1))
  expect_equal(e1$n_occupations, rep(6, 6))
  expect_identical(
    e1$threshold_label,
    c("Somewhat Important", NA, "Important", NA, "Very Important", NA)
  )
  expect_true(all(is.na(e1$n_not_relevant)))
  expect_true(all(is.na(e1$share_at_or_above_nr_floor)))
  e2 <- thr[thr$block == "es_im" & thr$element_id == "e2", ]
  expect_equal(e2$n_at_or_above, c(6, 5, 5, 5, 5, 1))

  lv <- thr[thr$block == "es_lv" & thr$element_id == "e1", ]
  expect_equal(lv$threshold, 1:6)
  expect_equal(lv$n_at_or_above, c(6, 5, 5, 5, 1, 1))
  expect_equal(lv$n_not_relevant, rep(1, 6))
  expect_equal(lv$n_at_or_above_excluding_not_relevant, c(5, 5, 5, 5, 1, 1))
  expect_equal(lv$share_at_or_above_excluding_not_relevant, c(1, 1, 1, 1, 0.2, 0.2))
  expect_equal(lv$share_at_or_above_nr_floor, c(5, 5, 5, 5, 1, 1) / 6)
  expect_identical(
    lv$threshold_basis, rep(c("grid_point", "level_example_anchor"), times = 3)
  )
  expect_identical(
    lv$threshold_label, c(NA, "Anchor e1 2", NA, "Anchor e1 4", NA, "Anchor e1 6")
  )

  other <- ext$skills_threshold_table(toy$long, toy$codes[-2], toy$codes, toy$anchors)
  other_e1 <- other[other$block == "es_im" & other$element_id == "e1", ]
  expect_equal(other_e1$n_occupations, rep(5, 6))
  expect_equal(other_e1$n_at_or_above, c(4, 4, 4, 1, 1, 1))
  expect_equal(other_e1$n_pool_complete, rep(6, 6))
  expect_equal(other_e1$share_at_or_above_pool_complete, c(5, 5, 5, 2, 1, 1) / 6)
})

test_that("skills_level_anchors keeps the Level 2, 4 and 6 anchors of the requested elements", {
  toy <- toy_skills()
  everything <- c("e1", "e2", "t1", "t2", "t3")
  anchors <- ext$skills_level_anchors(toy$anchors, everything)
  expect_equal(dim(anchors), c(15, 5))
  expect_named(
    anchors,
    c("element_id", "element_name", "scale", "anchor_value", "anchor_description")
  )
  expect_identical(anchors$element_id, rep(everything, each = 3))
  expect_equal(anchors$anchor_value, rep(c(2, 4, 6), times = 5))
  expect_identical(anchors$anchor_description[1:3], paste("Anchor e1", c(2, 4, 6)))
  expect_equal(nrow(ext$skills_level_anchors(toy$anchors, c("e1", "e2"))), 6)
  other_scale <- toy$anchors[1, ]
  other_scale$scale_id <- "IM"
  expect_equal(
    nrow(ext$skills_level_anchors(rbind(toy$anchors, other_scale), everything)), 15
  )

  expect_error(ext$skills_level_anchors(list(), "e1"), "Level Scale Anchors table")
  expect_error(
    ext$skills_level_anchors(toy$anchors[-1, ], everything), "exactly the Level 2, 4 and 6"
  )
  odd <- toy$anchors
  odd$anchor_value[2] <- 3L
  expect_error(ext$skills_level_anchors(odd, everything), "exactly the Level 2, 4 and 6")
  expect_error(
    ext$skills_level_anchors(toy$anchors, c("e1", "zz")), "exactly the Level 2, 4 and 6"
  )
})

test_that("neighbor overlap compares two neighbor tables and summarizes without a significance claim", {
  toy <- toy_skills()
  absolute <- ext$skills_block_distance(toy$matrices$es_im, 4)
  ordinal <- ext$skills_block_distance(toy$matrices$es_im, 4, "ordinal")
  overlap <- ext$skills_neighbor_overlap(
    ext$skills_neighbors(absolute, 3), ext$skills_neighbors(ordinal, 3), c(1, 2, 3)
  )
  overlap <- overlap[order(overlap$occupation), ]
  expect_named(overlap, c("occupation", "jaccard_k1", "jaccard_k2", "jaccard_k3"))
  expect_identical(overlap$occupation, toy$codes)
  expect_equal(overlap$jaccard_k1, c(1, 1, 1, 0.5, 1, 1))
  expect_equal(overlap$jaccard_k2, c(2 / 3, 1, 1, 1, 1, 2 / 3))
  expect_equal(overlap$jaccard_k3, c(1, 0.75, 0.75, 1, 0.75, 1))

  summary <- ext$skills_overlap_summary(overlap)
  expect_named(summary, c("k", "metric", "value"))
  expect_equal(nrow(summary), 12)
  value <- function(k, metric) summary$value[summary$k == k & summary$metric == metric]
  expect_equal(c(value(1, "mean_jaccard"), value(2, "mean_jaccard"), value(3, "mean_jaccard")),
               c(5.5 / 6, 16 / 18, 0.875))
  expect_equal(c(value(1, "share_identical"), value(2, "share_identical"), value(3, "share_identical")),
               c(5 / 6, 4 / 6, 3 / 6))
  expect_equal(c(value(1, "share_disjoint"), value(2, "share_disjoint"), value(3, "share_disjoint")),
               c(0, 0, 0))
  expect_equal(summary$value[summary$metric == "n_query_occupations"], rep(6, 3))
  expect_false(any(grepl("p_value|significan", summary$metric)))
  expect_error(
    ext$skills_overlap_summary(tibble::tibble(occupation = "a")),
    "output of skills_neighbor_overlap"
  )
})

# ---------------------------------------------------------------------------
# Shipped snapshots. These tests read only the small CSV files under
# inst/extdata/occupational-extensions, so they need neither the network nor
# the raw O*NET and BLS inputs. They check that the snapshots agree with each
# other, with the helper definitions and with the README.
# ---------------------------------------------------------------------------

skills_block_names <- c("es_im", "es_lv", "ts_im", "ts_lv")
skills_block_sizes <- c(es_im = 10L, es_lv = 10L, ts_im = 25L, ts_lv = 25L)
skills_block_ranges <- c(es_im = 4, es_lv = 7, ts_im = 4, ts_lv = 7)
pinned_sha256 <- c(
  onet = "7758ec966fd91895b3d290b83c9f1f1d46730d37fdda4faac67104d1c0d2a780",
  oews = "b5855a37f3e03e779f6fbf173d3bbc94aeeff33426aef32fa95ce6d025bab1af",
  definitions = "d927416c1165e2043e8626f380f3bcaaa2f98377bfa5ef5c08209e573d95bb24"
)

expect_max_diff <- function(object, expected, tolerance = 1e-8) {
  expect_length(object, length(expected))
  expect_false(anyNA(object))
  expect_lte(max(abs(object - expected)), tolerance)
}

accounting_value <- function(accounting, source, quantity) {
  value <- accounting$observed[
    accounting$source == source & accounting$quantity == quantity
  ]
  expect_length(value, 1L)
  value
}

test_that("skills common-universe snapshot follows the pool definitions and discloses attrition", {
  universe <- read_extension("skills-common-universe.csv")
  roster <- read_extension("employment-roster.csv")
  blocks <- skills_block_names

  expect_named(universe, c(
    "onet_soc_code", "title",
    paste0(blocks, "_n_elements"), paste0(blocks, "_complete"),
    paste0(blocks, "_n_recommend_suppress"), paste0(blocks, "_n_not_relevant"),
    "complete_all_blocks", "n_recommend_suppress", "n_not_relevant",
    "in_pool_complete", "in_common_universe", "in_pool_no_not_relevant",
    "in_pool_no_flags", "exclusion_reason"
  ))
  expect_equal(nrow(universe), 923L)
  expect_identical(
    universe$onet_soc_code,
    sort(roster$onet_soc_code[roster$profiled], method = "radix")
  )
  expect_identical(
    universe$title, roster$title[match(universe$onet_soc_code, roster$onet_soc_code)]
  )

  n_elements <- as.matrix(universe[paste0(blocks, "_n_elements")])
  complete <- as.matrix(universe[paste0(blocks, "_complete")])
  sizes <- matrix(
    unname(skills_block_sizes[blocks]),
    nrow = nrow(universe), ncol = length(blocks), byrow = TRUE
  )
  expect_true(all(n_elements <= sizes))
  expect_identical(unname(complete), unname(n_elements == sizes))
  expect_identical(
    universe$complete_all_blocks, unname(rowSums(complete) == length(blocks))
  )

  flags <- as.matrix(universe[paste0(blocks, "_n_recommend_suppress")])
  not_relevant <- as.matrix(universe[paste0(blocks, "_n_not_relevant")])
  expect_equal(universe$n_recommend_suppress, unname(rowSums(flags)))
  expect_equal(universe$n_not_relevant, unname(rowSums(not_relevant)))
  expect_true(all(flags <= n_elements))
  expect_true(all(not_relevant <= n_elements))
  # Recommend Suppress and Not Relevant are recorded on Level rows only.
  importance_blocks <- paste0(c("es_im", "ts_im"), "_n_recommend_suppress")
  expect_true(all(flags[universe$in_pool_complete, importance_blocks] == 0L))
  importance_blocks <- paste0(c("es_im", "ts_im"), "_n_not_relevant")
  expect_true(all(not_relevant[universe$in_pool_complete, importance_blocks] == 0L))

  primary <- universe$in_common_universe
  expect_identical(universe$in_pool_complete, universe$complete_all_blocks)
  expect_identical(
    primary, universe$complete_all_blocks & universe$n_recommend_suppress == 0L
  )
  expect_identical(
    universe$in_pool_no_not_relevant,
    universe$complete_all_blocks & universe$n_not_relevant == 0L
  )
  expect_identical(universe$in_pool_no_flags, primary & universe$n_not_relevant == 0L)
  expect_true(all(universe$in_pool_complete[primary]))
  expect_true(all(primary[universe$in_pool_no_flags]))
  expect_true(all(universe$in_pool_no_not_relevant[universe$in_pool_no_flags]))
  expect_equal(
    c(
      sum(universe$in_pool_complete), sum(primary),
      sum(universe$in_pool_no_not_relevant), sum(universe$in_pool_no_flags)
    ),
    c(894L, 823L, 93L, 84L)
  )
  # Not Relevant ratings are kept in the primary universe, not excluded.
  expect_gt(sum(primary & universe$n_not_relevant > 0L), 0L)

  reason <- universe$exclusion_reason
  expect_identical(is.na(reason), primary)
  expect_true(all(reason[!universe$complete_all_blocks] == "incomplete_skills_profile"))
  expect_true(all(
    reason[universe$complete_all_blocks & !primary] == "recommend_suppress_flag"
  ))
  expect_equal(sum(reason == "incomplete_skills_profile", na.rm = TRUE), 29L)
  expect_equal(sum(reason == "recommend_suppress_flag", na.rm = TRUE), 71L)
  expect_equal(sum(!is.na(reason)) + sum(primary), nrow(universe))
})

test_that("skills provenance records the pinned release, the sizes and the treatments", {
  provenance <- read_extension("skills-provenance.csv")
  universe <- read_extension("skills-common-universe.csv")
  reason <- universe$exclusion_reason

  expect_equal(nrow(provenance), 1L)
  expect_identical(as.character(provenance$release_version), "30.3")
  expect_identical(provenance$source_sha256, pinned_sha256[["onet"]])
  expect_match(
    provenance$source_url,
    "^https://www\\.onetcenter\\.org/dl_files/database/db_30_3_text\\.zip$"
  )
  expect_match(
    provenance$rating_procedures_report, "Analyst Occupational Skills Ratings",
    fixed = TRUE
  )
  expect_equal(provenance$n_dwa_occupations, nrow(universe))
  expect_equal(provenance$n_pool_complete, sum(universe$in_pool_complete))
  expect_equal(provenance$n_primary_universe, sum(universe$in_common_universe))
  expect_equal(
    provenance$n_excluded_incomplete_skills_profile,
    sum(reason == "incomplete_skills_profile", na.rm = TRUE)
  )
  expect_equal(
    provenance$n_excluded_recommend_suppress,
    sum(reason == "recommend_suppress_flag", na.rm = TRUE)
  )
  expect_equal(
    provenance$n_primary_universe + provenance$n_excluded_incomplete_skills_profile +
      provenance$n_excluded_recommend_suppress,
    provenance$n_dwa_occupations
  )
  expect_equal(provenance$n_pool_no_not_relevant, sum(universe$in_pool_no_not_relevant))
  expect_equal(provenance$n_pool_no_flags, sum(universe$in_pool_no_flags))
  expect_equal(
    c(provenance$n_elements_essential, provenance$n_elements_transferable), c(10, 25)
  )
  expect_equal(c(provenance$range_importance, provenance$range_level), c(4, 7))
  expect_equal(c(provenance$k_max, provenance$distance_digits), c(10, 8))
  expect_match(
    provenance$primary_distance, "absolute rating gap divided by scale range",
    fixed = TRUE
  )
  expect_match(provenance$primary_universe, "no Recommend Suppress flag", fixed = TRUE)
  expect_match(
    provenance$not_relevant_treatment, "recorded values are used", fixed = TRUE
  )
})

test_that("skills neighbor snapshot is tie-retaining, inside the primary universe and symmetric", {
  neighbors <- read_extension("skills-nearest-neighbors.csv")
  universe <- read_extension("skills-common-universe.csv")
  primary <- universe$onet_soc_code[universe$in_common_universe]

  expect_named(neighbors, c("block", "occupation", "rank", "neighbor", "distance"))
  expect_setequal(unique(neighbors$block), skills_block_names)
  expect_false(anyNA(neighbors))
  expect_true(all(neighbors$occupation %in% primary))
  expect_true(all(neighbors$neighbor %in% primary))
  expect_true(all(neighbors$occupation != neighbors$neighbor))
  expect_true(all(neighbors$rank >= 1L & neighbors$rank <= 10L))
  expect_true(all(neighbors$distance >= 0 & neighbors$distance <= 1))
  expect_equal(
    as.vector(table(neighbors$block)[skills_block_names]),
    c(8624L, 8486L, 8312L, 8281L)
  )

  forward <- paste(neighbors$block, neighbors$occupation, neighbors$neighbor, sep = "|")
  expect_false(anyDuplicated(forward) > 0L)
  per_query <- table(neighbors$block, neighbors$occupation)
  expect_identical(dim(per_query), c(4L, length(primary)))
  expect_true(all(per_query >= 10L))

  # The stored rank is the minimum rank of the distance, so ties at the
  # cutoff stay in the table and every query has a rank 1 neighbor.
  expected_rank <- stats::ave(
    neighbors$distance, neighbors$block, neighbors$occupation,
    FUN = function(x) rank(x, ties.method = "min")
  )
  expect_equal(neighbors$rank, expected_rank)
  best <- tapply(neighbors$rank, list(neighbors$block, neighbors$occupation), min)
  expect_true(all(best == 1L))

  backward <- paste(neighbors$block, neighbors$neighbor, neighbors$occupation, sep = "|")
  partner <- match(backward, forward)
  both <- which(!is.na(partner))
  expect_gt(length(both), 0L)
  expect_lte(max(abs(neighbors$distance[both] - neighbors$distance[partner[both]])), 1e-12)
})

test_that("sensitivity and overlap snapshots agree with the neighbor tables", {
  sens <- read_extension("skills-neighbor-sensitivity.csv")
  neighbors <- read_extension("skills-nearest-neighbors.csv")
  overlap <- read_extension("skills-ordinal-neighbor-overlap.csv")
  universe <- read_extension("skills-common-universe.csv")
  primary <- universe$onet_soc_code[universe$in_common_universe]
  ks <- c(1L, 5L, 10L)

  expect_named(sens, c("alternative", "block", "k", "metric", "value"))
  expect_false(anyNA(sens$value))
  expect_setequal(
    unique(sens$alternative),
    c(
      "primary_tie_retention", "ordinal_percentile", "composition_secondary",
      "nr_floor", "pool_894"
    )
  )
  expect_setequal(unique(sens$block[sens$alternative == "nr_floor"]), c("es_lv", "ts_lv"))
  expect_false(any(grepl("p_value|signific|confidence", sens$metric, ignore.case = TRUE)))
  sens_key <- paste(
    sens$alternative, sens$block, sens$metric, ifelse(is.na(sens$k), "none", sens$k),
    sep = "|"
  )
  expect_false(anyDuplicated(sens_key) > 0L)
  sens_value <- function(alternative, block, metric, k = NA) {
    key <- paste(alternative, block, metric, if (is.na(k)) "none" else k, sep = "|")
    position <- match(key, sens_key)
    expect_false(is.na(position), info = key)
    sens$value[position]
  }

  for (block in skills_block_names) {
    rows <- neighbors[neighbors$block == block, ]
    for (k in ks) {
      size <- as.vector(table(rows$occupation[rows$rank <= k]))
      expect_length(size, length(primary))
      expect_max_diff(mean(size), sens_value("primary_tie_retention", block, "mean_set_size", k))
      expect_max_diff(
        mean(size > k), sens_value("primary_tie_retention", block, "share_set_larger_than_k", k)
      )
    }
  }

  expect_named(overlap, c(
    "alternative", "block", "occupation", "jaccard_k1", "jaccard_k5", "jaccard_k10"
  ))
  expect_identical(unique(overlap$alternative), "ordinal_percentile")
  expect_false(anyNA(overlap))
  for (block in skills_block_names) {
    rows <- overlap[overlap$block == block, ]
    expect_identical(sort(rows$occupation, method = "radix"), primary)
    for (k in ks) {
      jaccard <- rows[[paste0("jaccard_k", k)]]
      expect_true(all(jaccard >= 0 & jaccard <= 1))
      expect_equal(
        sens_value("ordinal_percentile", block, "n_query_occupations", k), length(jaccard)
      )
      expect_max_diff(
        mean(jaccard), sens_value("ordinal_percentile", block, "mean_jaccard", k)
      )
      expect_max_diff(
        mean(jaccard == 1), sens_value("ordinal_percentile", block, "share_identical", k)
      )
      expect_max_diff(
        mean(jaccard == 0), sens_value("ordinal_percentile", block, "share_disjoint", k)
      )
    }
  }

  pick <- function(metric) {
    rows <- sens[sens$metric == metric & !is.na(sens$k), ]
    stats::setNames(rows$value, paste(rows$alternative, rows$block, rows$k))
  }
  mean_jaccard <- pick("mean_jaccard")
  identical_share <- pick("share_identical")[names(mean_jaccard)]
  disjoint_share <- pick("share_disjoint")[names(mean_jaccard)]
  expect_false(anyNA(identical_share))
  expect_false(anyNA(disjoint_share))
  expect_true(all(mean_jaccard >= 0 & mean_jaccard <= 1))
  expect_true(all(identical_share <= mean_jaccard + 1e-8))
  expect_true(all(mean_jaccard <= 1 - disjoint_share + 1e-8))
  expect_true(all(pick("n_query_occupations") == length(primary)))

  rho <- sens[sens$metric == "distance_spearman", ]
  expect_equal(nrow(rho), 10L)
  expect_true(all(is.na(rho$k)))
  expect_true(all(rho$value >= -1 & rho$value <= 1))
  expect_setequal(
    unique(rho$alternative), c("ordinal_percentile", "composition_secondary", "nr_floor")
  )
  expect_false(any(is.na(sens$k) & sens$metric != "distance_spearman"))

  outside <- sens[sens$metric == "share_neighbors_outside_primary_universe", ]
  expect_equal(nrow(outside), 12L)
  expect_setequal(unique(outside$alternative), "pool_894")
  expect_true(all(outside$value > 0 & outside$value < 1))
})

test_that("pair-gap examples reproduce each block distance from item-level gaps", {
  pairs <- read_extension("skills-pair-gap-examples.csv")
  distributions <- read_extension("skills-rating-distributions.csv")
  neighbors <- read_extension("skills-nearest-neighbors.csv")
  universe <- read_extension("skills-common-universe.csv")
  roster <- read_extension("employment-roster.csv")
  primary <- universe$onet_soc_code[universe$in_common_universe]
  kinds <- c("nearest_pair", "median_pair", "farthest_pair")

  expect_named(pairs, c(
    "block", "pair_kind", "occupation_a", "title_a", "occupation_b", "title_b",
    "block_distance", "element_id", "element_name", "value_a", "value_b", "gap",
    "gap_normalized"
  ))
  expect_false(anyNA(pairs))
  expect_equal(nrow(pairs), 210L)
  expect_setequal(unique(pairs$pair_kind), kinds)
  expect_true(all(pairs$occupation_a %in% primary & pairs$occupation_b %in% primary))
  expect_true(all(pairs$occupation_a < pairs$occupation_b))
  expect_identical(
    pairs$title_a, roster$title[match(pairs$occupation_a, roster$onet_soc_code)]
  )
  expect_identical(
    pairs$title_b, roster$title[match(pairs$occupation_b, roster$onet_soc_code)]
  )

  position <- match(
    paste(pairs$block, pairs$element_id),
    paste(distributions$block, distributions$element_id)
  )
  expect_false(anyNA(position))
  expect_identical(pairs$element_name, distributions$element_name[position])
  low <- distributions$min[position]
  high <- distributions$max[position]
  expect_true(all(pairs$value_a >= low - 1e-9 & pairs$value_a <= high + 1e-9))
  expect_true(all(pairs$value_b >= low - 1e-9 & pairs$value_b <= high + 1e-9))
  expect_max_diff(pairs$gap, abs(pairs$value_a - pairs$value_b), 1e-9)
  expect_max_diff(
    pairs$gap_normalized, pairs$gap / unname(skills_block_ranges[pairs$block]), 1e-7
  )

  for (block in skills_block_names) {
    size <- skills_block_sizes[[block]]
    distance <- numeric()
    for (kind in kinds) {
      rows <- pairs[pairs$block == block & pairs$pair_kind == kind, ]
      expect_equal(nrow(rows), size)
      expect_equal(length(unique(rows$block_distance)), 1L)
      expect_equal(nrow(unique(rows[c("occupation_a", "occupation_b")])), 1L)
      expect_equal(length(unique(rows$element_id)), size)
      expect_max_diff(mean(rows$gap_normalized), rows$block_distance[1L], 1e-7)
      distance[kind] <- rows$block_distance[1L]
    }
    expect_lte(distance[["nearest_pair"]], distance[["median_pair"]])
    expect_lte(distance[["median_pair"]], distance[["farthest_pair"]])

    block_neighbors <- neighbors[neighbors$block == block, ]
    expect_max_diff(distance[["nearest_pair"]], min(block_neighbors$distance), 1e-7)
    expect_gte(distance[["farthest_pair"]] + 1e-7, max(block_neighbors$distance))
    nearest <- pairs[pairs$block == block & pairs$pair_kind == "nearest_pair", ][1L, ]
    listed <- paste(block_neighbors$occupation, block_neighbors$neighbor)
    expect_true(any(
      c(
        paste(nearest$occupation_a, nearest$occupation_b),
        paste(nearest$occupation_b, nearest$occupation_a)
      ) %in% listed
    ))
  }
})

test_that("rating distributions summarize each element on the primary universe", {
  distributions <- read_extension("skills-rating-distributions.csv")
  universe <- read_extension("skills-common-universe.csv")
  audit <- read_extension("skills-rating-audit.csv")
  n_primary <- sum(universe$in_common_universe)

  expect_named(distributions, c(
    "block", "domain", "scale", "element_id", "element_name", "n_occupations",
    "mean", "sd", "min", "p10", "p25", "median", "p75", "p90", "max",
    "share_at_scale_minimum", "n_not_relevant"
  ))
  expect_equal(nrow(distributions), 70L)
  expect_equal(
    as.vector(table(factor(distributions$block, skills_block_names))),
    unname(skills_block_sizes)
  )
  expect_false(anyDuplicated(paste(distributions$block, distributions$element_id)) > 0L)
  expect_true(all(distributions$n_occupations == n_primary))
  expect_identical(
    distributions$domain,
    unname(c(
      es_im = "essential", es_lv = "essential", ts_im = "transferable",
      ts_lv = "transferable"
    )[distributions$block])
  )
  expect_identical(
    distributions$scale,
    unname(c(es_im = "IM", es_lv = "LV", ts_im = "IM", ts_lv = "LV")[distributions$block])
  )

  ids <- split(distributions$element_id, distributions$block)
  expect_setequal(ids$es_im, ids$es_lv)
  expect_setequal(ids$ts_im, ids$ts_lv)
  expect_length(intersect(ids$es_im, ids$ts_im), 0L)
  names_per_id <- tapply(
    distributions$element_name, distributions$element_id,
    function(x) length(unique(x))
  )
  expect_true(all(names_per_id == 1L))

  quantiles <- as.matrix(
    distributions[c("min", "p10", "p25", "median", "p75", "p90", "max")]
  )
  expect_false(anyNA(quantiles))
  expect_false(any(apply(quantiles, 1L, is.unsorted)))
  expect_true(all(distributions$mean >= distributions$min - 1e-8))
  expect_true(all(distributions$mean <= distributions$max + 1e-8))
  expect_true(all(distributions$sd >= 0))
  is_importance <- distributions$scale == "IM"
  expect_true(all(distributions$min[is_importance] >= 1))
  expect_true(all(distributions$max[is_importance] <= 5))
  expect_true(all(distributions$min[!is_importance] >= 0))
  expect_true(all(distributions$max[!is_importance] <= 7))

  # Not Relevant is recorded on Level rows only, and the primary universe keeps
  # those rows, so the element counts add up to the universe's block counts.
  expect_identical(is.na(distributions$n_not_relevant), is_importance)
  for (block in c("es_lv", "ts_lv")) {
    expect_equal(
      sum(distributions$n_not_relevant[distributions$block == block]),
      sum(universe[[paste0(block, "_n_not_relevant")]][universe$in_common_universe])
    )
  }

  floor_value <- ifelse(is_importance, 1, 0)
  expect_true(all(
    distributions$share_at_scale_minimum >= 0 & distributions$share_at_scale_minimum <= 1
  ))
  expect_identical(
    distributions$share_at_scale_minimum > 0, distributions$min <= floor_value + 1e-9
  )
  primary_minimum <- tapply(
    distributions$share_at_scale_minimum * distributions$n_occupations,
    distributions$block, sum
  )
  expect_true(all(primary_minimum[audit$block] <= audit$n_at_scale_minimum + 1e-3))
})

test_that("threshold sensitivity states its basis and agrees with the distributions", {
  thresholds <- read_extension("skills-thresholds.csv")
  distributions <- read_extension("skills-rating-distributions.csv")
  anchors <- read_extension("skills-level-anchors.csv")
  universe <- read_extension("skills-common-universe.csv")
  n_primary <- sum(universe$in_common_universe)
  n_pool <- sum(universe$in_pool_complete)

  expect_named(thresholds, c(
    "block", "domain", "scale", "element_id", "element_name", "threshold",
    "comparison", "threshold_basis", "threshold_label", "n_occupations",
    "n_at_or_above", "share_at_or_above", "n_not_relevant",
    "n_at_or_above_excluding_not_relevant",
    "share_at_or_above_excluding_not_relevant", "share_at_or_above_nr_floor",
    "n_pool_complete", "share_at_or_above_pool_complete"
  ))
  expect_equal(nrow(thresholds), 420L)
  key <- paste(thresholds$block, thresholds$element_id)
  distribution_key <- paste(distributions$block, distributions$element_id)
  expect_setequal(key, distribution_key)
  expect_true(all(table(key) == 6L))
  expect_identical(thresholds$comparison, rep(">=", nrow(thresholds)))

  is_importance <- thresholds$scale == "IM"
  expect_setequal(thresholds$threshold[is_importance], c(2, 2.5, 3, 3.5, 4, 4.5))
  expect_setequal(thresholds$threshold[!is_importance], 1:6)
  expect_false(any(tapply(thresholds$threshold, key, is.unsorted)))

  # Only thresholds that match a published scale label or a published Level
  # anchor carry a label. All other thresholds are plain grid points.
  expected_basis <- ifelse(
    is_importance & thresholds$threshold %in% c(2, 3, 4), "scale_category_label",
    ifelse(
      !is_importance & thresholds$threshold %in% c(2, 4, 6), "level_example_anchor",
      "grid_point"
    )
  )
  expect_identical(thresholds$threshold_basis, expected_basis)
  expect_identical(is.na(thresholds$threshold_label), expected_basis == "grid_point")
  category <- expected_basis == "scale_category_label"
  category_labels <- c(`2` = "Somewhat Important", `3` = "Important", `4` = "Very Important")
  expect_identical(
    thresholds$threshold_label[category],
    unname(category_labels[as.character(thresholds$threshold[category])])
  )
  anchored <- expected_basis == "level_example_anchor"
  anchor_text <- stats::setNames(
    anchors$anchor_description, paste(anchors$element_id, anchors$anchor_value)
  )
  expect_identical(
    thresholds$threshold_label[anchored],
    unname(anchor_text[paste(thresholds$element_id[anchored], thresholds$threshold[anchored])])
  )

  expect_true(all(thresholds$n_occupations == n_primary))
  expect_true(all(thresholds$n_pool_complete == n_pool))
  expect_true(all(
    thresholds$n_at_or_above >= 0L & thresholds$n_at_or_above <= thresholds$n_occupations
  ))
  expect_max_diff(
    thresholds$share_at_or_above, thresholds$n_at_or_above / thresholds$n_occupations, 1e-8
  )
  pool_count <- thresholds$share_at_or_above_pool_complete * thresholds$n_pool_complete
  expect_lte(max(abs(pool_count - round(pool_count))), 1e-5)
  by_element <- split(seq_len(nrow(thresholds)), key)
  expect_true(all(vapply(by_element, function(rows) {
    !is.unsorted(rev(thresholds$n_at_or_above[rows])) &&
      !is.unsorted(rev(thresholds$share_at_or_above_pool_complete[rows]))
  }, logical(1))))

  nr_columns <- c(
    "n_not_relevant", "n_at_or_above_excluding_not_relevant",
    "share_at_or_above_excluding_not_relevant", "share_at_or_above_nr_floor"
  )
  expect_true(all(is.na(thresholds[is_importance, nr_columns])))
  level <- thresholds[!is_importance, ]
  expect_false(anyNA(level[nr_columns]))
  relevant <- level$n_occupations - level$n_not_relevant
  expect_true(all(relevant > 0L))
  expect_true(all(level$n_at_or_above_excluding_not_relevant <= level$n_at_or_above))
  expect_max_diff(
    level$share_at_or_above_excluding_not_relevant,
    level$n_at_or_above_excluding_not_relevant / relevant, 1e-8
  )
  # A floored Not Relevant rating is 0, below every Level threshold.
  expect_max_diff(
    level$share_at_or_above_nr_floor,
    level$n_at_or_above_excluding_not_relevant / level$n_occupations, 1e-8
  )
  expect_true(all(level$share_at_or_above_nr_floor <= level$share_at_or_above + 1e-8))
  level_position <- match(paste(level$block, level$element_id), distribution_key)
  expect_equal(level$n_not_relevant, distributions$n_not_relevant[level_position])

  # Coherence with the distribution summary that holds for every threshold.
  position <- match(key, distribution_key)
  low <- distributions$min[position]
  high <- distributions$max[position]
  middle <- distributions$median[position]
  all_at_or_above <- thresholds$threshold <= low
  expect_true(all(
    thresholds$n_at_or_above[all_at_or_above] == thresholds$n_occupations[all_at_or_above]
  ))
  expect_true(all(thresholds$n_at_or_above[thresholds$threshold > high + 1e-9] == 0L))
  expect_true(all(thresholds$share_at_or_above[thresholds$threshold <= middle + 1e-9] >= 0.5))
  expect_true(all(thresholds$share_at_or_above[thresholds$threshold > middle + 1e-9] <= 0.5))
  lowest <- thresholds$threshold == ifelse(is_importance, 2, 1)
  floor_share <- distributions$share_at_scale_minimum[position]
  expect_true(all(thresholds$share_at_or_above[lowest] <= 1 - floor_share[lowest] + 1e-8))
})

test_that("level anchors cover every skill element at Level 2, 4 and 6", {
  anchors <- read_extension("skills-level-anchors.csv")
  distributions <- read_extension("skills-rating-distributions.csv")
  level <- distributions[distributions$scale == "LV", ]

  expect_named(
    anchors, c("element_id", "element_name", "scale", "anchor_value", "anchor_description")
  )
  expect_equal(nrow(anchors), 105L)
  expect_false(anyNA(anchors))
  expect_true(all(nzchar(anchors$anchor_description)))
  expect_identical(unique(anchors$scale), "LV")
  expect_setequal(anchors$anchor_value, c(2L, 4L, 6L))
  expect_false(anyDuplicated(paste(anchors$element_id, anchors$anchor_value)) > 0L)
  expect_true(all(table(anchors$element_id) == 3L))
  expect_equal(length(unique(level$element_id)), 35L)
  expect_setequal(anchors$element_id, level$element_id)
  expect_identical(
    anchors$element_name, level$element_name[match(anchors$element_id, level$element_id)]
  )
})

test_that("rating audit discloses flags, missing values and the Level auto-fill rule", {
  audit <- read_extension("skills-rating-audit.csv")
  universe <- read_extension("skills-common-universe.csv")
  pool <- universe$in_pool_complete

  expect_named(audit, c(
    "block", "domain", "scale", "n_rows", "n_occupations", "n_elements",
    "n_missing_value", "value_min", "value_max", "n_outside_scale",
    "n_not_two_decimals", "n_at_scale_minimum", "n_raters_min", "n_raters_max",
    "se_min", "se_max", "n_recommend_suppress", "n_recommend_suppress_missing",
    "se_min_recommend_suppress", "se_max_not_suppressed", "n_not_relevant",
    "n_not_relevant_missing", "n_level_zero", "n_level_zero_importance_at_floor",
    "n_importance_at_floor_level_positive", "importance_max_when_not_relevant",
    "level_max_when_not_relevant"
  ))
  expect_identical(audit$block, skills_block_names)
  expect_identical(audit$domain, c("essential", "essential", "transferable", "transferable"))
  expect_identical(audit$scale, c("IM", "LV", "IM", "LV"))
  expect_true(all(audit$n_occupations == sum(pool)))
  expect_equal(audit$n_elements, unname(skills_block_sizes[audit$block]))
  expect_equal(audit$n_rows, audit$n_occupations * audit$n_elements)
  expect_true(all(audit$n_missing_value == 0L))
  expect_true(all(audit$n_outside_scale == 0L))
  expect_true(all(audit$n_not_two_decimals == 0L))
  expect_true(all(audit$n_raters_min == 8L & audit$n_raters_max == 8L))
  expect_true(all(audit$se_min >= 0 & audit$se_max >= audit$se_min))

  level <- audit$scale == "LV"
  expect_true(all(audit$value_min[!level] >= 1 & audit$value_max[!level] <= 5))
  expect_true(all(audit$value_min[level] >= 0 & audit$value_max[level] <= 7))

  for (block in audit$block) {
    row <- audit[audit$block == block, ]
    expect_equal(
      row$n_recommend_suppress, sum(universe[[paste0(block, "_n_recommend_suppress")]][pool])
    )
    expect_equal(
      row$n_not_relevant, sum(universe[[paste0(block, "_n_not_relevant")]][pool])
    )
  }
  expect_true(all(audit$n_recommend_suppress_missing == 0L))
  # Importance rows carry no Not Relevant value and no Recommend Suppress flag.
  expect_true(all(audit$n_recommend_suppress[!level] == 0L))
  expect_true(all(audit$n_not_relevant[!level] == 0L))
  expect_equal(audit$n_not_relevant_missing[!level], audit$n_rows[!level])
  expect_true(all(audit$n_not_relevant_missing[level] == 0L))
  expect_true(all(audit$n_recommend_suppress[level] > 0L))
  expect_true(all(audit$n_not_relevant[level] > 0L))
  expect_identical(is.na(audit$se_min_recommend_suppress), !level)

  level_columns <- c(
    "n_level_zero", "n_level_zero_importance_at_floor",
    "n_importance_at_floor_level_positive", "importance_max_when_not_relevant",
    "level_max_when_not_relevant"
  )
  expect_true(all(is.na(audit[!level, level_columns])))
  expect_false(anyNA(audit[level, level_columns]))
  # Level is 0 exactly where Importance sits at its floor of 1.
  expect_equal(audit$n_level_zero[level], audit$n_level_zero_importance_at_floor[level])
  expect_true(all(audit$n_importance_at_floor_level_positive[level] == 0L))
  expect_equal(audit$n_at_scale_minimum[level], audit$n_level_zero[level])
  expect_equal(audit$n_at_scale_minimum[!level], audit$n_level_zero[level])
  expect_true(all(audit$importance_max_when_not_relevant[level] < 2))
  expect_true(all(audit$level_max_when_not_relevant[level] <= 1))
})

test_that("employment accounting reconciles the taxonomy, the raw file, the bridge and the panel", {
  accounting <- read_extension("employment-accounting.csv")
  roster <- read_extension("employment-roster.csv")
  audit <- read_extension("employment-combination-audit.csv")
  inventory <- read_extension("employment-cell-inventory.csv")
  value <- function(source, quantity) accounting_value(accounting, source, quantity)

  expect_named(accounting, c("source", "quantity", "official", "observed", "status"))
  expect_equal(nrow(accounting), 51L)
  expect_false(anyDuplicated(paste(accounting$source, accounting$quantity)) > 0L)
  expect_false(anyNA(accounting$observed))
  expect_true(all(accounting$status %in% c("match", "observed_only")))
  matched <- accounting$status == "match"
  expect_identical(matched, !is.na(accounting$official))
  expect_equal(accounting$official[matched], accounting$observed[matched])
  expect_equal(c(sum(matched), sum(!matched)), c(14L, 37L))

  # O*NET taxonomy: 1,016 titles split into data-level and other codes.
  taxonomy <- function(quantity) value("onet_taxonomy", quantity)
  expect_equal(taxonomy("n_titles"), nrow(roster))
  expect_equal(taxonomy("n_data_level"), sum(roster$profiled))
  expect_equal(
    taxonomy("n_soc_level_without_detailed") + taxonomy("n_soc_level_with_detailed") +
      taxonomy("n_detailed"),
    taxonomy("n_data_level")
  )
  expect_equal(
    taxonomy("n_military") + taxonomy("n_all_other_with_detailed") +
      taxonomy("n_all_other_without_detailed"),
    taxonomy("n_non_data_level")
  )
  expect_equal(taxonomy("n_data_level") + taxonomy("n_non_data_level"), taxonomy("n_titles"))
  expect_equal(taxonomy("n_socs"), length(unique(roster$soc_code)))
  expect_equal(
    taxonomy("n_socs_with_detailed"),
    taxonomy("n_soc_level_with_detailed") + taxonomy("n_all_other_with_detailed")
  )
  expect_equal(c(taxonomy("n_other_unprofiled"), taxonomy("n_detailed_unprofiled")), c(0L, 0L))
  class_counts <- table(roster$taxonomy_class)
  expect_equal(
    unname(c(
      class_counts[["soc_level"]], class_counts[["soc_level_with_detailed"]],
      class_counts[["detailed"]], class_counts[["military"]],
      class_counts[["all_other_with_detailed"]], class_counts[["all_other"]]
    )),
    c(
      taxonomy("n_soc_level_without_detailed"), taxonomy("n_soc_level_with_detailed"),
      taxonomy("n_detailed"), taxonomy("n_military"),
      taxonomy("n_all_other_with_detailed"), taxonomy("n_all_other_without_detailed")
    )
  )

  # Raw May 2025 national file and the public panel.
  raw <- function(quantity) value("oews_raw", quantity)
  expect_equal(
    raw("detailed_rows") + raw("broad_rows") + raw("minor_rows") + raw("major_rows") +
      raw("total_rows"),
    raw("rows_in_file")
  )
  expect_equal(
    raw("total_employment") - raw("detailed_employment"),
    raw("total_minus_detailed_employment")
  )
  expect_equal(raw("total_minus_detailed_employment"), 13890L)
  expect_equal(raw("detailed_rows"), nrow(inventory))
  expect_equal(raw("detailed_employment"), sum(inventory$employment))
  expect_equal(value("oews_panel", "panel_cells"), raw("detailed_rows"))
  expect_equal(
    raw("raw_rows_with_a_combination_code"),
    raw("combination_codes_with_detailed_row") + raw("combination_codes_also_broad_row")
  )
  expect_equal(
    value("oews_panel", "combination_codes_kept_in_panel"),
    raw("combination_codes_with_detailed_row")
  )
  expect_equal(raw("combination_codes_with_detailed_row"), nrow(audit))

  # Bridge between O*NET-SOC titles and OEWS cells.
  bridge <- function(quantity) value("bridge", quantity)
  expect_equal(
    c(bridge("direct"), bridge("oews_combination"), bridge("not_in_panel")),
    as.vector(table(factor(
      roster$map_type, c("direct", "oews_combination", "not_in_panel")
    )))
  )
  expect_equal(
    bridge("direct") + bridge("oews_combination") + bridge("not_in_panel"), nrow(roster)
  )
  expect_equal(
    bridge("profiled_codes_not_in_panel"),
    sum(roster$profiled & roster$map_type == "not_in_panel")
  )
  expect_equal(bridge("codes_where_bridge_and_definitions_disagree"), 0L)

  # BLS definitions and the package constituent list.
  bls_parts <- strsplit(audit$bls_parts, ";", fixed = TRUE)
  expect_equal(value("bls_definitions", "combination_codes"), nrow(audit))
  expect_equal(value("bls_definitions", "combination_parts"), sum(lengths(bls_parts)))
  expect_equal(
    value("bls_definitions", "definition_rows") -
      value("bls_definitions", "definition_codes_without_oews_cell"),
    nrow(inventory)
  )
  expect_equal(
    sum(inventory$n_parts), sum(inventory$cell_type == "direct") + sum(lengths(bls_parts))
  )
  expect_equal(value("package_constituent_list", "combination_codes_listed"), nrow(audit))
  expect_equal(
    value("package_constituent_list", "parts_not_in_bls_definitions"),
    sum(lengths(strsplit(stats::na.omit(audit$package_extra_parts), ";", fixed = TRUE)))
  )
  expect_equal(value("package_constituent_list", "bls_parts_missing_from_package"), 0L)
  expect_equal(value("package_constituent_list", "listed_codes_not_in_2025_cells"), 0L)

  # Combination rows in the raw file: seven codes also repeat as broad rows.
  has_broad <- grepl("broad", audit$raw_o_groups, fixed = TRUE)
  expect_equal(sum(audit$n_raw_rows), raw("raw_rows_with_a_combination_code"))
  expect_equal(sum(has_broad), raw("broad_codes_that_repeat_a_detailed_code"))
  expect_equal(sum(has_broad), raw("combination_codes_also_broad_row"))
  expect_equal(
    sum(audit$employment[has_broad]), raw("combination_employment_in_repeated_broad_rows")
  )

  # Disjoint units: the two rules and their universes.
  expect_equal(
    value("soc_rule_universe", "units"), sum(roster$soc_rule_in_universe)
  )
  expect_equal(
    value("code_rule_universe", "units"), sum(roster$code_rule_in_universe)
  )
  for (rule in c("soc", "code")) {
    members <- roster[roster[[paste0(rule, "_rule_in_universe")]], ]
    source <- paste0(rule, "_rule_universe")
    expect_equal(
      value(source, "minor_groups_four_characters"), length(unique(members$soc_minor_group))
    )
    expect_equal(
      value(source, "prefix_groups_five_characters"), length(unique(members$soc_prefix5))
    )
  }
})

test_that("combination audit shows every 2025 combination row is detailed and where the package list differs", {
  audit <- read_extension("employment-combination-audit.csv")
  inventory <- read_extension("employment-cell-inventory.csv")
  roster <- read_extension("employment-roster.csv")

  expect_named(audit, c(
    "oews_code", "oews_title", "raw_o_groups", "n_raw_rows", "n_raw_detailed_rows",
    "raw_broad_row_same_employment", "n_panel_rows", "employment", "bls_parts",
    "package_parts", "parts_agree", "package_extra_parts", "bls_missing_from_package",
    "n_parts_in_roster", "n_parts_with_profiled_00", "status_soc", "status_code"
  ))
  expect_identical(audit$oews_code, c(
    "13-1020", "13-2020", "21-1018", "25-2052", "25-9045", "29-2010", "31-1120",
    "39-7010", "47-4090", "51-2028", "51-2090", "53-1047"
  ))
  expect_setequal(
    audit$oews_code, inventory$oews_code[inventory$cell_type == "combination"]
  )

  # The public panel builder keeps every combination code, because the raw
  # file marks every one of them as a detailed row.
  has_broad <- grepl("broad", audit$raw_o_groups, fixed = TRUE)
  expect_true(all(grepl("detailed", audit$raw_o_groups, fixed = TRUE)))
  expect_true(all(audit$n_raw_detailed_rows == 1L))
  expect_true(all(audit$n_panel_rows == 1L))
  expect_equal(audit$n_raw_rows, ifelse(has_broad, 2L, 1L))
  expect_identical(!is.na(audit$raw_broad_row_same_employment), has_broad)
  expect_true(all(audit$raw_broad_row_same_employment[has_broad]))
  expect_equal(sum(has_broad), 7L)

  bls <- strsplit(audit$bls_parts, ";", fixed = TRUE)
  package <- strsplit(audit$package_parts, ";", fixed = TRUE)
  expect_identical(
    audit$parts_agree,
    unname(vapply(seq_along(bls), function(i) setequal(bls[[i]], package[[i]]), logical(1)))
  )
  extra <- vapply(
    seq_along(bls), function(i) paste(setdiff(package[[i]], bls[[i]]), collapse = ";"),
    character(1)
  )
  expect_identical(audit$package_extra_parts, ifelse(nzchar(extra), extra, NA_character_))
  expect_identical(audit$oews_code[!audit$parts_agree], "25-9045")
  expect_identical(audit$package_extra_parts[!audit$parts_agree], "25-9041")
  expect_true(all(vapply(
    seq_along(bls), function(i) length(setdiff(bls[[i]], package[[i]])) == 0L, logical(1)
  )))
  expect_true(all(is.na(audit$bls_missing_from_package)))

  expect_equal(
    audit$n_parts_in_roster,
    vapply(bls, function(parts) sum(parts %in% roster$soc_code), integer(1))
  )
  profiled_00 <- roster$onet_soc_code[roster$profiled]
  expect_equal(
    audit$n_parts_with_profiled_00,
    vapply(bls, function(parts) sum(paste0(parts, ".00") %in% profiled_00), integer(1))
  )
  expect_identical(audit$status_soc, audit$status_code)
  expect_identical(
    audit$status_soc == "complete", audit$n_parts_with_profiled_00 == lengths(bls)
  )
  expect_equal(sum(audit$status_soc == "complete"), 8L)
  expect_setequal(
    audit$oews_code[audit$status_soc == "partial"],
    c("25-9045", "47-4090", "51-2090", "53-1047")
  )

  position <- match(audit$oews_code, inventory$oews_code)
  expect_false(anyNA(position))
  expect_equal(inventory$employment[position], audit$employment)
  expect_identical(inventory$status_soc[position], audit$status_soc)
  expect_identical(inventory$status_code[position], audit$status_code)
  expect_equal(inventory$n_parts[position], lengths(bls))
  expect_equal(inventory$n_raw_rows_same_code[position], audit$n_raw_rows)
  expect_true(all(inventory$cell_type[position] == "combination"))
})

test_that("coverage rows are recomputed from the cell inventory and the official total", {
  coverage <- read_extension("employment-coverage.csv")
  inventory <- read_extension("employment-cell-inventory.csv")
  accounting <- read_extension("employment-accounting.csv")
  official <- accounting_value(accounting, "oews_raw", "total_employment")
  returned <- sum(inventory$employment)

  expect_named(coverage, c(
    "unit_rule", "bound_type", "scope", "n_cells", "employment",
    "share_official_total", "share_returned_panel"
  ))
  expect_equal(nrow(coverage), 21L)
  expect_identical(
    unique(coverage$bound_type[coverage$unit_rule == "soc"]), "disjoint_allocation_bound"
  )
  expect_identical(
    unique(coverage$bound_type[coverage$unit_rule == "code"]), "profile_envelope"
  )
  common_scopes <- c(
    "official_total", "returned_panel", "official_not_returned", "matched", "complete",
    "incomplete", "partial", "no_profile", "unmatched"
  )
  nested_scope <- "cells_with_all_other_soc_with_nested_detail"
  expect_identical(
    coverage$scope[coverage$unit_rule == "soc"],
    c(common_scopes, "incomplete_with_unused_nested_profiles", nested_scope)
  )
  expect_identical(
    coverage$scope[coverage$unit_rule == "code"], c(common_scopes, nested_scope)
  )

  cells_in_scope <- function(rule, scope) {
    status <- inventory[[paste0("status_", rule)]]
    nested <- inventory$n_socs_without_00_profile_with_nested > 0L
    switch(
      scope,
      returned_panel = rep(TRUE, nrow(inventory)),
      matched = status != "unmatched",
      complete = status == "complete",
      incomplete = status != "complete",
      partial = status == "partial",
      no_profile = status == "no_profile",
      unmatched = status == "unmatched",
      incomplete_with_unused_nested_profiles = status != "complete" & nested,
      cells_with_all_other_soc_with_nested_detail = nested
    )
  }
  for (rule in c("soc", "code")) {
    rows <- coverage[coverage$unit_rule == rule, ]
    for (i in seq_len(nrow(rows))) {
      scope <- rows$scope[[i]]
      label <- paste(rule, scope)
      if (scope %in% c("official_total", "official_not_returned")) {
        expect_true(is.na(rows$n_cells[[i]]), info = label)
        expected <- if (scope == "official_total") official else official - returned
        expect_equal(rows$employment[[i]], expected, info = label)
      } else {
        in_scope <- cells_in_scope(rule, scope)
        expect_equal(rows$n_cells[[i]], sum(in_scope), info = label)
        expect_equal(rows$employment[[i]], sum(inventory$employment[in_scope]), info = label)
      }
    }
    employment <- stats::setNames(rows$employment, rows$scope)
    expect_equal(employment[["complete"]] + employment[["incomplete"]], employment[["returned_panel"]])
    expect_equal(
      employment[["partial"]] + employment[["no_profile"]] + employment[["unmatched"]],
      employment[["incomplete"]]
    )
    expect_equal(
      employment[["returned_panel"]] + employment[["official_not_returned"]],
      employment[["official_total"]]
    )
    expect_equal(employment[["unmatched"]], 0)
  }

  # Shares use the official total, and the returned panel where that applies.
  expect_max_diff(coverage$share_official_total, coverage$employment / official, 1e-9)
  has_panel_share <- !coverage$scope %in% c("official_total", "official_not_returned")
  expect_identical(!is.na(coverage$share_returned_panel), has_panel_share)
  expect_max_diff(
    coverage$share_returned_panel[has_panel_share],
    coverage$employment[has_panel_share] / returned, 1e-9
  )

  # Cells that lack a profiled .00 code are counted, never filled with specialties.
  expect_equal(nrow(inventory), 830L)
  expect_false(anyDuplicated(inventory$oews_code) > 0L)
  expect_true(all(inventory$raw_o_group == "detailed"))
  expect_equal(sum(inventory$status_soc == "complete"), 756L)
  expect_identical(inventory$status_soc == "complete", inventory$status_code == "complete")
  expect_identical(inventory$n_socs_without_00_profile > 0L, inventory$status_soc != "complete")
  expect_equal(sum(inventory$status_soc != "complete"), 74L)
  expect_equal(
    inventory$n_socs_without_00_profile, inventory$n_units_soc - inventory$n_profiled_units_soc
  )
  expect_equal(inventory$n_units_soc, inventory$n_parts)
  expect_identical(
    inventory$status_code == "complete", inventory$n_profiled_units_code == inventory$n_units_code
  )
  complete <- inventory$status_soc == "complete"
  expect_equal(
    c(sum(inventory$n_units_soc[complete]), sum(inventory$n_units_code[complete])),
    c(765L, 848L)
  )
  nested <- coverage[coverage$scope == nested_scope, ]
  expect_true(all(nested$n_cells == 24L))
  expect_true(all(nested$employment == 3768470))
})

test_that("roster ties every O*NET-SOC title to its cell, its units and its universe flags", {
  roster <- read_extension("employment-roster.csv")
  inventory <- read_extension("employment-cell-inventory.csv")

  expect_named(roster, c(
    "onet_soc_code", "soc_code", "title", "profiled", "taxonomy_class",
    "n_codes_in_soc", "soc_minor_group", "soc_prefix5", "cell_code", "map_type",
    "soc_rule_unit", "code_rule_unit", "cell_status_soc", "cell_status_code",
    "soc_rule_in_universe", "code_rule_in_universe"
  ))
  expect_equal(nrow(roster), 1016L)
  expect_false(anyDuplicated(roster$onet_soc_code) > 0L)
  expect_equal(sum(roster$profiled), 923L)
  expect_identical(roster$soc_code, substr(roster$onet_soc_code, 1L, 7L))
  expect_identical(roster$soc_minor_group, substr(roster$soc_code, 1L, 4L))
  expect_identical(roster$soc_prefix5, substr(roster$soc_code, 1L, 5L))
  expect_equal(roster$n_codes_in_soc, as.vector(table(roster$soc_code)[roster$soc_code]))
  expect_identical(
    roster$profiled,
    roster$taxonomy_class %in% c("detailed", "soc_level", "soc_level_with_detailed")
  )

  # A title has no cell exactly when the bridge finds it outside the panel.
  expect_identical(is.na(roster$cell_code), roster$map_type == "not_in_panel")
  expect_identical(is.na(roster$cell_status_soc), is.na(roster$cell_code))
  expect_identical(is.na(roster$cell_status_code), is.na(roster$cell_code))
  expect_true(all(roster$cell_code[!is.na(roster$cell_code)] %in% inventory$oews_code))
  position <- match(roster$cell_code, inventory$oews_code)
  expect_identical(roster$cell_status_soc, inventory$status_soc[position])
  expect_identical(roster$cell_status_code, inventory$status_code[position])
  expect_false(anyNA(roster$cell_code[roster$soc_rule_unit | roster$code_rule_unit]))

  for (rule in c("soc", "code")) {
    unit <- roster[[paste0(rule, "_rule_unit")]]
    in_universe <- roster[[paste0(rule, "_rule_in_universe")]]
    status <- roster[[paste0("cell_status_", rule)]]
    expected_units <- inventory[[paste0("n_units_", rule)]]
    expect_equal(
      tabulate(match(roster$cell_code[unit], inventory$oews_code), nbins = nrow(inventory)),
      expected_units
    )
    expect_equal(sum(unit), sum(expected_units))
    expect_identical(in_universe, unit & status %in% "complete")
    members <- roster[in_universe, ]
    expect_true(all(members$profiled))
    expect_equal(nrow(members), c(soc = 765L, code = 848L)[[rule]])
    expect_equal(length(unique(members$soc_minor_group)), 94L)
    expect_equal(length(unique(members$soc_prefix5)), 109L)
  }
  # The soc rule uses one .00 profile per SOC occupation. The code rule adds the
  # nested detailed codes and so contains the soc universe.
  soc_members <- roster[roster$soc_rule_in_universe, ]
  expect_true(all(endsWith(soc_members$onet_soc_code, ".00")))
  expect_false(anyDuplicated(soc_members$soc_code) > 0L)
  expect_true(all(roster$code_rule_in_universe[roster$soc_rule_in_universe]))
  expect_false(anyDuplicated(roster$onet_soc_code[roster$code_rule_in_universe]) > 0L)
})

test_that("employment prevalence covers every activity under both rules with ordered bounds", {
  prevalence <- read_extension("activity-employment-prevalence.csv")
  published <- read_extension("activity-prevalence.csv", "occupational-distinctiveness")
  coverage <- read_extension("employment-coverage.csv")
  inventory <- read_extension("employment-cell-inventory.csv")

  expect_named(prevalence, c(
    "unit_rule", "bound_type", "dwa_element_id", "prevalence_equal_unit",
    "prevalence_equal_minor_group", "prevalence_equal_prefix5_group",
    "prevalence_employment", "prevalence_employment_two_stage", "employment_lower",
    "employment_upper", "employment_lower_wide", "employment_upper_wide",
    "idf_equal_unit", "idf_employment"
  ))
  expect_equal(nrow(prevalence), 2L * nrow(published))
  expect_equal(
    as.vector(table(factor(prevalence$unit_rule, c("soc", "code")))),
    rep(nrow(published), 2L)
  )
  expect_true(all(inventory$employment %% 10 == 0))

  bound_label <- c(soc = "disjoint_allocation_bound", code = "profile_envelope")
  n_units <- c(soc = 765L, code = 848L)
  n_linked <- c(soc = 2034L, code = 2074L)
  n_unlinked <- c(soc = 49L, code = 9L)
  scheme_columns <- c(
    "prevalence_equal_unit", "prevalence_equal_minor_group",
    "prevalence_equal_prefix5_group", "prevalence_employment"
  )
  for (rule in c("soc", "code")) {
    rows <- prevalence[prevalence$unit_rule == rule, ]
    scope_employment <- function(scope) {
      coverage$employment[coverage$unit_rule == rule & coverage$scope == scope]
    }
    complete <- scope_employment("complete")
    returned <- scope_employment("returned_panel")

    expect_identical(unique(rows$bound_type), bound_label[[rule]])
    expect_false(anyDuplicated(rows$dwa_element_id) > 0L)
    expect_setequal(rows$dwa_element_id, published$item)

    # All four weighting schemes run over one unit set, so they share one support.
    linked <- rows$prevalence_equal_unit > 0
    expect_equal(c(sum(linked), sum(!linked)), c(n_linked[[rule]], n_unlinked[[rule]]))
    for (column in scheme_columns) {
      expect_identical(rows[[column]] > 0, linked, info = paste(rule, column))
    }
    expect_true(all(unlist(rows[scheme_columns]) >= 0 & unlist(rows[scheme_columns]) <= 1))

    # Equal unit weights give a count over the matched units.
    counts <- rows$prevalence_equal_unit * n_units[[rule]]
    expect_lte(max(abs(counts - round(counts))), 5e-9 * n_units[[rule]] + 1e-9)
    expect_lte(max(round(counts)), n_units[[rule]])

    # Exact bounds sit around the employment weighted point estimate. The wide
    # bounds also let every incomplete cell link or not link the activity.
    expect_true(all(rows$employment_lower <= rows$prevalence_employment + 1e-12))
    expect_true(all(rows$prevalence_employment <= rows$employment_upper + 1e-12))
    expect_true(all(rows$employment_lower_wide <= rows$employment_lower + 1e-12))
    expect_true(all(rows$employment_upper <= rows$employment_upper_wide + 1e-12))
    expect_true(all(rows$employment_lower >= 0 & rows$employment_upper_wide <= 1))
    expect_max_diff(
      rows$employment_lower_wide, rows$employment_lower * complete / returned, 2e-8
    )
    expect_max_diff(
      rows$employment_upper_wide,
      (rows$employment_upper * complete + (returned - complete)) / returned, 2e-8
    )
    # Each exact bound adds whole cells, and cell employment is a multiple of ten.
    for (column in c("employment_lower", "employment_upper")) {
      workers <- rows[[column]] * complete
      expect_lte(max(abs(workers - 10 * round(workers / 10))), 5e-9 * complete + 1e-6)
    }
    expect_true(all(rows$employment_upper[!linked] == 0))
    expect_true(all(rows$employment_lower[!linked] == 0))
    expect_max_diff(
      rows$employment_upper_wide[!linked],
      rep((returned - complete) / returned, sum(!linked)), 1e-8
    )

    # IDF is log(1 / prevalence) from the unrounded prevalence, so the rounded
    # prevalence reproduces it within the rounding error.
    for (scheme in c("equal_unit", "employment")) {
      p <- rows[[paste0("prevalence_", scheme)]]
      idf <- rows[[paste0("idf_", scheme)]]
      expect_identical(is.na(idf), !(p > 0), info = paste(rule, scheme))
      expect_true(all(idf[p > 0] >= 0))
      expect_true(all(abs(idf[p > 0] + log(p[p > 0])) <= 2e-8 + 1e-8 / p[p > 0]))
    }
    expect_equal(sum(is.na(rows$idf_equal_unit)), n_unlinked[[rule]])

    # The two-stage scheme exists only under the synthetic profile reweighting rule.
    two_stage <- rows$prevalence_employment_two_stage
    if (rule == "soc") {
      expect_true(all(is.na(two_stage)))
    } else {
      expect_false(anyNA(two_stage))
      expect_identical(two_stage > 0, linked)
    }
  }
})

test_that("employment summary agrees with the prevalence table, the roster and the coverage rows", {
  summary <- read_extension("activity-employment-summary.csv")
  prevalence <- read_extension("activity-employment-prevalence.csv")
  published <- read_extension("activity-prevalence.csv", "occupational-distinctiveness")
  roster <- read_extension("employment-roster.csv")
  inventory <- read_extension("employment-cell-inventory.csv")
  coverage <- read_extension("employment-coverage.csv")

  expect_named(summary, c("unit_rule", "bound_type", "comparison", "metric", "value"))
  expect_equal(nrow(summary), 111L)
  expect_false(anyNA(summary))
  expect_identical(
    unique(summary$bound_type[summary$unit_rule == "soc"]), "disjoint_allocation_bound"
  )
  expect_identical(
    unique(summary$bound_type[summary$unit_rule == "code"]), "profile_envelope"
  )
  expect_false(any(grepl(
    "p_value|signific|confidence|interval|standard_error", summary$metric
  )))

  agreement_metrics <- c(
    "prevalence_n_items", "prevalence_spearman", "prevalence_mean_abs_difference",
    "prevalence_max_abs_difference", "unit_rank_spearman", "top10_jaccard",
    "top20_jaccard"
  )
  expected_metrics <- list(
    matched_universe = c(
      "n_units", "n_cells", "n_minor_groups", "n_prefix5_groups", "n_items_linked",
      "n_items_unlinked", "employment_complete", "employment_returned_panel"
    ),
    original_923_vs_equal_unit = agreement_metrics,
    equal_unit_vs_equal_minor_group = agreement_metrics,
    equal_unit_vs_equal_prefix5_group = agreement_metrics,
    equal_unit_vs_employment = agreement_metrics,
    equal_minor_group_vs_employment = agreement_metrics,
    employment_vs_employment_two_stage = agreement_metrics,
    employment_bounds = c(
      "within_bounds_share", "zero_width_share", "mean_width", "median_width",
      "max_width", "mean_width_wide", "max_width_wide",
      "share_equal_unit_outside_bounds", "share_equal_minor_group_outside_bounds"
    )
  )
  shared <- setdiff(names(expected_metrics), "employment_vs_employment_two_stage")
  expect_setequal(unique(summary$comparison[summary$unit_rule == "soc"]), shared)
  expect_setequal(
    unique(summary$comparison[summary$unit_rule == "code"]), names(expected_metrics)
  )
  for (rule in c("soc", "code")) {
    for (comparison in unique(summary$comparison[summary$unit_rule == rule])) {
      expect_identical(
        summary$metric[summary$unit_rule == rule & summary$comparison == comparison],
        expected_metrics[[comparison]], info = paste(rule, comparison)
      )
    }
  }

  pairs <- list(
    c("original_923", "equal_unit"), c("equal_unit", "equal_minor_group"),
    c("equal_unit", "equal_prefix5_group"), c("equal_unit", "employment"),
    c("equal_minor_group", "employment"), c("employment", "employment_two_stage")
  )
  for (rule in c("soc", "code")) {
    value <- function(comparison, metric) {
      hit <- summary$unit_rule == rule & summary$comparison == comparison &
        summary$metric == metric
      expect_equal(sum(hit), 1L, info = paste(rule, comparison, metric))
      summary$value[hit]
    }
    rows <- prevalence[prevalence$unit_rule == rule, ]
    linked <- rows$prevalence_equal_unit > 0
    members <- roster[roster[[paste0(rule, "_rule_in_universe")]], ]
    status <- inventory[[paste0("status_", rule)]]
    scope_employment <- function(scope) {
      coverage$employment[coverage$unit_rule == rule & coverage$scope == scope]
    }

    # The matched universe agrees with the roster, the inventory and the coverage rows.
    expect_equal(value("matched_universe", "n_units"), nrow(members))
    expect_equal(value("matched_universe", "n_cells"), length(unique(members$cell_code)))
    expect_equal(value("matched_universe", "n_cells"), sum(status == "complete"))
    expect_equal(
      value("matched_universe", "n_minor_groups"), length(unique(members$soc_minor_group))
    )
    expect_equal(
      value("matched_universe", "n_prefix5_groups"), length(unique(members$soc_prefix5))
    )
    expect_equal(value("matched_universe", "n_items_linked"), sum(linked))
    expect_equal(value("matched_universe", "n_items_unlinked"), sum(!linked))
    expect_equal(value("matched_universe", "employment_complete"), scope_employment("complete"))
    expect_equal(
      value("matched_universe", "employment_returned_panel"), scope_employment("returned_panel")
    )
    expect_equal(value("matched_universe", "employment_complete"), 146251550)
    expect_equal(value("matched_universe", "employment_returned_panel"), 155481840)

    # Prevalence agreement is recomputed from the rounded prevalence columns.
    # The Spearman correlation ranks the published 8-digit values, so it
    # reproduces from the table up to the 10-digit rounding of the summary.
    prevalence_of <- list(
      original_923 = published$prevalence[match(rows$dwa_element_id, published$item)],
      equal_unit = rows$prevalence_equal_unit,
      equal_minor_group = rows$prevalence_equal_minor_group,
      equal_prefix5_group = rows$prevalence_equal_prefix5_group,
      employment = rows$prevalence_employment,
      employment_two_stage = rows$prevalence_employment_two_stage
    )
    for (pair in pairs) {
      if (rule == "soc" && pair[[2]] == "employment_two_stage") {
        next
      }
      comparison <- paste(pair, collapse = "_vs_")
      x <- prevalence_of[[pair[[1]]]]
      y <- prevalence_of[[pair[[2]]]]
      keep <- x > 0 & y > 0
      expect_equal(value(comparison, "prevalence_n_items"), sum(keep), info = comparison)
      expect_max_diff(
        value(comparison, "prevalence_spearman"),
        stats::cor(x[keep], y[keep], method = "spearman"), 1e-9
      )
      expect_max_diff(
        value(comparison, "prevalence_mean_abs_difference"),
        mean(abs(x[keep] - y[keep])), 1e-7
      )
      expect_max_diff(
        value(comparison, "prevalence_max_abs_difference"),
        max(abs(x[keep] - y[keep])), 1e-7
      )
    }

    # Bound widths run over the linked activities. Equal weights are a different
    # estimand, so their share outside the bounds is descriptive only.
    width <- (rows$employment_upper - rows$employment_lower)[linked]
    width_wide <- (rows$employment_upper_wide - rows$employment_lower_wide)[linked]
    outside <- function(p) {
      mean(
        p[linked] < rows$employment_lower[linked] - 1e-12 |
          p[linked] > rows$employment_upper[linked] + 1e-12
      )
    }
    expect_equal(value("employment_bounds", "within_bounds_share"), 1)
    expect_max_diff(value("employment_bounds", "zero_width_share"), mean(width == 0), 1e-6)
    expect_max_diff(value("employment_bounds", "mean_width"), mean(width), 1e-7)
    expect_max_diff(value("employment_bounds", "median_width"), stats::median(width), 1e-7)
    expect_max_diff(value("employment_bounds", "max_width"), max(width), 1e-7)
    expect_max_diff(value("employment_bounds", "mean_width_wide"), mean(width_wide), 1e-7)
    expect_max_diff(value("employment_bounds", "max_width_wide"), max(width_wide), 1e-7)
    expect_max_diff(
      value("employment_bounds", "share_equal_unit_outside_bounds"),
      outside(rows$prevalence_equal_unit), 2e-3
    )
    expect_max_diff(
      value("employment_bounds", "share_equal_minor_group_outside_bounds"),
      outside(rows$prevalence_equal_minor_group), 2e-3
    )
    expect_lte(value("employment_bounds", "mean_width"), value("employment_bounds", "max_width"))
    expect_lte(
      value("employment_bounds", "mean_width"), value("employment_bounds", "mean_width_wide")
    )
    expect_lte(value("employment_bounds", "max_width"), value("employment_bounds", "max_width_wide"))

    agreement <- summary[
      summary$unit_rule == rule & summary$comparison != "matched_universe" &
        summary$comparison != "employment_bounds",
    ]
    spearman <- agreement$value[grepl("spearman", agreement$metric, fixed = TRUE)]
    jaccard <- agreement$value[grepl("jaccard", agreement$metric, fixed = TRUE)]
    expect_true(all(spearman >= -1 & spearman <= 1))
    expect_true(all(jaccard >= 0 & jaccard <= 1))
  }
})

test_that("rank sensitivity keeps the published ranks and lists every series that reaches the top ten", {
  rank <- read_extension("activity-employment-rank-sensitivity.csv")
  profiles <- read_extension("occupation-profiles.csv", "occupational-distinctiveness")
  roster <- read_extension("employment-roster.csv")
  series <- c(
    "original_923", "original_923_in_universe", "equal_unit", "equal_minor_group",
    "equal_prefix5_group", "employment", "employment_two_stage"
  )

  expect_named(rank, c(
    "unit_rule", "bound_type", "onet_soc_code", "title", "selected_reason",
    as.vector(rbind(paste0("mean_idf_", series), paste0("rank_", series)))
  ))
  expect_equal(nrow(rank), 64L)
  expect_equal(as.vector(table(factor(rank$unit_rule, c("soc", "code")))), c(32L, 32L))
  expect_false(anyNA(rank$selected_reason))
  expect_true(all(nzchar(rank$selected_reason)))
  expect_identical(rank$title, roster$title[match(rank$onet_soc_code, roster$onet_soc_code)])

  # The original series is the published article rank, left unchanged.
  published <- match(rank$onet_soc_code, profiles$occupation)
  expect_false(anyNA(published))
  expect_max_diff(rank$mean_idf_original_923, profiles$mean_idf[published], 1e-9)
  expect_equal(rank$rank_original_923, profiles$rarity_rank[published])
  expect_false(anyNA(rank$rank_original_923))

  bound_label <- c(soc = "disjoint_allocation_bound", code = "profile_envelope")
  universe_size <- c(soc = 765L, code = 848L)
  for (rule in c("soc", "code")) {
    rows <- rank[rank$unit_rule == rule, ]
    members <- roster$onet_soc_code[roster[[paste0(rule, "_rule_in_universe")]]]
    expect_identical(unique(rows$bound_type), bound_label[[rule]])
    expect_false(anyDuplicated(rows$onet_soc_code) > 0L)

    # A unit outside the matched universe keeps only its original rank. It was
    # selected for the original series alone.
    outside <- !(rows$onet_soc_code %in% members)
    expect_identical(is.na(rows$rank_equal_unit), outside)
    expect_identical(is.na(rows$rank_original_923_in_universe), outside)
    expect_true(all(rows$selected_reason[outside] == "original_923"))
    expect_equal(sum(outside), c(soc = 2L, code = 1L)[[rule]])
    expect_true(all(
      rows$rank_original_923_in_universe <= rows$rank_original_923, na.rm = TRUE
    ))

    tokens <- strsplit(rows$selected_reason, ";", fixed = TRUE)
    expect_true(all(unlist(tokens) %in% series))
    for (name in series) {
      score <- rows[[paste0("mean_idf_", name)]]
      position <- rows[[paste0("rank_", name)]]
      label <- paste(rule, name)
      expect_identical(is.na(score), is.na(position), info = label)
      listed <- vapply(tokens, function(x) name %in% x, logical(1))
      expect_identical(listed, !is.na(position) & position <= 10L, info = label)
      if (rule == "soc" && name == "employment_two_stage") {
        expect_true(all(is.na(position)))
        next
      }
      known <- !is.na(position)
      limit <- if (name == "original_923") 923L else universe_size[[rule]]
      expect_true(all(position[known] >= 1L & position[known] <= limit), info = label)
      expect_true(all(position[known] == round(position[known])), info = label)
      expect_gte(sum(position[known] <= 10L), 10L)
      ordered <- order(-score[known], position[known])
      expect_false(is.unsorted(position[known][ordered]), info = label)
    }
  }
})

# Decisions ledger, README and shipped file hygiene -------------------------

shipped_csv_names <- c(
  "activity-employment-prevalence.csv",
  "activity-employment-rank-sensitivity.csv",
  "activity-employment-summary.csv",
  "decisions-ledger.csv",
  "employment-accounting.csv",
  "employment-cell-inventory.csv",
  "employment-combination-audit.csv",
  "employment-coverage.csv",
  "employment-provenance.csv",
  "employment-roster.csv",
  "skills-common-universe.csv",
  "skills-level-anchors.csv",
  "skills-nearest-neighbors.csv",
  "skills-neighbor-sensitivity.csv",
  "skills-ordinal-neighbor-overlap.csv",
  "skills-pair-gap-examples.csv",
  "skills-provenance.csv",
  "skills-rating-audit.csv",
  "skills-rating-distributions.csv",
  "skills-thresholds.csv"
)
own_helper_names <- c(
  "occupational-neighbors.R", "occupational-skills.R", "occupational-employment.R"
)

article_helper_path <- function(file) {
  path <- system.file("article-helpers", file, package = "onet2r")
  if (!nzchar(path)) {
    path <- testthat::test_path("..", "..", "inst", "article-helpers", file)
  }
  path
}

file_bytes <- function(path) {
  readBin(path, "raw", n = file.info(path)$size)
}

read_ledger <- function() {
  utils::read.csv(
    extension_path("decisions-ledger.csv"),
    stringsAsFactors = FALSE,
    na.strings = character()
  )
}

# The README wraps lines, so phrases are matched after whitespace is collapsed.
read_readme <- function() {
  lines <- readLines(extension_path("README.txt"), warn = FALSE)
  gsub("\\s+", " ", paste(lines, collapse = " "))
}

test_that("decisions ledger has the declared columns, unique ids, known statuses and no blank cells", {
  ledger <- read_ledger()
  statuses <- c(
    "implemented", "implemented_sensitivity", "deferred", "delegated",
    "open_gap", "recorded_not_edited", "rejected"
  )

  expect_named(ledger, c(
    "decision_id", "topic", "status", "decision", "rationale", "revisit_trigger"
  ))
  expect_equal(nrow(ledger), 52L)
  expect_false(anyDuplicated(ledger$decision_id) > 0L)
  expect_match(ledger$decision_id, "^[a-z]+_[a-z0-9_]+$")
  expect_true(all(
    sub("_.*$", "", ledger$decision_id) %in%
      c("emp", "act", "skl", "xdom", "src", "gov")
  ))
  expect_true(all(ledger$status %in% statuses))
  expect_equal(
    as.vector(table(factor(ledger$status, statuses))),
    c(26L, 5L, 8L, 4L, 4L, 2L, 3L)
  )
  for (column in names(ledger)) {
    cells <- ledger[[column]]
    expect_false(anyNA(cells), info = column)
    expect_true(all(nzchar(trimws(cells))), info = column)
    expect_false(any(grepl("^\\s|\\s$|[\r\n]", cells)), info = column)
  }

  # An item that is deferred or still open names what would reopen it.
  open <- ledger$status %in% c("deferred", "open_gap")
  expect_false(any(ledger$revisit_trigger[open] == "None."))
})

test_that("decisions ledger keeps the interpretation gates, deferrals and open source gaps", {
  ledger <- read_ledger()
  coverage <- read_extension("employment-coverage.csv")
  expected <- c(
    emp_rule_soc = "implemented",
    emp_rule_code = "implemented_sensitivity",
    emp_cells_without_00_profile = "implemented",
    emp_bounds = "implemented",
    emp_combination_codes = "implemented",
    emp_package_constituent_discrepancy = "recorded_not_edited",
    emp_no_panel_builder_fix = "recorded_not_edited",
    emp_official_residual = "open_gap",
    emp_article_minor_group_wording = "delegated",
    emp_weighted_information_budget = "deferred",
    skl_not_relevant = "implemented",
    skl_common_universe = "implemented",
    skl_ordinal_sensitivity = "implemented_sensitivity",
    skl_complete_pool_sensitivity = "implemented_sensitivity",
    skl_presence_idf = "rejected",
    skl_information_budget = "deferred",
    xdom_neighbor_comparison = "delegated",
    src_pinned_inputs = "implemented",
    src_bls_browser_access = "open_gap",
    src_oews_field_descriptions = "open_gap",
    src_oews_methods_pdf = "open_gap",
    gov_nonexported_helpers = "implemented",
    gov_scope_boundaries = "delegated"
  )
  position <- match(names(expected), ledger$decision_id)
  expect_false(anyNA(position))
  expect_identical(ledger$status[position], unname(expected))

  decision <- function(id) ledger$decision[ledger$decision_id == id]
  expect_all_phrases <- function(id, phrases) {
    for (phrase in phrases) {
      expect_match(decision(id), phrase, fixed = TRUE, info = paste(id, phrase))
    }
  }

  # Only rule soc keeps the disjoint worker reading. Rule code is a synthetic
  # profile reweighting with a profile-allocation envelope.
  expect_all_phrases("emp_rule_soc", c(
    "disjoint worker groups", "disjoint_allocation_bound", "never substituted"
  ))
  expect_all_phrases("emp_rule_code", c(
    "synthetic profile reweighting", "not a disjoint worker allocation",
    "profile-allocation sensitivity", "not a sharp bound", "profile_envelope"
  ))
  expect_false(grepl("disjoint worker groups", decision("emp_rule_code"), fixed = TRUE))
  expect_all_phrases("emp_cells_without_00_profile", c(
    "does not fill them with specialties", "n_socs_without_00_profile"
  ))
  expect_all_phrases("emp_bounds", c(
    "allocation bounds over disjoint worker groups", "profile envelope",
    "not a sharp bound", "not confidence intervals",
    format(
      coverage$employment[coverage$scope == "official_not_returned"][1],
      big.mark = ","
    )
  ))
  expect_all_phrases("emp_union_upper_scenario", "top of the profile envelope")
})

test_that("README keeps the interpretation gates, source gaps and a line for every table", {
  readme <- read_readme()
  expect_phrase <- function(phrase) {
    expect_true(grepl(phrase, readme, fixed = TRUE), info = phrase)
  }

  for (phrase in c(
    "Rule soc (primary, bound_type disjoint_allocation_bound)",
    "so they are disjoint worker groups, and the bounds keep that reading",
    "Rule code (sensitivity, bound_type profile_envelope)",
    "It is synthetic profile reweighting, not a disjoint worker allocation.",
    "profile-allocation sensitivity, not a bound over mutually exclusive worker groups",
    paste(
      "Under rule soc they are bounds over disjoint worker groups.",
      "Under rule code they are the profile-allocation sensitivity described",
      "above and not a sharp bound."
    ),
    "Cells that lack a .00 profile are counted and are never filled with nested specialties.",
    "The discrepancy is recorded in employment-combination-audit.csv and is not edited here.",
    "BLS publishes no field-descriptions workbook",
    "The OEWS Survey Methods document was not read.",
    "Seven of the twelve also appear as broad rows with the same employment",
    "They are not an exposure measure, and the package exports no substantive measure from them.",
    "No table holds a p-value, a significance test or a confidence interval.",
    "prevalence_spearman ranks the prevalence values rounded to 8 decimals.",
    "Values that differ only by floating point noise stay tied",
    "distance_spearman compares all pairwise distances, ranked after rounding to 8 decimals."
  )) {
    expect_phrase(phrase)
  }
  for (name in c(shipped_csv_names, own_helper_names)) {
    expect_phrase(name)
  }
})

test_that("README quotes the counts and employment totals that the snapshots hold", {
  readme <- read_readme()
  coverage <- read_extension("employment-coverage.csv")
  accounting <- read_extension("employment-accounting.csv")
  audit <- read_extension("employment-combination-audit.csv")
  skills <- read_extension("skills-provenance.csv")
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  scope <- function(rule, name, column) {
    value <- coverage[[column]][coverage$unit_rule == rule & coverage$scope == name]
    expect_length(value, 1L)
    value
  }
  raw_value <- function(quantity) accounting_value(accounting, "oews_raw", quantity)

  not_complete <- !(audit$status_soc == "complete" & audit$status_code == "complete")
  expect_equal(sum(!not_complete), 8L)
  expect_equal(sum(not_complete), 4L)
  expect_equal(
    scope("soc", "cells_with_all_other_soc_with_nested_detail", "n_cells"), 24
  )

  phrases <- c(
    sprintf(
      "The May 2025 national file has %s detailed cells.",
      fmt(scope("soc", "returned_panel", "n_cells"))
    ),
    sprintf(
      "official OEWS total (%s workers)",
      fmt(scope("soc", "official_total", "employment"))
    ),
    sprintf(
      "returned panel (the %s detailed cells, %s workers)",
      fmt(scope("soc", "returned_panel", "n_cells")),
      fmt(scope("soc", "returned_panel", "employment"))
    ),
    sprintf(
      "exceeds the detailed rows by %s workers",
      fmt(scope("soc", "official_not_returned", "employment"))
    ),
    sprintf(
      "The soc rule has %s units in the %s complete cells.",
      fmt(accounting_value(accounting, "soc_rule_universe", "units")),
      fmt(scope("soc", "complete", "n_cells"))
    ),
    sprintf(
      "The code rule has %s units in the same %s complete cells.",
      fmt(accounting_value(accounting, "code_rule_universe", "units")),
      fmt(scope("code", "complete", "n_cells"))
    ),
    sprintf(
      "share the same %s complete cells, which hold %s workers",
      fmt(scope("soc", "complete", "n_cells")),
      fmt(scope("soc", "complete", "employment"))
    ),
    sprintf(
      "There are %s cells with %s workers.",
      fmt(scope("soc", "incomplete", "n_cells")),
      fmt(scope("soc", "incomplete", "employment"))
    ),
    sprintf(
      "The soc rule has %s partial and %s no_profile cells.",
      fmt(scope("soc", "partial", "n_cells")),
      fmt(scope("soc", "no_profile", "n_cells"))
    ),
    sprintf(
      "The code rule has %s partial and %s no_profile cells.",
      fmt(scope("code", "partial", "n_cells")),
      fmt(scope("code", "no_profile", "n_cells"))
    ),
    sprintf(
      "They hold %s workers and are incomplete under both rules.",
      fmt(scope("soc", "cells_with_all_other_soc_with_nested_detail", "employment"))
    ),
    "Twenty-four cells hold",
    sprintf(
      "The BLS definitions list %s constituent SOC occupations",
      fmt(accounting_value(accounting, "bls_definitions", "combination_parts"))
    ),
    sprintf(
      "%s workers in all",
      fmt(raw_value("combination_employment_in_repeated_broad_rows"))
    ),
    do.call(
      sprintf,
      c(
        list("The four that are not are %s, %s, %s and %s."),
        as.list(audit$oews_code[not_complete])
      )
    ),
    sprintf(
      "%d occupations have activity profiles. %d of them are complete in all four blocks, and %d lack a skills profile.",
      skills$n_dwa_occupations, skills$n_pool_complete,
      skills$n_excluded_incomplete_skills_profile
    ),
    sprintf(
      "%d of the %d have no Recommend Suppress flag in any block, and %d have at least one.",
      skills$n_primary_universe, skills$n_pool_complete,
      skills$n_excluded_recommend_suppress
    ),
    sprintf(
      "In the %d pool, %d occupations have no Not Relevant rating and %d have neither a Not Relevant rating nor a Recommend Suppress flag.",
      skills$n_pool_complete, skills$n_pool_no_not_relevant, skills$n_pool_no_flags
    ),
    sprintf(
      "Essential (%d elements) and Transferable (%d elements)",
      skills$n_elements_essential, skills$n_elements_transferable
    ),
    sprintf(
      "The range is %d for Importance and %d for Level.",
      skills$range_importance, skills$range_level
    ),
    sprintf("rounded to %d decimals", skills$distance_digits),
    sprintf("The table keeps every neighbor through rank %d.", skills$k_max)
  )
  for (phrase in phrases) {
    expect_true(grepl(phrase, readme, fixed = TRUE), info = phrase)
  }
})

test_that("provenance tables record the pinned inputs, releases and source URLs", {
  provenance <- read_extension("employment-provenance.csv")
  skills <- read_extension("skills-provenance.csv")

  expect_named(provenance, c(
    "input", "release", "file_name", "source_url", "sha256", "access_note"
  ))
  expect_identical(provenance$input, c(
    "onet_text_archive", "oews_national_may_2025",
    "bls_occupation_definitions_may_2025"
  ))
  expect_identical(provenance$release, c("O*NET 30.3", "May 2025", "May 2025"))
  expect_identical(provenance$file_name, c(
    "db_30_3_text.zip", "oesm25nat.zip", "occupation_definitions_m2025.xlsx"
  ))
  expect_identical(provenance$source_url, c(
    "https://www.onetcenter.org/dl_files/database/db_30_3_text.zip",
    "https://www.bls.gov/oes/special-requests/oesm25nat.zip",
    "https://www.bls.gov/oes/occupation_definitions_m2025.xlsx"
  ))
  expect_identical(provenance$sha256, unname(pinned_sha256[c("onet", "oews", "definitions")]))
  expect_match(provenance$sha256, "^[a-f0-9]{64}$")

  expect_identical(skills$source_sha256, provenance$sha256[[1L]])
  expect_identical(skills$source_url, provenance$source_url[[1L]])

  # The BLS rows keep the scripted-access gap explicit instead of hiding it.
  expect_true(all(grepl("403", provenance$access_note[2:3], fixed = TRUE)))
  expect_match(provenance$access_note[[3L]], "do not link", fixed = TRUE)
  expect_match(provenance$access_note[[2L]], "tables.htm", fixed = TRUE)
})

test_that("shipped tables carry no run timestamps and the file set stays small", {
  paths <- vapply(shipped_csv_names, extension_path, character(1))
  expect_false(anyNA(paths))
  expect_true(all(file.exists(paths)))
  expect_true(file.exists(extension_path("README.txt")))

  stamp_name <- "(^|_)(date|time|timestamp|retrieved|accessed|created|generated)(_|$)"
  stamp_value <- "[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9]{2}:[0-9]{2}"
  for (path in paths) {
    header <- strsplit(gsub("\"", "", readLines(path, n = 1L, warn = FALSE)), ",")[[1L]]
    expect_false(any(grepl(stamp_name, header)), info = basename(path))
    expect_false(
      any(grepl(stamp_value, readLines(path, warn = FALSE))), info = basename(path)
    )
  }

  # No raw input or cached response is bundled with the derived tables.
  files <- list.files(extension_dir(), recursive = TRUE)
  expect_false(any(grepl(
    "\\.(zip|xlsx|xls|pdf|json|rds|rda|dta|sav)$", files, ignore.case = TRUE
  )))

  # Neighbor tables keep k_max rows per occupation, not all pairs.
  sizes <- file.size(paths)
  expect_lt(max(sizes), 2 * 1024^2)
  expect_lt(sum(sizes), 4 * 1024^2)
})

test_that("shipped tables, README and helpers are ASCII, and the CSV files use LF line endings", {
  paths <- vapply(shipped_csv_names, extension_path, character(1))
  helpers <- vapply(own_helper_names, article_helper_path, character(1))

  for (path in c(paths, extension_path("README.txt"), helpers)) {
    bytes <- file_bytes(path)
    expect_gt(length(bytes), 0L)
    expect_false(any(bytes > as.raw(127)), info = basename(path))
  }
  for (path in paths) {
    bytes <- file_bytes(path)
    expect_false(any(bytes == as.raw(13)), info = basename(path))
    expect_identical(bytes[[length(bytes)]], as.raw(10), info = basename(path))
  }
})

test_that("shipped files and helpers hold no credentials, cached API text or network calls", {
  paths <- vapply(shipped_csv_names, extension_path, character(1))
  helpers <- vapply(own_helper_names, article_helper_path, character(1))
  credential <- paste0(
    "\\bapi[_ -]?key\\b|\\bbearer\\s|authorization:|\\bpassword\\b|\\btoken|",
    "renviron"
  )
  network <- "download\\.file|httr2|curl::|\\burl\\(|req_perform|onet_api"

  for (path in c(paths, extension_path("README.txt"), helpers)) {
    text <- readLines(path, warn = FALSE)
    expect_false(
      any(grepl(credential, text, ignore.case = TRUE)), info = basename(path)
    )
  }
  for (path in helpers) {
    code <- readLines(path, warn = FALSE)
    expect_false(any(grepl(network, code)), info = basename(path))
  }
})

test_that("the article helpers stay outside the package namespace exports", {
  helper_names <- ls(ext, all.names = TRUE)
  expect_gt(length(helper_names), 40L)
  expect_length(intersect(helper_names, getNamespaceExports("onet2r")), 0L)
  expect_false(any(startsWith(helper_names, "onet_")))
  for (name in c(
    "distance_neighbors", "classify_onet_roster", "skills_neighbors",
    "prevalence_agreement"
  )) {
    expect_true(is.function(ext[[name]]), info = name)
    expect_false(name %in% getNamespaceExports("onet2r"), info = name)
  }
})
