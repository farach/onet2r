software_helpers <- new.env(parent = globalenv())
for (file in c("occupational-distinctiveness.R", "distinctiveness-information.R",
               "software-distinctiveness.R", "occupational-neighbors.R")) {
  path <- system.file("article-helpers", file, package = "onet2r")
  if (!nzchar(path)) {
    path <- testthat::test_path("..", "..", "inst", "article-helpers", file)
  }
  sys.source(path, software_helpers)
}

test_that("binary overlap reproduces total variation and keeps inventory sizes", {
  data <- tibble::tribble(
    ~occupation, ~item, ~weight,
    "a", "shared", 1, "b", "shared", 1, "b", "extra", 1, "c", "other", 1
  )
  pairs <- software_helpers$binary_profile_pairs(data, c("a", "b", "c"))
  expect_equal(pairs$distance,
               software_helpers$profile_distances(data, c("a", "b", "c"))$distance)
  expect_equal(pairs$shared, c(1L, 0L, 0L))
  expect_equal(pairs$distance, c(0.5, 1, 1))
  expect_equal(pairs$jaccard_distance, c(0.5, 1, 1))
  data$weight[1] <- 2
  expect_error(software_helpers$binary_profile_pairs(data, c("a", "b", "c")),
               "one unit")
})

test_that("neighbors retain boundary ties and overlap checks the universe", {
  pairs <- tibble::tribble(
    ~occupation_a, ~occupation_b, ~distance,
    "a", "b", 0.2, "a", "c", 0.2, "b", "c", 0.8
  )
  nearest <- software_helpers$profile_neighbors(pairs)
  expect_equal(sum(nearest$occupation == "a"), 2L)
  overlap <- software_helpers$neighbor_overlap(pairs, pairs, 1L)
  expect_equal(overlap$neighbor_jaccard, rep(1, 3))
  expect_equal(overlap$n_a, c(2L, 1L, 1L))
  other <- pairs
  other$occupation_a[other$occupation_a == "a"] <- "d"
  expect_error(software_helpers$neighbor_overlap(pairs, other), "same")
  expect_error(software_helpers$profile_neighbors(pairs, 0), "positive integer")
  pairs$distance[1] <- NA_real_
  expect_error(software_helpers$profile_neighbors(pairs), "finite")
})

test_that("Sorensen separates containment from replacement without reranking", {
  data <- tibble::tibble(
    occupation = rep(c("a", "b", "c"), c(3, 6, 3)),
    item = c(letters[1:3], letters[1:6], "a", "b", "g"), weight = 1
  )
  pairs <- software_helpers$binary_profile_pairs(data, c("a", "b", "c"))
  ab <- pairs[pairs$occupation_a == "a" & pairs$occupation_b == "b", ]
  ac <- pairs[pairs$occupation_a == "a" & pairs$occupation_b == "c", ]
  expect_equal(ab$sorensen_total, 1 / 3)
  expect_equal(ac$sorensen_total, 1 / 3)
  expect_equal(ab$replacement_component, 0)
  expect_equal(ab$nestedness_resultant_component, 1 / 3)
  expect_equal(ac$replacement_component, 1 / 3)
  expect_equal(ac$nestedness_resultant_component, 0)
  expect_equal(ab$containment_a_in_b, 1)
  expect_equal(ab$containment_b_in_a, 0.5)
  expect_equal(pairs$sorensen_total, pairs$replacement_component +
                 pairs$nestedness_resultant_component)
  expect_true(all(pairs$nestedness_resultant_component >= -1e-12))
  expect_equal(pairs$sorensen_total,
               pairs$jaccard_distance / (2 - pairs$jaccard_distance))
})

test_that("software snapshot separates names, routes and mass-preserving grains", {
  read_snapshot <- function(name) {
    utils::read.csv(software_helpers$snapshot_csv_path(file.path(
      system.file("extdata", "occupational-software", package = "onet2r"),
      paste0(name, ".csv")
    )))
  }
  provenance <- read_snapshot("provenance")
  prevalence <- read_snapshot("prevalence")
  occupations <- read_snapshot("occupations")
  resolution <- read_snapshot("resolution")
  expect_equal(provenance$n_occupations, 923L)
  expect_equal(provenance$n_examples, 8753L)
  expect_equal(provenance$n_routes, 8908L)
  expect_equal(provenance$n_multiple_route_examples, 151L)
  expect_equal(provenance$n_within_occupation_multiple_routes, 0L)
  expect_equal(prevalence$prevalence, prevalence$n_occupations / 923)
  expect_equal(prevalence$idf, -log(prevalence$prevalence))
  expect_equal(prevalence$item[1], "Microsoft Excel")
  expect_equal(prevalence$n_occupations[1], 859L)
  expect_equal(sum(prevalence$n_occupations), sum(occupations$n_examples))
  expect_equal(resolution$n_labels, c(8908L, 134L, 17L))
  expect_true(all(diff(resolution$information_bits) <= 0))
  expect_true(all(diff(resolution$mean_distance) <= 0))
  expect_equal(resolution$retained_percent,
               100 * resolution$information_bits / resolution$information_bits[1])
  identity <- read_snapshot("identity-information")
  expect_gt(identity$category_annotation_bits, 0)
  expect_equal(identity$name_information_bits + identity$category_annotation_bits,
               identity$pair_information_bits)
  sensitivity <- read_snapshot("representation-sensitivity")
  expect_true(all(sensitivity$neighbor_jaccard >= 0 &
                    sensitivity$neighbor_jaccard <= 1))
})

test_that("route information can exceed exact-name information", {
  profiles <- tibble::tibble(
    occupation = c("a", "b"), item = c("route1", "route2"), weight = 1
  )
  mapping <- tibble::tibble(
    item = c("route1", "route2"), group = c("same example", "same example")
  )
  name_bits <- software_helpers$profile_partition_information(
    profiles, mapping, c("a", "b")
  )
  mapping$group <- mapping$item
  pair_bits <- software_helpers$profile_partition_information(
    profiles, mapping, c("a", "b")
  )
  expect_equal(name_bits, 0)
  expect_equal(pair_bits, 1)
})
