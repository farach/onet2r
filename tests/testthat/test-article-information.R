test_that("information budgets conserve identity signal and expose IDF equivalence", {
  env <- new.env(parent = globalenv())
  for (name in c("occupational-distinctiveness.R", "distinctiveness-information.R")) {
    sys.source(system.file("article-helpers", name, package = "onet2r"), env)
  }
  profiles <- tibble::tribble(
    ~occupation, ~item, ~weight,
    "a", "u", 1, "a", "v", 1, "b", "u", 1, "b", "w", 1
  )
  mapping <- tibble::tribble(
    ~item, ~iwa_element_id, ~gwa_element_id,
    "u", "i1", "g", "v", "i2", "g", "w", "i2", "g"
  )
  result <- env$profile_information_budget(profiles, mapping, c("a", "b"))
  expect_equal(result$summary$information_bits, c(0.5, 0, 0))
  expect_equal(result$summary$retained_dwa_percent, c(100, 0, 0))
  expect_equal(sum(result$parent_loss$lost_bits), 0.5)
  rarity <- env$profile_tfidf(profiles, c("a", "b"))
  expect_equal(sum(rarity$tf_idf) / 2 / log(2), 0.5)
  expect_error(env$profile_information_budget(
    profiles, mapping[-1, ], c("a", "b")
  ), "hierarchy")
  ambiguous <- rbind(mapping, mapping[1, ])
  expect_error(env$profile_information_budget(
    profiles, ambiguous, c("a", "b")
  ), "hierarchy")
  crossed <- mapping
  crossed$gwa_element_id[[3]] <- "other"
  expect_error(env$profile_information_budget(
    profiles, crossed, c("a", "b")
  ), "hierarchy")
  identical_profiles <- profiles
  identical_profiles$item[identical_profiles$item == "w"] <- "v"
  zero <- env$profile_information_budget(
    identical_profiles, mapping, c("a", "b")
  )
  expect_equal(zero$summary$information_bits, c(0, 0, 0))
  expect_true(all(is.na(zero$summary$retained_dwa_percent)))
})

test_that("bundled information and null results reconcile to the snapshot", {
  read_snapshot <- function(name) {
    utils::read.csv(system.file(
      "extdata", "occupational-distinctiveness", name, package = "onet2r"
    ))
  }
  information <- read_snapshot("information-summary.csv")
  loss <- read_snapshot("information-loss.csv")
  diagnostics <- read_snapshot("information-occupation-diagnostics.csv")
  null <- read_snapshot("null-summary.csv")
  resolution <- read_snapshot("resolution-summary.csv")
  expect_equal(information$n_occupations, rep(923L, 3))
  expect_equal(information$n_labels_with_positive_mass, c(2083L, 332L, 41L))
  expect_equal(information$information_bits,
               c(6.38972523406045, 3.95418509255745, 1.66557792795338))
  expect_true(all(diff(information$information_bits) <= 0))
  expect_equal(sum(loss$mixture_mass), 1)
  expect_equal(sum(loss$lost_bits), information$lost_from_dwa_bits[[3]])
  expect_equal(loss$conditional_lost_bits * loss$mixture_mass, loss$lost_bits)
  expect_equal(information$information_bits + information$conditional_entropy_bits,
               rep(log2(923), 3))
  expect_equal(nrow(diagnostics), 3L * 923L)
  for (grain in information$grain) {
    expect_equal(
      mean(diagnostics$information_bits[diagnostics$grain == grain]),
      information$information_bits[information$grain == grain]
    )
  }
  observed <- null[null$draw == 0L, ]
  expect_equal(observed$mean_pair_distance, resolution$mean_distance)
  expect_equal(nrow(null), 3L * 21L)
  expect_true(all(null$median_nearest_distance >= 0 &
                    null$median_nearest_distance <= 1))
  expect_true(all(null$mean_pair_distance >= null$median_nearest_distance))
  inventory <- read_snapshot("task-inventory.csv")
  expect_equal(nrow(inventory), 923L)
  expect_equal(sum(inventory$n_core > 0), 893L)
  expect_equal(inventory$n_tasks,
               inventory$n_core + inventory$n_supplemental +
                 inventory$n_unclassified)
  expect_equal(inventory$n_core[inventory$onet_soc_code == "51-3023.00"], 0L)
  expect_equal(inventory$n_supplemental[
    inventory$onet_soc_code == "51-3023.00"
  ], 14L)
  expect_equal(inventory$n_core[inventory$onet_soc_code == "41-9012.00"], 1L)
  expect_match(min(inventory$min_source_date), "^2004-")
  expect_match(max(inventory$max_source_date), "^2025-")
})
