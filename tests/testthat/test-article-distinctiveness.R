distinctiveness_helpers <- new.env(parent = globalenv())
distinctiveness_helper_path <- system.file(
  "article-helpers", "occupational-distinctiveness.R", package = "onet2r"
)
if (!nzchar(distinctiveness_helper_path)) {
  distinctiveness_helper_path <- testthat::test_path(
    "..", "..", "inst", "article-helpers", "occupational-distinctiveness.R"
  )
}
sys.source(
  distinctiveness_helper_path,
  envir = distinctiveness_helpers
)

test_that("article TF-IDF uses occupations, not task-link counts", {
  data <- tibble::tribble(
    ~occupation, ~item, ~weight,
    "a", "common", 1,
    "a", "rare", 1,
    "b", "common", 1,
    "c", "common", 1
  )
  scores <- distinctiveness_helpers$profile_tfidf(data, c("a", "b", "c"))
  expect_equal(scores$idf[scores$item == "common"], rep(0, 3))
  expect_equal(scores$idf[scores$item == "rare"], log(3))
  expect_equal(scores$tf_idf[scores$item == "rare"], log(3) / 2)
  expect_equal(scores$prevalence[scores$item == "rare"], 1 / 3)
  expect_equal(
    scores |>
      dplyr::summarise(total = sum(share), .by = occupation) |>
      dplyr::pull(total),
    rep(1, 3)
  )
  data$weight[data$occupation == "a"] <- 10
  expect_equal(
    distinctiveness_helpers$profile_tfidf(data, c("a", "b", "c"))$tf_idf,
    scores$tf_idf
  )
})

test_that("article profile distance distinguishes composition and ignores scale", {
  data <- tibble::tribble(
    ~occupation, ~item, ~weight,
    "a", "x", 1,
    "b", "x", 10,
    "c", "y", 1,
    "c", "unused", 0
  )
  distances <- distinctiveness_helpers$profile_distances(data, c("a", "b", "c"))
  expect_s3_class(distances, "tbl_df")
  expect_equal(distances$distance, c(0, 1, 1))
  scores <- distinctiveness_helpers$profile_tfidf(data, c("a", "b", "c"))
  expect_false("unused" %in% scores$item)
  expect_true(all(is.finite(scores$idf)))
  data$item <- "parent"
  collapsed <- data |>
    dplyr::summarise(weight = sum(weight), .by = c(occupation, item))
  expect_equal(
    distinctiveness_helpers$profile_distances(
      collapsed, c("a", "b", "c")
    )$distance,
    rep(0, 3)
  )
})

test_that("article profiles reject ambiguous denominators and missing coverage", {
  data <- tibble::tibble(
    occupation = c("a", "b"), item = c("x", "y"), weight = c(1, 1)
  )
  score <- distinctiveness_helpers$profile_tfidf
  expect_error(score(data, "a"), "at least two")
  expect_error(score(data, c("a", "a")), "unique")
  expect_error(score(data, c("a", "b", "c")), "positive, observed")
  expect_error(score(data, c("a", "c")), "declared universe")
  expect_error(score(dplyr::bind_rows(data, data[1, ]), c("a", "b")), "duplicate")
  for (invalid in c(-1, NA_real_, Inf, NaN)) {
    bad <- data
    bad$weight[1] <- invalid
    expect_error(score(bad, c("a", "b")), "finite, non-negative")
  }
  data$weight[1] <- 0
  expect_error(score(data, c("a", "b")), "positive, observed")
  expect_error(score(data[0, ], c("a", "b")), "positive, observed")
  data$item[1] <- NA_character_
  expect_error(score(data, c("a", "b")), "non-missing character")
  expect_error(score(data.frame(x = 1), c("a", "b")), "occupation, item")
})

test_that("bundled distinctiveness snapshot has coherent coverage and formulas", {
  read_snapshot <- function(name) {
    utils::read.csv(system.file(
      "extdata", "occupational-distinctiveness", name, package = "onet2r"
    ))
  }
  provenance <- read_snapshot("provenance.csv")
  activities <- read_snapshot("activity-prevalence.csv")
  occupations <- read_snapshot("occupation-profiles.csv")
  coverage <- read_snapshot("coverage.csv")
  resolution <- read_snapshot("resolution-summary.csv")
  n <- provenance$n_linked_occupations
  expect_equal(nrow(provenance), 1L)
  expect_equal(n, 923L)
  expect_equal(nrow(occupations), n)
  expect_equal(nrow(activities), provenance$n_activities)
  expect_equal(nrow(coverage), provenance$n_taxonomy_occupations)
  expect_equal(sum(coverage$in_universe), n)
  expect_equal(sum(coverage$n_tasks, na.rm = TRUE), provenance$n_tasks)
  expect_equal(sum(coverage$n_linked_tasks, na.rm = TRUE),
               provenance$n_linked_tasks)
  expect_equal(provenance$n_shared_task_ids, 0L)
  expect_match(provenance$source_sha256, "^[a-f0-9]{64}$")
  expect_equal(activities$prevalence, activities$n_occupations / n)
  expect_equal(activities$idf, log(n / activities$n_occupations))
  expect_equal(sum(activities$n_occupations), sum(occupations$n_activities))
  expect_true(all(occupations$mean_idf >= 0 & occupations$mean_idf <= log(n)))
  expect_true(all(occupations$nearest_distance >= 0 &
                    occupations$nearest_distance <= occupations$mean_distance))
  expect_equal(occupations$rarity_rank,
               rank(-occupations$mean_idf, ties.method = "min"))
  expect_equal(occupations$occupation[1], "51-3023.00")
  expect_equal(activities$n_occupations[1], 91L)
  expect_equal(resolution$n_pairs, rep(choose(n, 2), 3))
  expect_true(all(diff(resolution$mean_distance) <= 0))
  expect_equal(resolution$mean_distance_lost,
               resolution$mean_distance[1] - resolution$mean_distance)
})

test_that("snapshot CSV writer emits portable LF line endings", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  distinctiveness_helpers$write_snapshot_csv(
    tibble::tibble(item = c("one", "two"), count = c(1L, 2L)),
    path
  )
  bytes <- readBin(path, "raw", n = file.info(path)$size)
  expect_true(any(bytes == as.raw(10)))
  expect_false(any(
    bytes[-length(bytes)] == as.raw(13) & bytes[-1L] == as.raw(10)
  ))
})
