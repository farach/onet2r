test_that("domain neighbors use one complete eligible occupation universe", {
  helpers <- new.env(parent = globalenv())
  sys.source(system.file(
    "article-helpers", "occupational-distinctiveness.R", package = "onet2r"
  ), helpers)
  read_snapshot <- function(name) {
    utils::read.csv(helpers$snapshot_csv_path(file.path(
      system.file("extdata", "occupational-extensions", package = "onet2r"),
      paste0(name, ".csv")
    )))
  }
  roster <- read_snapshot("skills-common-universe")
  universe <- roster$onet_soc_code[roster$in_common_universe]
  neighbors <- read_snapshot("cross-domain-neighbors")
  blocks <- c("dwa", "software", "es_im", "es_lv", "ts_im", "ts_lv")
  expect_setequal(unique(neighbors$block), blocks)
  software_k5 <- neighbors[
    neighbors$block == "software" & neighbors$rank <= 5L, ]
  expect_equal(sum(table(software_k5$occupation) > 5L), 515L)
  for (block in blocks) {
    data <- neighbors[neighbors$block == block, ]
    expect_setequal(unique(data$occupation), universe)
    expect_true(all(data$neighbor %in% universe))
    expect_false(any(data$occupation == data$neighbor))
    expect_true(all(data$rank >= 1L & data$rank <= 10L))
    expect_false(anyDuplicated(data[c("occupation", "neighbor")]) > 0L)
  }
  overlap <- read_snapshot("cross-domain-neighbor-overlap")
  expect_equal(overlap$jaccard, overlap$n_both / overlap$n_union)
  expect_equal(overlap$n_union, overlap$n_a + overlap$n_b - overlap$n_both)
  expect_true(all(overlap$jaccard >= 0 & overlap$jaccard <= 1))
  expect_true(all(overlap$n_a >= overlap$k & overlap$n_b >= overlap$k))
  expect_equal(nrow(overlap), choose(length(blocks), 2) * 3 * length(universe))
  summary <- read_snapshot("cross-domain-overlap-summary")
  expect_true(all(summary$n_occupations == length(universe)))
})
