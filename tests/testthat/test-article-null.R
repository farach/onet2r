test_that("Curveball trades preserve both binary matrix margins", {
  env <- new.env(parent = globalenv())
  sys.source(system.file(
    "article-helpers", "distinctiveness-null.R", package = "onet2r"
  ), env)
  sets <- list(a = c("u", "v"), b = c("v", "w", "x"), c = c("x", "y"))
  set.seed(171)
  shuffled <- env$trade_activity_sets(sets, 100L)
  expect_identical(lengths(shuffled), lengths(sets))
  expect_identical(table(unlist(shuffled)), table(unlist(sets)))
  expect_true(all(vapply(shuffled, anyDuplicated, integer(1)) == 0L))
  expect_identical(env$trade_activity_sets(sets, 0L), sets)
  expect_error(env$trade_activity_sets(sets, -1), "nonnegative")
  expect_error(env$trade_activity_sets(list(a = "u"), 1), "nonempty")
  expect_error(env$trade_activity_sets(list(a = "u", b = character()), 1),
               "nonempty")
})

test_that("binary overlap distances match the full total-variation formula", {
  env <- new.env(parent = globalenv())
  sys.source(system.file(
    "article-helpers", "distinctiveness-null.R", package = "onet2r"
  ), env)
  sets <- list(a = c("u", "v"), b = c("u", "v", "w", "x"),
               c = c("u", "v"), d = "z")
  distances <- env$binary_activity_distances(sets)
  expect_equal(unname(diag(distances)), rep(0, 4))
  expect_equal(distances["a", "b"], 0.5)
  expect_equal(distances["a", "c"], 0)
  expect_equal(distances["a", "d"], 1)
  items <- unique(unlist(sets))
  shares <- t(vapply(sets, function(x) as.numeric(items %in% x) / length(x),
                     numeric(length(items))))
  expect_equal(unname(distances), unname(as.matrix(stats::dist(
    shares, method = "manhattan"
  )) / 2))
})
