test_that("onet_decompose_change components sum to total change", {
  from_scores <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    measure_score = c(1, 2),
    safely_comparable = c(TRUE, FALSE)
  )
  to_scores <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    measure_score = c(2, 2.5),
    safely_comparable = c(TRUE, FALSE)
  )
  from_weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    employment = c(100, 100)
  )
  to_weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    employment = c(150, 50)
  )

  result <- onet_decompose_change(from_scores, to_scores, from_weights, to_weights)

  expected <- c(0.5, -0.25, 0.125, 0.25, 0.625)
  expect_equal(result$value, expected)
  components <- sum(result$value[result$component != "total_change"])
  total <- result$value[result$component == "total_change"]
  expect_equal(components, total)
  expect_s3_class(result, "onet_decomposition")
  expect_equal(onet_coverage(result)$leakage, 0)
})

test_that("onet_decompose_change uses from-side comparable flags", {
  from_scores <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    measure_score = c(1, 2),
    safely_comparable = c(TRUE, FALSE)
  )
  to_scores <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    measure_score = c(2, 3)
  )
  weights <- tibble::tibble(
    reference_soc_code = c("15-1252", "29-1141"),
    employment = c(100, 100)
  )

  result <- onet_decompose_change(from_scores, to_scores, weights, weights)

  expect_equal(result$value[result$component == "within"], 0.5)
  expect_equal(result$value[result$component == "unclassifiable"], 0.5)
  expect_equal(onet_coverage(result)$n_safely_comparable, 1L)
})

test_that("onet_decompose_change treats missing comparable flags conservatively", {
  from_scores <- tibble::tibble(
    reference_soc_code = "15-1252",
    measure_score = 1,
    safely_comparable = NA
  )
  to_scores <- tibble::tibble(
    reference_soc_code = "15-1252",
    measure_score = 2,
    safely_comparable = TRUE
  )
  weights <- tibble::tibble(reference_soc_code = "15-1252", employment = 100)

  result <- onet_decompose_change(from_scores, to_scores, weights, weights)

  expect_equal(result$value[result$component == "within"], 0)
  expect_equal(result$value[result$component == "unclassifiable"], 1)
})
