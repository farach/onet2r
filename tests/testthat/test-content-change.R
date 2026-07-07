# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Software (15-1132.00) spans 22.1, 23.1, 25.1: task 3 is dropped and task 4
# added at 23.1, and 23.1 -> 25.1 crosses the v25.1 SOC seam with no content
# change. Nurse (29-1141.00) spans 20.1 -> 21.1, which crosses the v21.0 Task
# Relevance (scale) seam. The 21.1 -> 22.1 pair shares no occupation.
content_fixture <- function() {
  tibble::tibble(
    onet_soc_code = c(
      "29-1141.00", # 20.1
      "29-1141.00", # 21.1
      "15-1132.00", "15-1132.00", "15-1132.00", # 22.1
      "15-1132.00", "15-1132.00", "15-1132.00", # 23.1
      "15-1132.00", "15-1132.00", "15-1132.00" # 25.1
    ),
    release_version = c(
      "20.1", "21.1",
      "22.1", "22.1", "22.1",
      "23.1", "23.1", "23.1",
      "25.1", "25.1", "25.1"
    ),
    release_date = as.Date(c(
      "2015-10-01", "2016-11-01",
      "2017-10-01", "2017-10-01", "2017-10-01",
      "2018-11-01", "2018-11-01", "2018-11-01",
      "2020-11-01", "2020-11-01", "2020-11-01"
    )),
    soc_vintage = c(
      "2010", "2010",
      "2010", "2010", "2010",
      "2010", "2010", "2010",
      "2019", "2019", "2019"
    ),
    task_id = c(
      "5001", "5001",
      "1", "2", "3",
      "1", "2", "4",
      "1", "2", "4"
    ),
    scale_id = "IM",
    data_value = c(
      4.0, 4.0,
      4.0, 3.0, 2.0,
      4.5, 3.0, 3.0,
      4.5, 3.0, 3.0
    )
  )
}

# ---------------------------------------------------------------------------
# Golden metric values
# ---------------------------------------------------------------------------

test_that("content_change computes exact set and rating metrics for a churn pair", {
  cc <- onet_content_change(content_fixture())
  pair <- cc |>
    dplyr::filter(
      .data$onet_soc_code == "15-1132.00",
      .data$from_release == "22.1", .data$to_release == "23.1"
    )

  expect_equal(pair$n_from, 3L)
  expect_equal(pair$n_to, 3L)
  expect_equal(pair$n_added, 1L)
  expect_equal(pair$n_dropped, 1L)
  expect_equal(pair$n_retained, 2L)
  expect_equal(pair$jaccard, 0.5)
  expect_equal(pair$churn_rate, 0.5)
  # Retained deltas: task 1 (+0.5), task 2 (0) -> L2 = 0.5.
  expect_equal(pair$rating_delta_l2, 0.5)
  # Cosine over union {1,2,3,4}: from=(4,3,2,0), to=(4.5,3,0,3).
  expect_equal(pair$cosine, 27 / (sqrt(29) * sqrt(38.25)))
  expect_false(pair$seam)
  expect_true(pair$safely_comparable)
})

test_that("content_change flags the v25.1 SOC seam but still reports metrics", {
  cc <- onet_content_change(content_fixture())
  pair <- cc |>
    dplyr::filter(.data$from_release == "23.1", .data$to_release == "25.1")
  expect_equal(nrow(pair), 1L)
  expect_true(pair$seam)
  expect_equal(pair$seam_type, "soc_seam")
  expect_false(pair$safely_comparable)
  # Identical task set and ratings across the seam.
  expect_equal(pair$jaccard, 1)
  expect_equal(pair$churn_rate, 0)
  expect_equal(pair$rating_delta_l2, 0)
  expect_equal(pair$cosine, 1)
})

test_that("content_change flags the v21.0 Task Relevance scale seam", {
  cc <- onet_content_change(content_fixture())
  pair <- cc |>
    dplyr::filter(.data$from_release == "20.1", .data$to_release == "21.1")
  expect_equal(nrow(pair), 1L)
  expect_true(pair$seam)
  expect_equal(pair$seam_type, "scale_seam")
  expect_false(pair$safely_comparable)
})

test_that("content_change drops pairs that share no occupation", {
  cc <- onet_content_change(content_fixture())
  # 21.1 -> 22.1 shares no occupation.
  bridge <- cc |>
    dplyr::filter(.data$from_release == "21.1", .data$to_release == "22.1")
  expect_equal(nrow(bridge), 0L)
})

# ---------------------------------------------------------------------------
# from / to selection
# ---------------------------------------------------------------------------

test_that("from and to restrict output to a single pair", {
  cc <- onet_content_change(content_fixture(), from = "22.1", to = "23.1")
  expect_equal(unique(cc$from_release), "22.1")
  expect_equal(unique(cc$to_release), "23.1")
})

test_that("content_change errors on an unknown from or to version", {
  expect_error(
    onet_content_change(content_fixture(), from = "22.1", to = "99.9"),
    "99.9"
  )
  expect_error(
    onet_content_change(content_fixture(), from = "22.1"),
    "both"
  )
})

test_that("min_importance filters items before set membership", {
  panel <- content_fixture()
  # Drop task 3 (importance 2.0) from 22.1 via the floor.
  cc <- onet_content_change(panel, min_importance = 2.5, from = "22.1", to = "23.1")
  pair <- cc |> dplyr::filter(.data$onet_soc_code == "15-1132.00")
  # 22.1 now holds tasks 1,2; 23.1 holds 1,2,4 -> one added, none dropped.
  expect_equal(pair$n_from, 2L)
  expect_equal(pair$n_added, 1L)
  expect_equal(pair$n_dropped, 0L)
})

# ---------------------------------------------------------------------------
# Property tests
# ---------------------------------------------------------------------------

test_that("content_change metrics satisfy their algebraic invariants", {
  cc <- onet_content_change(content_fixture())
  expect_true(all(cc$jaccard >= 0 & cc$jaccard <= 1))
  expect_true(all(dplyr::near(cc$churn_rate, 1 - cc$jaccard)))
  cos_ok <- is.na(cc$cosine) | (cc$cosine >= -1e-9 & cc$cosine <= 1 + 1e-9)
  expect_true(all(cos_ok))
  expect_equal(cc$n_added + cc$n_retained, cc$n_to)
  expect_equal(cc$n_dropped + cc$n_retained, cc$n_from)
})

test_that("an identical release pair has jaccard 1 and zero churn", {
  cc <- onet_content_change(content_fixture(), from = "23.1", to = "23.1")
  expect_true(all(cc$jaccard == 1))
  expect_true(all(cc$churn_rate == 0))
  expect_true(all(cc$rating_delta_l2 == 0))
  expect_true(all(dplyr::near(cc$cosine, 1)))
})

# ---------------------------------------------------------------------------
# Empty schema
# ---------------------------------------------------------------------------

test_that("content_change returns a stable empty schema", {
  empty <- onet_content_change(content_fixture()[0, ])
  expect_equal(nrow(empty), 0L)
  expect_true(all(
    c(
      "onet_soc_code", "from_release", "to_release", "n_added", "n_dropped",
      "n_retained", "jaccard", "churn_rate", "rating_delta_l2", "cosine",
      "seam", "seam_type", "safely_comparable"
    ) %in% names(empty)
  ))
})

test_that("a single-release panel yields no comparison rows", {
  panel <- content_fixture() |> dplyr::filter(.data$release_version == "22.1")
  expect_equal(nrow(onet_content_change(panel)), 0L)
})

test_that("content_change errors on a missing required column", {
  panel <- content_fixture()
  panel$data_value <- NULL
  expect_error(onet_content_change(panel), "data_value")
})
