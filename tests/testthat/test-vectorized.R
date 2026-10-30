# Vectorized helpers must return exactly what the per-group dplyr code they
# replace returned, so these tests compare with identical(), not a tolerance.

grouped_input <- function() {
  set.seed(20261030)
  n <- 500
  tibble::tibble(
    key = sample(c(letters[1:12], NA), n, replace = TRUE),
    cell = sample(c("x", "y"), n, replace = TRUE),
    whole = as.double(sample(c(0:900, NA), n, replace = TRUE)),
    fraction = ifelse(runif(n) < 0.1, NA, runif(n) * 1.07),
    weight = ifelse(
      runif(n) < 0.1, NA, ifelse(runif(n) < 0.1, 0, runif(n) * 10)
    ),
    flag = runif(n) < 0.4,
    label = sample(c("p", "q", "", NA), n, replace = TRUE)
  )
}

test_that("map_unique matches applying the function to every element", {
  x <- rep(
    c("15-1252.00", " 151252 ", NA, "", "29-1141", "11-1011.01", "x"),
    50
  )
  expect_identical(
    onet2r:::map_unique(x, onet2r:::standardize_soc_code_values),
    onet2r:::standardize_soc_code_values(x)
  )
  expect_identical(
    onet2r:::standardize_onet_soc_code(factor(x)),
    onet2r:::standardize_onet_soc_code_values(x)
  )
  months <- rep(c("07/2025", "13/2025", NA, "", "1/2024"), 20)
  expect_identical(
    onet2r:::parse_onet_month(months),
    onet2r:::parse_onet_month_values(months)
  )
  expect_identical(onet2r:::standardize_soc_code(character()), character())
})

test_that("group_sum matches sum() within dplyr groups exactly", {
  data <- grouped_input()
  group <- onet2r:::group_index(data["key"])
  expected <- function(expr) {
    dplyr::summarise(data, value = {{ expr }}, .by = "key")$value
  }

  expect_identical(
    onet2r:::group_sum(data$whole, group, na.rm = TRUE),
    expected(sum(whole, na.rm = TRUE))
  )
  expect_identical(
    onet2r:::group_sum(data$fraction, group, na.rm = TRUE),
    expected(sum(fraction, na.rm = TRUE))
  )
  expect_identical(
    onet2r:::group_sum(data$fraction, group),
    expected(sum(fraction))
  )
  expect_identical(onet2r:::group_sum(data$flag, group), expected(sum(flag)))
  expect_identical(
    onet2r:::group_sum(double(), onet2r:::group_index(data["key"][0, ])),
    double()
  )
})

test_that("group_sum uses double accumulation only when it is exact", {
  expect_true(onet2r:::double_sum_is_exact(c(1, 2, NA, Inf, 3e6)))
  expect_false(onet2r:::double_sum_is_exact(c(1, 2.5)))
  expect_false(onet2r:::double_sum_is_exact(c(2^52, 2^52)))
})

test_that("grouped summaries match the dplyr expressions they replace", {
  data <- grouped_input()
  group <- onet2r:::group_index(data[c("key", "cell")])
  expected <- dplyr::summarise(
    data,
    mean = onet2r:::weighted_mean_na(fraction, weight),
    distinct = dplyr::n_distinct(label),
    first = dplyr::first(label[!is.na(label)]),
    collapsed = onet2r:::collapse_unique(label),
    .by = c("key", "cell")
  )

  expect_identical(
    vctrs::vec_slice(data[c("key", "cell")], group$first),
    expected[c("key", "cell")]
  )
  expect_identical(
    onet2r:::group_weighted_mean(data$fraction, data$weight, group),
    expected$mean
  )
  expect_identical(
    onet2r:::group_n_distinct(data$label, group),
    expected$distinct
  )
  expect_identical(
    onet2r:::group_first_present(data$label, group),
    expected$first
  )
  expect_identical(
    onet2r:::group_collapse_unique(data$label, group),
    expected$collapsed
  )
})

test_that("replicate totals match per-cell sums for any weights", {
  data <- grouped_input()
  data$crosswalk_weight <- 1
  data$rep1 <- as.integer(data$whole)
  data$rep2 <- data$whole + 1
  data$.pums_row <- seq_len(nrow(data))
  cells <- onet2r:::group_index(data[c("key", "cell")])
  per_cell <- function(joined, column) {
    dplyr::summarise(
      joined,
      value = sum(.data[[column]] * .data$crosswalk_weight, na.rm = TRUE),
      .by = c("key", "cell")
    )$value
  }
  totals <- function(joined, columns, cells) {
    onet2r:::replicate_cell_totals(joined, data, columns, cells)
  }

  expect_identical(
    totals(data, c("rep1", "rep2"), cells),
    cbind(per_cell(data, "rep1"), per_cell(data, "rep2"))
  )
  expect_identical(
    totals(data, c("fraction", "rep2"), cells),
    cbind(per_cell(data, "fraction"), per_cell(data, "rep2"))
  )

  # A crosswalk can repeat and drop source rows. Totals read the source rows.
  joined <- data[c(3:200, 5, 5, 17), ]
  joined$crosswalk_weight <- rep(c(1, 2, 3), length.out = nrow(joined))
  joined_cells <- onet2r:::group_index(joined[c("key", "cell")])
  expect_identical(
    totals(joined, c("rep1", "fraction"), joined_cells),
    cbind(per_cell(joined, "rep1"), per_cell(joined, "fraction"))
  )
})

test_that("parse_onet_number keeps full double precision", {
  expect_identical(onet2r:::parse_onet_number(1 / 3), 1 / 3)
  expect_identical(onet2r:::parse_onet_number(c(a = 0.1 + 0.2)), 0.1 + 0.2)
  expect_identical(onet2r:::parse_onet_number(c(2L, NA)), c(2, NA))
  expect_identical(
    onet2r:::parse_onet_number(c("1,234.5", "n/a", NA)),
    c(1234.5, NA, NA)
  )
  expect_identical(onet2r:::parse_onet_number(TRUE), NA_real_)
})

test_that("duplicate_key_rows keeps its schema with and without duplicates", {
  data <- tibble::tibble(a = c("x", "y", "x", NA, NA), b = c(1, 2, 1, NA, NA))

  none <- onet2r:::duplicate_key_rows(data[1:2, ], c("a", "b"))
  expect_identical(
    none,
    tibble::as_tibble(list(a = character(), b = double(), .rows = integer()))
  )

  found <- onet2r:::duplicate_key_rows(data, c("a", "b"))
  expect_identical(
    found,
    tibble::as_tibble(list(a = c("x", NA), b = c(1, NA), .rows = c(2L, 2L)))
  )
})

test_that("the resurvey clock carries forward across an unsurveyed release", {
  panel <- tibble::tibble(
    release_version = c("22.1", "23.1", "24.1"),
    release_date = as.Date(c("2017-10-01", "2018-11-01", "2019-11-01")),
    soc_vintage = "2010",
    onet_soc_code = "15-1132.00",
    task_id = "1001",
    scale_id = "IM",
    data_value = 4,
    source_date = as.Date(c("2016-07-01", "2018-01-01", "2019-01-01")),
    domain_source = c("Incumbent", "Analyst", "Incumbent")
  )

  rp <- onet_resurvey_panel(panel)

  expect_identical(
    rp$occ_survey_date,
    as.Date(c("2016-07-01", NA, "2019-01-01"))
  )
  expect_identical(
    rp$prev_survey_date,
    as.Date(c(NA, "2016-07-01", "2016-07-01"))
  )
  expect_identical(rp$prev_release_version, c(NA, "22.1", "23.1"))
  expect_identical(rp$resurvey_event, c(NA, FALSE, TRUE))
  expect_identical(rp$cycle_index, c(0L, 0L, 1L))
  resolved <- as.numeric(as.Date("2019-01-01") - as.Date("2016-07-01"))
  expect_equal(rp$age_resolved, c(NA, NA, resolved / 365.25))
})
