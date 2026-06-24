test_that("onet_weighted_summary computes employment and wage weighted output", {
  skills <- tibble::tibble(
    code = c("15-1252.00", "15-1252.00", "29-1141.00", "29-1141.00"),
    element_id = c("2.A.1.a", "2.A.1.b", "2.A.1.a", "2.A.1.b"),
    element_name = c(
      "Reading Comprehension",
      "Active Listening",
      "Reading Comprehension",
      "Active Listening"
    ),
    data_value = c(4, 3, 2, 5)
  )
  oews <- tibble::tibble(
    occ_code = c("15-1252", "29-1141"),
    tot_emp = c(10, 30),
    a_median = c(100, 200)
  )

  result <- onet_weighted_summary(
    skills,
    group = c("element_id", "element_name"),
    value = "data_value",
    oews = oews
  )

  expect_named(
    result,
    c(
      "element_id",
      "element_name",
      "n_records",
      "n_occupations",
      "total_weight",
      "weighted_mean",
      "wage_weighted_mean"
    )
  )
  expect_equal(nrow(result), 2)
  expect_equal(result$n_occupations, c(2L, 2L))
  expect_equal(result$total_weight, c(40, 40))

  reading <- result[result$element_id == "2.A.1.a", ]
  listening <- result[result$element_id == "2.A.1.b", ]

  expect_equal(reading$weighted_mean, 2.5)
  expect_equal(listening$weighted_mean, 4.5)
  expect_equal(reading$wage_weighted_mean, 16 / 7)
  expect_equal(listening$wage_weighted_mean, 33 / 7)
})

test_that("onet_weighted_summary works with pre-joined weights", {
  data <- tibble::tibble(
    code = c("15-1252.00", "29-1141.00"),
    task = "Analyze data",
    value = c(5, 3),
    tot_emp = c(20, 60)
  )

  result <- onet_weighted_summary(
    data,
    group = "task",
    value = "value"
  )

  expect_equal(result$weighted_mean, 3.5)
  expect_equal(result$wage_weighted_mean, NA_real_)
})

test_that("onet_weighted_summary requires weights or OEWS data", {
  data <- tibble::tibble(
    code = "15-1252.00",
    task = "Analyze data",
    value = 5
  )

  expect_error(
    onet_weighted_summary(data, group = "task", value = "value"),
    "does not contain the weight column"
  )
})
