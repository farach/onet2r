test_that("onet_pums_employment_weights aggregates weighted SOC counts", {
  pums <- tibble::tibble(
    SOCP = c("151252", "15-1252", "291141", NA),
    PWGTP = c(120, 80, 200, 999)
  )

  expect_warning(
    result <- onet_pums_employment_weights(pums),
    class = "lifecycle_warning_deprecated"
  )

  expect_named(result, c("soc_code", "employment", "records"))
  expect_equal(result$soc_code, c("15-1252", "29-1141"))
  expect_equal(result$employment, c(200, 200))
  expect_equal(result$records, c(2L, 1L))
})

test_that("onet_pums_employment_weights can count rows without weights", {
  pums <- tibble::tibble(SOCP = c("151252", "151252", "291141"))

  expect_warning(
    result <- onet_pums_employment_weights(pums, weight = NULL),
    class = "lifecycle_warning_deprecated"
  )

  expect_equal(result$soc_code, c("15-1252", "29-1141"))
  expect_equal(result$employment, c(2, 1))
})
