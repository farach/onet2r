test_that("onet_crosswalk_military validates result ranges before requests", {
  local_mocked_bindings(
    onet_request = function(...) {
      stop("request should not be reached", call. = FALSE)
    }
  )

  expect_error(
    onet_crosswalk_military("11B", start = 0),
    "start.*positive number"
  )
  expect_error(
    onet_crosswalk_military("11B", start = 2, end = 1),
    "greater than or equal"
  )
})
