test_that("validate_onet_code accepts valid codes", {
  # Should not error on valid codes
  expect_silent(validate_onet_code("15-1252.00"))
  expect_silent(validate_onet_code("11-1011.00"))
  expect_silent(validate_onet_code("15-1252"))
})

test_that("validate_onet_code rejects invalid codes", {
  expect_error(validate_onet_code("invalid"), "Invalid O\\*NET-SOC code format")
  expect_error(validate_onet_code("15-12"), "Invalid O\\*NET-SOC code format")
  expect_error(validate_onet_code("1512"), "Invalid O\\*NET-SOC code format")
  expect_error(validate_onet_code(c("15-1252.00", "11-1011.00")), "must be a single character string")
  expect_error(validate_onet_code(123), "must be a single character string")
})
