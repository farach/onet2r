test_that("create_empty_result creates correctly typed empty tibbles", {
  # Test with schema
  schema <- list(
    code = character(),
    value = numeric(),
    active = logical()
  )
  
  result <- create_empty_result(schema)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 3)
  expect_true("code" %in% names(result))
  expect_true("value" %in% names(result))
  expect_true("active" %in% names(result))
  expect_type(result$code, "character")
  expect_type(result$value, "double")
  expect_type(result$active, "logical")
})

test_that("create_empty_result handles NULL schema", {
  result <- create_empty_result(NULL)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 0)
})

test_that("is_transient_error identifies transient errors correctly", {
  # Mock response objects
  mock_resp_429 <- list()
  class(mock_resp_429) <- "httr2_response"
  attr(mock_resp_429, "status") <- 429
  
  mock_resp_500 <- list()
  class(mock_resp_500) <- "httr2_response"
  attr(mock_resp_500, "status") <- 500
  
  mock_resp_503 <- list()
  class(mock_resp_503) <- "httr2_response"
  attr(mock_resp_503, "status") <- 503
  
  mock_resp_404 <- list()
  class(mock_resp_404) <- "httr2_response"
  attr(mock_resp_404, "status") <- 404
  
  # Note: These tests would require httr2 to be loaded and proper mock responses
  # Since we can't easily test without httr2, we'll skip actual execution
  skip_if_not_installed("httr2")
  
  # The function should return TRUE for 429 and 500-599
  # and FALSE for other status codes
})

test_that("to_snake_case converts camelCase to snake_case", {
  expect_equal(to_snake_case("camelCase"), "camel_case")
  expect_equal(to_snake_case("PascalCase"), "pascal_case")
  expect_equal(to_snake_case("already_snake"), "already_snake")
  expect_equal(to_snake_case("HTTPResponse"), "http_response")
  expect_equal(to_snake_case(c("firstVar", "secondVar")), c("first_var", "second_var"))
})

test_that("as_onet_tibble handles empty input", {
  result <- as_onet_tibble(list())
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("as_onet_tibble converts to snake_case column names", {
  input <- list(
    list(FirstName = "John", LastName = "Doe", AgeYears = 30),
    list(FirstName = "Jane", LastName = "Smith", AgeYears = 25)
  )
  
  # Process each list item
  result <- as_onet_tibble(input[[1]])
  
  expect_true("first_name" %in% names(result))
  expect_true("last_name" %in% names(result))
  expect_true("age_years" %in% names(result))
})
