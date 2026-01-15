test_that("empty_tibble creates correct schema", {
  # Test with no arguments
  empty <- onet2r:::empty_tibble()
  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0)
  expect_equal(ncol(empty), 0)
  
  # Test with character columns
  schema <- onet2r:::empty_tibble(
    code = character(),
    title = character()
  )
  expect_s3_class(schema, "tbl_df")
  expect_equal(nrow(schema), 0)
  expect_equal(ncol(schema), 2)
  expect_equal(names(schema), c("code", "title"))
  expect_type(schema$code, "character")
  expect_type(schema$title, "character")
  
  # Test with mixed types
  schema <- onet2r:::empty_tibble(
    id = character(),
    value = numeric(),
    flag = logical()
  )
  expect_equal(ncol(schema), 3)
  expect_type(schema$id, "character")
  expect_type(schema$value, "double")
  expect_type(schema$flag, "logical")
})

test_that("to_snake_case converts correctly", {
  expect_equal(onet2r:::to_snake_case("CamelCase"), "camel_case")
  expect_equal(onet2r:::to_snake_case("HTTPResponse"), "http_response")
  expect_equal(onet2r:::to_snake_case("alreadyLowercase"), "already_lowercase")
  expect_equal(onet2r:::to_snake_case("API"), "api")
  expect_equal(onet2r:::to_snake_case("snake_case"), "snake_case")
  
  # Test vector conversion
  input <- c("FirstName", "LastName", "EmailAddress")
  expected <- c("first_name", "last_name", "email_address")
  expect_equal(onet2r:::to_snake_case(input), expected)
})

test_that("as_onet_tibble handles empty input", {
  result <- onet2r:::as_onet_tibble(list())
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("as_onet_tibble converts column names", {
  input <- list(
    list(FirstName = "John", LastName = "Doe"),
    list(FirstName = "Jane", LastName = "Smith")
  )
  
  # Convert single item
  result <- onet2r:::as_onet_tibble(input[[1]])
  expect_equal(names(result), c("first_name", "last_name"))
  expect_equal(result$first_name, "John")
})

test_that("as_onet_tibble handles NULL columns", {
  # Test with NULL values in list - simulates API returning NULL for a field
  input <- list(
    code = "15-1252.00",
    title = "Software Developer",
    not_relevant = NULL,
    scale_id = "IM"
  )
  
  # Should convert NULL to NA without error
  result <- onet2r:::as_onet_tibble(input)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$code, "15-1252.00")
  expect_equal(result$title, "Software Developer")
  expect_true(is.na(result$not_relevant))
  expect_equal(result$scale_id, "IM")
})

test_that("extract_list_data handles empty results", {
  # Test with NULL data
  resp_null <- list(occupation = NULL)
  schema <- onet2r:::empty_tibble(code = character(), title = character())
  result <- onet2r:::extract_list_data(resp_null, "occupation", schema = schema)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("code", "title"))
  
  # Test with empty list
  resp_empty <- list(occupation = list())
  result <- onet2r:::extract_list_data(resp_empty, "occupation", schema = schema)
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("code", "title"))
  
  # Test without schema
  result <- onet2r:::extract_list_data(resp_null, "occupation")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("validate_onet_code validates format correctly", {
  # Valid codes
  expect_invisible(onet2r:::validate_onet_code("15-1252.00"))
  expect_invisible(onet2r:::validate_onet_code("15-1252"))
  expect_invisible(onet2r:::validate_onet_code("29-1141.00"))
  
  # Invalid codes
  expect_error(
    onet2r:::validate_onet_code("invalid"),
    "Invalid O\\*NET-SOC code format"
  )
  expect_error(
    onet2r:::validate_onet_code("15-12"),
    "Invalid O\\*NET-SOC code format"
  )
  expect_error(
    onet2r:::validate_onet_code("15-1252.0"),
    "Invalid O\\*NET-SOC code format"
  )
  
  # Non-string input
  expect_error(
    onet2r:::validate_onet_code(123),
    "must be a single character string"
  )
  expect_error(
    onet2r:::validate_onet_code(c("15-1252.00", "15-1251.00")),
    "must be a single character string"
  )
})
