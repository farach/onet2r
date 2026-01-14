test_that("onet_api_key returns error when not set", {
  # Temporarily unset the API key
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  
  Sys.unsetenv("ONET_API_KEY")
  
  expect_error(onet_api_key(), "O\\*NET API key not found")
})

test_that("onet_api_key returns key when set", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  
  test_key <- "test-api-key-123"
  Sys.setenv(ONET_API_KEY = test_key)
  
  expect_equal(onet_api_key(), test_key)
})

test_that("has_onet_api_key works correctly", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  
  # Test when key is not set
  Sys.unsetenv("ONET_API_KEY")
  expect_false(has_onet_api_key())
  
  # Test when key is set
  Sys.setenv(ONET_API_KEY = "test-key")
  expect_true(has_onet_api_key())
})
