test_that("onet_request builds URL correctly with path only", {
  # This test verifies that onet_request can handle path-only endpoints
  # without query parameters (which was causing errors before)
  
  # Mock the API key
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  Sys.setenv(ONET_API_KEY = "test-key")
  
  # Build a request with only path
  req <- onet2r:::onet_request("database")
  
  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "https://api-v2.onetcenter.org/database")
})

test_that("onet_request builds URL correctly with path segments", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  Sys.setenv(ONET_API_KEY = "test-key")
  
  # Build a request with path segments
  req <- onet2r:::onet_request("online/occupations", .path_segments = c("15-1252.00", "summary"))
  
  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "https://api-v2.onetcenter.org/online/occupations/15-1252.00/summary")
})

test_that("onet_request builds URL correctly with query parameters", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  Sys.setenv(ONET_API_KEY = "test-key")
  
  # Build a request with query parameters
  req <- onet2r:::onet_request("online/search", .query = list(keyword = "software", start = 1, end = 20))
  
  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "https://api-v2.onetcenter.org/online/search")
  # The query params should be in the URL
  expect_match(req$url, "keyword=software")
  expect_match(req$url, "start=1")
  expect_match(req$url, "end=20")
})

test_that("onet_request builds URL correctly with both path segments and query parameters", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  Sys.setenv(ONET_API_KEY = "test-key")
  
  # Build a request with both path segments and query parameters
  req <- onet2r:::onet_request("database/rows", .path_segments = "skills", .query = list(start = 1, end = 100))
  
  expect_s3_class(req, "httr2_request")
  expect_match(req$url, "https://api-v2.onetcenter.org/database/rows/skills")
  expect_match(req$url, "start=1")
  expect_match(req$url, "end=100")
})

test_that("onet_request includes API key header", {
  old_key <- Sys.getenv("ONET_API_KEY")
  on.exit(Sys.setenv(ONET_API_KEY = old_key))
  test_key <- "test-api-key-123"
  Sys.setenv(ONET_API_KEY = test_key)
  
  req <- onet2r:::onet_request("database")
  
  expect_equal(req$headers[["X-API-Key"]], test_key)
})
