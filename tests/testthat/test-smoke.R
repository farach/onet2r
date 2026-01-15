test_that("about endpoint works", {
  skip_if_not(nzchar(Sys.getenv("ONET_API_KEY")))
  x <- onet2r:::onet_perform(onet2r:::onet_request("about"))
  expect_true(is.list(x))
  expect_true(!is.null(x$api_version))
})

test_that("occupation endpoint works", {
  skip_if_not(nzchar(Sys.getenv("ONET_API_KEY")))
  x <- onet_occupation("15-1252.00")
  expect_identical(x$code, "15-1252.00")
})
