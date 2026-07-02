test_that("skills parse a realistic response body end to end", {
  body <- jsonlite::fromJSON(
    test_path("fixtures", "skills_body.json"),
    simplifyVector = FALSE
  )
  local_mocked_bindings(
    onet_perform = function(req) body,
    .package = "onet2r"
  )

  out <- onet_skills("15-1252.00")

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("id", "related", "name", "description", "importance"))
  expect_equal(out$importance, c(75, 72))
})
