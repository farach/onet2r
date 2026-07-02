test_that("onet_occupation_details returns a tibble of sections", {
  local_mocked_bindings(
    onet_occupation = function(code) {
      list(
        code = code,
        title = "Software Developers",
        details_contents = list(
          list(
            href = "https://api-v2.onetcenter.org/online/occupations/15-1252.00/details/tasks",
            title = "Tasks"
          ),
          list(
            href = "https://api-v2.onetcenter.org/online/occupations/15-1252.00/details/skills",
            title = "Skills"
          )
        )
      )
    },
    .package = "onet2r"
  )

  out <- onet_occupation_details("15-1252.00")

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("title", "href"))
  expect_equal(out$title, c("Tasks", "Skills"))
})

test_that("onet_occupation_details returns a stable empty schema", {
  local_mocked_bindings(
    onet_occupation = function(code) list(code = code, title = "X"),
    .package = "onet2r"
  )

  out <- onet_occupation_details("15-1252.00")

  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("title", "href"))
  expect_equal(nrow(out), 0L)
})
