test_that("onet_table_info parses live column metadata shape", {
  local_mocked_bindings(
    onet_request = function(...) {
      list()
    },
    onet_perform = function(req) {
      list(
        column = list(
          list(
            column_id = "onetsoc_code",
            title = "O*NET-SOC Code",
            type = "Text",
            format = "Character(10)",
            optional = FALSE,
            description = "O*NET-SOC Code"
          )
        )
      )
    },
    .package = "onet2r"
  )

  result <- onet_table_info("occupation_data")

  expect_named(
    result,
    c("name", "title", "type", "format", "optional", "description")
  )
  expect_equal(result$name, "onetsoc_code")
  expect_equal(result$title, "O*NET-SOC Code")
  expect_equal(result$optional, FALSE)
})
