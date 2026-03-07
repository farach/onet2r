test_that("onet_occupations returns page data", {
  local_mocked_bindings(
    onet_occupations_page = function(start = 1, end = 1000) {
      expect_equal(start, 1)
      expect_equal(end, 1000)

      list(
        data = tibble::tibble(
          code = "15-1252.00",
          title = "Software Developers"
        ),
        start = start,
        end = end,
        total = 1
      )
    },
    .package = "onet2r"
  )

  result <- onet_occupations()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1)
  expect_equal(result$code, "15-1252.00")
})

test_that("onet_occupations_all paginates across pages", {
  local_mocked_bindings(
    onet_occupations_page = function(start = 1, end = 1000) {
      if (start == 1) {
        return(list(
          data = tibble::tibble(
            code = c("11-1011.00", "15-1252.00"),
            title = c("Chief Executives", "Software Developers")
          ),
          start = 1,
          end = 2,
          total = 4
        ))
      }

      list(
        data = tibble::tibble(
          code = c("19-1042.00", "29-1141.00"),
          title = c("Medical Scientists", "Registered Nurses")
        ),
        start = 3,
        end = 4,
        total = 4
      )
    },
    .package = "onet2r"
  )

  result <- onet_occupations_all(page_size = 2, show_progress = FALSE)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 4)
  expect_equal(
    result$code,
    c("11-1011.00", "15-1252.00", "19-1042.00", "29-1141.00")
  )
})

test_that("all-page detail helpers paginate the expected sections", {
  local_mocked_bindings(
    onet_details_element_page = function(code, section, start = 1, end = 20) {
      expect_equal(code, "15-1252.00")
      expect_true(section %in% c("skills", "work_context", "work_activities"))

      if (start == 1) {
        return(list(
          data = tibble::tibble(
            element = c("first", "second"),
            value = c(1, 2)
          ),
          start = 1,
          end = 2,
          total = 4
        ))
      }

      list(
        data = tibble::tibble(
          element = c("third", "fourth"),
          value = c(3, 4)
        ),
        start = 3,
        end = 4,
        total = 4
      )
    },
    .package = "onet2r"
  )

  skills <- onet_skills_all("15-1252.00", page_size = 2, show_progress = FALSE)
  work_context <- onet_work_context_all("15-1252.00", page_size = 2, show_progress = FALSE)
  work_activities <- onet_work_activities_all("15-1252.00", page_size = 2, show_progress = FALSE)

  expect_equal(nrow(skills), 4)
  expect_equal(nrow(work_context), 4)
  expect_equal(nrow(work_activities), 4)
})

test_that("all-page helpers validate page size", {
  expect_error(
    onet_occupations_all(page_size = 0),
    "must be between 1 and 2000"
  )
  expect_error(
    onet_skills_all("15-1252.00", page_size = 2001),
    "must be between 1 and 2000"
  )
  expect_error(
    onet_work_context_all("15-1252.00", page_size = 2001),
    "must be between 1 and 2000"
  )
  expect_error(
    onet_work_activities_all("15-1252.00", page_size = 2001),
    "must be between 1 and 2000"
  )
})
