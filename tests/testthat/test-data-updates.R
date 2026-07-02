test_that("parse_data_updates normalizes observed update sheets", {
  raw <- list(
    `Incumbent or OE Updates` = tibble::tibble(
      `O*NET-SOC Code` = c("11101100", "15-1252.00"),
      `O*NET-SOC Title` = c("Chief Executives", "Software Developers"),
      `Number of Updates (as of 08/2025)` = c("3", "2"),
      `Current Date` = c("08/2023", "08/2024"),
      `Current Database` = c("28.0", "29.0"),
      `O*NET-SOC Code used in Current Database` = c("11-1011.00", "15125200")
    ),
    `Skills Analyst Updates` = tibble::tibble(
      `O*NET-SOC Code` = "29-1141.00",
      `O*NET-SOC Title` = "Registered Nurses",
      `Number of Updates (as of 08/2025)` = "1",
      `Current Date` = "08/2025",
      `Current Database` = "30.0",
      `O*NET-SOC Code used in Current Database` = "29-1141.00"
    )
  )

  out <- onet2r:::parse_data_updates(raw)

  expect_s3_class(out, "tbl_df")
  expect_named(
    out,
    c(
      "data_update_type",
      "onet_soc_code",
      "title",
      "number_of_updates_as_of_08_2025",
      "current_date",
      "current_database",
      "onet_soc_code_used_in_current_database"
    )
  )
  expect_equal(out$onet_soc_code, c("11-1011.00", "15-1252.00", "29-1141.00"))
  expect_equal(
    out$data_update_type,
    c("Incumbent or OE Updates", "Incumbent or OE Updates", "Skills Analyst Updates")
  )
})
