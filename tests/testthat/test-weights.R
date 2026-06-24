test_that("onet_reference_soc_resolve maps source codes through a crosswalk", {
  data <- tibble::tibble(code = c("151252", "151252", "291141"))
  crosswalk <- tibble::tibble(
    source_code = c("151252", "151252", "291141"),
    reference_soc_code = c("15-1252", "15-1253", "29-1141"),
    crosswalk_weight = c(0.75, 0.25, 1),
    map_type = c("split", "split", "one_to_one")
  )

  result <- onet_reference_soc_resolve(
    data,
    code = "code",
    source_taxonomy = "2010 SOC",
    reference_taxonomy = "2018 SOC",
    source_year = 2017,
    crosswalk = crosswalk
  )

  expect_equal(result$reference_soc_code, c("15-1252", "15-1253", "29-1141"))
  expect_equal(result$crosswalk_weight, c(0.75, 0.25, 1))
  expect_equal(result$source_year, c(2017L, 2017L, 2017L))
})

test_that("onet_weight_panel_oews creates reference-SOC weights", {
  oews <- tibble::tibble(
    occ_code = c("15-1252", "29-1141"),
    tot_emp = c("100", "300")
  )

  result <- onet_weight_panel_oews(oews, year = 2024)

  expect_equal(result$reference_soc_code, c("15-1252", "29-1141"))
  expect_equal(result$employment, c(100, 300))
  expect_equal(result$weight_share, c(0.25, 0.75))
  expect_equal(result$source, c("OEWS", "OEWS"))
})

test_that("onet_weight_panel_pums aggregates weights and replicate SEs", {
  pums <- tibble::tibble(
    SOCP = c("151252", "151252", "291141"),
    PWGTP = c(60, 40, 300),
    sex = c("F", "M", "F"),
    PWGTP1 = c(62, 38, 310),
    PWGTP2 = c(58, 42, 290)
  )

  result <- onet_weight_panel_pums(
    pums,
    year = 2022,
    group = "sex",
    replicate_weights = c("PWGTP1", "PWGTP2")
  )

  expect_equal(result$reference_soc_code, c("15-1252", "15-1252", "29-1141"))
  expect_equal(result$employment, c(60, 40, 300))
  expect_true(all(result$employment_se >= 0))
  expect_equal(unique(result$source), "PUMS")
})
