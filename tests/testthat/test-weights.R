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

test_that("onet_weight_panel_oews excludes OEWS hierarchy rows", {
  oews <- tibble::tibble(
    occ_code = c("00-0000", "15-0000", "15-1252", "29-1141"),
    o_group = c("total", "major", "detailed", "detailed"),
    tot_emp = c(400, 100, 100, 300)
  )

  result <- suppressMessages(onet_weight_panel_oews(oews, year = 2024))

  expect_equal(result$reference_soc_code, c("15-1252", "29-1141"))
  expect_equal(result$employment, c(100, 300))
  expect_equal(result$weight_share, c(0.25, 0.75))
})

test_that("onet_weight_panel_pums aggregates weights and replicate SEs", {
  pums <- tibble::tibble(
    SOCP = c("151252", "151252", "291141"),
    PWGTP = c(60, 40, 300),
    sex = c("F", "M", "F")
  )
  replicate_data <- as.data.frame(
    matrix(rep(pums$PWGTP, 80), nrow = nrow(pums), ncol = 80)
  )
  names(replicate_data) <- paste0("PWGTP", seq_len(80))
  pums <- dplyr::bind_cols(pums, replicate_data)

  result <- onet_weight_panel_pums(
    pums,
    year = 2022,
    group = "sex",
    replicate_weights = names(replicate_data)
  )

  expect_equal(result$reference_soc_code, c("15-1252", "15-1252", "29-1141"))
  expect_equal(result$employment, c(60, 40, 300))
  expect_equal(result$employment_se, c(0, 0, 0))
  expect_equal(unique(result$source), "PUMS")
})

test_that("onet_weight_panel_pums drops ACS SOCP aggregate codes", {
  pums <- tibble::tibble(
    SOCP = c("1191XX", "291141"),
    PWGTP = c(500, 300)
  )

  result <- suppressWarnings(onet_weight_panel_pums(pums, year = 2022))

  expect_equal(result$reference_soc_code, "29-1141")
  expect_equal(result$employment, 300)
  expect_equal(result$weight_share, 1)
})
