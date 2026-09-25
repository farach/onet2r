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

combination_weight_panel <- function(codes) {
  tibble::tibble(
    reference_soc_code = codes,
    year = 2024L,
    employment = rep(100, length(codes)),
    weight_share = rep(1 / length(codes), length(codes)),
    source = "OEWS",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
}

test_that("onet_oews_bridge maps occupations into OEWS combination codes", {
  # Codes follow the BLS OEWS combinations 31-1120, 25-9045, and 53-1047.
  weights <- combination_weight_panel(c(
    "25-9044", "25-9045", "29-1141", "31-1120", "53-1041", "53-1047"
  ))
  codes <- c(
    "31-1121.00", "31-1122.00", "25-9042.00", "25-9043.00", "25-9044.00",
    "25-9049.00", "29-1141.00", "29-1141.01", "53-1041.00", "53-1042.00",
    "55-1011.00"
  )

  expect_message(
    bridge <- onet_oews_bridge(codes, weights),
    "into 3 OEWS combination codes"
  )

  expect_s3_class(bridge, "tbl_df")
  expect_named(
    bridge,
    c(
      "from_onet_soc_code", "from_soc_code", "reference_soc_code", "map_type",
      "crosswalk_weight", "crosswalk_path"
    )
  )
  expect_equal(bridge$from_onet_soc_code, sort(codes))
  lookup <- stats::setNames(bridge$reference_soc_code, bridge$from_onet_soc_code)
  types <- stats::setNames(bridge$map_type, bridge$from_onet_soc_code)
  expect_equal(unname(lookup[c("31-1121.00", "31-1122.00")]), rep("31-1120", 2))
  expect_equal(
    unname(lookup[c("25-9042.00", "25-9043.00", "25-9049.00")]),
    rep("25-9045", 3)
  )
  expect_equal(unname(lookup["53-1042.00"]), "53-1047")
  expect_equal(unname(types["53-1042.00"]), "oews_combination")
  expect_equal(unname(lookup["25-9044.00"]), "25-9044")
  expect_equal(unname(lookup["53-1041.00"]), "53-1041")
  expect_equal(unname(lookup["29-1141.01"]), "29-1141")
  expect_equal(unname(types[c("25-9044.00", "29-1141.00", "29-1141.01")]), rep("direct", 3))
  expect_equal(unname(types["55-1011.00"]), "not_in_panel")
  expect_equal(unname(lookup["55-1011.00"]), "55-1011")
  expect_equal(bridge$crosswalk_weight, rep(1, length(codes)))
  expect_equal(
    unique(bridge$crosswalk_path),
    "O*NET-SOC -> 2018 SOC with OEWS combinations"
  )
})

test_that("onet_oews_bridge does not merge occupations a panel omits into a combined code", {
  # A state-style panel: 53-1041, 25-9044, and 21-1013 are published nationally
  # but suppressed here, next to the combined codes 53-1047, 25-9045, and 21-1018.
  weights <- combination_weight_panel(c("53-1047", "25-9045", "21-1018", "21-1019"))
  codes <- c(
    "53-1041.00", "53-1042.00", "25-9044.00", "25-9042.00",
    "21-1013.00", "21-1014.00", "21-1011.00"
  )

  expect_no_warning(
    expect_message(
      bridge <- onet_oews_bridge(codes, weights),
      "Mapped 4 O\\*NET occupations into 3 OEWS combination codes"
    )
  )

  lookup <- stats::setNames(bridge$reference_soc_code, bridge$from_onet_soc_code)
  types <- stats::setNames(bridge$map_type, bridge$from_onet_soc_code)
  expect_equal(
    unname(types[c("53-1041.00", "25-9044.00", "21-1013.00")]),
    rep("not_in_panel", 3)
  )
  expect_equal(
    unname(lookup[c("53-1041.00", "25-9044.00", "21-1013.00")]),
    c("53-1041", "25-9044", "21-1013")
  )
  expect_equal(
    unname(lookup[c("53-1042.00", "25-9042.00", "21-1011.00", "21-1014.00")]),
    c("53-1047", "25-9045", "21-1018", "21-1018")
  )
})

test_that("onet_oews_bridge maps parts only when their combined code is in the panel", {
  weights <- combination_weight_panel(c("29-1141", "31-1131"))

  expect_no_message(bridge <- onet_oews_bridge(c("31-1121.00", "29-1141.00"), weights))

  expect_equal(bridge$map_type, c("direct", "not_in_panel"))
  expect_equal(bridge$reference_soc_code, c("29-1141", "31-1121"))
})

test_that("onet_oews_bridge rejects panels from before May 2021", {
  hybrid <- combination_weight_panel(c("29-1228", "29-1248"))
  hybrid$year <- 2020L
  stacked <- rbind(combination_weight_panel("31-1120"), hybrid)
  missing_year <- combination_weight_panel("31-1120")
  missing_year$year <- NA_integer_

  expect_error(onet_oews_bridge("29-1241.00", hybrid), "from 2021 on")
  expect_error(onet_oews_bridge("31-1121.00", stacked), "2020")
  expect_error(onet_oews_bridge("31-1121.00", missing_year), "NA")
  expect_error(
    onet_oews_bridge("31-1121.00", combination_weight_panel(character())),
    "no rows"
  )
})

test_that("onet_oews_bridge uses the BLS May 2021 combination definitions", {
  parts <- onet2r:::oews_combination_parts()

  expect_equal(
    sort(unique(parts$reference_soc_code)),
    c(
      "13-1020", "13-2020", "21-1018", "25-2052", "25-9045", "29-2010",
      "31-1120", "39-7010", "47-4090", "51-2028", "51-2090", "53-1047"
    )
  )
  expect_equal(anyDuplicated(parts$part_soc_code), 0L)
  expect_true(all(grepl("^\\d{2}-\\d{4}$", parts$part_soc_code)))
  expect_false(any(parts$part_soc_code %in% parts$reference_soc_code))
})

test_that("onet_oews_bridge accepts data frames and occupation measures", {
  weights <- combination_weight_panel(c("29-1141", "31-1120"))
  measure <- onet_measure(
    tibble::tibble(onet_soc_code = c("31-1121.00", "29-1141.00"), score = c(1, 2)),
    key = "onet_soc_code",
    score = "score",
    measure_id = "stylized_bridge_input"
  )

  from_measure <- suppressMessages(onet_oews_bridge(measure, weights))
  from_frame <- suppressMessages(onet_oews_bridge(
    tibble::tibble(code = c("311121", "29-1141.00", NA)),
    weights,
    occupation_code = "code"
  ))

  expect_equal(from_measure, from_frame)
  expect_equal(from_measure$reference_soc_code, c("29-1141", "31-1120"))

  empty <- onet_oews_bridge(character(), weights)
  expect_equal(nrow(empty), 0L)
  expect_named(
    empty,
    c(
      "from_onet_soc_code", "from_soc_code", "reference_soc_code", "map_type",
      "crosswalk_weight", "crosswalk_path"
    )
  )
  expect_type(empty$map_type, "character")
  expect_type(empty$crosswalk_weight, "double")
})

test_that("onet_oews_bridge validates its inputs", {
  weights <- combination_weight_panel(c("29-1141", "31-1120"))
  task_measure <- onet_measure(
    tibble::tibble(task_id = "1", score = 1),
    key = "task_id",
    score = "score",
    key_type = "task"
  )

  expect_error(onet_oews_bridge(1:3, weights), "character vector")
  expect_error(onet_oews_bridge(tibble::tibble(x = "a"), weights), "occupation_code")
  expect_error(onet_oews_bridge(task_measure, weights), "occupation-level")
  expect_error(onet_oews_bridge("31-1121.00", tibble::tibble(x = 1)), "reference_soc_code")
  expect_error(
    onet_oews_bridge("31-1121.00", tibble::tibble(reference_soc_code = "31-1120")),
    "year"
  )
})

test_that("onet_oews_bridge closes OEWS combination gaps in aggregates", {
  weights <- tibble::tibble(
    reference_soc_code = c("31-1120", "29-1141"),
    year = 2024L,
    employment = c(300, 100),
    weight_share = c(0.75, 0.25),
    source = "OEWS",
    source_taxonomy = "2018 SOC",
    reference_taxonomy = "2018 SOC"
  )
  scores <- tibble::tibble(
    onet_soc_code = c("31-1121.00", "31-1122.00", "29-1141.00"),
    measure_score = c(0.2, 0.6, 0.9)
  )
  bridge <- suppressMessages(onet_oews_bridge(scores, weights))

  unbridged <- suppressMessages(
    onet_measure_aggregate(scores, weights, measure_id = "stylized_gap")
  )
  bridged <- onet_measure_aggregate(
    scores,
    weights,
    bridge = bridge,
    measure_id = "stylized_gap"
  )

  expect_equal(unbridged$employment_coverage_share, 0.25)
  expect_equal(bridged$employment_coverage_share, 1)
  expect_equal(bridged$aggregate, 0.75 * 0.4 + 0.25 * 0.9)
  expect_equal(bridged$n_occupations, 3L)
  expect_equal(bridged$n_reference_soc, 2L)
  expect_equal(
    onet_provenance(bridged)$crosswalk_path,
    "O*NET-SOC -> 2018 SOC with OEWS combinations"
  )
  expect_identical(onet_provenance(bridged)$bridge_used, TRUE)
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
