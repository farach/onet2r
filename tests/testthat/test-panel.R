tiny_archive_zip <- function() {
  tmp <- tempfile("onet-archive-fixture-")
  dir.create(tmp)
  withr::defer(unlink(tmp, recursive = TRUE), testthat::teardown_env())
  source_dir <- file.path(tmp, "db_30_3_text")
  dir.create(source_dir)

  write.table(
    tibble::tibble(
      `O*NET-SOC Code` = c("15-1252.00", "29-1141.00"),
      Title = c("Software Developers", "Registered Nurses"),
      `Element ID` = c("1.A.1.a.1", "1.A.1.a.1"),
      `Element Name` = c("Oral Comprehension", "Oral Comprehension"),
      Scale = c("IM", "IM"),
      `Data Value` = c(4.12, 4.71),
      `N` = c(26L, 29L),
      `Standard Error` = c(0.15, 0.11),
      `Lower CI Bound` = c(3.82, 4.49),
      `Upper CI Bound` = c(4.42, 4.93),
      `Recommend Suppress` = c("N", "N"),
      Date = c("07/2025", "08/2025"),
      `Domain Source` = c("Analyst", "Incumbent")
    ),
    file = file.path(source_dir, "Abilities.txt"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  old_dir <- setwd(tmp)
  withr::defer(setwd(old_dir), testthat::teardown_env())
  zipfile <- file.path(tmp, "db_30_3_text.zip")
  utils::zip(
    zipfile,
    files = file.path("db_30_3_text", "Abilities.txt"),
    flags = "-q"
  )
  zipfile
}

test_that("onet_releases parses archive rows into stable fields", {
  html <- paste(
    '<a href="/dl_files/database/db_30_3_text.zip">30.3 text</a>',
    '<a href="/dictionary/30.3/excel/">30.3 dictionary</a>',
    "<td>O*NET 30.3 Database</td><td>May 2026</td>",
    '<a href="/dl_files/database/db_30_2_text.zip">30.2 text</a>',
    '<a href="/dictionary/30.2/excel/">30.2 dictionary</a>',
    "<td>O*NET 30.2 Database</td><td>February 2026</td>",
    '<a href="/dl_files/db_15_1.zip">15.1 text</a>',
    "<td>O*NET 15.1 Database</td><td>July 2010</td>"
  )

  local_mocked_bindings(
    onet_read_lines = function(url) html,
    .package = "onet2r"
  )

  result <- onet_releases()

  expect_named(
    result,
    c(
      "version", "release_date", "year", "month", "soc_vintage",
      "text_url", "dictionary_url"
    )
  )
  expect_equal(result$version, c("30.3", "30.2", "15.1"))
  expect_equal(
    result$release_date,
    as.Date(c("2026-05-01", "2026-02-01", "2010-07-01"))
  )
  expect_equal(as.character(result$soc_vintage), c("2019", "2019", "2010"))
  expect_match(result$text_url[1], "db_30_3_text\\.zip$")
  expect_match(result$text_url[3], "db_15_1\\.zip$")
  expect_match(result$dictionary_url[1], "/dictionary/30.3/excel/", fixed = TRUE)
})

test_that("onet_archive_read normalizes descriptor archive tables", {
  zipfile <- tiny_archive_zip()

  local_mocked_bindings(
    onet_archive_download = function(version, dir = onet_cache_dir(), force = FALSE) {
      expect_equal(version, "30.3")
      zipfile
    },
    onet_releases = function() {
      tibble::tibble(
        version = "30.3",
        release_date = as.Date("2026-05-01"),
        year = 2026L,
        month = "May",
        soc_vintage = "2019",
        text_url = "https://www.onetcenter.org/dl_files/database/db_30_3_text.zip",
        dictionary_url = "https://www.onetcenter.org/dictionary/30.3/excel/"
      )
    },
    .package = "onet2r"
  )

  result <- onet_archive_read("30.3", "Abilities")

  expect_named(
    result,
    c(
      "release_version", "release_date", "soc_vintage", "domain",
      "onet_soc_code", "soc_code", "title", "task_id", "task", "task_type",
      "incumbents_responding", "dwa_element_id", "dwa_element_name",
      "iwa_element_id", "iwa_element_name", "gwa_element_id",
      "gwa_element_name", "element_id", "element_name", "scale_id",
      "scale_name", "category", "data_value", "n", "standard_error", "lower_ci_bound",
      "upper_ci_bound", "recommend_suppress", "not_relevant", "source_date",
      "domain_source"
    )
  )
  expect_equal(nrow(result), 2L)
  expect_equal(result$release_version, c("30.3", "30.3"))
  expect_equal(as.character(result$soc_vintage), c("2019", "2019"))
  expect_equal(result$domain, c("Abilities", "Abilities"))
  expect_equal(result$source_date, as.Date(c("2025-07-01", "2025-08-01")))
  expect_equal(result$data_value, c(4.12, 4.71))
})

test_that("onet_archive_read preserves task and DWA native fields", {
  archive_dir <- system.file(
    "extdata",
    "onet-mini",
    "db_30_3_text",
    package = "onet2r"
  )

  tasks <- onet_archive_read(
    "30.3",
    "Task Statements",
    path = archive_dir,
    release_date = "2026-05-01"
  )
  ratings <- onet_archive_read(
    "30.3",
    "Task Ratings",
    path = archive_dir,
    release_date = "2026-05-01"
  )
  dwa <- onet_archive_read(
    "30.3",
    "Tasks to DWAs",
    path = archive_dir,
    release_date = "2026-05-01"
  )

  expect_equal(tasks$task_id, c("1001", "1002", "2001"))
  expect_equal(tasks$task_type, c("Core", "Supplemental", "Core"))
  expect_equal(ratings$scale_id, factor(c("RT", "IM", "RT", "RT", "IM")))
  expect_equal(dwa$dwa_element_id[[1]], "4.A.2.a.1.a.1")
  expect_equal(dwa$element_name[[1]], "Analyze user needs and software requirements.")
})

test_that("onet_archive_read accepts a local extracted archive directory", {
  archive_dir <- system.file(
    "extdata",
    "onet-mini",
    "db_30_3_text",
    package = "onet2r"
  )

  result <- onet_archive_read(
    "30.3",
    "Abilities",
    path = archive_dir,
    release_date = "2026-05-01"
  )

  expect_equal(nrow(result), 7L)
  expect_equal(unique(result$release_version), "30.3")
  expect_equal(unique(result$release_date), as.Date("2026-05-01"))
  expect_equal(unique(result$not_relevant), "N")
  expect_equal(
    result[result$onet_soc_code == "15-1252.00", "data_value", drop = TRUE][[1]],
    4.35
  )
})

test_that("onet_panel accepts local archives by version", {
  archive_base <- system.file("extdata", "onet-mini", package = "onet2r")
  archives <- c(
    `30.2` = file.path(archive_base, "db_30_2_text"),
    `30.3` = file.path(archive_base, "db_30_3_text")
  )
  release_dates <- c(`30.2` = "2026-02-01", `30.3` = "2026-05-01")

  result <- onet_panel(
    "Abilities",
    versions = c("30.2", "30.3"),
    scale = "IM",
    archives = archives,
    release_dates = release_dates
  )

  expect_equal(nrow(result), 14L)
  expect_equal(unique(result$release_version), c("30.2", "30.3"))
  expect_equal(unique(result$release_date), as.Date(c("2026-02-01", "2026-05-01")))
})

test_that("onet_panel reads cross-vintage and task fixtures", {
  archive_base <- system.file("extdata", "onet-mini", package = "onet2r")
  archives <- c(
    `24.3` = file.path(archive_base, "db_24_3_text"),
    `25.1` = file.path(archive_base, "db_25_1_text")
  )
  release_dates <- c(`24.3` = "2020-08-01", `25.1` = "2020-11-01")

  abilities <- onet_panel(
    "Abilities",
    versions = c("24.3", "25.1"),
    scale = "IM",
    archives = archives,
    release_dates = release_dates
  )
  bridge <- tibble::tibble(
    from_vintage = "2010",
    to_vintage = "2019",
    from_onet_soc_code = c("15-1132.00", "15-1132.00", "29-1141.00"),
    to_onet_soc_code = c("15-1252.00", "15-1253.00", "29-1141.00"),
    map_type = c("split", "split", "one_to_one"),
    crosswalk_weight = c(0.5, 0.5, 1)
  )
  reconciled <- onet_panel_reconcile(abilities, bridge)
  tasks <- onet_panel(
    "Task Ratings",
    versions = c("24.3", "25.1"),
    scale = "RT",
    archives = archives,
    release_dates = release_dates
  )

  expect_equal(as.character(unique(abilities$soc_vintage)), c("2010", "2019"))
  expect_true(any(reconciled$transition_data))
  expect_true(any(reconciled$crosswalk_uncertain))
  expect_false(any(reconciled$safely_comparable[reconciled$transition_data]))
  expect_equal(nrow(tasks), 5L)
  expect_equal(unique(as.character(tasks$scale_id)), "RT")
  expect_true(all(as.character(tasks$scale_id) == "RT"))
  expect_true(all(tasks$scale_name == "Relevance of Task"))
})

test_that("onet_crosswalk_bridge classifies split and merge mappings", {
  local_mocked_bindings(
    read_adjacent_crosswalk = function(from, to) {
      expect_equal(from, "2010")
      expect_equal(to, "2019")
      data <- tibble::tibble(
        from_vintage = "2010",
        to_vintage = "2019",
        from_soc_code = c("11-1011", "15-1252", "15-1252", "29-1141"),
        from_title = c("Chief Executives", "Software Devs", "Software Devs", "RNs"),
        to_soc_code = c("11-1011", "15-1252", "15-1253", "29-1141"),
        to_title = c("Chief Executives", "Software Devs", "Software QA", "RNs")
      )
      onet2r:::classify_crosswalk(data)
    },
    .package = "onet2r"
  )

  result <- onet_crosswalk_bridge("2010", "2019")

  expect_named(
    result,
    c(
      "from_vintage", "to_vintage", "from_onet_soc_code",
      "to_onet_soc_code", "from_soc_code", "to_soc_code", "from_title",
      "to_title", "step_count", "map_type", "crosswalk_weight"
    )
  )
  expect_equal(
    result$from_onet_soc_code,
    c("11-1011.00", "15-1252.00", "15-1252.00", "29-1141.00")
  )
  expect_equal(
    result$to_onet_soc_code,
    c("11-1011.00", "15-1252.00", "15-1253.00", "29-1141.00")
  )
  expect_equal(result$from_soc_code, c("11-1011", "15-1252", "15-1252", "29-1141"))
  expect_equal(
    as.character(result$map_type),
    c("one_to_one", "split", "split", "one_to_one")
  )
  expect_equal(result$crosswalk_weight, c(1, 0.5, 0.5, 1))
})

test_that("onet_crosswalk_bridge chains native O*NET-SOC detail codes", {
  local_mocked_bindings(
    read_adjacent_crosswalk = function(from, to) {
      if (from == "2009" && to == "2010") {
        return(onet2r:::classify_crosswalk(tibble::tibble(
          from_vintage = "2009",
          to_vintage = "2010",
          from_onet_soc_code = c("15-1252.00", "15-1252.00"),
          to_onet_soc_code = c("15-1252.00", "15-1252.01"),
          from_title = "Software Developers",
          to_title = c("Software Developers", "Software Testers")
        )))
      }
      onet2r:::classify_crosswalk(tibble::tibble(
        from_vintage = "2010",
        to_vintage = "2019",
        from_onet_soc_code = c("15-1252.00", "15-1252.01"),
        to_onet_soc_code = c("15-1252.00", "15-1252.01"),
        from_title = c("Software Developers", "Software Testers"),
        to_title = c("Software Developers", "Software Testers")
      ))
    },
    .package = "onet2r"
  )

  result <- onet_crosswalk_bridge("2009", "2019")

  expect_equal(result$step_count, c(2L, 2L))
  expect_equal(result$from_onet_soc_code, c("15-1252.00", "15-1252.00"))
  expect_equal(result$to_onet_soc_code, c("15-1252.00", "15-1252.01"))
  expect_equal(result$crosswalk_weight, c(0.5, 0.5))
})

test_that("onet_panel_reconcile implements the change truth table", {
  panel <- tibble::tibble(
    release_version = rep(c("1.0", "2.0"), each = 4),
    release_date = rep(as.Date(c("2020-01-01", "2021-01-01")), each = 4),
    soc_vintage = "2019",
    domain = "Abilities",
    onet_soc_code = rep(c("11-1011.00", "15-1252.00", "29-1141.00", "41-1011.00"), 2),
    soc_code = rep(c("11-1011", "15-1252", "29-1141", "41-1011"), 2),
    title = "Occupation",
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = "IM",
    data_value = c(4, 4, 4, 4, 4, 5, 4, 5),
    n = NA_integer_,
    standard_error = NA_real_,
    lower_ci_bound = NA_real_,
    upper_ci_bound = NA_real_,
    recommend_suppress = NA_character_,
    source_date = c(
      as.Date(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01")),
      as.Date(c("2020-01-01", "2021-01-01", "2021-01-01", "2020-01-01"))
    ),
    domain_source = c(
      "Analyst", "Analyst", "Analyst", "Analyst",
      "Analyst", "Analyst", "Analyst", "Analyst"
    )
  )
  bridge <- tibble::tibble(
    from_vintage = "2019",
    to_vintage = "2019",
    from_soc_code = c("11-1011", "15-1252", "29-1141", "41-1011"),
    to_soc_code = c("11-1011", "15-1252", "29-1141", "41-1011"),
    from_title = "Occupation",
    to_title = "Occupation",
    step_count = 0L,
    map_type = c("one_to_one", "one_to_one", "one_to_one", "one_to_one"),
    crosswalk_weight = 1
  )

  result <- onet_panel_reconcile(panel, bridge)

  expect_equal(
    as.character(result$change_type),
    c(
      "stale_carryforward",
      "real_update",
      "resampled_stable",
      "recode_or_recalc_flag"
    )
  )
  expect_equal(result$value_changed, c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(result$date_changed, c(FALSE, TRUE, TRUE, FALSE))
  expect_equal(result$safely_comparable, c(TRUE, TRUE, TRUE, FALSE))
})

test_that("onet_panel_reconcile handles transition and suppressed rows explicitly", {
  panel <- tibble::tibble(
    release_version = rep(c("1.0", "2.0"), each = 2),
    release_date = rep(as.Date(c("2020-01-01", "2021-01-01")), each = 2),
    soc_vintage = "2019",
    domain = "Abilities",
    onet_soc_code = rep(c("15-1252.00", "29-1141.00"), 2),
    soc_code = rep(c("15-1252", "29-1141"), 2),
    title = "Occupation",
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = "IM",
    data_value = c(4, 4, 5, 5),
    n = NA_integer_,
    standard_error = NA_real_,
    lower_ci_bound = NA_real_,
    upper_ci_bound = NA_real_,
    recommend_suppress = c("N", "N", "N", "Y"),
    source_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01")),
    domain_source = c(
      "Analyst - Transition", "Incumbent",
      "Analyst", "Incumbent"
    )
  )
  bridge <- tibble::tibble(
    from_vintage = "2019",
    to_vintage = "2019",
    from_onet_soc_code = c("15-1252.00", "29-1141.00"),
    to_onet_soc_code = c("15-1252.00", "29-1141.00"),
    map_type = "one_to_one",
    crosswalk_weight = 1
  )

  result <- onet_panel_reconcile(panel, bridge)

  expect_equal(as.character(result$change_type), c("transition_data", "suppressed_change"))
  expect_equal(result$transition_data, c(TRUE, FALSE))
  expect_equal(result$suppressed_change, c(FALSE, TRUE))
  expect_equal(result$safely_comparable, c(FALSE, FALSE))
})

test_that("onet_panel_reconcile reports new and dropped content rows", {
  panel <- tibble::tibble(
    release_version = c("1.0", "2.0"),
    release_date = as.Date(c("2020-01-01", "2021-01-01")),
    soc_vintage = "2019",
    domain = "Abilities",
    onet_soc_code = c("15-1252.00", "29-1141.00"),
    soc_code = c("15-1252", "29-1141"),
    title = c("Software Developers", "Registered Nurses"),
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = "IM",
    data_value = c(4, 5),
    n = NA_integer_,
    standard_error = NA_real_,
    lower_ci_bound = NA_real_,
    upper_ci_bound = NA_real_,
    recommend_suppress = "N",
    source_date = as.Date(c("2020-01-01", "2021-01-01")),
    domain_source = "Analyst"
  )
  bridge <- tibble::tibble(
    from_vintage = "2019",
    to_vintage = "2019",
    from_onet_soc_code = c("15-1252.00", "29-1141.00"),
    to_onet_soc_code = c("15-1252.00", "29-1141.00"),
    map_type = "one_to_one",
    crosswalk_weight = 1
  )

  result <- onet_panel_reconcile(panel, bridge)

  expect_equal(as.character(result$change_type), c("dropped", "new"))
  expect_equal(as.character(result$coverage_status), c("dropped", "new"))
  expect_equal(result$from_onet_soc_code, c("15-1252.00", NA_character_))
  expect_equal(result$to_onet_soc_code, c("15-1252.00", "29-1141.00"))
  expect_equal(result$safely_comparable, c(FALSE, FALSE))
})

test_that("onet_panel_reconcile uses native O*NET-SOC codes across vintages", {
  panel <- tibble::tibble(
    release_version = c("1.0", "2.0", "2.0"),
    release_date = as.Date(c("2020-01-01", "2021-01-01", "2021-01-01")),
    soc_vintage = c("2010", "2019", "2019"),
    domain = "Abilities",
    onet_soc_code = c("15-1252.00", "15-1252.00", "15-1252.01"),
    soc_code = "15-1252",
    title = c("Software Developers", "Software Developers", "Software Testers"),
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = "IM",
    data_value = c(4, 99, 5),
    n = NA_integer_,
    standard_error = NA_real_,
    lower_ci_bound = NA_real_,
    upper_ci_bound = NA_real_,
    recommend_suppress = "N",
    source_date = as.Date(c("2020-01-01", "2021-01-01", "2021-01-01")),
    domain_source = "Analyst"
  )
  bridge <- tibble::tibble(
    from_vintage = "2010",
    to_vintage = "2019",
    from_onet_soc_code = "15-1252.00",
    to_onet_soc_code = "15-1252.01",
    map_type = "one_to_one",
    crosswalk_weight = 1
  )

  result <- onet_panel_reconcile(panel, bridge)

  matched <- result[result$coverage_status == "matched", ]
  expect_equal(nrow(matched), 1L)
  expect_equal(matched$to_onet_soc_code, "15-1252.01")
  expect_equal(matched$to_value, 5)
})

test_that("onet_panel_reconcile flags method breaks and uncertain crosswalks", {
  panel <- tibble::tibble(
    release_version = rep(c("1.0", "2.0"), each = 2),
    release_date = rep(as.Date(c("2020-01-01", "2021-01-01")), each = 2),
    soc_vintage = "2019",
    domain = "Abilities",
    onet_soc_code = rep(c("15-1252.00", "29-1141.00"), 2),
    soc_code = rep(c("15-1252", "29-1141"), 2),
    title = "Occupation",
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = "IM",
    data_value = c(4, 4, 5, 5),
    n = NA_integer_,
    standard_error = NA_real_,
    lower_ci_bound = NA_real_,
    upper_ci_bound = NA_real_,
    recommend_suppress = NA_character_,
    source_date = as.Date(c("2020-01-01", "2020-01-01", "2021-01-01", "2021-01-01")),
    domain_source = c("Analyst", "Incumbent", "Incumbent", "Incumbent")
  )
  bridge <- tibble::tibble(
    from_vintage = "2019",
    to_vintage = "2019",
    from_soc_code = c("15-1252", "29-1141"),
    to_soc_code = c("15-1252", "29-1141"),
    from_title = "Occupation",
    to_title = "Occupation",
    step_count = 0L,
    map_type = c("one_to_one", "split"),
    crosswalk_weight = c(1, 0.5)
  )

  result <- onet_panel_reconcile(panel, bridge)

  expect_equal(result$method_break, c(TRUE, FALSE))
  expect_equal(result$crosswalk_uncertain, c(FALSE, TRUE))
  expect_equal(result$safely_comparable, c(FALSE, FALSE))
})

test_that("onet_panel_reconcile returns a stable empty schema", {
  panel <- tibble::tibble(
    release_version = "1.0",
    release_date = as.Date("2020-01-01"),
    soc_vintage = "2019",
    domain = "Abilities",
    onet_soc_code = "15-1252.00",
    soc_code = "15-1252",
    title = "Software Developers",
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = "IM",
    data_value = 4,
    n = NA_integer_,
    standard_error = NA_real_,
    lower_ci_bound = NA_real_,
    upper_ci_bound = NA_real_,
    recommend_suppress = NA_character_,
    source_date = as.Date("2020-01-01"),
    domain_source = "Analyst"
  )
  bridge <- tibble::tibble(
    from_vintage = "2019",
    to_vintage = "2019",
    from_soc_code = "15-1252",
    to_soc_code = "15-1252",
    from_title = "Software Developers",
    to_title = "Software Developers",
    step_count = 0L,
    map_type = "one_to_one",
    crosswalk_weight = 1
  )

  result <- onet_panel_reconcile(panel, bridge)

  expect_equal(nrow(result), 0L)
  expect_named(
    result,
    c(
      "from_release", "to_release", "from_release_date", "to_release_date",
      "from_onet_soc_code", "to_onet_soc_code", "from_soc_code",
      "to_soc_code", "soc_vintage_from", "soc_vintage_to",
      "domain", "element_id", "element_name", "scale_id", "from_value",
      "to_value", "value_change", "value_percent_change", "from_source_date",
      "to_source_date", "from_domain_source", "to_domain_source",
      "from_recommend_suppress", "to_recommend_suppress", "from_not_relevant",
      "to_not_relevant", "date_changed", "value_changed", "transition_data",
      "suppressed_change", "change_type", "coverage_status", "method_break",
      "crosswalk_uncertain",
      "safely_comparable", "map_type", "crosswalk_weight"
    )
  )
})

test_that("onet_change_summary reports overall and job-family shares", {
  changes <- tibble::tibble(
    from_release = "1.0",
    to_release = "2.0",
    from_release_date = as.Date("2020-01-01"),
    to_release_date = as.Date("2021-01-01"),
    from_soc_code = c("15-1252", "15-1253", "29-1141"),
    to_soc_code = c("15-1252", "15-1253", "29-1141"),
    soc_vintage_from = "2019",
    soc_vintage_to = "2019",
    domain = "Abilities",
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = "IM",
    from_value = c(4, 4, 4),
    to_value = c(5, 4, 5),
    value_change = c(1, 0, 1),
    value_percent_change = c(0.25, 0, 0.25),
    from_source_date = as.Date("2020-01-01"),
    to_source_date = as.Date("2021-01-01"),
    date_changed = c(TRUE, FALSE, TRUE),
    value_changed = c(TRUE, FALSE, TRUE),
    change_type = c("real_update", "stale_carryforward", "real_update"),
    method_break = c(FALSE, FALSE, TRUE),
    crosswalk_uncertain = c(FALSE, FALSE, FALSE),
    safely_comparable = c(TRUE, TRUE, FALSE),
    map_type = "one_to_one",
    crosswalk_weight = 1
  )

  result <- onet_change_summary(changes, by = "job_family")

  expect_named(
    result,
    c(
      "summary_level", "job_family", "n_group", "share_group",
      "mean_value_change", "median_abs_value_change",
      "share_safely_comparable", "share_method_break",
      "share_crosswalk_uncertain", "change_type", "n", "share"
    )
  )
  expect_equal(
    result$summary_level,
    c("overall", "overall", "job_family", "job_family", "job_family")
  )
  expect_equal(result$job_family, c(NA_character_, NA_character_, "15", "15", "29"))
  expect_equal(result$n_group, c(3L, 3L, 2L, 2L, 1L))
  expect_equal(result$share_group, c(1, 1, 2 / 3, 2 / 3, 1 / 3))
  expect_equal(
    result$change_type,
    c("real_update", "stale_carryforward", "real_update", "stale_carryforward", "real_update")
  )
  expect_equal(result$n, c(2L, 1L, 1L, 1L, 1L))
  expect_equal(result$share, c(2 / 3, 1 / 3, 1 / 2, 1 / 2, 1))
})

test_that("onet_panel_reconcile marks missing source dates as unknown", {
  panel <- tibble::tibble(
    release_version = rep(c("1.0", "2.0"), each = 1),
    release_date = as.Date(c("2020-01-01", "2021-01-01")),
    soc_vintage = "2019",
    domain = "Abilities",
    onet_soc_code = "15-1252.00",
    soc_code = "15-1252",
    title = "Software Developers",
    element_id = "1.A.1.a.1",
    element_name = "Oral Comprehension",
    scale_id = "IM",
    data_value = c(4, 4),
    n = NA_integer_,
    standard_error = NA_real_,
    lower_ci_bound = NA_real_,
    upper_ci_bound = NA_real_,
    recommend_suppress = "N",
    not_relevant = "N",
    source_date = as.Date(c(NA, "2021-01-01")),
    domain_source = "Analyst"
  )
  bridge <- tibble::tibble(
    from_vintage = "2019",
    to_vintage = "2019",
    from_onet_soc_code = "15-1252.00",
    to_onet_soc_code = "15-1252.00",
    map_type = "one_to_one",
    crosswalk_weight = 1
  )

  result <- onet_panel_reconcile(panel, bridge)

  expect_equal(as.character(result$change_type), "source_date_missing")
  expect_equal(result$safely_comparable, FALSE)
  expect_equal(result$from_not_relevant, "N")
  expect_equal(result$to_not_relevant, "N")
})
