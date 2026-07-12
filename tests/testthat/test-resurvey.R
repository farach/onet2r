# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Default seam registry
# ---------------------------------------------------------------------------

test_that("the default seam registry contains only the verified v25.1 taxonomy seam", {
  seams <- onet_known_seams()
  expect_equal(nrow(seams), 1L)
  expect_equal(seams$seam_type, "soc_seam")
  expect_equal(seams$seam_date, as.Date("2020-11-01"))
  # v21.0 / 2016-08-01 is not a package-verified default seam.
  expect_false("scale_seam" %in% seams$seam_type)
  expect_false(as.Date("2016-08-01") %in% seams$seam_date)
})

# Two occupations over three same-vintage releases plus a fourth release that
# crosses the v25.1 SOC seam. Software (15-1132.00) is re-surveyed once, at
# 24.1; the nurse (29-1141.00) is re-surveyed once, at 23.1.
resurvey_fixture <- function() {
  tibble::tibble(
    release_version = rep(c("22.1", "23.1", "24.1", "25.1"), each = 2),
    release_date = rep(
      as.Date(c("2017-10-01", "2018-11-01", "2019-11-01", "2020-11-01")),
      each = 2
    ),
    soc_vintage = c("2010", "2010", "2010", "2010", "2010", "2010", "2019", "2019"),
    onet_soc_code = rep(c("15-1132.00", "29-1141.00"), times = 4),
    soc_code = rep(c("15-1132", "29-1141"), times = 4),
    task_id = rep(c("1001", "2001"), times = 4),
    task = rep(c("Write code.", "Assess patients."), times = 4),
    scale_id = "IM",
    data_value = c(4.1, 4.6, 4.1, 4.8, 4.5, 4.8, 4.5, 4.8),
    source_date = as.Date(c(
      "2016-07-01", "2016-07-01", # 22.1
      "2016-07-01", "2018-07-01", # 23.1 nurse resurveyed
      "2019-01-01", "2018-07-01", # 24.1 software resurveyed
      "2019-01-01", "2018-07-01" # 25.1 carry-forward, no clock advance
    )),
    domain_source = c(
      "Incumbent", "Incumbent",
      "Incumbent", "Incumbent",
      "Incumbent", "Incumbent",
      "Analyst - Transition", "Analyst - Transition"
    ),
    recommend_suppress = "N"
  )
}

# ---------------------------------------------------------------------------
# onet_resurvey_panel()
# ---------------------------------------------------------------------------

test_that("resurvey clock advances only when the survey source_date advances", {
  rp <- onet_resurvey_panel(resurvey_fixture())

  soft <- rp |>
    dplyr::filter(.data$onet_soc_code == "15-1132.00") |>
    dplyr::arrange(.data$release_date)
  expect_equal(soft$resurvey_event, c(NA, FALSE, TRUE, FALSE))
  expect_equal(soft$cycle_index, c(0L, 0L, 1L, 1L))

  nurse <- rp |>
    dplyr::filter(.data$onet_soc_code == "29-1141.00") |>
    dplyr::arrange(.data$release_date)
  expect_equal(nurse$resurvey_event, c(NA, TRUE, FALSE, FALSE))
  expect_equal(nurse$cycle_index, c(0L, 1L, 1L, 1L))
})

test_that("resurvey_panel resolves staleness in years for a resurvey event", {
  rp <- onet_resurvey_panel(resurvey_fixture())
  soft_24 <- rp |>
    dplyr::filter(.data$onet_soc_code == "15-1132.00", .data$release_version == "24.1")
  expect_equal(
    soft_24$age_resolved,
    as.numeric(as.Date("2019-01-01") - as.Date("2016-07-01")) / 365.25
  )
  # A non-event carries NA staleness.
  soft_23 <- rp |>
    dplyr::filter(.data$onet_soc_code == "15-1132.00", .data$release_version == "23.1")
  expect_true(is.na(soft_23$age_resolved))
})

test_that("the v25.1 SOC seam is marked as an incoming seam, not a resurvey", {
  rp <- onet_resurvey_panel(resurvey_fixture())
  seam_rows <- rp |> dplyr::filter(.data$release_version == "25.1")
  expect_true(all(seam_rows$seam_in))
  expect_true(all(seam_rows$seam_type == "soc_seam"))
  # The clock did not advance across the seam.
  expect_true(all(!dplyr::coalesce(seam_rows$resurvey_event, FALSE)))
})

test_that("cycle_index is monotone non-decreasing within an occupation", {
  rp <- onet_resurvey_panel(resurvey_fixture())
  by_occ <- rp |>
    dplyr::arrange(.data$onet_soc_code, .data$release_date) |>
    dplyr::group_by(.data$onet_soc_code) |>
    dplyr::summarise(mono = all(diff(.data$cycle_index) >= 0), .groups = "drop")
  expect_true(all(by_occ$mono))
})

test_that("analyst transition rows never count toward the survey clock", {
  panel <- resurvey_fixture()
  # Give the seam release a newer analyst source_date; it must not advance the
  # survey clock because analyst transitions are excluded.
  panel$source_date[panel$release_version == "25.1"] <- as.Date("2020-06-01")
  rp <- onet_resurvey_panel(panel)
  seam_rows <- rp |> dplyr::filter(.data$release_version == "25.1")
  expect_true(all(is.na(seam_rows$occ_survey_date)))
})

test_that("min_importance drops low-importance items before the clock is built", {
  panel <- resurvey_fixture()
  panel$data_value[panel$onet_soc_code == "15-1132.00"] <- 1.0
  rp <- onet_resurvey_panel(panel, min_importance = 3)
  expect_false(any(rp$onet_soc_code == "15-1132.00"))
})

test_that("resurvey_panel returns a stable empty schema", {
  empty <- onet_resurvey_panel(resurvey_fixture()[0, ])
  expect_s3_class(empty, "onet_resurvey_panel")
  expect_equal(nrow(empty), 0L)
  expect_true(all(
    c(
      "onet_soc_code", "task_id", "occ_survey_date", "resurvey_event",
      "cycle_index", "age_resolved", "seam_in", "seam_type"
    ) %in% names(empty)
  ))
})

test_that("resurvey_panel honours a custom item column", {
  panel <- resurvey_fixture()
  panel$element_id <- panel$task_id
  rp <- onet_resurvey_panel(panel, item = "element_id")
  expect_true("element_id" %in% names(rp))
  expect_equal(names(rp)[3], "element_id")
})

test_that("resurvey_panel errors on a missing required column", {
  panel <- resurvey_fixture()
  panel$source_date <- NULL
  expect_error(onet_resurvey_panel(panel), "source_date")
})

# ---------------------------------------------------------------------------
# onet_condition_on_resurvey()
# ---------------------------------------------------------------------------

test_that("condition_on_resurvey labels selection reasons with seam precedence", {
  rp <- onet_resurvey_panel(resurvey_fixture())
  cr <- onet_condition_on_resurvey(rp)

  # Seam release rows are taxonomy_seam even though they carry ratings.
  seam_rows <- cr |> dplyr::filter(.data$release_version == "25.1")
  expect_true(all(seam_rows$selection_reason == "taxonomy_seam"))
  expect_true(all(!seam_rows$at_risk))

  # The two genuine resurvey events are at risk.
  expect_equal(sum(cr$at_risk), 2L)
  at_risk <- cr |> dplyr::filter(.data$at_risk)
  expect_setequal(at_risk$release_version, c("23.1", "24.1"))
})

test_that("suppressed cells outrank the resurvey signal", {
  rp <- onet_resurvey_panel(resurvey_fixture())
  # Force the software 24.1 resurvey event to be suppressed.
  rp$recommend_suppress[rp$onet_soc_code == "15-1132.00" &
    rp$release_version == "24.1"] <- "Y"
  cr <- onet_condition_on_resurvey(rp)
  target <- cr |>
    dplyr::filter(.data$onet_soc_code == "15-1132.00", .data$release_version == "24.1")
  expect_equal(as.character(target$selection_reason), "suppressed")
  expect_false(target$at_risk)
})

test_that("at_risk_only returns only the resurveyed rows", {
  rp <- onet_resurvey_panel(resurvey_fixture())
  cr <- onet_condition_on_resurvey(rp, at_risk_only = TRUE)
  expect_true(all(cr$selection_reason == "resurveyed"))
  expect_true(all(cr$at_risk))
})

test_that("selection_reason uses the full factor level set", {
  rp <- onet_resurvey_panel(resurvey_fixture())
  cr <- onet_condition_on_resurvey(rp)
  expect_equal(
    levels(cr$selection_reason),
    c("resurveyed", "unrevisited", "taxonomy_seam", "suppressed")
  )
})

# ---------------------------------------------------------------------------
# seams override (additive; default reproduces M1 behavior)
# ---------------------------------------------------------------------------

# One occupation observed on either side of the August 2016 (v21.0) release
# date. The survey clock advances across the gap, and v21.0 is not a
# package-verified default seam, so this reads as a resurvey by default. This
# mirrors a Work Activities / Abilities call, or any caller without
# channel-specific evidence of a v21.0 seam.
scale_span_fixture <- function() {
  tibble::tibble(
    release_version = c("21.1", "22.0"),
    release_date = as.Date(c("2016-02-01", "2017-08-01")),
    soc_vintage = c("2010", "2010"),
    onet_soc_code = c("15-1132.00", "15-1132.00"),
    soc_code = c("15-1132", "15-1132"),
    element_id = c("4.A.1", "4.A.1"),
    scale_id = "IM",
    data_value = c(4.1, 4.6),
    source_date = as.Date(c("2015-07-01", "2017-07-01")),
    domain_source = c("Incumbent", "Incumbent"),
    recommend_suppress = c("N", "N")
  )
}

test_that("resurvey_panel seams = NULL reproduces the default output exactly", {
  expect_equal(
    onet_resurvey_panel(resurvey_fixture(), seams = NULL),
    onet_resurvey_panel(resurvey_fixture())
  )
})

test_that("by default a survey step across August 2016 is not seam-masked", {
  rp <- onet_resurvey_panel(scale_span_fixture(), item = "element_id")
  step <- rp |> dplyr::filter(.data$release_version == "22.0")
  expect_false(step$seam_in)
  expect_true(is.na(step$seam_type))
  # The advancing clock reads as a genuine resurvey, since v21.0 is not
  # treated as a default seam.
  expect_true(step$resurvey_event)
})

test_that("a caller-supplied v21.0 seam still masks the survey step", {
  custom <- tibble::tibble(
    seam_type = "scale_seam",
    seam_date = as.Date("2016-08-01")
  )
  rp <- onet_resurvey_panel(
    scale_span_fixture(),
    item = "element_id",
    seams = custom
  )
  step <- rp |> dplyr::filter(.data$release_version == "22.0")
  expect_true(step$seam_in)
  expect_equal(step$seam_type, "scale_seam")
  expect_false(dplyr::coalesce(step$resurvey_event, FALSE))
})

test_that("an empty seams table matches the default: no v21.0 masking", {
  no_date_seams <- tibble::tibble(
    seam_type = character(),
    seam_date = as.Date(character())
  )
  rp <- onet_resurvey_panel(
    scale_span_fixture(),
    item = "element_id",
    seams = no_date_seams
  )
  step <- rp |> dplyr::filter(.data$release_version == "22.0")
  expect_false(step$seam_in)
  expect_true(step$resurvey_event)
})

test_that("an empty seams table still flags the cross-vintage SOC seam", {
  no_date_seams <- tibble::tibble(
    seam_type = character(),
    seam_date = as.Date(character())
  )
  rp <- onet_resurvey_panel(resurvey_fixture(), seams = no_date_seams)
  seam_rows <- rp |> dplyr::filter(.data$release_version == "25.1")
  expect_true(all(seam_rows$seam_in))
  expect_true(all(seam_rows$seam_type == "soc_seam"))
})

test_that("resurvey_panel rejects a malformed seams table", {
  expect_error(
    onet_resurvey_panel(resurvey_fixture(), seams = tibble::tibble(x = 1)),
    "seam_type"
  )
  expect_error(
    onet_resurvey_panel(resurvey_fixture(), seams = 42),
    "data frame"
  )
})

