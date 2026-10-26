test_that("article figures preserve the published data and denominators", {
  skip_if_not_installed("ggplot2")
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  env <- new.env(parent = globalenv())
  sys.source(system.file("vignette-helpers.R", package = "onet2r"), env)
  sys.source(system.file(
    "article-helpers", "distinctiveness-figures.R", package = "onet2r"
  ), env)
  sys.source(system.file(
    "article-helpers", "distinctiveness-audit-figures.R", package = "onet2r"
  ), env)
  read_snapshot <- function(name) {
    tibble::as_tibble(utils::read.csv(system.file(
      "extdata", "occupational-distinctiveness", name, package = "onet2r"
    )))
  }
  prevalence <- read_snapshot("activity-prevalence.csv")
  occupations <- read_snapshot("occupation-profiles.csv")
  resolution <- read_snapshot("resolution-summary.csv")
  plots <- env$distinctiveness_figures(prevalence, occupations, resolution)
  expect_named(plots, c("common", "rarity_isolation", "inventory_size", "resolution"))
  expect_equal(nrow(plots$rarity_isolation$data), 923L)
  bars <- ggplot2::ggplot_build(plots$common)$data[[1]]
  expect_equal(sort(bars$x), sort(head(prevalence$prevalence, 8)))
  expect_equal(plots$resolution$data$mean_distance, resolution$mean_distance)
  expect_equal(plots$inventory_size$data$n_activities, occupations$n_activities)
  for (plot in plots) {
    expect_s3_class(plot, "ggplot")
    expect_match(plot$labels$caption, "O\\*NET 30.3")
    expect_error(ggplot2::ggplotGrob(plot), NA)
  }
  information <- read_snapshot("information-summary.csv")
  loss <- read_snapshot("information-loss.csv")
  null <- read_snapshot("null-summary.csv")
  random_hierarchy <- read_snapshot("information-random-hierarchy.csv")
  audit <- env$distinctiveness_audit_figures(
    information, loss, null, random_hierarchy
  )
  expect_named(audit, c("information", "loss", "null", "random_hierarchy"))
  expect_equal(audit$information$data$information_bits, information$information_bits)
  expect_equal(nrow(audit$loss$data), 8L)
  expect_equal(nrow(audit$null$data), 6L)
  expect_equal(nrow(audit$random_hierarchy$data), 200L)
  for (plot in audit) {
    expect_s3_class(plot, "ggplot")
    expect_match(plot$labels$caption, "Source: O\\*NET 30.3")
    expect_error(ggplot2::ggplotGrob(plot), NA)
  }
})

test_that("extension figure helpers use declared dependencies", {
  description <- utils::packageDescription("onet2r")
  declared <- paste(description[c("Depends", "Imports", "Suggests")],
                    collapse = ",")
  declared <- gsub("\\([^)]*\\)", "", declared)
  declared <- strsplit(declared, "[[:space:],]+")[[1]]
  for (file in c("extension-figures.R", "software-figures.R")) {
    path <- system.file("article-helpers", file, package = "onet2r")
    tokens <- utils::getParseData(parse(file = path, keep.source = TRUE))
    namespaces <- unique(tokens$text[tokens$token == "SYMBOL_PACKAGE"])
    expect_setequal(setdiff(namespaces, c("base", declared)), character())
  }
})

test_that("employment and software figure layers preserve snapshot values", {
  skip_if_not_installed("ggplot2")
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  env <- new.env(parent = globalenv())
  sys.source(system.file("vignette-helpers.R", package = "onet2r"), env)
  for (file in c("occupational-distinctiveness.R", "extension-figures.R",
                 "software-figures.R")) {
    sys.source(system.file("article-helpers", file, package = "onet2r"), env)
  }
  read_snapshot <- function(directory, name) {
    utils::read.csv(env$snapshot_csv_path(file.path(
      system.file("extdata", directory, package = "onet2r"),
      paste0(name, ".csv")
    )))
  }
  employment <- read_snapshot(
    "occupational-extensions", "activity-employment-prevalence"
  ) |>
    dplyr::filter(.data$unit_rule == "soc") |>
    dplyr::left_join(
      read_snapshot("occupational-distinctiveness", "activity-prevalence") |>
        dplyr::select("item", "dwa_element_name"),
      by = c("dwa_element_id" = "item"), relationship = "one-to-one"
    ) |>
    dplyr::arrange(dplyr::desc(.data$prevalence_employment), .data$dwa_element_id) |>
    dplyr::slice_head(n = 8L)
  plot <- env$employment_prevalence_figure(employment)
  points <- plot$layers[[2]]$data
  expect_equal(nrow(points), 2L * nrow(employment))
  expect_setequal(points$weighting,
                  c("prevalence_equal_unit", "prevalence_employment"))
  expect_equal(
    sort(points$prevalence[points$weighting == "prevalence_equal_unit"]),
    sort(employment$prevalence_equal_unit)
  )
  expect_equal(
    sort(points$prevalence[points$weighting == "prevalence_employment"]),
    sort(employment$prevalence_employment)
  )
  expect_equal(nrow(ggplot2::ggplot_build(plot)$data[[2]]), nrow(points))
  expect_error(ggplot2::ggplotGrob(plot), NA)

  directory <- "occupational-software"
  pairs <- read_snapshot(directory, "selected-pairs")
  plots <- env$software_figures(
    read_snapshot(directory, "prevalence"),
    read_snapshot(directory, "occupations"),
    read_snapshot(directory, "resolution"), pairs
  )
  components <- plots$components$data
  expect_equal(nrow(components), 2L * nrow(pairs))
  expect_equal(
    sort(components$value[components$component == "Replacement"]),
    sort(pairs$replacement_component)
  )
  expect_equal(
    sort(components$value[components$component == "Nestedness-resultant"]),
    sort(pairs$nestedness_resultant_component)
  )
  expect_equal(
    sort(as.vector(tapply(components$value, components$label, sum))),
    sort(pairs$sorensen_total)
  )
  for (plot in plots) {
    expect_gt(nrow(ggplot2::ggplot_build(plot)$data[[1]]), 0L)
    expect_error(ggplot2::ggplotGrob(plot), NA)
  }
})
