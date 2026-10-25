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
  audit <- env$distinctiveness_audit_figures(information, loss, null)
  expect_named(audit, c("information", "loss", "null"))
  expect_equal(audit$information$data$information_bits, information$information_bits)
  expect_equal(nrow(audit$loss$data), 8L)
  expect_equal(nrow(audit$null$data), 6L)
  for (plot in audit) {
    expect_s3_class(plot, "ggplot")
    expect_match(plot$labels$caption, "Source: O\\*NET 30.3")
    expect_error(ggplot2::ggplotGrob(plot), NA)
  }
})
