# Article plots reuse the package's portable vignette theme.
distinctiveness_figure_style <- function(plot) {
  widths <- c(title = 50L, subtitle = 75L, caption = 95L)
  for (label in names(widths)) {
    plot$labels[[label]] <- paste(
      strwrap(plot$labels[[label]], width = widths[[label]]), collapse = "\n"
    )
  }
  plot + onet2r_theme(base_size = 16)
}

distinctiveness_figures <- function(prevalence, occupations, resolution) {
  common <- prevalence |>
    dplyr::slice_max(.data$n_occupations, n = 8, with_ties = TRUE) |>
    dplyr::arrange(.data$n_occupations, dplyr::desc(.data$item)) |>
    dplyr::mutate(label = factor(
      .data$dwa_element_name, levels = .data$dwa_element_name
    ))
  common_plot <- ggplot2::ggplot(
    common, ggplot2::aes(x = .data$prevalence, y = .data$label)
  ) +
    ggplot2::geom_col(fill = onet2r_colors[["teal"]], width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(.data$n_occupations, " occupations")),
      hjust = -0.08, size = 13, size.unit = "pt",
      color = onet2r_colors[["ink"]]
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(round(100 * x), "%"),
      breaks = seq(0, 0.12, 0.02),
      expand = ggplot2::expansion(mult = c(0, 0.45))
    ) +
    ggplot2::scale_y_discrete(labels = onet2r_wrap(44)) +
    ggplot2::labs(
      title = "The most common detailed activity reaches fewer than 10%",
      subtitle = "Any linked task, including Supplemental tasks; 923 occupations",
      x = "Share of covered occupations", y = NULL,
      caption = "Source: O*NET 30.3. Binary occupation-DWA presence, not worker prevalence."
    ) +
    onet2r_theme()

  leaders <- occupations |>
    dplyr::filter(.data$rarity_rank == 1L |
                    .data$nearest_distance == max(.data$nearest_distance))
  leaders$label <- ifelse(
    leaders$rarity_rank == 1L,
    "Slaughterers and Meat Packers\n(highest activity rarity)",
    "Models (most isolated profile)"
  )
  distinction_plot <- ggplot2::ggplot(
    occupations,
    ggplot2::aes(x = .data$mean_idf, y = .data$nearest_distance)
  ) +
    ggplot2::geom_point(color = onet2r_colors[["gray"]], alpha = 0.4, size = 1.6) +
    ggplot2::geom_point(
      data = leaders, shape = 21, size = 3.3,
      fill = onet2r_colors[["teal"]], color = onet2r_colors[["ink"]]
    ) +
    ggplot2::geom_text(
      data = leaders, ggplot2::aes(label = .data$label),
      hjust = 1.07, vjust = -0.9, size = 13, size.unit = "pt"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1), expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::labs(
      title = "Rare activities and isolation identify different occupations",
      subtitle = "Each point is one of 923 covered occupations",
      x = "Mean IDF (natural logs)",
      y = "Nearest-profile distance (total variation)",
      caption = "Source: O*NET 30.3. Binary activity shares; no employment or task-rating weights."
    ) +
    onet2r_theme()

  size_plot <- ggplot2::ggplot(
    occupations, ggplot2::aes(x = .data$n_activities, y = .data$mean_idf)
  ) +
    ggplot2::geom_point(color = onet2r_colors[["slate"]], alpha = 0.4, size = 1.6) +
    ggplot2::geom_point(
      data = leaders[leaders$rarity_rank == 1L, ],
      shape = 21, size = 3.3,
      fill = onet2r_colors[["teal"]], color = onet2r_colors[["ink"]]
    ) +
    ggplot2::geom_text(
      data = leaders[leaders$rarity_rank == 1L, ],
      label = "Slaughterers and\nMeat Packers (5 DWAs)",
      hjust = -0.05, vjust = 1.1, size = 13, size.unit = "pt"
    ) +
    ggplot2::labs(
      title = "Check the activity inventory before trusting a rarity rank",
      subtitle = "The top rarity profile has only five linked detailed activities",
      x = "Number of distinct linked DWAs",
      y = "Mean IDF (natural logs)",
      caption = "Source: O*NET 30.3. Inventory size is a diagnostic, not a measure of job breadth."
    ) +
    onet2r_theme()

  resolution <- resolution |>
    dplyr::mutate(label = factor(
      .data$grain, levels = c("dwa_element_id", "iwa_element_id", "gwa_element_id"),
      labels = c("Detailed", "Intermediate", "Generalized")
    ))
  resolution_plot <- ggplot2::ggplot(
    resolution, ggplot2::aes(x = .data$label, y = .data$mean_distance, group = 1)
  ) +
    ggplot2::geom_line(color = onet2r_colors[["teal"]], linewidth = 1) +
    ggplot2::geom_point(color = onet2r_colors[["teal"]], size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", .data$mean_distance)),
      vjust = -0.9, size = 11, size.unit = "pt",
      color = onet2r_colors[["ink"]]
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.06), breaks = seq(0, 1, 0.2)
    ) +
    ggplot2::labs(
      title = "Broader labels erase some of the measured differences",
      subtitle = "The same 425,503 occupation pairs at each activity resolution",
      x = "Activity vocabulary", y = "Mean pairwise total-variation distance",
      caption = "Source: O*NET 30.3. Fixed activity mass and universe; this is not change over time."
    ) +
    onet2r_theme()
  lapply(list(
    common = common_plot, rarity_isolation = distinction_plot,
    inventory_size = size_plot, resolution = resolution_plot
  ), distinctiveness_figure_style)
}
