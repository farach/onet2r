software_figures <- function(prevalence, occupations, resolution, pairs) {
  common <- prevalence |>
    dplyr::slice_head(n = 8) |>
    dplyr::arrange(.data$prevalence) |>
    dplyr::mutate(label = factor(.data$item, levels = .data$item))
  common_plot <- ggplot2::ggplot(
    common, ggplot2::aes(.data$prevalence, .data$label)
  ) +
    ggplot2::geom_col(fill = onet2r_colors[["teal"]], width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%", 100 * .data$prevalence)),
      hjust = -0.15, size = 12, size.unit = "pt"
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.08), breaks = seq(0, 1, 0.2),
      labels = function(x) paste0(round(100 * x), "%"),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::scale_y_discrete(labels = onet2r_wrap(32)) +
    ggplot2::labs(
      title = "Excel is listed for 93% of covered occupations",
      subtitle = "Eight most widely linked software examples; 923 occupation codes",
      x = "Share of covered occupation codes", y = NULL,
      caption = "O*NET 30.3. Listed examples, not software-use rates or employment shares."
    ) +
    onet2r_theme()
  resolution$label <- factor(
    resolution$grain, levels = c("route_id", "category", "family"),
    labels = c("Example +\ncategory", "Software\ncategory", "Broader\nfamily")
  )
  resolution_plot <- ggplot2::ggplot(
    resolution, ggplot2::aes(.data$label, .data$information_bits)
  ) +
    ggplot2::geom_col(fill = onet2r_colors[["slate"]], width = 0.55) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf(
        "%.2f bits\n%.1f%% retained", .data$information_bits, .data$retained_percent
      )), vjust = -0.3, size = 12, size.unit = "pt", lineheight = 1.05
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 6.2), breaks = 0:6,
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "Broad software families retain about a fifth of the fine-label signal",
      subtitle = "Fixed example-category mass and equal occupation weights",
      x = NULL, y = "Information about occupation identity (bits)",
      caption = "O*NET 30.3. This accounting experiment is separate from the exact-name prevalence analysis."
    ) +
    onet2r_theme()
  size_plot <- ggplot2::ggplot(
    occupations, ggplot2::aes(.data$n_examples, .data$mean_idf)
  ) +
    ggplot2::geom_point(
      color = onet2r_colors[["slate"]], alpha = 0.45, size = 1.7
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, 500, 100)) +
    ggplot2::scale_y_continuous(limits = c(0, 7.1), breaks = 0:7) +
    ggplot2::labs(
      title = "Longer software lists can contain plenty of rare names",
      subtitle = "Each point is one occupation; list length is not job complexity",
      x = "Number of distinct listed examples",
      y = "Mean log inverse prevalence",
      caption = "O*NET 30.3. Exact strings, equal occupation weights, no alias merging."
    ) +
    onet2r_theme()
  pair_labels <- c(
    "13-2011.00" = "Accountants / bookkeeping clerks",
    "15-1252.00" = "Developers / software testers",
    "27-1021.00" = "Industrial / graphic designers",
    "29-1141.00" = "Registered / practical nurses"
  )
  pair_components <- pairs |>
    dplyr::mutate(label = unname(pair_labels[.data$occupation_a]))
  pair_components <- dplyr::bind_rows(
    dplyr::transmute(
      pair_components, label = .data$label,
      component = "replacement_component", value = .data$replacement_component
    ),
    dplyr::transmute(
      pair_components, label = .data$label,
      component = "nestedness_resultant_component",
      value = .data$nestedness_resultant_component
    )
  ) |>
    dplyr::mutate(
      label = factor(.data$label, levels = rev(unname(pair_labels))),
      component = factor(.data$component,
                         levels = c("replacement_component",
                                    "nestedness_resultant_component"),
                         labels = c("Replacement", "Nestedness-resultant"))
    )
  components_plot <- ggplot2::ggplot(
    pair_components, ggplot2::aes(.data$value, .data$label, fill = .data$component)
  ) +
    ggplot2::geom_col(width = 0.6, position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::scale_fill_manual(values = c(
      "Replacement" = onet2r_colors[["teal"]],
      "Nestedness-resultant" = onet2r_colors[["amber"]]
    )) +
    ggplot2::scale_x_continuous(
      limits = c(0, 0.65), breaks = seq(0, 0.6, 0.1),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      title = "Similar totals can hide different kinds of list overlap",
      subtitle = "Selected pairs; components add to Sorensen dissimilarity",
      x = "Sorensen dissimilarity", y = NULL, fill = NULL,
      caption = "O*NET 30.3. Replacement means different listed examples, not change through time."
    ) +
    onet2r_theme()
  list(common = common_plot, resolution = resolution_plot, size = size_plot,
       components = components_plot)
}
