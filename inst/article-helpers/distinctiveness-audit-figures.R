distinctiveness_audit_figures <- function(information, loss, null, random_hierarchy) {
  grain_labels <- c(
    dwa_element_id = "Detailed", iwa_element_id = "Intermediate",
    gwa_element_id = "Generalized"
  )
  information$label <- factor(
    information$grain, levels = rev(names(grain_labels)),
    labels = rev(unname(grain_labels))
  )
  information_plot <- ggplot2::ggplot(
    information, ggplot2::aes(x = .data$information_bits, y = .data$label)
  ) +
    ggplot2::geom_col(fill = onet2r_colors[["teal"]], width = 0.55) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf(
        "%.2f bits (%.0f%% retained)",
        .data$information_bits, .data$retained_dwa_percent
      )),
      hjust = -0.06, size = 13, size.unit = "pt"
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 10, 2), expand = ggplot2::expansion(mult = c(0, 0.55))
    ) +
    ggplot2::labs(
      title = "Generalized labels retain 26% of the detailed-label signal",
      subtitle = "Information about occupation identity from one synthetic activity draw",
      x = "Expected information gain (bits)", y = NULL,
      caption = paste(
        "Source: O*NET 30.3. Equal prior over 923 occupations; fixed original DWA mass.",
        "Retained percent uses detailed-label bits. Labels changed, not jobs.",
        sep = "\n"
      )
    ) +
    onet2r_theme()

  top <- loss |>
    dplyr::slice_max(.data$lost_bits, n = 8, with_ties = FALSE) |>
    dplyr::arrange(.data$lost_bits) |>
    dplyr::mutate(label = factor(
      .data$gwa_element_name, levels = .data$gwa_element_name
    ))
  loss_plot <- ggplot2::ggplot(
    top, ggplot2::aes(x = .data$lost_bits, y = .data$label)
  ) +
    ggplot2::geom_col(fill = onet2r_colors[["slate"]], width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf(
        "%.2f bits; %.1f%% of draw mass", .data$lost_bits, 100 * .data$mixture_mass
      )),
      hjust = -0.05, size = 12, size.unit = "pt"
    ) +
    ggplot2::scale_y_discrete(labels = onet2r_wrap(38)) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, 0.2), expand = ggplot2::expansion(mult = c(0, 0.65))
    ) +
    ggplot2::labs(
      title = "Eight broad headings with the largest weighted information loss",
      subtitle = "Contributions to detailed-to-generalized information loss",
      x = "Weighted contribution to discarded information (bits)", y = NULL,
      caption = paste(
        "Source: O*NET 30.3. Eight largest contributions of 41; full results in information-loss.csv.",
        "Group size and draw mass affect contributions; this is not intrinsic complexity.",
        sep = "\n"
      )
    ) +
    onet2r_theme()

  metrics <- c("mean_pair_distance", "median_nearest_distance")
  observed <- dplyr::bind_rows(lapply(metrics, function(metric) {
    null |>
      dplyr::filter(.data$draw == 0L) |>
      dplyr::transmute(
        grain = .data$grain, metric = metric, value = .data[[metric]]
      )
  }))
  reference <- dplyr::bind_rows(lapply(metrics, function(metric) {
    null |>
      dplyr::filter(.data$draw > 0L) |>
      dplyr::summarise(
        lower = min(.data[[metric]]), upper = max(.data[[metric]]),
        .by = "grain"
      ) |>
      dplyr::mutate(metric = metric)
  }))
  for (name in c("observed", "reference")) {
    object <- get(name)
    object$grain <- factor(
      object$grain, levels = rev(names(grain_labels)),
      labels = rev(unname(grain_labels))
    )
    object$metric <- factor(
      object$metric, levels = metrics,
      labels = c("Mean pair distance", "Median nearest distance")
    )
    if (name == "observed") observed <- object else reference <- object
  }
  null_plot <- ggplot2::ggplot(
    observed, ggplot2::aes(x = .data$value, y = .data$grain)
  ) +
    ggplot2::geom_segment(
      data = reference,
      ggplot2::aes(x = .data$lower, xend = .data$upper,
                   y = .data$grain, yend = .data$grain),
      inherit.aes = FALSE, linewidth = 4, color = onet2r_colors[["gray"]]
    ) +
    ggplot2::geom_point(
      shape = 17, size = 3, color = onet2r_colors[["teal"]]
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", .data$value)),
      vjust = -1, hjust = 0.7, size = 13, size.unit = "pt"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data$metric), nrow = 1) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1.05), breaks = seq(0, 1, 0.25)
    ) +
    ggplot2::labs(
      title = "Nearest profiles are closer than in these fixed-margin shuffles",
      subtitle = "Triangles: observed. Gray segments: range of 20 randomized matrices.",
      x = "Total-variation distance (0 to 1)", y = NULL,
      caption = paste(
        "Source: O*NET 30.3. Curveball preserves every occupation size and DWA frequency.",
        "20 starts, 20 trades per occupation each; descriptive reference, not a test or confidence interval.",
        sep = "\n"
      )
    ) +
    onet2r_theme()

  observed_generalized_information <- information$information_bits[
    information$grain == "gwa_element_id"
  ]
  hierarchy_plot <- ggplot2::ggplot(
    random_hierarchy, ggplot2::aes(x = .data$information_bits)
  ) +
    ggplot2::geom_histogram(
      bins = 25, fill = onet2r_colors[["slate"]],
      color = "white", linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      xintercept = observed_generalized_information,
      color = onet2r_colors[["teal"]], linewidth = 1.2
    ) +
    ggplot2::labs(
      title = "Compare the published grouping with size-preserving random partitions",
      subtitle = paste(
        nrow(random_hierarchy),
        "random DWA-to-GWA assignments; each group's DWA count is preserved"
      ),
      x = "Information about occupation identity (bits)",
      y = "Random assignments",
      caption = paste(
        "Source: O*NET 30.3. Teal line: observed GWA grouping.",
        "Random assignments do not preserve IWA nesting or represent an economic null model.",
        sep = "\n"
      )
    ) +
    onet2r_theme()
  lapply(
    list(
      information = information_plot, loss = loss_plot, null = null_plot,
      random_hierarchy = hierarchy_plot
    ),
    distinctiveness_figure_style
  )
}
