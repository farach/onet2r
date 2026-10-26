employment_prevalence_figure <- function(data) {
  data <- data |>
    dplyr::arrange(.data$prevalence_employment) |>
    dplyr::mutate(label = factor(.data$dwa_element_name,
                                 levels = .data$dwa_element_name))
  points <- dplyr::bind_rows(
    dplyr::transmute(
      data, label = .data$label, weighting = "prevalence_equal_unit",
      prevalence = .data$prevalence_equal_unit
    ),
    dplyr::transmute(
      data, label = .data$label, weighting = "prevalence_employment",
      prevalence = .data$prevalence_employment
    )
  )
  ggplot2::ggplot(data, ggplot2::aes(y = .data$label)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$prevalence_equal_unit,
                   xend = .data$prevalence_employment, yend = .data$label),
      color = onet2r_colors[["gray"]], linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = points,
      ggplot2::aes(x = .data$prevalence, color = .data$weighting,
                   shape = .data$weighting), size = 3
    ) +
    ggplot2::scale_color_manual(
      values = c(prevalence_equal_unit = onet2r_colors[["slate"]],
                 prevalence_employment = onet2r_colors[["teal"]]),
      breaks = c("prevalence_equal_unit", "prevalence_employment"),
      labels = c("Equal SOC descriptions", "Employment allocation")
    ) +
    ggplot2::scale_shape_manual(
      values = c(prevalence_equal_unit = 1, prevalence_employment = 16),
      breaks = c("prevalence_equal_unit", "prevalence_employment"),
      labels = c("Equal SOC descriptions", "Employment allocation")
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) paste0(round(x * 100), "%"),
      limits = c(0, NA)
    ) +
    ggplot2::scale_y_discrete(labels = onet2r_wrap(40)) +
    ggplot2::labs(
      title = "Employment weights change which activities look common",
      subtitle = "Eight largest allocated-employment shares; same 765 SOC descriptions",
      x = "Prevalence under the stated weighting rule", y = NULL,
      color = NULL, shape = NULL,
      caption = "O*NET 30.3; May 2025 national OEWS. Complete cells only. Description links, not worker time."
    ) +
    onet2r_theme()
}

skill_distribution_figure <- function(data) {
  data <- data |>
    dplyr::filter(.data$block == "es_im") |>
    dplyr::arrange(.data$median, .data$element_name) |>
    dplyr::mutate(label = factor(.data$element_name,
                                 levels = .data$element_name))
  ggplot2::ggplot(data, ggplot2::aes(y = .data$label)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$p10, xend = .data$p90, yend = .data$label),
      color = onet2r_colors[["teal"]], linewidth = 2
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$median),
                        color = onet2r_colors[["ink"]], size = 2.5) +
    ggplot2::scale_x_continuous(limits = c(1, 5), breaks = 1:5) +
    ggplot2::labs(
      title = "Shared skills still differ in their required importance",
      subtitle = "Essential Skills; 823 complete, unsuppressed occupation profiles",
      x = "Importance rating (1 to 5)", y = NULL,
      caption = "Point: median. Line: 10th to 90th percentile across occupations, not a confidence interval."
    ) +
    onet2r_theme()
}

cross_domain_figure <- function(summary) {
  labels <- c(
    dwa = "Activities", software = "Software",
    es_im = "Essential\nImportance", es_lv = "Essential\nLevel",
    ts_im = "Transferable\nImportance", ts_lv = "Transferable\nLevel"
  )
  x_labels <- c(
    dwa = "Activities", software = "Software",
    es_im = "Essential\nIM", es_lv = "Essential\nLV",
    ts_im = "Transfer.\nIM", ts_lv = "Transfer.\nLV"
  )
  cells <- summary |>
    dplyr::filter(.data$k == 5L) |>
    dplyr::transmute(a = .data$block_a, b = .data$block_b,
                     overlap = .data$median_overlap)
  cells <- dplyr::bind_rows(
    cells, dplyr::transmute(cells, a = .data$b, b = .data$a,
                            overlap = .data$overlap),
    tibble::tibble(a = names(labels), b = names(labels), overlap = 1)
  ) |>
    dplyr::mutate(
      a = factor(.data$a, levels = rev(names(labels))),
      b = factor(.data$b, levels = names(labels))
    )
  ggplot2::ggplot(cells, ggplot2::aes(.data$b, .data$a, fill = .data$overlap)) +
    ggplot2::geom_tile(color = "white", linewidth = 1.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$overlap),
                   color = .data$overlap > 0.5),
      size = 12, size.unit = "pt"
    ) +
    ggplot2::scale_color_manual(
      values = c("FALSE" = onet2r_colors[["ink"]], "TRUE" = "white"),
      guide = "none"
    ) +
    ggplot2::scale_fill_gradient(low = onet2r_colors[["bg"]],
                                 high = onet2r_colors[["teal"]],
                                 limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    ggplot2::scale_x_discrete(labels = x_labels, expand = c(0, 0)) +
    ggplot2::scale_y_discrete(labels = labels, expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = "Neighbor agreement across activities, skills and software",
      subtitle = "Same eligible occupation codes; five nearest neighbors plus ties",
      x = NULL, y = NULL, fill = "Median neighbor-set overlap",
      caption = paste(
        "O*NET 30.3. IM: Importance; LV: Level. Shared neighbors / union.",
        "Native domain distances remain separate.", sep = "\n"
      )
    ) +
    onet2r_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}
