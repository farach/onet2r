onet2r_colors <- c(
  teal = "#0f766e",
  mint = "#14b8a6",
  cyan = "#0ea5e9",
  slate = "#334155",
  gray = "#64748b",
  light_gray = "#cbd5e1",
  amber = "#d97706",
  rose = "#be123c",
  bg = "#fbfdfc",
  ink = "#0f172a"
)

onet2r_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = onet2r_colors[["bg"]],
        color = onet2r_colors[["light_gray"]],
        linewidth = 0.35
      ),
      panel.background = ggplot2::element_rect(
        fill = onet2r_colors[["bg"]],
        color = NA
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = "#dbe4ea",
        linewidth = 0.3
      ),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = ggplot2::element_text(
        color = onet2r_colors[["ink"]],
        face = "bold",
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        color = onet2r_colors[["slate"]],
        margin = ggplot2::margin(b = 10)
      ),
      axis.title = ggplot2::element_text(color = onet2r_colors[["slate"]]),
      axis.text = ggplot2::element_text(color = onet2r_colors[["ink"]]),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = onet2r_colors[["slate"]]),
      legend.text = ggplot2::element_text(color = onet2r_colors[["ink"]]),
      plot.margin = ggplot2::margin(14, 16, 14, 16)
    )
}

# Wrap long discrete axis labels so they do not push the panel sideways.
onet2r_wrap <- function(width = 22) {
  function(x) vapply(
    strwrap(x, width = width, simplify = FALSE),
    paste,
    character(1),
    collapse = "\n"
  )
}

onet2r_discrete_fill <- function(...) {
  ggplot2::scale_fill_manual(
    values = c(
      onet2r_colors[["teal"]],
      onet2r_colors[["mint"]],
      onet2r_colors[["cyan"]],
      onet2r_colors[["amber"]],
      onet2r_colors[["rose"]],
      onet2r_colors[["gray"]]
    ),
    ...
  )
}

onet2r_discrete_color <- function(...) {
  ggplot2::scale_color_manual(
    values = c(
      "TRUE" = onet2r_colors[["teal"]],
      "FALSE" = onet2r_colors[["rose"]],
      "Safe" = onet2r_colors[["teal"]],
      "Not safe" = onet2r_colors[["rose"]],
      "Baseline" = onet2r_colors[["slate"]],
      "Scenario" = onet2r_colors[["teal"]]
    ),
    ...
  )
}

onet_kable <- function(x, digits = 3, caption = NULL) {
  knitr::kable(
    x,
    digits = digits,
    caption = caption,
    align = "l"
  )
}
