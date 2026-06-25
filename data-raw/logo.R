dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

png(
  filename = "man/figures/logo.png",
  width = 2400,
  height = 2400,
  res = 300,
  bg = "transparent"
)

op <- par(mar = rep(0, 4), xaxs = "i", yaxs = "i")
plot.new()
plot.window(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), asp = 1)

theta <- seq(pi / 6, 2 * pi + pi / 6, length.out = 7)
x <- cos(theta)
y <- sin(theta)
polygon(x, y, col = "#0f766e", border = "#0b4f49", lwd = 12)
polygon(0.82 * x, 0.82 * y, col = "#f8fafc", border = NA)
polygon(0.72 * x, 0.72 * y, col = "#ecfeff", border = "#99f6e4", lwd = 4)

text(
  0,
  0.12,
  "onet2r",
  col = "#0f172a",
  cex = 6.0,
  font = 2,
  family = "sans"
)
text(
  0,
  -0.28,
  "O*NET",
  col = "#1e293b",
  cex = 3.0,
  font = 2,
  family = "sans"
)
par(op)
dev.off()
