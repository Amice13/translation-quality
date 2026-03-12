# Default theme for ggplot2
default_theme <- theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

theme_set(default_theme)
rm(default_theme)

custom_ggsave <- function(
  filename,
  plot = last_plot(),
  width = 120,
  ratio = 0.75,
  units = "mm",
  dpi = 600
) {
  ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = width * ratio,
    units = units,
    dpi = dpi,
    device = tools::file_ext(filename)
  )
}

results = list()
