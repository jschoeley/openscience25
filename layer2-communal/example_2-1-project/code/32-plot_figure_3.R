# Create Figure 3
# Different representations of the color key for the
# (centered) ternary balance scheme showing the workforce composition by
# region in Europe, 2016.

# Libraries -------------------------------------------------------

library(yaml)
library(dplyr)
library(ggplot2)
library(ggtern)

# Constants -------------------------------------------------------

# input and output paths
paths <- list()
paths$input <- list(
  config = './code/_config.yaml',
  ternary_functions.R = './code/_define_ternary_functions.R',
  euro_sectors_colors_noncentered.rds = './output/31-euro_sectors_colors_noncentered.rds',
  euro_sectors_colors_centered.rds = './output/31-euro_sectors_colors_centered.rds'
)
paths$output <- list(
  figure3a.pdf = './output/32-figure3a.pdf',
  figure3b.pdf = './output/32-figure3b.pdf',
  figure3c.pdf = './output/32-figure3c.pdf',
  figure3d.pdf = './output/32-figure3d.pdf',
  figure3e.pdf = './output/32-figure3e.pdf'
)

# global functions
source(paths$input$ternary_functions.R)

# global configuration
config <- read_yaml(paths$input$config)

# Load data -------------------------------------------------------

euro_sectors_colors_noncentered <-
  readRDS(paths$input$euro_sectors_colors_noncentered.rds)

euro_sectors_colors_centered <-
  readRDS(paths$input$euro_sectors_colors_centered.rds)

# eu center
eu_center <- Centre(euro_sectors_colors_noncentered[,1:3])

# Figure 3A ---------------------------------------------------------------

# Non centered legend

legend_style_a <-
  ColorKey(
    h_ = config$colorscale$hue,
    c_ = config$colorscale$chroma,
    l_ = config$colorscale$lightness,
    contrast = config$colorscale$contrast,
    center = config$colorscale$barycenter
  ) +
  # grid
  lline(
    Lintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  tline(
    Tintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  rline(
    Rintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  # center
  lline(Lintercept = eu_center[1]) +
  tline(Tintercept = eu_center[2]) +
  rline(Rintercept = eu_center[3]) +
  geom_point(
    aes(x = p1, y = p2, z = p3),
    alpha = 0.2, size = 1,
    data = euro_sectors_colors_noncentered
  ) +
  labs(x = '% primary', y = '% secondary', z = '% ternary')

# Figure 3B ---------------------------------------------------------------

# Transformed data on standard grid

legend_style_b <-
  ColorKey(
    h_ = config$colorscale$hue,
    c_ = config$colorscale$chroma,
    l_ = config$colorscale$lightness,
    contrast = config$colorscale$contrast,
    center = config$colorscale$barycenter
  ) +
  lline(
    Lintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  tline(
    Tintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  rline(
    Rintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  geom_point(
    aes(x = cp1, y = cp2, z = cp3),
    alpha = 0.2, size = 1,
    data = euro_sectors_colors_centered
  ) +
  labs(x = '% yellow', y = '% cyan', z = '% magenta')


# Figure 3C -----------------------------------------------------

# Transformed data on transformed gridlines

# transformed grid
gridlines_pct_diff <- TernaryGridCentered(eu_center, N = 10)

legend_style_c <-
  ColorKey(
    h_ = config$colorscale$hue,
    c_ = config$colorscale$chroma,
    l_ = config$colorscale$lightness,
    contrast = config$colorscale$contrast,
    center = config$colorscale$barycenter
  ) +
  geom_segment(
    aes(
      x = L_from, xend = L_to,
      y = T_from, yend = T_to,
      z = R_from, zend = R_to,
      alpha = center
    ),
    show.legend = FALSE,
    data = gridlines_pct_diff$centered_grid
  ) +
  geom_point(aes(x = cp1, y = cp2, z = cp3),
             alpha = 0.2, size = 1,
             data = euro_sectors_colors_centered) +
  scale_L_continuous(
    breaks =
      gridlines_pct_diff$centered_breaks$L,
    labels =
      paste0(formatC(gridlines_pct_diff$labels$L*100, format = 'd', flag = '+'),
             ' (', round(gridlines_pct_diff$non_centered_breaks$L*100, 0), ')')
  ) +
  scale_T_continuous(
    breaks =
      gridlines_pct_diff$centered_breaks$T,
    labels =
      paste0(formatC(gridlines_pct_diff$labels$T*100, format = 'd', flag = '+'),
             ' (', round(gridlines_pct_diff$non_centered_breaks$T*100, 0), ')')
  ) +
  scale_R_continuous(
    breaks =
      gridlines_pct_diff$centered_breaks$R,
    labels =
      paste0(formatC(gridlines_pct_diff$labels$R*100, format = 'd', flag = '+'),
             ' (', round(gridlines_pct_diff$non_centered_breaks$R*100, 0), ')')
  ) +
  scale_alpha_manual(values = c(`FALSE` = 0.2, `TRUE` = 1)) +
  labs(
    x = '%pt. diff. primary (%)',
    y = '%pt. diff. secondary (%)',
    z = '%pt. diff. ternary (%)'
  )

# Figure 3D -----------------------------------------------------

legend_style_d <-
  ColorKey(
    h_ = config$colorscale$hue,
    c_ = config$colorscale$chroma,
    l_ = config$colorscale$lightness,
    contrast = config$colorscale$contrast,
    center = eu_center
  ) +
  # grid
  lline(
    Lintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  tline(
    Tintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  rline(
    Rintercept = config$colorscale$grid_intercept,
    size = config$colorscale$grid_size,
    color = config$colorscale$grid_color
  ) +
  # center
  lline(Lintercept = eu_center[1]) +
  tline(Tintercept = eu_center[2]) +
  rline(Rintercept = eu_center[3]) +
  geom_point(aes(x = cp1, y = cp2, z = cp3),
             alpha = 0.2, size = 1,
             data = euro_sectors_colors_noncentered) +
  labs(x = '% primary', y = '% secondary', z = '% ternary')

# Figure 3E -----------------------------------------------------

euro_sectors_colors_alr_transformed <-
  AlrTransform(
    as.matrix(euro_sectors_colors_centered[,4:6])
  )
colnames(euro_sectors_colors_alr_transformed) <- c('x', 'y')
euro_sectors_colors_centered <-
  bind_cols(euro_sectors_colors_centered,
            as.data.frame(euro_sectors_colors_alr_transformed))

legend_style_e <-
  euro_sectors_colors_centered %>%
  filter(is.finite(x)) %>%
  ggplot() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(
    aes(x = x, y = y, fill = hexsrgb),
    shape = 21, color = 'black',
    size = 3
  ) +
  scale_x_continuous(
    expression(log(p[pri]/p[ter])-log(c[pri]/c[ter])),
    breaks = scales::breaks_width(1)
  ) +
  scale_y_continuous(
    expression(log(p[sec]/p[ter])-log(c[sec]/c[ter])),
    breaks = scales::breaks_width(1)
  ) +
  scale_fill_identity() +
  theme_minimal() +
  theme(
    panel.grid.minor =
      element_blank(),
    panel.grid.major =
      element_line(color = 'black', size = 0.2)
  ) +
  theme(
    panel.background = element_rect(fill = 'grey30', color = NA)
  ) +
  coord_equal(clip = 'off')

# Export ----------------------------------------------------------

ggsave(
  filename = paths$output$figure3a.pdf,
  plot = legend_style_a,
  width = 6, height = 6
)

ggsave(
  filename = paths$output$figure3b.pdf,
  plot = legend_style_b,
  width = 6, height = 6
)

ggsave(
  filename = paths$output$figure3c.pdf,
  plot = legend_style_c,
  width = 6, height = 6
)

ggsave(
  filename = paths$output$figure3d.pdf,
  plot = legend_style_d,
  width = 6, height = 6
)

ggsave(
  filename = paths$output$figure3e.pdf,
  plot = legend_style_e,
  width = 6, height = 6
)
