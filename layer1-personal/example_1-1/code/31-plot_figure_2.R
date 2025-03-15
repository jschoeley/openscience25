# Create Figure 2
# Demonstration of the centered ternary balance scheme in comparison
# with the non-centered scheme showing the workforce composition by
# region in Europe, 2016

# Libraries -------------------------------------------------------

library(yaml)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(ggtern)

# Constants -------------------------------------------------------

# input and output paths
paths <- list()
paths$input <- list(
  config = './code/_config.yaml',
  ternary_functions.R = './code/_define_ternary_functions.R',
  euro_basemap.rds = './data/20-euro_basemap.rds',
  euro_sectors.csv = './data/10-euro_sectors.csv',
  euro_geo_nuts2.rds = './data/10-euro_geo_nuts2.rds'
)
paths$output <- list(
  figure2a.pdf = './output/31-figure2a.pdf',
  figure2b.pdf = './output/31-figure2b.pdf',
  euro_sectors_colors_noncentered.rds = './output/31-euro_sectors_colors_noncentered.rds',
  euro_sectors_colors_centered.rds = './output/31-euro_sectors_colors_centered.rds'
)

# global functions
source(paths$input$ternary_functions.R)

# global configuration
config <- read_yaml(paths$input$config)

# Load data -------------------------------------------------------

euro_basemap <- readRDS(paths$input$euro_basemap.rds)
euro_sectors <- read_csv(paths$input$euro_sectors.csv)
euro_geo_nuts2 <- readRDS(paths$input$euro_geo_nuts2.rds)

# Calculate center of EU labor force composition ------------------

eu_center <- Centre(euro_sectors[,2:4])

# Plot Figure 2A --------------------------------------------------

# TBS map of regional labor force distribution in Europe

# perform color coding
euro_sectors_colors_noncentered <-
  ColorMap(
    as.matrix(euro_sectors[,2:4]),
    h_ = config$colorscale$hue,
    c_ = config$colorscale$chroma,
    l_ = config$colorscale$lightness,
    contrast = config$colorscale$contrast,
    center = config$colorscale$barycenter
  ) %>%
  mutate(id = euro_sectors$id)

# generate color key
plot_euro_sectors_key <-
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
  lline(
    Lintercept = eu_center[1],
    size = config$colorscale$grid_size*5,
    color = config$colorscale$grid_color
  ) +
  tline(
    Tintercept = eu_center[2],
    size = config$colorscale$grid_size*5,
    color = config$colorscale$grid_color
  ) +
  rline(
    Rintercept = eu_center[3],
    size = config$colorscale$grid_size*5,
    color = config$colorscale$grid_color
  ) +
  geom_point(
    aes(x = p1, y = p2, z = p3),
    alpha = 0.2, size = 0.1,
    data = euro_sectors_colors_noncentered
  ) +
  labs(
    L = '% Primary', T = '% Secondary', R = '% Tertiary'
  ) +
  theme(text = element_text(size = 7))

# join geographic data with color information
euro_sectors_map <-
  left_join(euro_geo_nuts2, euro_sectors_colors_noncentered, by = 'id')

# generate choropleth map
plot_euro_sectors_map <-
  euro_basemap +
  geom_sf(
    aes(
      geometry = geometry,
      fill = hexsrgb
    ),
    color = NA,
    data = euro_sectors_map
  ) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      plot_euro_sectors_key +
        theme(plot.background = element_rect(fill = NA, color = NA))
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(title = 'A')

# Plot Figure 2B --------------------------------------------------

# Centered TBS map of regional labor force distribution in Europe

# transformed grid
gridlines_pct <-
  TernaryGridCentered(
    eu_center, N = 5, relative_grid = FALSE
  )

# perform color coding
euro_sectors_colors_centered <-
  ColorMap(
    as.matrix(euro_sectors[,2:4]),
    h_ = config$colorscale$hue,
    c_ = config$colorscale$chroma,
    l_ = config$colorscale$lightness,
    contrast = config$colorscale$contrast,
    center = eu_center
  ) %>%
  mutate(id = euro_sectors$id)

# generate color key
plot_euro_sectors_key_centered <-
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
    data = gridlines_pct$centered_grid
  ) +
  geom_point(
    aes(x = cp1, y = cp2, z = cp3),
    alpha = 0.2, size = 0.1,
    data = euro_sectors_colors_centered
  ) +
  scale_L_continuous(
    breaks = gridlines_pct$centered_breaks$L,
    labels = formatC(gridlines_pct$labels$L, format = 'f', digits = 2)
  ) +
  scale_T_continuous(
    breaks = gridlines_pct$centered_breaks$T,
    labels = formatC(gridlines_pct$labels$T, format = 'f', digits = 2)
  ) +
  scale_R_continuous(
    breaks = gridlines_pct$centered_breaks$R,
    labels = formatC(gridlines_pct$labels$R, format = 'f', digits = 2)
  ) +
  labs(
    L = '% Primary', T = '% Secondary', R = '% Tertiary'
  ) +
  theme(text = element_text(size = 7))

# join geographic data with color information
euro_sectors_map_centered <-
  left_join(euro_geo_nuts2, euro_sectors_colors_centered, by = 'id')

# generate choropleth map
plot_euro_sectors_map_centered <-
  euro_basemap +
  theme_void() +
  geom_sf(
    aes(
      geometry = geometry,
      fill = hexsrgb
    ),
    color = NA,
    data = euro_sectors_map_centered
  ) +
  scale_fill_identity() +
  annotation_custom(
    ggplotGrob(
      plot_euro_sectors_key_centered +
        theme(plot.background = element_rect(fill = NA, color = NA))
    ),
    xmin = 53e5, xmax = Inf, ymin = 35e5, ymax = Inf) +
  labs(title = 'B')

# Export ----------------------------------------------------------

ggsave(
  filename = paths$output$figure2a.pdf,
  plot = plot_euro_sectors_map,
  width = 6, height = 6
)

ggsave(
  filename = paths$output$figure2b.pdf,
  plot = plot_euro_sectors_map_centered,
  width = 6, height = 6
)

saveRDS(
  euro_sectors_colors_noncentered,
  paths$output$euro_sectors_colors_noncentered.rds
)

saveRDS(
  euro_sectors_colors_centered,
  paths$output$euro_sectors_colors_centered.rds
)
