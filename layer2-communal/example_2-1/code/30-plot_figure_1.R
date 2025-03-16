# Create Figure 1
# Demonstration of the ternary balance scheme showing the composition of
# educational attainment by region in Europe, 2016

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
  euro_education.csv = './data/10-euro_education.csv',
  euro_geo_nuts2.rds = './data/10-euro_geo_nuts2.rds'
)
paths$output <- list(
  figure1a.pdf = './output/30-figure1a.pdf',
  figure1b.pdf = './output/30-figure1b.pdf'
)

# global functions
source(paths$input$ternary_functions.R)

# global configuration
config <- read_yaml(paths$input$config)

# Load data -------------------------------------------------------

euro_basemap <- readRDS(paths$input$euro_basemap.rds)
euro_education <- read_csv(paths$input$euro_education.csv)
euro_geo_nuts2 <- readRDS(paths$input$euro_geo_nuts2.rds)

# Figure 1A -------------------------------------------------------

# Ternary diagram of regional education levels in Europe

# generate color key
plot_euro_education_key <-
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
    aes(x = ed_0to2, y = ed_3to4, z = ed_5to8),
    shape = 21,
    data = euro_education
  ) +
  labs(
    L = '% Lower secondary\nor less',
    T = '% Upper secondary',
    R = '% Tertiary',
    title = 'A'
  ) +
  theme_arrowlarge()

# Figure 1B ---------------------------------------------------------------

# Ternary balance scheme map of regional education
# levels in Europe

# perform color coding
euro_education_colors <-
  ColorMap(
    as.matrix(euro_education[,2:4]),
    h_ = config$colorscale$hue,
    c_ = config$colorscale$chroma,
    l_ = config$colorscale$lightness,
    contrast = config$colorscale$contrast,
    center = config$colorscale$barycenter
  ) %>%
  mutate(
    id = euro_education$id
  )

euro_education_map <-
  left_join(euro_geo_nuts2, euro_education_colors, by = 'id')

plot_euro_education_map <-
  euro_basemap +
  geom_sf(
    aes(
      geometry = geometry,
      fill = hexsrgb
    ),
    color = NA,
    data = euro_education_map
  ) +
  scale_fill_identity() +
  labs(title = 'B')

# Export ----------------------------------------------------------

ggsave(
  filename = paths$output$figure1a.pdf,
  plot = plot_euro_education_key,
  width = 6, height = 6
)

ggsave(
  filename = paths$output$figure1b.pdf,
  plot = plot_euro_education_map,
  width = 6, height = 6
)
