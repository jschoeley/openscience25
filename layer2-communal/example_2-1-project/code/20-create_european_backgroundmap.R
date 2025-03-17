# Create a flat map of Europe

# Libraries -------------------------------------------------------

library(yaml)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggplot2)

# Constants -------------------------------------------------------

# input and output paths
paths <- list()
paths$input <- list(
  config = './code/_config.yaml'
  
)
paths$output <- list(
  euro_basemap.rds = './data/20-euro_basemap.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# Download Eurasian geodata ---------------------------------------

eura_sf <-
  # download geospatial data for European, Asian and African countries
  ne_countries(continent = c('europe', 'asia', 'africa'),
               returnclass = 'sf', scale = 10) %>%
  # project to crs suitable for Europe
  st_transform(crs = config$crs) %>%
  # merge into single polygon
  st_union(by_feature = FALSE) %>%
  # crop to Europe
  st_crop(xmin = config$eurocrop$xmin,
          xmax = config$eurocrop$xmax,
          ymin = config$eurocrop$ymin,
          ymax = config$eurocrop$ymax)

# Draw a basemap of Europe ----------------------------------------

euro_basemap <-
  ggplot(eura_sf) +
  geom_sf(
    aes(geometry = geometry),
    color = NA, fill = 'grey90'
  ) +
  coord_sf(expand = FALSE, datum = NA) +
  theme_void()

# Export ----------------------------------------------------------

saveRDS(
  euro_basemap,
  file = paths$output$euro_basemap.rds,
  compress = 'xz'
)
