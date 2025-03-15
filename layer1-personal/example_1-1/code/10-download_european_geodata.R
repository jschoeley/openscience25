# Download geodata for European NUTS-2 regions with added variables

# Libraries -------------------------------------------------------

library(yaml)
library(eurostat)
library(dplyr)
library(tidyr)
library(readr)
library(sf)
library(stringi)

# Constants -------------------------------------------------------

# input and output paths
paths <- list()
paths$input <- list(
  config = './code/_config.yaml'
)
paths$output <- list(
  euro_geo_nuts2.rds = './data/10-euro_geo_nuts2.rds',
  euro_education.csv = './data/10-euro_education.csv',
  euro_sectors.csv = './data/10-euro_sectors.csv'
)

# global configuration
config <- read_yaml(paths$input$config)

# Download Euro NUTS-2 geodata ------------------------------------

# download geodata on nuts-2 regions
euro_geo_nuts2 <-
  get_eurostat_geospatial(output_class = 'sf',
                          resolution = '10', nuts_level = 2, year = 2016) %>%
  # exclude some regions which don't report
  # the statistics we're interested in
  filter(!(stri_detect(geo, regex = '^AL') | stri_detect(geo, regex = '^LI') | geo == 'FI20')) %>%
  # reproject to crs suitable for Europe
  st_transform(crs = config$crs) %>%
  # pseudo-buffer regions to avoid self-intersection errors
  st_buffer(0) %>%
  # crop to Europe
  st_crop(xmin = config$eurocrop$xmin,
          xmax = config$eurocrop$xmax,
          ymin = config$eurocrop$ymin,
          ymax = config$eurocrop$ymax) %>%
  # transliterate non-ASCII characters in region names
  mutate(
    name = stri_trans_general(NUTS_NAME, id = 'any-latin; latin-ascii')
  ) %>%
  # select nuts id, region name and geometry columns
  select(id, name, geometry)

# Download Euro data on education composition ---------------------

# download data on education composition by NUTS-2 level for Europe
educ <- get_eurostat('edat_lfse_04')

# select data for 2016 and calculate shares
euro_education <-
  educ %>%
  mutate(year = lubridate::year(TIME_PERIOD),
         id = as.character(geo)) %>%
  # year 2016, total population, nuts 2 levels
  filter(year == 2016,
         stri_length(geo) == 4,
         isced11 %in% c('ED0-2', 'ED3_4', 'ED5-8'),
         sex == 'T',
         age == 'Y25-64') %>%
  mutate(values = values/100) %>%
  spread(isced11, values) %>%
  select(id, ed_0to2 = `ED0-2`, ed_3to4 = `ED3_4`, ed_5to8 = `ED5-8`) %>%
  drop_na()

# Download Euro data on labor-force composition -------------------

# download data on labor-force composition by NUTS-2 level for Europe
lf <- get_eurostat('lfst_r_lfe2en2')

# select data for 2016, recode to ternary sectors and calculate shares
euro_sectors <-
  lf %>%
  # recode time as year and geo as character
  mutate(
    year = as.integer(lubridate::year(TIME_PERIOD)),
    geo = as.character(geo)
  ) %>%
  # subset to total age, year 2016 and NUTS-2 regions
  filter(
    age == 'Y_GE15',
    stri_length(geo) == 4,
    year == 2016
  ) %>%
  # if a sector wasn't reported, assume no one worked there
  # (this is motivated by the "missing" agricultural workers in innner london)
  complete(nace_r2, geo, year, fill = list(values = 0)) %>%
  # recode into three sectors
  mutate(
    sector = recode(as.character(nace_r2),
                    `A` = 'primary',
                    `B-E` = 'secondary',
                    `F` = 'secondary'),
    sector = ifelse(!sector %in% c('primary', 'secondary', 'TOTAL'),
                    'tertiary',
                    sector)
  ) %>%
  group_by(year, geo, sector) %>%
  summarise(N = sum(values, na.rm = TRUE)) %>%
  ungroup() %>%
  # calculate shares on total
  spread(sector, N) %>%
  mutate_at(vars(primary, secondary, tertiary), .funs = ~ ./TOTAL) %>%
  # simplify
  select(id = geo, lf_pri = primary, lf_sec = secondary, lf_ter = tertiary) %>%
  drop_na()

# Export ----------------------------------------------------------

write_csv(euro_education, file = paths$output$euro_education.csv)
write_csv(euro_sectors, file = paths$output$euro_sectors.csv)
saveRDS(
  euro_geo_nuts2,
  file = paths$output$euro_geo_nuts2.rds,
  compress = 'xz', version = 2
)
