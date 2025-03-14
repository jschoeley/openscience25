# list packages used in project
# dput(unique(renv::dependencies()$Package))
packages <- c("dplyr", "eurostat", "lubridate", "readr", "sf", "stringi", 
              "tidyr", "yaml", "ggplot2", "rnaturalearth", "rnaturalearthdata", 
              "ggtern", "scales")
# install/update packages
install.packages(packages)

# additional packages installed outside CRAN
additional <- c(
  rnaturalearthhires = "rnaturalearthhires"
)
install.packages(
  additional["rnaturalearthhires"],
  repos = "https://ropensci.r-universe.dev",
  type = "source"
)

# write out list of dependencies and currently used versions
versions <- installed.packages()[c(packages, additional),c('Package', 'Version')]
write.csv(versions, './output/00-package_versions.csv', row.names = FALSE)
