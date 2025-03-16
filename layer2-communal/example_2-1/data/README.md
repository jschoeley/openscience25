# Data to reproduce Schöley (2021): The centered ternary balance scheme

[![Data DOI](https://img.shields.io/badge/Zenodo_DOI-10.5281/zenodo.15033155-%23005462?style=flat-square)](https://doi.org/10.5281/zenodo.15033155)

**Data are not committed but can be downloaded from the [Zenodo repository](https://doi.org/10.5281/zenodo.15033155).**

Jonas Schöley (2021). The centered ternary balance scheme: A technique to visualize surfaces of unbalanced three-part compositions [10.4054/DemRes.2021.44.19](https://www.demographic-research.org/articles/volume/44/19)

- `10-euro_education.csv`: Relative share of population ages 25 to 64 by educational attainment in the European NUTS-2 regions 2016. Data derived from Eurostat table "edat_lfse_04".
  - `id`: NUTS-2 code
  - `ed_0to2`: Share of population with highest attained education "lower secondary or less".
  - `ed_3to4`: Share of population with highest attained education "upper secondary".
  - `ed_5to8`: Share of population with highest attained education "tertiary".
- `10-euro_sectors.csv`: Relative share of workers by labor-force sector in the European NUTS-2 regions 2016. The original NACE (rev. 2) codes have been recoded into the three sectors "primary" (A), "secondary" (B-E & F) and "tertiary" (all other NACE codes). Data derived from Eurostat table "lfst_r_lfe2en2".
  - `id`: NUTS-2 code
  - `lf_pri`: Share of labor-force in primary sector.
  - `lf_sec`: Share of labor-force in secondary sector.
  - `lf_ter`: Share of labor-force in tertiary sector.
- `10-euro_geo_nuts2.rds`: A [simple-features](https://cran.r-project.org/package=sf) dataframe containing the NUTS-2 level polygons of European regions. Derived from Eurostat European Geodata. (c) EuroGeographics for the administrative boundaries (http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/).
  - `id`: NUTS-2 code.
  - `name`: Name of NUTS-2 region.
  - `geometry`: Polygon outlines for regions in `sf` package format.
- `20-euro_basemap.rds`: A `ggplot` object representing a simple map of Europe.
