# Download analysis input data from Zenodo

download.file(
  url = 'https://zenodo.org/records/15033155/files/10-euro_education.csv?download=1',
  destfile = './data/10-euro_education.csv'
)
download.file(
  url = 'https://zenodo.org/records/15033155/files/10-euro_sectors.csv?download=1',
  destfile = './data/10-euro_sectors.csv'
)
download.file(
  url = 'https://zenodo.org/records/15033155/files/10-euro_geo_nuts2.rds?download=1',
  destfile = './data/10-euro_geo_nuts2.rds'
)
