# Schöley (2021): The centered ternary balance scheme

[![Paper DOI](https://img.shields.io/badge/Paper_DOI-10.4054/DemRes.2021.44.19-%23005462?style=flat-square)](https://doi.org/10.4054/DemRes.2021.44.19)


This is a repository to accompany 'The centered ternary balance scheme', published in [Demographic Research](https://doi.org/10.4054/DemRes.2021.44.19).

## Structure

- `./code` code to replicate the data transformation, analysis and visualisation
- `./data` input data
- `./fig` edited figures for publication
- `./output` output data and figures

## Running the Code

As a pre-requisite to running this locally, you will need a working installation of [**R**](https://www.r-project.org/) with all of the necessary dependencies (see `./code/00-install_dependencies.R`).

To run this code, execute each of the scripts in ascending numeric order, which will undertake sequential tasks recreating the plots featured in the paper. Output files are tagged with the same numeric prefix as the source file generating the file.

- `00-install_dependencies.R` Install the required R packages
- `10-download_european_geodata.R` Download the geodata used for the examples in the paper.
- `20-create_european_backgroundmap.R` Create a map of Eurasia to be used as a background for the plots created in later steps.
- `30-plot_figure_1.R` Create Figure 1. Demonstration of the ternary balance scheme showing the composition of educational attainment by region in Europe, 2016.
- `31-plot_figure_2.R` Create Figure 2. Demonstration of the centered ternary balance scheme in comparison with the non-centered scheme showing the workforce composition by region in Europe, 2016.
- `32-plot_figure_3.R` Create Figure 3. Different representations of the color key for the (centered) ternary balance scheme showing the workforce composition by region in Europe, 2016.

The following scripts are sourced by the analysis:

- `_config.yaml` Global configuration for the analysis.
- `_define_ternary_functions` Functions implementing the centered ternary balance scheme.