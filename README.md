# BACE-legacy-effects

<!-- badges: start -->
[![DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.agee.2023.108513-blue)](https://doi.org/10.1016/j.agee.2023.108513)
[![code](https://zenodo.org/badge/370156054.svg)](https://zenodo.org/badge/latestdoi/370156054)

<!-- badges: end -->

Data analysis for a project at the Boston Area Climate Experiment (BACE) looking at legacy effects of precipitation treatments on crop growth and nutrients.


When using this code or data, please cite the archive of this repository as well as the associated publication:

>Scott, E. R., Jackson, E. D., Casolaro, C., Nebeker, R. S., Dukes, J. S., Griffin, T., & Orians, C. M. (2023). Code and Data from: Current and Legacy Effects of Precipitation Treatments on Growth and Nutrition in Contrasting Crops (Version v1.0) [Computer software]. https://doi.org/10.5281/zenodo.6478160

>Jackson, E.D., Casolaro, C., Nebeker, R.S., Scott, E.R., Dukes, J.S., Griffin, T.S., Orians, C.M., 2023. Current and legacy effects of precipitation treatments on growth and nutrition in contrasting crops. Agriculture, Ecosystems & Environment 352, 108513. https://doi.org/10.1016/j.agee.2023.108513


# Reproducibility

This research compendium makes use of the [`targets`](https://books.ropensci.org/targets/) package for a reproducible workflow and the [`renv`](https://rstudio.github.io/renv/articles/renv.html) package for managing R package dependencies.

### Steps to reproduce this analysis:

1. Clone this repository
2. Open it in RStudio
3. Install dependencies by running `renv::restore()` (NOTE: you may need to install `gfortran` if you don't have it in order to compile some packages from source.  Install from here: https://cran.r-project.org/bin/macosx/tools/)
4. Run analysis pipeline with `targets::tar_make()`

To reproduce the analysis, fork this repository, clone it, open it in RStudio, install the `targets` package as well as all packages in `packages.R`. Then run `targets::tar_make()`.
