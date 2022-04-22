# BACE-legacy-effects

<!-- badges: start -->

<!-- badges: end -->

Data analysis for a project at the Boston Area Climate Experiment (BACE) looking at legacy effects of precipitation treatments on crop growth and nutrients.

[Report for co-authors](https://aariq.github.io/BACE-legacy-effects/)

# Reproducibility

This research compendium makes use of the [`targets`](https://books.ropensci.org/targets/) package for a reproducible workflow and the [`renv`](https://rstudio.github.io/renv/articles/renv.html) package for managing R package dependencies.

### Steps to reproduce this analysis:

1. Clone this repository
2. Open it in RStudio
3. Install dependencies by running `renv::restore()`
4. Run analysis pipeline with `targets::tar_make()`

To reproduce the analysis, fork this repository, clone it, open it in RStudio, install the `targets` package as well as all packages in `packages.R`. Then run `targets::tar_make()`.