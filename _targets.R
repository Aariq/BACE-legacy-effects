## Load your packages, e.g. library(targets).
source("./packages.R")

## Load your R files
lapply(list.files("./R", full.names = TRUE), source)

## Options
options(tidyverse.quiet = TRUE)
tar_option_set()

## tar_plan supports drake-style targets and also tar_target()
tar_plan(
  # longitudinal data
  tar_target(longdata_file, here("data","longdata.xlsx"), format = "file"),
  longdata_raw = read_excel(longdata_file, na = c(".", "*")),
  longdata_clean = wrangle_longdata(longdata_raw),
  longdata = calc_growth(longdata_clean),
  
  # eda
  longfig = make_longfig(longdata),
  tar_render(eda, "doc/eda.Rmd"),
  
  # establish cutoffs for exponential growth period
  oat_cutoff = ymd("2018-08-07"),
  bean_cutoff = ymd("2018-07-31"),
  
  #fit models
  k_ht = fit_ht(longdata %>% filter(species == "kale")),
  b_ht = fit_ht(longdata %>% filter(species == "beans") %>% filter(date <= bean_cutoff)),
  o_ht = fit_ht(longdata %>% filter(species == "oats") %>% filter(date <= oat_cutoff), log_trans = FALSE),
  
  #model validation
  tar_render(model_validation, "doc/model_validation.Rmd"),
  
  #Results
  tar_render(long_results, "doc/long_results.Rmd")
  
)
