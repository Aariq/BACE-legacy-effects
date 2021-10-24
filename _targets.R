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
  
  # Descriptive plots ------------
  longfig = make_longfig(longdata),
  growth_fig = make_growth_fig(longdata),
  tar_target(growth_fig_png, ggsave(here("doc", "figs", "growth.png"), growth_fig), format = "file"),
  tar_target(growth_fig_pdf, ggsave(here("doc", "figs", "growth.pdf"), growth_fig), format = "file"),
  
  # establish cutoffs for exponential growth period
  oat_cutoff = ymd("2018-08-07"),
  bean_cutoff = ymd("2018-07-31"),
  
  # fit models for height change ----------
  k_ht = fit_ht(longdata %>% filter(species == "kale")),
  b_ht = fit_ht(longdata %>% filter(species == "beans") %>% filter(date <= bean_cutoff)),
  o_ht = fit_ht(longdata %>% filter(species == "oats") %>% filter(date <= oat_cutoff), log_trans = FALSE),
  
  #model validation
  # tar_render(model_validation, "doc/model_validation.Rmd"),
  
  # plot slopes ------------
  k_slopes = plot_slopes_panel(k_ht),
  b_slopes = plot_slopes_panel(b_ht),
  o_slopes = plot_slopes_panel(o_ht, exp = FALSE),
  slopes_plot = plot_slopes(k_slopes, b_slopes, o_slopes),
  tar_target(slopes_fig_png, ggsave(here("doc", "figs", "slopes.png"), slopes_plot), format = "file"),
  tar_target(slopes_fig_pdf, ggsave(here("doc", "figs", "slopes.pdf"), slopes_plot), format = "file"),
  
  #Longitudinal Results --------

  #Nutrient data
  tar_target(nutr_file, here("data", "three_period_data_5_13_2021.xlsx"), format = "file"),

  ### Kale-------
  kale_nutr_raw = read_excel(nutr_file, sheet = "Kale (Latepot)"),
  kale_plant_raw = read_excel(nutr_file, sheet = "Kale (plant)"),
  kale_herb = calc_herbivory(kale_plant_raw, "kale"),
  kale_nutr_clean = clean_kale_nutr(kale_nutr_raw),
  kale_nutr = clean_nutr(kale_nutr_clean, kale_herb, "kale"),
  
  ### Beans-------
  bean_nutr_raw = read_excel(nutr_file, sheet = "Beans (pot)"),
  bean_plant_raw = read_excel(nutr_file, sheet = "Beans (plant)"),
  bean_herb = calc_herbivory(bean_plant_raw, "beans"),
  bean_nutr_clean = clean_bean_nutr(bean_nutr_raw),
  bean_nutr = clean_nutr(bean_nutr_clean, bean_herb, "beans"),
  
  ### Oats-------
  oat_nutr_raw = read_excel(nutr_file, sheet = "Oats(pot)"),
  oat_plant_raw = read_excel(nutr_file, "Oats (plant)"),
  oat_herb = calc_herbivory(oat_plant_raw, "oats"),
  oat_nutr_clean = clean_oat_nutr(oat_nutr_raw),
  oat_nutr = clean_nutr(oat_nutr_clean, oat_herb, "oats"),
  
  #RDAs-------
  
  kale_rda = fit_rda(kale_nutr),
  bean_rda = fit_rda(bean_nutr),
  oat_rda = fit_rda(oat_nutr),
  
  ## Score and correlation plots-------
  
  kale_score = plot_rda_scores(kale_rda, kale_nutr),
  bean_score = plot_rda_scores(bean_rda, bean_nutr),
  oat_score = plot_rda_scores(oat_rda, oat_nutr),
  
  kale_cor = plot_rda_cor(kale_rda),
  bean_cor = plot_rda_cor(bean_rda),
  oat_cor = plot_rda_cor(oat_rda),
  
  rda_plot = make_rda_plot(kale_score, bean_score, oat_score, kale_cor, bean_cor, oat_cor),
  
  # Nutrient boxplots --------
  kale_boxplot = plot_nutr_b oxplot(kale_nutr),
  bean_boxplot = plot_nutr_boxplot(bean_nutr),
  oat_boxplot = plot_nutr_boxplot(oat_nutr),
  # One report to rule them all
  # tar_render(report, "doc/index.Rmd")
  
)
