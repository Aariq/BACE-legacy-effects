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
  longdata = read_wrangle_longdata(longdata_file),

  # Descriptive plots ------------
  longfig = make_longfig(longdata),
  growth_fig = make_growth_fig(longdata),
  tar_target(growth_fig_png,
             ggsave(here("docs", "figs", "growth.png"), growth_fig),
             format = "file"),
  tar_target(growth_fig_pdf,
             ggsave(here("docs", "figs", "growth.pdf"), growth_fig), 
             format = "file"),
  
  # establish cutoffs for exponential growth period
  oat_cutoff = ymd("2018-08-07"),
  bean_cutoff = ymd("2018-07-31"),
  
  # fit models for height change ----------
  k_ht = fit_ht(longdata %>% filter(species == "kale")),
  b_ht = fit_ht(longdata %>% filter(species == "beans") %>% filter(date <= bean_cutoff)),
  o_ht = fit_ht(longdata %>% filter(species == "oats") %>% filter(date <= oat_cutoff),
                log_trans = FALSE),
  
  #model validation
  tar_render(model_validation, "docs/model_validation.Rmd"),
  
  # plot slopes ------------
  k_slopes = plot_slopes_panel(k_ht),
  b_slopes = plot_slopes_panel(b_ht),
  o_slopes = plot_slopes_panel(o_ht, exp = FALSE),
  slopes_plot = plot_slopes(k_slopes, b_slopes, o_slopes),
  tar_target(slopes_fig_png,
             ggsave(here("docs", "figs", "slopes.png"), slopes_plot),
             format = "file"),
  tar_target(slopes_fig_pdf, 
             ggsave(here("docs", "figs", "slopes.pdf"), slopes_plot),
             format = "file"),
  
  #Longitudinal Results --------

  #Nutrient data
  tar_target(nutr_file, 
             here("data", "three_period_data_5_13_2021.xlsx"),
             format = "file"),

  ### Kale-------
  kale_nutr = read_wrangle_kale(nutr_file),
  
  ### Beans-------
  bean_nutr = read_wrangle_bean(nutr_file),
  
  ### Oats-------
  oat_nutr = read_wrangle_oat(nutr_file),
  
  #RDAs-------
  
  kale_rda = fit_rda(kale_nutr),
  bean_rda = fit_rda(bean_nutr),
  oat_rda  = fit_rda(oat_nutr),
  
  ## Score and correlation plots-------
  
  kale_score = plot_rda_scores(kale_rda, kale_nutr),
  bean_score = plot_rda_scores(bean_rda, bean_nutr),
  oat_score  = plot_rda_scores(oat_rda, oat_nutr),
  
  kale_cor = plot_rda_cor(kale_rda),
  bean_cor = plot_rda_cor(bean_rda),
  oat_cor  = plot_rda_cor(oat_rda),
  
  rda_plot = make_rda_plot(kale_score, bean_score, oat_score, kale_cor, bean_cor, oat_cor),
  tar_target(rda_plot_png,
             ggsave(here("docs", "figs", "RDA.png"),
                    rda_plot,
                    width = 6.5, height = 8),
             format = "file"),
  tar_target(rda_plot_pdf,
             ggsave(here("docs", "figs", "RDA.pdf"),
                    rda_plot,
                    width = 6.5, height = 8),
             format = "file"),
  
  # Nutrient boxplots --------
  
  kale_boxplot = plot_nutr_boxplot(kale_nutr),
  bean_boxplot = plot_nutr_boxplot(bean_nutr),
  oat_boxplot  = plot_nutr_boxplot(oat_nutr),
  
  tar_target(kale_boxplot_png,
             ggsave(here("docs", "figs", "kale_boxplot.png"), 
                    kale_boxplot,
                    width = 6.5, height = 4),
             format = "file"),
  tar_target(kale_boxplot_pdf,
             ggsave(here("docs", "figs", "kale_boxplot.pdf"), 
                    kale_boxplot,
                    width = 6.5, height = 4),
             format = "file"),
  
  tar_target(bean_boxplot_png,
             ggsave(here("docs", "figs", "bean_boxplot.png"), 
                    bean_boxplot,
                    width = 6.5, height = 4),
             format = "file"),
  tar_target(bean_boxplot_pdf,
             ggsave(here("docs", "figs", "bean_boxplot.pdf"), 
                    bean_boxplot,
                    width = 6.5, height = 4),
             format = "file"),
  
  tar_target(oat_boxplot_png,
             ggsave(here("docs", "figs", "oat_boxplot.png"), 
                    oat_boxplot,
                    width = 6.5, height = 4),
             format = "file"),
  tar_target(oat_boxplot_pdf,
             ggsave(here("docs", "figs", "oat_boxplot.pdf"), 
                    oat_boxplot,
                    width = 6.5, height = 4),
             format = "file"),
  
  # Report for coauthors ------
  tar_render(report, "docs/results.Rmd")
  
)
