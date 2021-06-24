wrangle_longdata <- function(longdata_raw) {
  longdata_raw %>% 
    slice(-1) %>% 
    select(-"...10") %>% 
    janitor::clean_names() %>% 
    #get column types right
    mutate(july_herbivory_count = as.integer(july_herbivory_count),
           july_herbivory = case_when(july_herbivory_count == 0L ~ 0L,
                                      july_herbivory_count > 0L ~ 1L,
                                      TRUE ~ NA_integer_),
           height = as.double(height),
           across(c(plant_species, plot, curr_treat, hist_treat, pot, plant), as.factor)) %>% 
    rename(date = week, species = plant_species) %>% 
    #days since start date
    mutate(days = (date - min(date)) %>% as.period() %>% day()) %>% 
    #recode variables
    mutate(species = fct_recode(
      species,
      kale = "k",
      beans = "b",
      oats = "o"
    )) %>%
    mutate(
      curr_treat = fct_recode(
        curr_treat,
        "ambient" = "0",
        "75%" = "25",
        "50%" = "50"
      ),
      hist_treat = fct_recode(
        hist_treat,
        "high" = "0",
        "medium" = "25",
        "low" = "50"
      )
    ) %>%
    #make unique pot and plant IDs
    mutate(
      plot_id = glue::glue(
        "{species}_plot{plot}_{hist_treat}"
      ),
      pot_id = glue::glue(
        "{plot_id}_pot{pot}"
      ),
      plant_id = glue::glue(
        "{pot_id}_{plant}"
      )
    ) %>%
    mutate(across(ends_with("_id"), as.character))
}

calc_growth <- function(longdata) {
  out <- longdata %>%
    group_by(species, plant_id) %>%
    arrange(date) %>% 
    mutate(growth = height - lag(height)) %>% 
    ungroup()
  return(out)
}


clean_oat_nutr <- function(oat_nutr_raw) {
  oat_nutr_raw %>% 
    clean_names() %>% 
    slice(-1) %>%  #remove row with units
    rename(aug_ht = pot_mean_august_height,
           aug_cum_herbivory_mean = august_herbivory,
           harvest_mass = grainharvest,
           mass_above = pot_mean_aboveground_mass) %>% 
    select(-id) %>% 
    #fix typo
    mutate(historical = if_else(historical == 35, 25, historical))
}
  

clean_bean_nutr <- function(bean_nutr_raw) {
  bean_nutr_raw %>% 
    clean_names() %>% 
    slice(-1) %>%  #remove row with units
    rename(current = current_moisture,
           historical = historical_treatment,
           aug_ht = august_height,
           aug_cum_herbivory_mean = august_herbivory,
           harvest_mass = beanmass) %>% 
    select(-id)
}


clean_kale_nutr <- function(kale_nutr_raw) {
  kale_nutr_raw %>% 
    clean_names() %>%
    slice(-1) %>%  #remove row with units
    rename(current = current_moisture,
           historical = historical_treatment,
           aug_ht = pot_mean_aug_height,
           aug_cum_herbivory_mean = august_herbivory,
           mass_above = abvmass,
           mass_below = pot_mean_aug_blw,
           mass_total = total_potmean_aug) %>% 
    select(-num_range("x", 23:28), -id)
}
  
clean_nutr <- function(nutr, longdata){
  jul_herb_summary <-
    longdata %>%
    select(pot_id, july_herbivory_count, july_herbivory) %>% 
    group_by(pot_id) %>% 
    summarize(jul_herbivory_mean = mean(july_herbivory_count)) %>% 
    mutate(jul_herbivory = ifelse(jul_herbivory_mean > 0, "some", "none"))
  
  nutr_clean <-
    nutr %>% 
    select(
      current,
      historical,
      plot,
      pot_number,
      maine_id,
      n:zn,
      aug_ht,
      aug_cum_herbivory_mean,
      everything()
    ) %>% 
    mutate(across(n:zn, ~str_remove(.x, "<"))) %>%  #just include numbers < LOQ
    mutate(across(n:aug_cum_herbivory_mean, ~as.numeric(.x))) %>%
    #add binary column for pot-level herbivory mean.
    #1 = at least one plant had at least 1 herbivory event
    mutate(aug_cum_herbivory = ifelse(aug_cum_herbivory_mean > 0, "some", "none"), .after = aug_cum_herbivory_mean) %>% 
    mutate(
      current = fct_recode(
        as.character(current),
        "ambient" = "0",
        "75%" = "25",
        "50%" = "50"
      ),
      historical = fct_recode(
        as.character(historical),
        "high" = "0",
        "medium" = "25",
        "low" = "50"
      )
    ) %>%
    mutate(pot_number = str_extract(pot_number, "\\d+")) %>%
    mutate(
      plot_id = glue::glue(
        "kale_plot{plot}_{historical}"
      ),
      pot_id = glue::glue(
        "{plot_id}_pot{pot_number}"
      )
    ) %>%
    mutate(across(ends_with("_id"), as.character))
  
  left_join(nutr_clean, jul_herb_summary)
}
