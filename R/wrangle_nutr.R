clean_oat_nutr <- function(oat_nutr_raw) {
  oat_nutr_raw %>% 
    clean_names() %>% 
    slice(-1) %>%  #remove row with units
    rename(aug_ht = pot_mean_august_height,
           harvest_mass = grainharvest,
           mass_above = pot_mean_aboveground_mass) %>% 
    select(-id, -august_herbivory) %>% 
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
           harvest_mass = beanmass) %>% 
    select(-id, -august_herbivory)
}


clean_kale_nutr <- function(kale_nutr_raw) {
  kale_nutr_raw %>% 
    clean_names() %>%
    slice(-1) %>%  #remove row with units
    rename(current = current_moisture,
           historical = historical_treatment,
           aug_ht = pot_mean_aug_height,
           mass_above = abvmass,
           mass_below = pot_mean_aug_blw,
           mass_total = total_potmean_aug) %>% 
    select(-num_range("x", 23:28), -id, -august_herbivory)
}


calc_herbivory <- function(data_raw, species) {
  data_raw %>% 
    slice(-1) %>% 
    clean_names() %>% 
    rename(historical = historical_treatment,
           current = current_treatment) %>% 
    mutate(across(ends_with("count"), as.numeric)) %>% 
    mutate(late_herbivory = august_herbivory_count - july_herbivory_count) %>% 
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
        "{species}_plot{plot}_{historical}"
      ),
      pot_id = glue::glue(
        "{plot_id}_pot{pot_number}"
      )
    ) %>%
    mutate(across(ends_with("_id"), as.character)) %>% 
    group_by(pot_id) %>% 
    summarize(jul_herbivory_mean = mean(july_herbivory_count),
              late_herbivory_mean = mean(late_herbivory)) %>% 
    mutate(jul_herbivory = ifelse(jul_herbivory_mean > 0, "some", "none"),
           late_herbivory = ifelse(late_herbivory_mean > 0, "some", "none"))
}


clean_nutr <- function(nutr, herbivory_data, species){
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
      everything()
    ) %>% 
    mutate(across(n:zn, ~str_remove(.x, "<"))) %>%  #just include numbers < LOQ
    mutate(across(n:aug_ht, ~as.numeric(.x))) %>%
    #add binary column for pot-level herbivory mean.
    #1 = at least one plant had at least 1 herbivory event
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
        "{species}_plot{plot}_{historical}"
      ),
      pot_id = glue::glue(
        "{plot_id}_pot{pot_number}"
      )
    ) %>%
    mutate(across(ends_with("_id"), as.character))
  
  left_join(nutr_clean, herbivory_data)
}
