read_wrangle_oat <- function(nutr_file) {
  #read in data from excel
  oat_nutr_raw  <- read_excel(nutr_file, sheet = "Oats(pot)")
  oat_plant_raw <- read_excel(nutr_file, "Oats (plant)")
  
  #calculate herbivory
  oat_herb <- calc_herbivory(oat_plant_raw, "oats")
  
  #clean up oat data
  oat_nutr_clean <- 
    oat_nutr_raw %>% 
    clean_names() %>% 
    slice(-1) %>%  #remove row with units
    rename(aug_ht = pot_mean_august_height,
           harvest_mass = grainharvest,
           mass_above = pot_mean_aboveground_mass) %>% 
    select(-id, -august_herbivory) %>% 
    #fix typo
    mutate(historical = if_else(historical == 35, 25, historical))
  
  #combine and more cleaning
  clean_nutr(oat_nutr_clean, oat_herb, "oats")
  
}

read_wrangle_bean <- function(nutr_file) {
  #read in data from excel
  bean_nutr_raw  <- read_excel(nutr_file, sheet = "Beans (pot)")
  bean_plant_raw <- read_excel(nutr_file, sheet = "Beans (plant)")
  
  #calculate herbivory
  bean_herb <- calc_herbivory(bean_plant_raw, "beans")
  
  #clean up bean data
  bean_nutr_clean <- 
    bean_nutr_raw %>% 
    clean_names() %>% 
    slice(-1) %>%  #remove row with units
    rename(current = current_moisture,
           historical = historical_treatment,
           aug_ht = august_height,
           harvest_mass = beanmass) %>% 
    select(-id, -august_herbivory)
  
  #combine and more cleaning
  clean_nutr(bean_nutr_clean, bean_herb, "beans")
  
}

read_wrangle_kale <- function(nutr_file) {
  #read in from excel
  kale_nutr_raw  <- read_excel(nutr_file, sheet = "Kale (Latepot)")
  kale_plant_raw <- read_excel(nutr_file, sheet = "Kale (plant)")
  
  #calculate herbivory
  kale_herb <- calc_herbivory(kale_plant_raw, "kale")
  
  #clean up kale data
  kale_nutr_clean <- 
    kale_nutr_raw %>% 
    clean_names() %>%
    slice(-1) %>%  #remove row with units
    rename(current = current_moisture,
           historical = historical_treatment,
           aug_ht = pot_mean_aug_height,
           mass_above = abvmass,
           mass_below = pot_mean_aug_blw,
           mass_total = total_potmean_aug) %>% 
    select(-num_range("x", 23:28), -id, -august_herbivory) %>% 
    mutate(across(starts_with("mass_"), as.numeric))
  
  #combine and more cleaning
  clean_nutr(kale_nutr_clean, kale_herb, "kale")
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
      n:mn,
      aug_ht,
      everything()
    ) %>% 
    mutate(across(n:mn, ~str_remove(.x, "<"))) %>%  #just include numbers < LOQ
    mutate(across(n:aug_ht, ~as.numeric(.x))) %>%
    # Remove 0s. 0s don't make sense, since should be reported as < LOQ
    filter(across(n:mn, ~.x>0)) %>% 
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
    mutate(across(ends_with("_id"), as.character)) %>% 
    # add "house" variable
    mutate(house = case_when(
      plot %in% 1:3 ~ "1",
      plot %in% 4:6 ~ "2",
      plot %in% 7:9 ~ "3"
    )) %>% 
    # prep for nutrient analysis by scaling and creating a matrix column
    mutate(across(c(n, ca, k, mg, p, al, b, cu, fe, mn),
                  ~scale(.x),
                  .names = "{.col}_scaled")) %>%
    mutate(nutr = as.matrix(select(., ends_with("_scaled"))))
  
  left_join(nutr_clean, herbivory_data)
}


