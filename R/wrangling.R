wrangle_longdata <- function(longdata_raw) {
  longdata_raw %>% 
    slice(-1) %>% 
    select(-"...10") %>% 
    janitor::clean_names() %>% 
    #get column types right
    mutate(july_herbivory_count = as.integer(july_herbivory_count),
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
    mutate(growth = height - lag(height))
  return(out)
}
