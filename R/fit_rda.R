#scale nutrient data and create a matrix column
prep_rda_data <- function(data) {
  nutr_df <-
    data %>% 
    mutate(across(c(n, ca, k, mg, p, al, b, cu, fe, mn, zn), ~scale(.x), .names = "{.col}_scaled")) %>%
    mutate(nutr = as.matrix(select(., ends_with("_scaled"))))
  return(nutr_df)
}

#fit RDA using prepped data
fit_rda <- function(nutr_df) {
  crop_rda <- rda(nutr_df["nutr"] ~ historical*current + jul_herbivory + late_herbivory, 
                  data = nutr_df,
                  scale = FALSE)
  return(crop_rda)
}
