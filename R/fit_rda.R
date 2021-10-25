#fit RDA using prepped data
fit_rda <- function(nutr_df) {
  rda(nutr_df["nutr"] ~ historical*current + jul_herbivory + late_herbivory +
        Condition(house), 
      data = nutr_df,
      scale = FALSE)
}
