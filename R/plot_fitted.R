my_augment <- function(model) {
  mf <- model.frame(model)
  newdata <-  expand_grid(hist_treat = factor(levels(mf$hist_treat), levels(mf$hist_treat)),
                          curr_treat = factor(levels(mf$curr_treat), levels(mf$curr_treat)),
                          days = seq(min(mf$days), max(mf$days), length.out = 50),
                          july_herbivory_count = 0,
                          plot_id = "newlevel",
                          pot_id = "newlevel",
                          plant_id = "newlevel")
  plotdf <- augment(model, newdata = newdata, allow.new.levels = TRUE)
  return(plotdf)
}