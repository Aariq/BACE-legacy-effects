fit_ht <- function(data, log_trans = TRUE) {
  if (log_trans == TRUE) {
    y = "log(height)"
  } else {
    y = "height"
  }
  #too few levels of house to include as random effect.
  f <- formula(paste0(y, "~ days * hist_treat * curr_treat + july_herbivory + house + (1|pot_id) + (1|plant_id)"))
  k_ht <- lmer(f, data)
}