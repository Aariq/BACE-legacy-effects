#' Plot trends
#' 
#' Uses `emmeans::emtrends` to calculate marginal effects for interaction between historical and current treatment on slope over days.  Plots with 84% CI (by default) so overlap = no sig. diff.
#'
#' @param model model object
#' @param exp back-transform by exp() ?
#' @param conf confidence level for error bars
#'
#' @return a ggplot object
plot_slopes_panel <- function(model, exp = TRUE, conf = 0.84) {
  slopes <- 
    emtrends(model, ~ hist_treat*curr_treat, var = "days", level = conf) %>% 
    as_tibble() 
  
  if (exp == TRUE) {
    # back-transform linear slope to exponential growth rate?
    slope_df <-
      mutate(slopes, across(c(days.trend, lower.CL, upper.CL), ~ exp(.x)))
  } else {
    slope_df <- slopes
  }
  
  ggplot(slope_df) +
    geom_pointrange(
      aes(
        x = hist_treat,
        shape = curr_treat,
        color = curr_treat,
        y = days.trend,
        ymin = lower.CL,
        ymax = upper.CL
      ),
      position = position_dodge(width = 0.2)
    ) +
    scale_color_viridis_d("Current", begin = .25, end = 0.95) +
    scale_shape_discrete("Current", solid = TRUE) +
    labs(
      x = "Historical") + 
    theme_bw() 
    
}

#' Create 3-panel figure of emtrends plots
#'
#' @param k_slopes 
#' @param b_slopes 
#' @param o_slopes 
#'
#' @return 3-panel figure made with `patchwork`
plot_slopes <-
  function(k_slopes, b_slopes, o_slopes) {
    rm_axis <- theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank())
    
    (k_slopes + rm_axis + scale_y_continuous("growth (exponent)")) / 
    (b_slopes + rm_axis + scale_y_continuous("growth (exponent)")) / 
    (o_slopes + scale_y_continuous("growth (cm/day)")) + plot_layout(guides = "collect") & 
      theme(panel.grid = element_blank()) & plot_annotation(tag_levels = "a")
    
}
  