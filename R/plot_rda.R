plot_rda_scores <- function(rda, data) {
  scores <- MVA.scores(rda, xax = 1, yax = 2)
  score_df <- bind_cols(data, scores$coord)
  
  ggplot(score_df, aes(x = `Constr. comp. 1`, y = `Constr. comp. 2`,
                       fill = current,
                       color = current,
                       linetype = current)) +
    # Plot points and convex hull
    stat_chull(geom = "polygon", alpha = 0.4, color = "black") +
    geom_point(aes(shape = historical), color = "black", size = 2, stroke = 0.75) +
    # Scales and legends
    scale_color_viridis_d("Current", begin = .25, end = 0.95) +
    scale_linetype_manual("Current", values = c("solid", "longdash", "dotted")) +
    scale_shape_discrete("Historical", solid = FALSE) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    labs(
      x = glue::glue("Constrained comp. 1 ({MVA.synt(rda)[[2]]$tab[1,2] %>% round(2)}%)"),
      y = glue::glue("Constrained comp. 2 ({MVA.synt(rda)[[2]]$tab[2,2] %>% round(2)}%)")
    ) +
    # Theme
    theme_bw() +
    theme(panel.grid= element_blank())
}

plot_rda_loadings <- function(rda, data) {
  
}