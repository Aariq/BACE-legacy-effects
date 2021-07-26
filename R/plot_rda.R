plot_rda_scores <- function(rda, data) {
  scores <- MVA.scores(rda, xax = 1, yax = 2)
  score_df <- bind_cols(data, scores$coord)
  
  ggplot(score_df, aes(x = `Constr. comp. 1`, y = `Constr. comp. 2`,
                       fill = current,
                       linetype = current)) +
    # Plot points and convex hull
    stat_chull(geom = "polygon", alpha = 0.4, color = "black") +
    geom_point(aes(shape = historical), color = "black", size = 2, stroke = 0.75) +
    # Scales and legends
    scale_fill_viridis_d("Current", begin = .25, end = 0.95) +
    scale_linetype_manual("Current", values = c("solid", "longdash", "dotted")) +
    scale_shape_discrete("Historical", solid = FALSE) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    labs(
      x = glue::glue("Constr. comp. 1 ({MVA.synt(rda)[[2]]$tab[1,2] %>% round(2)}%)"),
      y = glue::glue("Constr. comp. 2 ({MVA.synt(rda)[[2]]$tab[2,2] %>% round(2)}%)")
    ) +
    # Theme
    theme_bw() +
    theme(panel.grid = element_blank())
}

plot_rda_loadings <- function(rda, data) {
  #extract loadings
  load_df <-
    MVA.load(rda)$loads %>% 
    as_tibble(rownames = "nutrient") %>% 
    #format nutrients for plotting
    mutate(nutrient = str_extract(nutrient, "(?<=nutr\\.)\\w{1,2}(?=_scaled)")) %>% 
    mutate(nutrient = str_to_title(nutrient)) %>% 
    #math to put labels at tips of arrows
    mutate(m = `Constr. comp. 2`/`Constr. comp. 1`,
           xplus = sqrt(1 / (1 + m^2)),
           yplus = abs(m * xplus)) %>% 
    mutate(xplus = ifelse(sign(`Constr. comp. 1`) == -1, xplus*-1, xplus),
           yplus = ifelse(sign(`Constr. comp. 2`) == -1, yplus*-1, yplus))
  
  # plot
  ggplot(load_df) +
    geom_segment(
      aes(
        x = 0,
        y = 0,
        xend = `Constr. comp. 1`,
        yend = `Constr. comp. 2`
      ),
      arrow = arrow(length = unit(0.02, "npc"))
    ) +
    geom_label_repel(
      aes(x = `Constr. comp. 1`,
          y = `Constr. comp. 2`,
          label = nutrient),
      size = 2.4,
      segment.alpha = 0.6,
      segment.size = 0.3,
      nudge_x = load_df$xplus * .01,
      nudge_y = load_df$yplus * .01,
      seed = 678 #just for reproducibility
    ) +
    # grey out non-significant arrows
    scale_alpha_manual(values = c(0.5, 1)) +
    labs(
      x = glue::glue("Constr. comp. 1 ({MVA.synt(rda)[[2]]$tab[1,2] %>% round(2)}%)"),
      y = glue::glue("Constr. comp. 2 ({MVA.synt(rda)[[2]]$tab[2,2] %>% round(2)}%)")
    ) +
    # Theme
    theme_bw() +
    theme(panel.grid= element_blank(), legend.position = "none")
}

plot_rda_cor <- function(rda, thresh = 0.5) {
  cor_df <-
    MVA.cor(rda)$corr %>% 
    as_tibble(rownames = "nutrient") %>% 
    #format nutrients for plotting
    mutate(nutrient = str_extract(nutrient, "(?<=nutr\\.)\\w{1,2}(?=_scaled)")) %>% 
    mutate(nutrient = str_to_title(nutrient)) %>% 
    #math to put labels at tips of arrows
    mutate(m = `Constr. comp. 2`/`Constr. comp. 1`,
           xplus = sqrt(1 / (1 + m^2)),
           yplus = abs(m * xplus)) %>% 
    mutate(xplus = ifelse(sign(`Constr. comp. 1`) == -1, xplus*-1, xplus),
           yplus = ifelse(sign(`Constr. comp. 2`) == -1, yplus*-1, yplus)) %>% 
    mutate(thresh = abs(`Constr. comp. 1`) > thresh | abs(`Constr. comp. 2`) > thresh)
  
  ggplot(cor_df) +
    geom_segment(aes(
      x = 0,
      y = 0,
      xend = `Constr. comp. 1`,
      yend = `Constr. comp. 2`,
      alpha = thresh
    ),
    arrow = arrow(length = unit(0.02, "npc"))) +
    geom_label_repel(
      aes(x = `Constr. comp. 1`,
          y = `Constr. comp. 2`,
          label = nutrient),
      size = 2.4,
      segment.alpha = 0.6,
      segment.size = 0.3,
      nudge_x = cor_df$xplus * .01,
      nudge_y = cor_df$yplus * .01,
      seed = 678
    ) +
    scale_alpha_manual(values = c(0.5, 1)) +
    scale_x_continuous(
      glue::glue("Constr. comp. 1 ({MVA.synt(rda)[[2]]$tab[1,2] %>% round(2)}%)"),
      limits = symmetric_limits
    ) +
    scale_y_continuous(
      glue::glue("Constr. comp. 2 ({MVA.synt(rda)[[2]]$tab[2,2] %>% round(2)}%)"),
      limits = symmetric_limits
    ) +
    # Theme
    theme_bw() +
    theme(panel.grid= element_blank(), legend.position = "none")
}