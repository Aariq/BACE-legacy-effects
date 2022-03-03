plot_nutr_boxplot <- function(data) {
  df <- 
    data %>% 
    select(current, historical, N = n, Fe = fe, Ca = ca, K = k, Mg = mg) %>% 
    #convert from ppm to %
    mutate(Fe = Fe/1000) %>% 
    pivot_longer(N:Mg,
                 names_to = "nutrient",
                 values_to = "conc")

  ggplot(df, aes(x = historical, y = conc, fill = current)) + 
    geom_boxplot(outlier.size = 0.8, size = 0.3) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    scale_fill_viridis_d("Current", begin = .25, end = 0.95) +
    facet_wrap(~nutrient, scales = "free_y") +
    theme_bw() +
    labs(x = "Historical", y = "% Dry Weight")
}