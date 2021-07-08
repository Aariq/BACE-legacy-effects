make_longfig <- function(longdata) {
  p <-
    ggplot(longdata, aes(x = date, y = height)) +
    stat_summary(aes(color = curr_treat, linetype = hist_treat), geom = "line", fun = "mean") +
    facet_wrap(~fct_relevel(species, "kale", "beans", "oats"), scales = "free_y", ncol = 1) +
    scale_x_datetime("Date", date_breaks = "1 week", date_labels = "%m/%d") +
    scale_y_continuous("Height (cm)") +
    scale_color_viridis_d("Current", begin = .25, end = 0.95) +
    scale_linetype_manual("Historical", values = c("solid", "longdash", "dotted")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}


make_growth_panel <- function(species_data, date_lims) {
  ggplot(species_data, aes(x = date, y = height)) +
    stat_summary(aes(color = curr_treat, linetype = hist_treat),
                 geom = "line",
                 fun = "mean") +
    scale_x_datetime(
      "Date",
      date_breaks = "1 week",
      date_labels = "%m/%d",
      limits = date_lims
    )+
    scale_y_continuous("Height (cm)", breaks = breaks_width(10)) +
    scale_color_viridis_d("Current", begin = .25, end = 0.95) +
    scale_linetype_manual("Historical", values = c("solid", "longdash", "dotted")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_growth_fig <- function(longdata) {
  
  kale <-  longdata %>% filter(species == "kale")
  beans <- longdata %>% filter(species == "beans")
  oats <-  longdata %>% filter(species == "oats")
  
  p_kale <- make_growth_panel(kale, range(longdata$date))
  p_beans <- make_growth_panel(beans, range(longdata$date))
  p_oats <- make_growth_panel(oats, range(longdata$date))
  
  rm_axis <- theme(axis.title.x = element_blank(),
                   axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())
  
  # multipanel figure
  (p_kale + rm_axis + theme(axis.title.y = element_blank())) / 
  (p_beans + rm_axis) /
  (p_oats + theme(axis.title.y = element_blank())) + 
    plot_layout(guides = "collect") & plot_annotation(tag_levels = "a")
}