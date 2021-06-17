make_longfig <- function(longdata) {
  p <-
    ggplot(longdata, aes(x = date, y = height)) +
    stat_summary(aes(color = hist_treat, linetype = curr_treat), geom = "line", fun = "mean") +
    facet_wrap(~species, scales = "free_y", ncol = 1) +
    scale_x_datetime("Date", date_breaks = "1 week", date_labels = "%m/%d") +
    scale_y_continuous("Height (cm)") +
    scale_color_discrete("Historical") +
    scale_linetype_discrete("Current") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(p)
}
