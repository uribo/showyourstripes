plot_temperature_changes <- function(data) {
  col_strip <- RColorBrewer::brewer.pal(11, "RdBu")
  data %>%
    ggplot(aes(year, diff, fill = diff)) +
    geom_bar(stat = "identity", color = "white", size = 0.05) +
    scale_fill_gradientn(name = NULL,
                         # limits = c(variance(df$value) - 2.6,
                         #            variance(df$value) + 2.6),
                         colors = rev(col_strip)) +
    geom_text(aes(x = min(year) - 6,
                  y = 0),
              hjust = 0,
              size = 1.2,
              fontface = "plain",
              colour = "#FFFFEE",
              label  = glue::glue("{min(data$year)[1]}")) +
    geom_text(aes(x = max(year) + 2,
                  y = 0),
              hjust = 0,
              size = 1.2,
              fontface = "plain",
              colour = "#FFFFEE",
              label  = glue::glue("{max(data$year)[1]}")) +
    #ggplot2::expand_limits(x = length(df$year) + 5) +
    theme_void(base_size = 6, base_family = "TsukuARdGothic-Regular") +
    xlab(NULL) +
    ylab(NULL) +
    theme(text = element_text(colour = "#FFFFEE"),
          plot.background = element_rect(fill = "#272728"),
          legend.background = element_rect(fill = "#272728", color = NA),
          legend.key.width = unit(0.2, "line"),
          legend.key.height = unit(1.0, "line"),
          plot.caption = element_markdown(size = 4)) +
    labs(title = glue::glue("{data$station_name}観測所での気温の変化"),
         subtitle = glue::glue("{min(data$year)}年から{max(data$year)}年までの年平均気温({format(round(mean(data$value), digits = 1), nsmall = 1)}℃)との差"),
         caption = "#ShowYourStripes, データ元: 気象庁, <span style='font-family: \"Font Awesome 5 Brands\"; color:#55acee'>&#61593;</span>@u_ribo")
}
.plot_warming_stripes <- function(data) {
  col_strip <- RColorBrewer::brewer.pal(11, "RdBu")
  data %>%
    ggplot(aes(year, y = 1, fill = value)) +
    geom_tile(show.legend = FALSE) +
    scale_fill_gradientn(name = NULL,
                         colors = rev(col_strip)) +
    xlab(NULL) +
    ylab(NULL) +
    scale_y_discrete(expand = c(0, 0)) +
    theme_void(base_size = 6, base_family = "TsukuARdGothic-Regular") +
    theme(text = element_text(colour = "#FFFFEE"),
          rect = element_blank(),
          plot.background = element_rect(fill = "#272728"),
          legend.background = element_rect(fill = "#272728", color = NA),
          legend.key.height = unit(2.0, "line"),
          plot.caption = element_markdown(size = 4))
}

plot_warming_stripes <- function(data) {
  .plot_warming_stripes(data) +
    labs(title = glue::glue("{data$station_name}観測所での気温の変化"),
         caption = "#ShowYourStripes, データ元: 気象庁, <span style='font-family: \"Font Awesome 5 Brands\"; color:#55acee'>&#61593;</span>@u_ribo")
}
