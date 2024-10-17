plot_guideline_str <- function(
    dat,
    y_var,
    x_lab = "Guideline date",
    y_lab = "Number of text matches",
    plot_title = "String matches from NCCN PDF",
    plot_subtitle = "Strings matching 'Cape*', except 'Capello'"
) {
  
  gg <- ggplot(
    dat,
    aes(x = date, y = .data[[y_var]])
  ) + 
    geom_point() + 
    labs(
      x = x_lab, 
      y = y_lab,
      title = plot_title,
      subtitle = plot_subtitle
    ) + 
    scale_x_date(
      expand = expansion(mult = c(0.05, 0.1))
    ) + 
    geom_smooth(
      se = F, 
      color = "#7f7f7f55", 
      method = 'loess',
      span = 1,
      linewidth = 2
    ) + 
    scale_y_continuous(
      breaks = 0:10 * 10^(-4),
      labels = 0:10,
      name = "Matches per 10,000 characters",
      limits = c(-0.01 * 10^-4, NA)
    ) + 
    theme_light() + 
    theme(
      plot.title.position = "plot"
    )
  
  return(gg)
  
}