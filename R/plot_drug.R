
plot_drug <- function(
    dat_drug_sub,
    pal = c("#EBAC23", "#B80058", "#008CF9", "#006E00", "#00BBAD", "#D163E6", "#B24502", "#FF9287", "#5954D6", "#00C6F8", "#878500", "#00A76C", "#BDBDBD"),
    plot_title = NULL,
    plot_subtitle = "Colors represent regimen coding"
) {
  if (length(unique(dat_drug_sub$record_id)) > 1) {
    cli::cli_abort(
      "Function only intended to work on one person.  More than one exists in the input data"
    )
  }
  
  dat_drug_sub %<>%
    mutate(
      end_show = if_else(
        # if the end date is the same as the start date, perturb by 0.1 to get
        # a displayed interval.
        abs(dx_drug_end_or_lastadm_int - dx_drug_start_int) < 0.5,
        dx_drug_end_or_lastadm_int + 1,
        dx_drug_end_or_lastadm_int,
        dx_drug_end_or_lastadm_int
      )
    )
  
  dat_drug_sub$end_show
  
  dat_drug_sub %<>%
    arrange(regimen_number, dx_drug_start_int) %>%
    mutate(
      agent_f = forcats::fct_inorder(agent),
      agent_f = forcats::fct_rev(agent_f),
      regimen_number_f = factor(regimen_number),
      start_yr = dx_drug_start_int / 365.25,
      end_yr = end_show / 365.25
    )
  
  if (is.null(plot_title)) {
    
    rec <- unique(dat_drug_sub$record_id)
    cs <- unique(dat_drug_sub$ca_seq)
    coh <- unique(dat_drug_sub$cohort)
    
    plot_title <- glue(
      "{rec}, ca_seq = {cs}, {coh} cohort."
    )
  }
  
  gg <- ggplot(
    dat_drug_sub,
    aes(xmin = start_yr, xmax = end_yr, y = agent_f, color = regimen_number_f),
  ) + 
    geom_linerange(size = 2) +
    theme_bw() +
    scale_color_manual(values = pal, guide = 'none') + 
    scale_x_continuous(
      minor_breaks = seq(0, 100, by = (1/6))
    ) + 
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      y = NULL,
      x = "Years from dx"
    ) + 
    theme(
      plot.title.position = "plot"
    )
  
  
  return(gg)
}