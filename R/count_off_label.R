count_off_label(
  dat_det
) {
  dat_det %>%
    group_by(cohort, agent) %>%
    summarize(
      n_uses = n(),
      n_on_label = sum(ind_exists, na.rm = T),
      n_off_label = n_uses - n_on_label,
      prop_off_label = n_off_label / n_uses, # currently 0 or 1.
      .groups = "drop"
    )
}