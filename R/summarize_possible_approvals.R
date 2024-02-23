

summarize_possible_approvals <- function(
    dat_poss_app,
    group_cols = c('cohort', 'record_id', 'ca_seq', 'regimen_number',
                   'drug_number', 'agent')
) {
  check_test_cols_true_false(dat_poss_app)
  
  rtn <- dat_poss_app %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      ind_exists = sum(test_all, na.rm = T) >= 1,
      # currently needs to be manually updated, which I hate.
      failure_type = case_when(
        all(test_ind_exists %in% F) ~ "test_ind_exists",
        all(test_ind_exists & test_met %in% F) ~ "test_met",
        T ~ NA_character_
      ),
      .groups = "drop"
    )
  
  # Check that failure type is always categorized if indication does not exist.
  chk_failure_type <- rtn %>%
    filter(is.na(failure_type)) %>%
    pull(ind_exists) %>%
    is_in(., T) %>%
    all(.)
  if (!chk_failure_type) {
    cli_alert_danger("Some failure types failed to be categorized when indications did not exist - needs to be fixed.")
  }
  
  return(rtn)
  
}