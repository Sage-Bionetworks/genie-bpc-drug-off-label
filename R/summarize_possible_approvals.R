

summarize_possible_approvals <- function(
    dat_poss_app,
    group_cols = c('cohort', 'record_id', 'ca_seq', 'regimen_number',
                   'drug_number', 'agent')
) {
  check_test_cols_true_false(dat_poss_app)
  
  rtn <- dat_poss_app %>%
    group_by(across(all_of(group_cols))) %>%
    summarize(
      valid_ind_exists = sum(test_all, na.rm = T) >= 1,
      # currently needs to be manually updated, which I hate.
      failure_type = case_when(
        !any(test_ind_exists) ~ "test_ind_exists",
        !any(test_ind_exists & test_met) ~ "test_met",
        !any(test_ind_exists & test_met & test_date_definite) ~ "test_date_definite",
        T ~ NA_character_
      ),
      .groups = "drop"
    )
  
  # Check that failure type is always categorized if indication does not exist.
  chk_failure_type <- rtn %>%
    filter(is.na(failure_type) & !valid_ind_exists)
  if (nrow(chk_failure_type) > 0) {
    cli_abort("Some failure types failed to be categorized when indications did not exist - needs to be fixed.  Returning failed cases.")
  }
  
  return(rtn)
  
}
