library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Load the indications data (limited to FDA) and crosswalks:
dft_ind_lim <- readr::read_rds(
  here('data', 'warner_materials', 'indications_mapped_limited.rds')
)
dft_cw_condition <- readr::read_rds(
  here('data', 'warner_materials', 'cw_condition.rds')
)
dft_cw_drug <- readr::read_rds(
  here('data', 'warner_materials', 'cw_drug.rds')
)
vec_excl_drugs <- readr::read_rds(
  here('data', 'warner_materials', 'old_drugs_list.rds')
)

# Load and create the hdrug cohort data:
dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)



dft_hdrug_cohort_lim <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug) %>%
  filter(!(agent %in% vec_excl_drugs))

dft_poss_app <- make_possible_indication_cohort( 
  dat_hdrug = dft_hdrug_cohort_lim,
  dat_ind = dft_ind_lim
)
# Leaves open the possibility to add more indications based on TMB, etc (not cohort)

dft_poss_app <- add_checks_possible_approvals(dat_poss_app = dft_poss_app)

readr::write_rds(
  dft_poss_app,
  here('data', 'linked_approvals', 'possible_approvals.rds')
)


summarize_possible_approvals <- function(
    dat_poss_app,
    group_cols = c('cohort', 'record_id', 'ca_seq', 'regimen_number',
                   'drug_number', 'agent')
) {
  check_tests_cols_true_false(dat_poss_app)
  
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


# readr::write_rds(
#   dft_hdrug_app,
#   here('data', 'linked_approvals', 'hdrug_ind.rds')
# )










