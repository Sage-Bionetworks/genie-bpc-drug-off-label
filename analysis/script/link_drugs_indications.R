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

dft_poss_app <- add_checks_hdrug_ind_join(dat_poss_app = dft_poss_app)

readr::write_rds(
  dft_poss_app
  here('data', 'linked_approvals', 'possible_approvals.rds')
)


check_tests_cols_true_false <- function(
    dat
) {
  dat %>%
    select(matches("^test_")) %>%
    summarize(
      across(
        .cols = everything(),
        .fns = \(x) all(x %in% c(T,F))
      )
    ) %>%
    pivot_longer(
      cols = everything()
    ) %>%
    pull(value) %>%
    all(.)
}
      



summarize_possible_approvals <- function(
    dft_poss_app,
    group_cols = c('cohort', 'record_id', 'ca_seq', 'regimen_number',
                   'drug_number', 'agent')
) {
  
  
  rtn <- dft_poss_app %>%
    group_by(all_of(group_cols)) %>%
    summarize(
      
      .groups = "drop"
    )
  
}


# readr::write_rds(
#   dft_hdrug_app,
#   here('data', 'linked_approvals', 'hdrug_ind.rds')
# )










