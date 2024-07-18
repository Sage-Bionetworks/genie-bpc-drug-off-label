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
# Load the tracking for drugs:
dft_drug_tracking <- readr::read_rds(
  here('data', 'cohort', 'drug_tracking_01.rds')
)


# Fix up the date column from the indications sheet:
dft_ind_lim %<>% 
  mutate(
    date = case_when(
      # If the date is uncertain we just assume it was always approved.
      # This has the effect of never flagging someone as taking these too early.
      date %in% "Uncertain date" ~ ymd(rep('1900-01-01', times = n())),
      T ~ suppressWarnings(ymd(date))
    )
  )



# Here we eliminate the old drugs:
dft_hdrug_cohort_lim <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug) %>%
  filter(!(agent %in% vec_excl_drugs))

# Note how many drugs were removed in this step and save:
dft_drug_tracking <- bind_rows(
  dft_drug_tracking, 
  tibble(
    step = "Remove drugs approved before Jan 1, 1997",
    drug_key = list(select(
      dft_hdrug_cohort_lim,
      cohort, record_id, ca_seq, regimen_number, drug_number, agent
    ))
  )
)
readr::write_rds(
  x = dft_drug_tracking,
  file = here('data', 'cohort', 'drug_tracking_02.rds')
)





dft_poss_app <- make_possible_indication_cohort( 
  dat_hdrug = dft_hdrug_cohort_lim,
  dat_ind = dft_ind_lim
)




# Leaves open the possibility to add more indications based on TMB, etc (not cohort)

dft_poss_app <- dft_poss_app %>%
  add_check_met(.) %>%
  add_check_date_definite(.) %>%
  add_check_multiple_tests(
    dat_poss_app = .,
    # not strictly needed...
    test_cols_to_include = c(
      'test_ind_exists',
      'test_met',
      'test_date_definite'
    )
  )

readr::write_rds(
  dft_poss_app,
  here('data', 'linked_approvals', 'possible_approvals.rds')
)


dft_hdrug_determinations <- summarize_possible_approvals(dft_poss_app) 

levs_failure_type <- c(
  "No indications found",
  "Not metastatic at use",
  "Started before approval"
)

dft_hdrug_determinations %<>%
  mutate(
    failure_type_f = case_when(
      failure_type %in% "test_ind_exists" ~ levs_failure_type[1],
      failure_type %in% "test_met" ~ levs_failure_type[2],
      failure_type %in% "test_date_definite" ~ levs_failure_type[3],
      T ~ NA_character_
    ),
    failure_type_f = factor(failure_type_f, levels = levs_failure_type)
  )

readr::write_rds(
  dft_hdrug_determinations,
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)










