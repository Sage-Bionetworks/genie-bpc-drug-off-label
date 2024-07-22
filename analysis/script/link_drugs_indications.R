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

# Load the hdrug cohort data:
dft_hdrug_cohort_lim <- readr::read_rds(
  here('data', 'cohort', 'hdrug_with_conmeds.rds')
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



dft_poss_app <- make_possible_indication_cohort( 
  dat_hdrug = dft_hdrug_cohort_lim,
  dat_ind = dft_ind_lim
)


# Leaves open the possibility to add more indications based on TMB, etc (not cohort)

dft_poss_app <- dft_poss_app %>%
  add_check_met(.) %>%
  add_check_date_definite(.) %>%
  add_check_monotherapy(.) %>%
  add_check_multiple_tests(dat_poss_app = .) # by default selects all "test" columns


readr::write_rds(
  dft_poss_app,
  here('data', 'linked_approvals', 'possible_approvals.rds')
)

# searching for cases that SHOULD fail:
# dft_poss_app %>%
#   group_by(record_id, regimen_number, drug_number) %>%
#   mutate(
#     .all_ind_and_met_ok = any(test_ind_exists & test_met),
#     .mono_off = !any(test_monotherapy)
#   ) %>%
#   filter(.all_ind_and_met_ok & .mono_off)

dft_hdrug_determinations <- summarize_possible_approvals_2(dft_poss_app) 

tabyl(dft_hdrug_determinations, failure_type, cohort)

levs_failure_type <- c(
  "No indications found",
  "Not metastatic at use",
  "Started before approval",
  "Not used as monotherapy"
)

dft_hdrug_determinations %<>%
  mutate(
    failure_type_f = case_when(
      failure_type %in% "test_ind_exists" ~ levs_failure_type[1],
      failure_type %in% "test_met" ~ levs_failure_type[2],
      failure_type %in% "test_date_definite" ~ levs_failure_type[3],
      failure_type %in% "test_monotherapy" ~ levs_failure_type[4],
      T ~ NA_character_
    ),
    failure_type_f = factor(failure_type_f, levels = levs_failure_type)
  )

readr::write_rds(
  dft_hdrug_determinations,
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)










