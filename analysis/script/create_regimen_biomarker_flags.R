library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_hdrug_cohort_lim <- readr::read_rds(
  here('data', 'cohort', 'hdrug_with_conmeds.rds')
)

dft_skel <- dft_hdrug_cohort_lim %>%
  select(cohort, record_id, ca_seq, regimen_number, drug_number,
         dob_drug_start_int)

dft_biom_breast <- readr::read_rds(
  file = here('data', 'cohort', 'biomarker_flags', 
              'biom_breast_1row_per_record.rds')
)

dft_skel %<>%
  left_join(
    dft_biom_breast,
    dft_skel,
    by = c("cohort", "record_id")
  )

# a bit of redundant coding here, can clean up later if you like.
# goal coding of outputs:
# T = definitely had the biomarker at time of agent start.
# F = definitely did not have the biomarker at the time of agent start (tested with a negative result).
# NA = no record of a test before this result, so we really don't know.
dft_skel %<>%
  mutate(
    biom_er = case_when(
      is.na(dob_biom_test_er) ~ NA,
      is.na(dob_biom_pos_er) ~ 0, # tested but never positive
      (dob_biom_pos_er - 0.5) > dob_drug_start_int ~ 0,
      (dob_biom_pos_er - 0.5) <= dob_drug_start_int ~ 1,
      T ~ -Inf # should never happen 
    ),
    biom_pr = case_when(
      is.na(dob_biom_test_pr) ~ NA,
      is.na(dob_biom_pos_pr) ~ 0, # tested but never positive
      (dob_biom_pos_pr - 0.5) > dob_drug_start_int ~ 0,
      (dob_biom_pos_pr - 0.5) <= dob_drug_start_int ~ 1,
      T ~ -Inf # should never happen 
    ),
    biom_hr = case_when(
      is.na(dob_biom_test_hr) ~ NA,
      is.na(dob_biom_pos_hr) ~ 0, # tested but never positive
      (dob_biom_pos_hr - 0.5) > dob_drug_start_int ~ 0,
      (dob_biom_pos_hr - 0.5) <= dob_drug_start_int ~ 1,
      T ~ -Inf # should never happen 
    ),
    biom_her2 = case_when(
      is.na(dob_biom_test_her2) ~ NA,
      is.na(dob_biom_pos_her2) ~ 0, # tested but never positive
      (dob_biom_pos_her2 - 0.5) > dob_drug_start_int ~ 0,
      (dob_biom_pos_her2 - 0.5) <= dob_drug_start_int ~ 1,
      T ~ -Inf # should never happen 
    )
  )

dft_skel %>% count(biom_er)      
dft_skel %>% count(biom_pr)      
dft_skel %>% count(biom_her2)      
dft_skel %>% count(biom_hr)      

