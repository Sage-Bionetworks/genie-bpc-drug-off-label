library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dir_bio_flags <- here('data', 'cohort', 'biomarker_flags')

# At the moment this script just copies one file and renames it. 
# As the biomarker stuff gets more complex it will combine multiple derivations.

dft_negflags_clinical <- readr::read_rds(
  here(dir_bio_flags, 'negflags_clinical.rds')
)

dft_negflags <- dft_negflags_clinical

readr::write_rds(
  dft_negflags,
  here(dir_bio_flags, 'negative_flags_long.rds')
)



# We want to populate this so it matches up with the cohort of people we're analyzing.  To do so, we load up the hdrug data:
# Load the hdrug cohort data:
dft_hdrug_cohort_lim <- readr::read_rds(
  here('data', 'cohort', 'hdrug_with_conmeds.rds')
)

dft_hdrug_cohort_lim %>% 
  select(cohort, record_id) %>% 
  distinct(.)

  
dft_negflags_list <- dft_negflags %>%
  group_by(cohort, record_id) %>%
  arrange(neg_flag) %>% 
  summarize(
    biomark_neg = list(neg_flag),
    biomark_neg_vec = paste(neg_flag, collapse = ", "),
    .groups = "drop"
  )

# We want to populate this so it matches up with the cohort of people we're analyzing.  To do so, we load up the hdrug data:
# Load the hdrug cohort data:
dft_skeleton <- readr::read_rds(
  here('data', 'cohort', 'hdrug_with_conmeds.rds')
) %>%
  select(cohort, record_id) %>% 
  distinct(.)

dft_negflags_list <- dft_skeleton %>%
  left_join(
    .,
    dft_negflags_list,
    by = c("cohort", 'record_id')
  )

dft_negflags_list %<>%
  replace_na(replace = list(biomark_neg_vec = ""))

# I think the null list values will actually be OK.

readr::write_rds(
  dft_negflags_list,
  here(dir_bio_flags, 'negative_flags_list.rds')
)
