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
  
dft_negflags_list <- dft_negflags %>%
  group_by(cohort, record_id) %>%
  arrange(neg_flag) %>% 
  summarize(
    biomark_neg = list(neg_flag),
    biomark_neg_vec = paste(neg_flag, collapse = ", "),
    .groups = "drop"
  )

readr::write_rds(
  dft_negflags,
  here(dir_bio_flags, 'negative_flags_list.rds')
)
