library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# dft_clin_dat_wide <- readr::read_rds(
#   here('data', 'cohort', 'clin_dat_wide.rds')
# )  

dft_path_breast <- readr::read_csv(
  here('data-raw', 'BrCa', 'pathology_report_level_dataset.csv')
)

dft_path_breast %<>% 
  # something weird happened with the naming here, just making it consistent with 2-5:
  rename(
    path_her2ihc_1 = path_herihc_1,
    path_her2ish_1 = path_herish_1
  ) %>%
  select(
    cohort, record_id, path_proc_number, path_rep_number,
    dx_path_proc_days, # from dx
    dob_path_proc_days = path_proc_int, # from birth, probably use this.
    #contains("erprher"),
    matches("er_[1-5]"),
    matches("pr_[1-5]"),
    matches("her2ihc_[1-5]"),
    matches("her2ish_[1-5]")
  )

dft_path_breast %<>% 
  pivot_longer(
    cols = c(path_er_1:path_her2ish_5)
  ) %>%
  separate_wider_delim(
    cols = "name",
    delim = "_",
    # I'm using the vague "test_num" here because that's all the info I have.
    # "Up to 3 tests may be associated with each report" - I don't know whether
    #   that means sampling spots or tissue cores or what, but I'll take any
    #   positive as a sign of positivity.
    names = c("path", "test", "test_num"),
    too_few = "error",
    too_many = "error"
  ) %>%
  mutate(test_num = as.numeric(test_num)) %>%
  select(-path) # filler

# Found some levels which are errors:
dft_path_breast %<>%
  filter(!is.na(value)) %>%
  mutate(value = case_when(
    value %in% "Test Not Done" ~ "Test not done",
    T ~ value
  ))

known_breast_biomarker_levels <- c(
  "Equivocal",
  "Negative",
  "Negative (1+) IHC only",
  "Positive",
  "Test not done"
)

if (any(!(dft_path_breast$value %in% known_breast_biomarker_levels))) {
  cli_abort("Unaccounted biomarker levels detected for ER, PR or HER2 - check and fix")
}

dft_path_breast %<>%
  group_by(cohort, record_id, path_proc_number, path_rep_number, test,
           # just grouping by these to keep them in the data
           dx_path_proc_days, dob_path_proc_days) %>%
  summarize(
    biom_tested = any(!(value %in% "Test not done")),
    biom_pos = any(value %in% "Positive"),
    .groups = "drop"
  )

# now for each person, we need a value showing their first tested time and
#   positive time for each biomarker.

dft_path_breast %<>%
  group_by(cohort, record_id, test) %>%
  summarize(
    dob_biom_tested = min(dob_path_proc_days[biom_tested], na.rm = T),
    dob_biom_pos = min(dob_path_proc_days[biom_pos], na.rm = T),
    .groups = "drop"
  )


dft_path_breast %<>%
  mutate(across(.cols = matches("^dob_biom"), .fns = \(z) {
    case_when(is.infinite(z) ~ NA_real_, T ~ z)
  }))
  

ggplot(
  dft_path_breast,
  aes(x = dob_biom_tested, y = dob_biom_pos - dob_biom_tested)
) + 
  geom_point() + 
  geom_hline(yintercept = 0)

dft_path_breast %>% glimpse
           
    
    
    