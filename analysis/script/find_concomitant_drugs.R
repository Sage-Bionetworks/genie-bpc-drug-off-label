library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Load and create the hdrug cohort data:
dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)
dft_all_cases <- readr::read_rds(
  here('data-raw', 'clin_dat_untouched.rds')
)

# Here we eliminate the old drugs:
dft_hdrug_cohort <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug) 

dft_hdrug_all <- dft_all_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug)
