library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_clin_dat_wide <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)  