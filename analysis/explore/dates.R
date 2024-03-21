library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)

dft_cohort_ex_row <- dft_cohort_cases %>% slice(6)

ex_pt <- dft_cohort_ex_row %>% pull(pt) %>% `[[`(.,1)
ex_ca_ind <- dft_cohort_ex_row %>% pull(ca_ind) %>% `[[`(.,1)
ex_reg <- dft_cohort_ex_row %>% pull(reg) %>% `[[`(.,1)

# Combination option 1:
contains_vars(
  ex_pt, 
  c('birth_year')
)
contains_vars(
  ex_reg,
  c('drugs_startdt_int_1')
)
# Ok great, that works.
    

contains_vars <- function(
    dat,
    vars,
    return_dat = T
) {
  vec <- vars %in% names(dat)
  rtn = tibble(var = vars, included = vec)
  
  if (return_dat) {
    return(rtn)
  } else {
    return(all(vec, na.rm = T))
  }
  
}
