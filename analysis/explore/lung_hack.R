library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_clin_dat_wide <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)


dft_reg_nsclc <- dft_clin_dat_wide %>% 
  filter(cohort %in% "NSCLC") %>%
  pull(reg) %>%
  `[[`(.,1)
    
dft_reg_nsclc %>%
  filter(str_detect(tolower(regimen_drugs), "aflibercept")) # no cases

create_drug_dat(dft_reg_nsclc) %>%
  count(agent)
