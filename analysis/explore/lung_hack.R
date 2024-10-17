library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_clin_dat_wide <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)

dft_clin_dat_wide <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)


dft_reg_nsclc <- dft_clin_dat_wide %>% 
  filter(cohort %in% "NSCLC") %>%
  pull(reg) %>%
  `[[`(.,1)

dft_ca_non_ind_nsclc <- fread(
  here('data-raw', 'NSCLC', 
       'cancer_level_dataset_non_index.csv')
)

dft_cpt_nsclc <- fread(
  here('data-raw', 'NSCLC', 
       'cancer_panel_test_level_dataset.csv')
)

dft_ca_non_ind_nsclc %>% 
  select(record_id, ca_seq, ca_type, ca_d_site) %>%
  inner_join(
    .,
    dft_cpt_nsclc,
    by = c('record_id', 'ca_seq')
  ) %>%
  as_tibble

dft_ca_non_ind_nsclc %>% 
  as_tibble(.) %>%
  select(record_id, ca_seq, ca_type, ca_d_site) %>%
  inner_join(
    .,
    as_tibble(dft_cpt_nsclc),
    by = c('record_id', 'ca_seq')
  ) %>%
  as_tibble %>% glimpse



    
dft_reg_nsclc %>%
  filter(str_detect(tolower(regimen_drugs), "aflibercept")) # no cases

create_drug_dat(dft_reg_nsclc) %>%
  count(agent)
