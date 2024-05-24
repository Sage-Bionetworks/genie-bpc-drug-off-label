# Investigating the detailed staging for people who are Stage IV without distant mets in prostate cancer.

library(fs); library(purrr); library(here);
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dat_prostate_ca_ind <- readr::read_rds(
  here('data', 'no_ca_seq_filter', 'clin_dat_wide.rds')
) %>%
  filter(cohort %in% "Prostate") %>%
  pull(ca_ind) %>%
  `[[`(.,1)
dat_prostate_ca_ind %<>% 
  filter(stage_dx_iv %in% "Stage IV")

dat_prostate_ca_ind %<>%
  mutate(
    ca_dmets_yn = case_when(
      str_detect(ca_dmets_yn, "No") ~ "No, S4, no dmet ...",
      T ~ ca_dmets_yn
    )
  )

dat_prostate_ca_ind %>%
  tabyl(
    ca_dmets_yn, best_ajcc_stage_cd
  )

      
