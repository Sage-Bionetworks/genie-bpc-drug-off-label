library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# The purpose of this script is deriving negative flags for biomarkers.
# A negative flag shows when someone does NOT have a biomarker.  This is an
#   intentional design choice building on the fact that we don't have complete
#   knowledge.  For example, some people have an unknown HER2 status, others 
#   may have had a genomic panel with inadequate coverate used.

# At the time of this writing we're only looking at cases with ca_seq = 0.
# This lets us take lots of shortcuts when deriving things, but it would be 
# catastrophically inaccurate if you tried to generalize it.

dft_clin_dat_wide <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)

dft_ca_ind_breast <- dft_clin_dat_wide %>%
  filter(cohort %in% "Breast") %>%
  pull(ca_ind) %>%
  `[[`(.,1)

dft_cpt_breast <- dft_clin_dat_wide %>%
  filter(cohort %in% "Breast") %>%
  pull(cpt) %>%
  `[[`(.,1)




# At some point we should move this out of here and into a separate script.
# Takes a while to run as is and we'll need this for other genes soon.
cna <- data.table::fread(
  here('data-raw', 'main_genie', 'data_CNA.txt')
)

cna_long <- 
  pivot_longer(
    cna,
    cols = -Hugo_Symbol,
    names_to = "cpt_genie_sample_id"
  )

dft_erbb2_amp <- cna_long %>%
  filter(Hugo_Symbol %in% "ERBB2" & value %in% 2) %>%
  left_join(
    ., 
    select(dft_cpt_breast, cpt_genie_sample_id, record_id, ca_seq),
    by = c('cpt_genie_sample_id')
  ) %>%
  filter(!is.na(record_id)) # had non-breast cases included above.



dft_ca_ind_breast <- dft_clin_dat_wide %>%
  filter(cohort %in% "Breast") %>%
  pull(ca_ind) %>%
  `[[`(.,1)

vec_normal_er <- c('Borderline', 'Negative/normal') # same for PR
vec_normal_pr <- vec_normal_er # same for PR
vec_normal_her2 <- c("Borderline/equivocal/indeterminant", 
                     "Negative/normal within normal limits")

# Put a flag in for whether an ERBB2 amplification was revealed.
dft_ca_ind_breast <- dft_erbb2_amp %>%
  select(record_id) %>%
  distinct(.) %>%
  mutate(erbb2_amp = T) %>%
  left_join(
    dft_ca_ind_breast,
    .,
    by = 'record_id'
  ) %>%
  replace_na(list(erbb2_amp = F))
  
dft_bca_negflag <- dft_ca_ind_breast %>% 
  select(cohort, record_id, ca_bca_er, ca_bca_pr, ca_bca_her_summ, erbb2_amp) %>%
  group_by(cohort, record_id) %>%
  summarize(
    # not ER+ if they had at least one interpretable test, and none were positive.
    `not_ER+` = ca_bca_er %in% vec_normal_er,
    `not_PR+` = ca_bca_pr %in% vec_normal_pr,
    # HR is negative if both ER and PR are negative.
    `not_HR+` = `not_ER+` & `not_PR+`,
    
    `not_HER2+` = ca_bca_her_summ %in% vec_normal_her2 & !erbb2_amp,
    
    `not_HER2-` = ca_bca_her_summ %in% "Positive/elevated/amplified" | erbb2_amp,
    # They definitely do NOT have TNBC if they have any positives at all.
    `not_TNBC` = ca_bca_er %in% "Positive/elevated" |
      ca_bca_pr %in% "Positive/elevated" |
      ca_bca_her_summ %in% "Positive/elevated/amplified" | erbb2_amp,
    .groups = "drop"
  ) 

dft_bca_negflag %<>% 
  pivot_longer(cols = -c(cohort, record_id), names_to = "neg_flag", values_to = "dropping_soon") %>%
  filter(dropping_soon) %>%
  select(-dropping_soon) %>%
  mutate(
    neg_flag = str_replace_all(neg_flag, "^not_", "")
  )

readr::write_rds(
  dft_bca_negflag,
  here('data', 'cohort', 'biomarker_flags', 'negflags_clinical.rds')
)


    
  
