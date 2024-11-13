# Description: Takes the MAF file (mutation information) and creates a dataset 
#   that says whether a gene was "tested for" in each sample.
# This is fairly inexact in GENIE, but probably the best we can do currently.
# Author: Alex Paynter

library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

hugo_list <- readr::read_rds(
  here('data', 'cohort', 'biomarker_flags', 'gene_list_inclusion.rds')
)
maf <- fread(here('data-raw', 'main_genie', 'data_mutations_extended.txt'))

maf %<>% as_tibble(.)

# Much like with TMB, we won't count silent (aka synonymous) mutations:
maf %<>% filter(!(Variant_Classification %in% "Silent"))

cpt_bpc_combined <- readr::read_rds(
  here('data-raw', 'clin_dat_untouched.rds')
) %>%
  filter(short_name %in% "cpt") %>%
  pull(dat) %>%
  bind_rows(.)

# Only need the samples included in at least one BPC cohort:
maf %<>%
  rename(sample_id = Tumor_Sample_Barcode) %>%
  filter(sample_id %in% cpt_bpc_combined$cpt_genie_sample_id)

# First we'll just get a bulk matrix of alterations by sample:
sample_gene_alt <- maf %>%
  filter(Hugo_Symbol %in% hugo_list) %>% # just for computational ease.
  group_by(sample_id, Hugo_Symbol) %>%
  summarize(altered = 1, .groups = 'drop') %>%
  pivot_wider(
    names_from = Hugo_Symbol,
    values_from = altered, 
    values_fill = 0
  )
# There are some samples missing here. That's OK, we'll get them back merging with custom features (ironically).

# In addition to those simple gene features, we will want some specific ones:
custom_gene_feat <- maf %>%
  mutate(
    EGFR_exon19del = case_when(
      !(Hugo_Symbol %in% "EGFR") ~ F,
      Exon_Number %in% "19/28" & Consequence %in% "inframe_deletion" ~ T,
      T ~ F
    ),
    EGFR_pL858R = case_when(
      !(Hugo_Symbol %in% "EGFR") ~ F,
      str_detect(HGVSp_Short, "^p.L858R$") ~ T,
      T ~ F
    ),
    EGFR_pT790M = case_when(
      !(Hugo_Symbol %in% "EGFR") ~ F,
      str_detect(HGVSp_Short, "^p.T790M$") ~ T,
      T ~ F
    ),
    BRAF_pV600E = case_when(
      !(Hugo_Symbol %in% "BRAF") ~ F,
      str_detect(HGVSp_Short, "^p.V600E$") ~ T,
      T ~ F
    )
  ) %>%
  select(sample_id, EGFR_exon19del:last_col()) %>%
  group_by(sample_id) %>%
  summarize(
    across(
      .cols = EGFR_exon19del:last_col(),
      .fns = \(z) any(z)
    )
  )

gene_feat_all <- full_join(
  custom_gene_feat,
  sample_gene_alt,
  by = "sample_id"
)

# Need to do time-based features according to people.
# Also consider whether you should only pull in samples associated with first and only cancers.  I think it's probably OK though.
# Should also look at the biomarker code again to remind myself of my format choices.
           
  
      
    
    
