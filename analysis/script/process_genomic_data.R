library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

# I'm going to grab all the data in the BPC cohorts for now.
# Meaning we'll get people who have 2+ cancers - it's OK, just a rough trim.

dft_clin_dat_wide <- readr::read_rds(
  here('data', 'no_ca_seq_filter', 'clin_dat_wide.rds')
)

dft_clin_samp <- readr::read_tsv(
  here('data-raw', 'main_genie', 'data_clinical_sample.txt'),
  comment = "#"
)

dft_maf <- data.table::fread(
  here('data-raw', 'main_genie', 'data_mutations_extended.txt')
)

dft_bpc_id <- dft_clin_dat_wide %>%
  select(ca_ind) %>%
  mutate(ca_ind = map(.x = ca_ind, .f = \(z) select(z, record_id, ca_seq))) %>%
  unnest(ca_ind) 

dft_bpc_id <- left_join(
    distinct(select(dft_bpc_id, record_id)),
    select(dft_clin_samp, record_id = PATIENT_ID, cpt_genie_sample_id = SAMPLE_ID),
    by = 'record_id',
    relationship = 'one-to-many'
  ) %>%
  select(cpt_genie_sample_id, record_id)

dft_maf %<>%
  filter(Tumor_Sample_Barcode %in% dft_bpc_id$cpt_genie_sample_id) %>% 
  # add record ids for my ease.
  left_join(
    ., 
    dft_bpc_id,
    by = c(Tumor_Sample_Barcode = "cpt_genie_sample_id")
  )

readr::write_rds(
  dft_maf,
  here('data', 'no_ca_seq_filter', 'bpc_maf.rds')
)



dft_maf %<>%
  as_tibble(.) %>%
  filter(
    # use the cbioportal filter to start:
    !(Variant_Classification %in% c(
      "3'Flank", "3'UTR", "5'Flank", "5'UTR", "Intron", "Silent"
    ))
  ) 

dft_maf_sum <- dft_maf %>%
  group_by(record_id) %>%
  summarize(
    any_BRAF = any(Hugo_Symbol %in% "BRAF", na.rm = T),
    any_ERBB2 = any(Hugo_Symbol %in% "ERBB2", na.rm = T)
  )

readr::write_rds(
  dft_maf_sum,
  here('data', 'no_ca_seq_filter', 'genomic_sum.rds')
)
  





