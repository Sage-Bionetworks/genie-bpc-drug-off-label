# Description: Grabs the raw data from Synapse.
# Author: Alex Paynter

library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

synLogin()

dft_datasets_to_get <- tribble(
  ~synapse_name, ~save_name,
  "cancer_level_dataset_index.csv", "ca_ind",
  "cancer_level_dataset_non_index.csv", "ca_non_ind",
  "cancer_panel_test_level_dataset.csv", "cpt",
  "patient_level_dataset.csv", "pt",
  "regimen_cancer_level_dataset.csv", "reg"   
)

dft_folders <- tibble::tribble(
  ~cohort, ~synid,
  "BLADDER", "syn28495599", # 1.1 consortium
  "BrCa", "syn39802381", # 1.2 consortium
  "CRC", "syn39802279", # 2.0 public
  # "NSCLC", "syn27245047", # 2.0 public
  "NSCLC", "syn54107384", # 3.1 consortium
  "PANC", "syn50612197", # 1.2 consortium
  "Prostate", "syn50612196", # 1.2 consortium
  # cohorts that had no available releases at the time:
  #   CRC2, ESOPHAGO, MELANOMA, NSCLC2, OVARIAN, RENAL
)

# Sets up raw and cleaned data for one cohort (by name)
dc_help <- function(cohort_name) {
  fs::dir_create(here("data-raw", cohort_name))
}
# Sets up
purrr::walk(.x = dft_folders$cohort, .f = dc_help)


dft_datasets <- dft_folders %>%
  mutate(
    children = purrr::map(
      .x = synid,
      .f = (function(id) {
        dat <- get_syn_children_df(id) %>%
          # only need limited info for this project
          select(
            dat_name = name,
            dat_synid = id
          ) %>%
          filter(dat_name %in% dft_datasets_to_get$synapse_name)
        
        dat %<>% left_join(., dft_datasets_to_get,
                           by = c(dat_name = "synapse_name"))
        return(dat)
      })
    )
  ) %>%
  unnest(children)

get_and_save_dataset <- function(
  synid,
  subfolder) {
  
  synGet(
    entity = synid, 
    downloadLocation = here(
      "data-raw", 
      subfolder
      ),
    ifcollision = "overwrite.local"
    )
}

purrr::pwalk(
  .l = with(
    dft_datasets, 
    list(
      synid = dat_synid, 
      subfolder = cohort
    )
  ),
  .f = get_and_save_dataset
)




# Add in genomic data:
synid_maf <- "syn59553458"
synid_cna <- "syn59553452"
synid_clin_samp <- "syn59553454"
fs::dir_create('data-raw', 'main_genie')

get_and_save_dataset(synid_maf, 'main_genie')
get_and_save_dataset(synid_cna, 'main_genie')
get_and_save_dataset(synid_clin_samp, 'main_genie')




