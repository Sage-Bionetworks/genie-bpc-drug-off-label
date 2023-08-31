# Description: Grabs the raw data from Synapse.
# Author: Alex Paynter


dft_folders <- tibble::tribble(
  ~cohort, ~synid,
  "BLADDER", "syn28495599", # 1.1 consortium
  "BrCa", "syn39802381", # 1.2 consortium
  "CRC", "syn39802279", # 2.0 public
  "NSCLC", "syn27245047", # 2.0 public 
  "PANC", "syn50612197", # 1.2 consortium
  "Prostate", "syn50612196", # 1.2 consortium
  # cohorts that had no available releases at the time:
  #   CRC2, ESOPHAGO, MELANOMA, NSCLC2, OVARIAN, RENAL
)


# The Synapse folder containing the clinical data files.
synid_clin_data <- "syn50612196"

library(cli)
library(synapser)
library(purrr)
library(dplyr)
library(here)
library(stringr)
library(magrittr)

synLogin()

# create directories for data and data-raw
dir.create(here("data"), showWarnings = F)
dir.create(here("data-raw"), showWarnings = F)
dir.create(here("data-raw", "genomic"), showWarnings = F)


df_clin_children <- synGetChildren(synid_clin_data) %>%
  as.list %>%
  purrr::map_dfr(.x = .,
                 .f = as_tibble)

if (any(stringr::str_detect(df_clin_children$name, ".csv^"))) {
  warning("Non-CSV files unexpectedly contained in {synid_clin_data}.")
}

syn_store_in_dataraw <- function(sid) {
  synGet(entity = sid, downloadLocation = here("data-raw"))
}

purrr::walk(.x = df_clin_children$id, 
            .f = syn_store_in_dataraw)




# Get the genomic data from the "cBioPortal_files" directory.
df_geno_children <- synGetChildren(synid_cbio_data) %>%
  as.list %>%
  purrr::map_dfr(.x = .,
                 .f = as_tibble)

df_geno_children %<>%
  mutate(
    is_panel = str_detect(name, "^data_gene_panel_.*\\.txt$"),
    is_included = name %in% geno_files_included
  ) %>%
  filter(is_panel | is_included) 

syn_store_in_dataraw_geno <- function(sid) {
  synGet(entity = sid, downloadLocation = here("data-raw", "genomic"))
}

purrr::walk(.x = df_geno_children$id, 
            .f = syn_store_in_dataraw_geno)