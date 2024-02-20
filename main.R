# Description: Top level workflow for the project.  
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

source(here('analysis', 'script', 'create_folders.R'))

# Todo: Fix this so it can be run again without pasting data copies all over.
# source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'process_clinical_data.R'))
source(here('analysis', 'script', 'crosswalk_indications.R'))
source(here('analysis', 'script', 'link_drugs_indications.R'))

rmarkdown::render(
  input = here('analysis', 'report', '01-genie-bpc-off-label.Rmd'),
  output_file = here('output', '01-genie-bpc-off-label.html')
)
