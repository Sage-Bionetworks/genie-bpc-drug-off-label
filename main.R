# Description: Top level workflow for the project.  
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

source(here('analysis', 'script', 'create_folders.R'))
source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'process_clinical_data.R'))
rmarkdown::render(
  input = here('analysis', 'report', 'genie-bpc-off-label.Rmd'),
  output_file = here('output', 'genie-bpc-off-label.html')
)
