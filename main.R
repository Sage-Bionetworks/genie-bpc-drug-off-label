# Description: Top level workflow for the project.  
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

source(here('analysis', 'script', 'create_folders.R'))

# source(here('analysis', 'script', 'get_raw_data.R'))
source(here('analysis', 'script', 'process_clinical_data.R'))
source(here('analysis', 'script', 'remove_old_drugs_save_hdrug.R'))
source(here('analysis', 'script', 'find_concomitant_drugs.R')) # takes a minute.
source(here('analysis', 'script', 'process_genomic_data.R'))
source(here('analysis', 'script', 'crosswalk_indications.R'))
source(here('analysis', 'script', 'derive_first_approvals.R'))

source(here('analysis', 'script', 'declare_with_tests.R'))
source(here('analysis', 'script', 'link_drugs_indications.R'))
source(here('analysis', 'script', 'stepwise_test_analysis.R'))
source(here('analysis', 'script', 'stepwise_tree_explanation.R'))

source(here('analysis', 'script', 'scrape_guidelines.R'))

# no longer an rmarkdown doc:
# rmarkdown::render(
#   input = here('analysis', 'report', '01-genie-bpc-off-label.Rmd'),
#   output_file = here('output', '01-genie-bpc-off-label.html')
# )

# Render for 02 report, 03 report, etc.