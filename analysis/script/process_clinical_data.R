# Description:  Transform data as needed and save in /data/cohort.

library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)



dft_data_names <- tribble(
  ~short_name, ~synapse_name,
  "ca_ind", "cancer_level_dataset_index.csv",
  "ca_non_ind", "cancer_level_dataset_non_index.csv",
  "cpt", "cancer_panel_test_level_dataset.csv",
  "pt", "patient_level_dataset.csv",
  "reg", "regimen_cancer_level_dataset.csv"
)


dft_clin_dat <- tibble(
  cohort = dir("data-raw")
) %>%
  mutate(cohort_path = here("data-raw", cohort))

folder_load_helper <- function(fold, dn) {
  vec_paths <- fs::dir_ls(fold)
  
  dat_names <- dir(fold)
  
  rtn <- tibble(
    path = vec_paths,
    syn_name = dat_names
  )
  
  rtn %<>%
    mutate(
      dat = purrr::map(
        .x = path,
        .f = (function(p) {
          read_csv(file = p, show_col_types = F) 
        })
      )
    )
  
  rtn %<>% select(syn_name, dat) %>%
    left_join(., dn, by = c(syn_name = "synapse_name"))
  
  return(rtn)
        
}

# Run for one cohort example:
# folder_load_helper(slice(dft_clin_dat,1)$cohort_path,
#                    dn = dft_data_names)

dft_clin_dat %<>%
  mutate(
    loaded_data = purrr::map(
      .x = cohort_path,
      .f = (function(p) {
        folder_load_helper(p, dn = dft_data_names)
      })
    )
  ) %>%
  unnest(loaded_data)


# At this point, apply any crosswalking, coding or filtering needed for analysis.


dft_clin_dat_wide <- dft_clin_dat %>%
  select(cohort, short_name, dat) %>%
  pivot_wider(
    names_from = short_name,
    values_from = dat
  )

# TODO:  regimen filtering for index cases.

      



###############
# Output data #
###############

clin_output_helper <- function(dat, name, subfolder) {
  readr::write_rds(
    x = dat,
    file = here('data', 'cohort', subfolder, paste0(name, ".rds"))
  )
}

# Example for one row:
# clin_output_helper(
#   dat = (dft_clin_dat %>% slice(1) %>% pull(dat)),
#   name = (dft_clin_dat %>% slice(1) %>% pull(short_name)),
#   subfolder = (dft_clin_dat %>% slice(1) %>% pull(cohort))
# )

# Do all the rows:
purrr::pwalk(
  .l = with(
    dft_clin_dat,
    list(
      dat = dat,
      name = short_name,
      subfolder = cohort
    )
  ),
  .f = clin_output_helper
)
