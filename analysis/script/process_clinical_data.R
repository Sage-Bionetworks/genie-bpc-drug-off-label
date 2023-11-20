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



dft_clin_dat_wide <- dft_clin_dat %>%
  select(cohort, short_name, dat) %>%
  pivot_wider(
    names_from = short_name,
    values_from = dat
  )


# dft_clin_dat_wide
# reg_breast <- dft_clin_dat_wide %>% filter(cohort %in% "BrCa") %>% pull(reg) %>% `[[`(.,1)
# get_colname_dat(reg_breast)



# Fix the breast-type deviances in regimen data:
dft_clin_dat_wide %<>%
  mutate(
    # hreg = harmonized regimen data.
    hreg = purrr::map(
      .x = reg,
      .f = \(x) {
        breast_regimen_fix(x) 
      }
    )
  )


# Sloppy way to confirm no differences except on breast:
# 
# dft_clin_dat_wide %<>%
#   mutate(
#     reg = purrr::map(
#       .x = reg,
#       .f = \(x) {
#         arrange(x, record_id, ca_seq, regimen_number)
#       }
#     ),
#     hreg = purrr::map(
#       .x = hreg,
#       .f = \(x) {
#         arrange(x, record_id, ca_seq, regimen_number)
#       }
#     )
#   )
# dft_clin_dat_wide %>%
#   select(cohort, reg, hreg) %>%
#   mutate(
#     iden = purrr::map2_chr(
#       .x = reg,
#       .y = hreg,
#       .f = \(x,y) {print(waldo::compare(x,y)); return("dummy")}
#     )
#   )



# Select only the columns we have consistently across all (after fixes)
hreg_req_col <- c(
  # The ones I actually want:
  'cohort',
  'record_id',
  'institution',
  'ca_seq',
  'regimen_number',
  'redcap_ca_index',
  'drugs_num',
  "drugs_ct_yn",
  "regimen_drugs",
  "drugs_drug_1", 
  "drugs_drug_2", 
  "drugs_drug_3",
  "drugs_drug_4", 
  "drugs_drug_5",
  
  # dob to start
  "drugs_startdt_int_1", 
  "drugs_startdt_int_2", 
  "drugs_startdt_int_3", 
  "drugs_startdt_int_4", 
  "drugs_startdt_int_5", 
  
  # dx to start 
  "dx_drug_start_int_1", 
  "dx_drug_start_int_2", 
  "dx_drug_start_int_3", 
  "dx_drug_start_int_4", 
  "dx_drug_start_int_5",
  
  # dx to end if dc'd
  "dx_drug_end_int_1", 
  "dx_drug_end_int_2", 
  "dx_drug_end_int_3", 
  "dx_drug_end_int_4", 
  "dx_drug_end_int_5",
  
  # dx to (end OR last known admin)
  "dx_drug_end_or_lastadm_int_1", 
  "dx_drug_end_or_lastadm_int_2", 
  "dx_drug_end_or_lastadm_int_3", 
  "dx_drug_end_or_lastadm_int_4", 
  "dx_drug_end_or_lastadm_int_5",
  
  "dx_reg_start_int",
  "dx_reg_end_any_int", # dx to end of first drug
  "dx_reg_end_all_int",
  
  "os_d_status",
  "tt_os_d1_days", 
  "tt_os_d2_days", 
  "tt_os_d3_days", 
  "tt_os_d4_days", 
  "tt_os_d5_days",
  
  "os_g_status", # g = ... regimen? can't figure that one out.
  "tt_os_g_days",
  
  "pfs_i_and_m_g_status",
  "tt_pfs_i_and_m_g_days"
)

dft_clin_dat_wide %<>%
  mutate(
    # hreg = harmonized regimen data.
    hreg = purrr::map(
      .x = hreg,
      .f = \(x) {
        select(x, all_of(hreg_req_col)) %>%
          arrange(record_id, ca_seq, regimen_number)
      }
    )
  )


# Add the versions of the regimen variables that include last known administration.
dft_clin_dat_wide %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = \(x) {
        add_regimen_lastadm_vars(x)
      }
    )
  )

# Remove any investigational drug regimens from the dataset.
dft_clin_dat_wide %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = \(x) {
        x %>%
          filter(str_detect(regimen_drugs, "Investigational Drug", negate = T))
      }
    )
  )

# Create a drug-keyed dataset:
dft_clin_dat_wide %<>%
  mutate(
    hdrug = purrr::map(
      .x = hreg,
      .f = create_drug_dat
    )
  )


# A few of the cohort names just annoy me:
dft_clin_dat_wide %<>% fix_cohort_names


readr::write_rds(
  file = here('data', 'no_ca_seq_filter', 'clin_dat_wide.rds'),
  x = dft_clin_dat_wide
)





# New proposal: we will start with "first and only" cancer cases.
# This is neatly described in PRISSMM fortunately.
# Later on this will probably get significantly more annoying.
help_first_and_only <- function(dat) {
  filter(dat, ca_seq %in% 0)
}

dft_clin_dat_wide %<>%
  mutate(
    across(
      .cols = -c(cohort, pt),
      .fns = (function(l_of_dat) {
        purrr::map(
          .x = l_of_dat,
          .f = help_first_and_only
        )
      })
    )
  )

# You will now notice that the ca_non_ind column has no data, as expected:
# lapply(dft_clin_dat_wide$ca_non_ind, nrow)
# We could delete it for coherence, but I'm relying on having it so... nevermind.

dft_clin_dat_wide %<>%
  # for the patient column we'll just limit to those which are in the ca_ind column:
  mutate(
    pt = purrr::map2(
      .x = pt,
      .y = ca_ind,
      .f = \(x,y) {
        r <- y$record_id
        x <- filter(x, record_id %in% r)
        return(x)
      }
    )
  )

# Now it's expected that pt and ca_seq have exactly the same number of rows:
# dft_clin_dat_wide %>% select(ca_ind, pt)
    



###############
# Output data #
###############

# I'm switching output formats to just one object:
readr::write_rds(
  x = dft_clin_dat_wide,
  file = here('data', 'cohort', 'clin_dat_wide.rds')
)

