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


dft_clin_dat_wide













# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # For those with more than one index cancer, we will generally look at only the first one.
# 
# dft_clin_dat_wide %<>%
#   mutate(
#     ca_ind = purrr::map(
#       .x = ca_ind,
#       .f = (function(d) {
#         d %>%
#           group_by(record_id) %>%
#           arrange(ca_seq) %>%
#           slice(1) %>%
#           ungroup
#       })
#     )
#   )
# 
# 
# # Filter out index cancers that are preceded by a non-index case.
# dft_clin_dat_wide %<>%
#   mutate(
#     ca_ind = purrr::map2(
#       .x = ca_ind,
#       .y = ca_non_ind,
#       .f = filter_pre_nonindex
#     )
#   )
# 
# 
# # Filter the regimen data down to only {record_id, ca_seq} pairs found in the reference data.
# dft_clin_dat_wide %<>%
#   mutate(
#     reg = purrr::map2(
#       .x = ca_ind,
#       .y = reg,
#       .f = (function(ref_dat, dat_to_filter) {
#         inner_join(
#           select(ref_dat, record_id, ca_seq),
#           dat_to_filter,
#           by = c("record_id", "ca_seq")
#         )
#       })
#     )
#   )
# 
# dft_clin_dat <- dft_clin_dat_wide %>%
#   pivot_longer(
#     cols = -cohort,
#     names_to = "short_name",
#     values_to = "dat"
#   )
# 
# 
# ###############
# # Output data #
# ###############
# 
# clin_output_helper <- function(dat, name, subfolder) {
#   readr::write_rds(
#     x = dat,
#     file = here('data', 'cohort', subfolder, paste0(name, ".rds"))
#   )
# }
# 
# # Example for one row:
# # clin_output_helper(
# #   dat = (dft_clin_dat %>% slice(1) %>% pull(dat)),
# #   name = (dft_clin_dat %>% slice(1) %>% pull(short_name)),
# #   subfolder = (dft_clin_dat %>% slice(1) %>% pull(cohort))
# # )
# 
# # Do all the rows:
# purrr::pwalk(
#   .l = with(
#     dft_clin_dat,
#     list(
#       dat = dat,
#       name = short_name,
#       subfolder = cohort
#     )
#   ),
#   .f = clin_output_helper
# )
