library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

dft_ind <- readr::read_csv(
  here('data-raw', 'manual', 'indications working copy.csv')
)

dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)

dft_hdrug_cohort <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug)

# Notes on this:
# - I did not include pancreatic NET as neuroendocrine tumors of the pancreas
#.   didn't appear in the oncotree codes for the data guide.
# - Malignant solid neoplasm NTRK-mutated could have things later on.
# - Skeletal metastases from solid tumors could have things later on.
# - Small bowel adenocarinoma was not added (considered for CRC) as the SBC onoctree code was not part of the inclusion criteria.
# - Benign prostatic hyperplasia was not included for prostate.
# - Several strings with "malignant neoplasm" are listed.
dft_cw_condition <- tribble(
  ~condition, ~cohort,
  "Breast cancer", "Breast",
  "Prostate cancer", "Prostate",
  "Colorectal cancer", "CRC",
  "Non-small cell lung cancer nonsquamous", "NSCLC", # obviously needs later mapping
  "Pancreatic cancer", "Pancreas",
  "Colon cancer", "CRC",
  "Non-small cell lung cancer squamous", "NSCLC",
  "Bladder cancer", "Bladder",
  "Rectal cancer", "CRC",
  "Bladder CIS", "Bladder" # I know CIS probably doesn't merit inclusion.
)

readr::write_rds(
  dft_cw_condition,
  here('data', 'warner_materials', 'cw_condition.rds')
)

bpc_drugs <- dft_hdrug_cohort %>%
  count(agent, sort = T) %>%
  mutate(
    found_in_ind = tolower(agent) %in% unique(tolower(dft_ind$component))
  )

exact_matches <- bpc_drugs %>%
  filter(found_in_ind) %>%
  select(agent) %>%
  mutate(component = agent)

strings_to_trim <- c(
  "Hydrochloride",
  "Calcium",
  "Citrate",
  "Disodium",
  "Acetate",
  "Dichloride",
  "Dimaleate",
  "Sulfate",
  "Hydrochloride",
  "Malate",
  "Mesylate",
  "Camsylate",
  "Phosphate",
  "Tartrate",
  "Smalate"
)

bpc_drugs %<>%
  mutate(
    agent_trim = str_replace_all(
      agent, 
      pattern = paste(strings_to_trim, collapse = "|"),
      replacement = ""
    ),
    agent_trim = str_trim(agent_trim)
  ) %>%
  mutate(
    trimmed_in_ind = tolower(agent_trim) %in% unique(tolower(dft_ind$component))
  ) 

trimmed_matches <- bpc_drugs %>%
  filter(!str_detect(agent, "Etoposide")) %>%
  filter(trimmed_in_ind & !found_in_ind) %>%
  mutate(component = agent_trim) %>%
  select(agent, component)
  
# For the ones that don't match exactly we'll did some looking:
# bpc_drugs %>% 
#   filter(!found_in_ind & !trimmed_in_ind) %>% 
#   select(agent, n) %>%
#   print(n = 500) #%>% slice(1:10)
# dft_ind %>% 
#   filter(str_detect(tolower(component), "vedoliz")) %>%
#   count(component, regulator) 

# The manually added ones:
#dft_cw_component <- tribble(
manual_matches <- tribble(
  ~component, ~agent, 
  "Paclitaxel nanoparticle albumin-bound", "Nabpaclitaxel",
  "Goserelin", "Goserlin Acetate",
  "Radium-223", "Radium RA 223 Dichloride",
  "Lapatinib", "Lapatinib Ditosylate",
  "Sipuleucel-T", "Sipuleucel T",
  "Ziv-aflibercept", "Ziv Aflibercept",
  "Sorafenib", "Sorafenib Tosylate",
  "Interferon alfa-2b", "Interferon"
  # Leaving this one behind:
#  "Interferon alfa-2b", "Recombinant Interferon Alfa",
)

dft_cw_drug <- rbind(
  mutate(exact_matches, agent_map_type = "exact"),
  mutate(trimmed_matches, agent_map_type = "trimmed"),
  mutate(manual_matches, agent_map_type = "manual")
)

if (dft_cw_drug %>%
    filter(duplicated(component)) %>%
    nrow()
) {
  cli_abort("Duplicate agent rows in drug mapping table")
}

readr::write_rds(
  dft_cw_drug,
  here('data', 'warner_materials', 'cw_drug.rds')
)
  

dft_cw_drug_limited <- dft_ind %>%
  filter(!is.na(component)) %>%
  filter(!(component %in% "NONE")) %>% # pending decision here
  filter(regulator %in% "FDA")


readr::write_rds(
  dft_cw_drug_limited,
  here('data', 'warner_materials', 'cw_drug_limited.rds')
)








