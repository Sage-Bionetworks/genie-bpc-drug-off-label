
# Leftover functions after crosswalk_indications.R


dft_ind_converted <- left_join(
  dft_cw_drug_limited,
  dft_cw_drug,
  by = "component"
) %>%
  rename(mapped_agent = agent)

dft_ind_converted <- left_join(
  dft_ind_converted,
  dft_cw_condition,
  by = 'condition'
) %>%
  rename(mapped_cohort = cohort)

dft_ind_converted %<>%
  filter(!is.na(mapped_cohort) & !is.na(mapped_agent))

possible_approvals <- left_join(
  (dft_hdrug_cohort %>% 
     select(cohort, record_id, ca_seq, regimen_number, drug_number, agent)
  ),
  (dft_ind_converted %>%
     select(
       cohort = mapped_cohort,
       agent = mapped_agent,
       date,
       context, 
       stage_or_status,
       condition,
       agent_map_type
     ) %>%
     mutate(approval_exists = T)
  ),
  by = c('cohort', 'agent'),
  relationship = "many-to-many"
)

possible_approvals %<>% 
  mutate(
    approval_exists = if_else(
      is.na(approval_exists), 
      F, 
      approval_exists
    )
  )

# oooo messy.
dft_hdrug_app <- possible_approvals %>%
  # This is excessive to get uniqueness - I just want the drug and cohort for
  #   the sake of documentation.
  group_by(
    cohort, record_id, ca_seq, regimen_number, drug_number, agent
  ) %>%
  summarize(
    approved = sum(approval_exists, na.rm = T) >= 1,
    .groups = "drop"
  )

dft_hdrug_app %>%
  group_by(cohort, agent) %>%
  summarize(
    n_uses = n(),
    n_approved = sum(approved, na.rm = T),
    prop_approved = n_approved / n_uses,
    .groups = "drop"
  )






# Notes:

# - I did not add nabpaclitaxel since it appeared there were papers debating
#.  its equivalence to paclitaxel.
# - Wasn't really sure what to do with interferon.  I did the most common.




bpc_drugs <- count(dft_hdrug_cohort, agent, sort = T)

bpc_drugs %>%
  mutate(
    found_in_ind = agent %in% unique(dft_ind$component)
  )

dft_ind %>%
  filter(str_detect(component, "Fluorou"))

bpc_drugs

dft_ind %>% 
  filter(condition %in% "NONE") %>% 
  mutate(
    found_in_bpc_drugs = component %in% unique(bpc_drugs$agent)
  ) %>%
  # filter(found_in_bpc_drugs) %>%
  arrange(regulator, date) %>%
  View(.)


