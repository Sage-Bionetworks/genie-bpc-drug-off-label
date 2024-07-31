library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Load the indications data (limited to FDA) and crosswalks:
dft_ind_lim <- readr::read_rds(
  here('data', 'warner_materials', 'indications_mapped_limited.rds')
)
dft_cw_condition <- readr::read_rds(
  here('data', 'warner_materials', 'cw_condition.rds')
)
dft_cw_drug <- readr::read_rds(
  here('data', 'warner_materials', 'cw_drug.rds')
)
vec_excl_drugs <- readr::read_rds(
  here('data', 'warner_materials', 'old_drugs_list.rds')
)

# Load the hdrug cohort data:
dft_hdrug_cohort_lim <- readr::read_rds(
  here('data', 'cohort', 'hdrug_with_conmeds.rds')
)


# Fix up the date column from the indications sheet:
dft_ind_lim %<>% 
  mutate(
    date = case_when(
      # If the date is uncertain we just assume it was always approved.
      # This has the effect of never flagging someone as taking these too early.
      date %in% "Uncertain date" ~ ymd(rep('1900-01-01', times = n())),
      T ~ suppressWarnings(ymd(date))
    )
  )



dft_poss_app <- make_possible_indication_cohort( 
  dat_hdrug = dft_hdrug_cohort_lim,
  dat_ind = dft_ind_lim
)


# Leaves open the possibility to add more indications based on TMB, etc (not cohort)

dft_poss_app <- dft_poss_app %>%
  add_check_met(.) 
 
dft_poss_app %<>% add_check_monotherapy(.) 


# Should move this over to the clinical processing file
# unique_drugs_in_overlap <- sort(unique(purrr::reduce(dft_poss_app$drug_overlaps, c)))


# Working tip: add_check_with_simple() has a verbose_results setting to print
#   some checks.  Use this in line-by-line runs to look for typos.

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "5-FU-based regimen",
    agent_req = "Fluorouracil"
  )

# agent_req defaults to with_req
# test_name defaults to test_with_{agent_req}
dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Cisplatin"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Docetaxel",
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Gemcitabine",
    agent_req = "Gemcitabine Hydrochloride",
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Letrozole"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Fulvestrant"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Abemaciclib"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Palbociclib"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Ribociclib"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Ipilimumab"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Irinotecan",
    agent_req = "Irinotecan Hydrochloride" # Add Irinotecan liposome?
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Capecitabine",
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Bevacizumab",
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Exemestane",
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Erlotinib",
    agent_req = "Erlotinib Hydrochloride"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Carboplatin"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Dabrafenib"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Trametinib"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Nivolumab"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Pembrolizumab"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Cetuximab"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Encorafenib"
  )



# add_check_with_simple can also handle clauses that only have AND operators:

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Doxorubicin AND Cyclophosphamide AND Paclitaxel",
    agent_req = c(
      "Doxorubicin Hydrochloride",
      "Cyclophosphamide",
      "Paclitaxel"
    ),
    # For these combo regimens I'm going to give them new names because
    #   the auto generated variable names get burdensome long.
    test_name = "test_with_dox_cyclo_pac"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Carboplatin AND Pembrolizumab",
    agent_req = c(
      "Carboplatin",
      "Pembrolizumab"
    ),
    test_name = "test_with_carbo_pembro"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Carboplatin AND Paclitaxel",
    agent_req = c(
      "Carboplatin",
      "Paclitaxel"
    ),
    test_name = "test_with_carbotaxol"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Carboplatin AND Pemetrexed",
    agent_req = c(
      "Carboplatin",
      "Pemetrexed"
    ),
    test_name = "test_with_carbopem"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "FOLFIRI",
    agent_req = c(
      "Leucovorin Calcium",
      "Fluorouracil",
      "Irinotecan Hydrochloride"
    ),
    test_name = "test_with_folfiri"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "FOLFOX",
    agent_req = c(
      "Leucovorin Calcium",
      "Fluorouracil",
      "Oxaliplatin"
    ),
    test_name = "test_with_folfox"
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Fluorouracil AND Folinic acid",
    agent_req = c(
      "Fluorouracil",
      "Leucovorin Calcium"
    ),
    test_name = "test_with_folf" # fol = folinic acid = leucovorin
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Paclitaxel, nanoparticle albumin-bound AND Carboplatin",
    agent_req = c(
      "Nabpaclitaxel",
      "Carboplatin"
    ),
    test_name = "test_with_carbo_nabpac" 
  )

# We can't check prednisone in BPC (not collected) so we just do Abi here.
dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Abiraterone AND (Prednisone|Prednisolone)",
    agent_req = c(
      "Abiraterone Acetate"
    ),
    test_name = "test_with_abiraterone" 
  )

dft_poss_app %<>%
  add_check_with_simple(
    dat_poss_app = .,
    with_req = "Cisplatin AND Gemcitabine",
    agent_req = c(
      "Cisplatin",
      "Gemcitabine Hydrochloride"
    ),
    test_name = "test_with_gem_cis" 
  )


# Example to chekc this is working:
# dft_poss_app %>%
#   filter(ind_with %in% "FOLFOX") %>%
#   arrange(test_with_folfox) %>%
#   mutate(drug_over_str = purrr::map_chr(
#     .x = drug_overlaps,
#     .f = \(z) paste(z, collapse = ",")
#   )) %>%
#   select(drug_over_str, test_with_folfox) %>%
#   View(.)










dft_poss_app %<>% add_check_date_definite(.)
  
dft_poss_app %<>% add_check_multiple_tests(dat_poss_app = .) # by default selects all "test" columns


readr::write_rds(
  dft_poss_app,
  here('data', 'linked_approvals', 'possible_approvals.rds')
)

dft_hdrug_determinations <- summarize_possible_approvals_2(dft_poss_app)

# tabyl(dft_hdrug_determinations, cohort, valid_ind_exists)
# tabyl(dft_hdrug_determinations, failure_type, cohort) %>%
#   View(.)

levs_failure_type <- c(
  "No indications found",
  "Specific conditions not met",
  "Started before approval"
)

dft_hdrug_determinations %<>%
  mutate(
    failure_type_f = case_when(
      failure_type %in% "test_ind_exists" ~ levs_failure_type[1],
      failure_type %in% "test_date_definite" ~ levs_failure_type[3],
      # All other types are lumped:
      !is.na(failure_type) ~ levs_failure_type[2],
      T ~ NA_character_
    ),
    failure_type_f = factor(failure_type_f, levels = levs_failure_type)
  )

readr::write_rds(
  dft_hdrug_determinations,
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)










