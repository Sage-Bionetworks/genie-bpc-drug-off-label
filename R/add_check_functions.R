add_check_met <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    mutate(
      test_met = case_when(
        # This check only really applies when we have metastatic.
        # For the moment we're ignoring "advanced or metastatic" and similar.
        is.na(stage_or_status) ~ T, 
        !(stage_or_status %in% "Metastatic") ~ T,
        dmet_at_drug_start ~ T,
        T ~ F
      )
    )
  return(dat_poss_app)
}


add_check_date_definite <- function(
    dat_poss_app
) {
  dat_poss_app %<>%
    mutate(
      test_date_definite = case_when(
        drug_start_date_max < ind_date ~ F,
        T ~ T
      )
    )
  return(dat_poss_app)
}

add_check_date_possible <- function(
    dat_poss_app
) {
  
  dat_poss_app %<>%
    mutate(
      test_date_possible = case_when(
        drug_start_date_min < ind_date ~ F,
        T ~ T
      )
    )
  return(dat_poss_app)
}
