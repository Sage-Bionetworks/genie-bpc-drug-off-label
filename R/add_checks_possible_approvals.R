
add_checks_possible_approvals <- function(
    dat_poss_app,
    test_cols_to_include = NULL
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
  
  # Add an "all" test.
  if (is.null(test_cols_to_include)) {
    test_cols_to_include <- names(dat_poss_app)[
      str_detect(names(dat_poss_app), "^test_")
    ]
  }
  
  dat_poss_app %<>%
    mutate(
      test_all = as.logical(pmin(!!!rlang::syms(test_cols_to_include)))
    )
  
  
  return(dat_poss_app)
}
