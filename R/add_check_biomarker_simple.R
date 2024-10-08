
#' @param dat_poss_app A 'possible approvals' dataset, made with
#'   make_possible_indication_cohort().
#' @param with_req The 'with' entry required to merit this check.
#' @param drug_req The 'agent' entry required to satisfy this check.
add_check_biomarker_simple <- function(
    dat_poss_app,
    ind_sheet_bio_req,
    neg_flags_violating = NULL,
    test_name = NULL,
    verbose_results = T
) {
  if (!any(dat_poss_app$ind_biomarker %in% ind_sheet_bio_req)) {
    cli_warn("There are no rows with biomarker = {ind_sheet_bio_req} - typo?")
  }
  
  if (is.null(neg_flags_violating)) {
    neg_flags_violating = ind_sheet_bio_req
  }
  
  if (is.null(test_name)) {
    # default:  use the agent_req name.
    test_name = paste0(
      "test_bio_", 
      tolower(
        str_replace_all(
          paste(
            neg_flags_violating, collapse = "_and_"
          ),
          " ", "_"
        )
      )
    )
  }
  
  
  dat_poss_app %<>%
    mutate(
      .violation = map_lgl(
        .x = biomark_neg,
        .f = \(z) any(neg_flags_violating %in% z)
      ),
      .test_temp = case_when(
        is.na(ind_biomarker) ~ T, 
        !(ind_biomarker %in% ind_sheet_bio_req) ~ T,
        .violation ~ F,
        T ~ T
      )
    )
  
  if (verbose_results) {
    # it would be odd (not impossible) if a drug we're check had NO overlapping uses in the data.
    verbose_dat <- dat_poss_app %>%
      filter(
        !is.na(ind_biomarker),
        ind_biomarker %in% ind_sheet_bio_req 
      )
    nrow_tot <- nrow(verbose_dat)
    nrow_passed <- nrow(filter(verbose_dat, .test_temp))
    nrow_failed <- nrow(filter(verbose_dat, !.test_temp))
    cli_inform("Of the {nrow_tot} rows with ind_biomarker = {ind_sheet_bio_req}, {nrow_failed} were definitely not {neg_flags_violating} (failing) and {nrow_passed} were (passing).")
  }
  
  dat_poss_app %<>%
    select(-.violation) %>%
    rename({{test_name}} := .test_temp)
  
  return(dat_poss_app)
  
}
