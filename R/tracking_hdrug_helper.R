
# cdl = cohort data list.  This is a very specific format with the columns
#   {cohort, reg} where reg is a list column of regimen datasets.
# The only point of this function is tracking drugs as we process data.
tracking_hdrug_helper <- function(cdl) {
  cdl %>%
    select(reg) %>%
    unnest(reg) 
}
  
# tracking_hdrug_helper(dft_clin_dat_wide)
#
