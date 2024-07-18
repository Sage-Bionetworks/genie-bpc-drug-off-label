library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

# Load and create the hdrug cohort data:
dft_cohort_cases <- readr::read_rds(
  here('data', 'cohort', 'clin_dat_wide.rds')
)
dft_all_cases <- readr::read_rds(
  here('data', 'no_ca_seq_filter', 'clin_dat_just_before_drug_removal.rds')
)


dft_hdrug_cohort <- dft_cohort_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug) 





# The list of all cases hasn't been processed into an hdrug dataset yet, so we 
#   need to repeat some steps (skipping removals):
dft_all_cases %<>%
  mutate(
    # hreg = harmonized regimen data.
    hreg = purrr::map(
      .x = hreg,
      .f = select_hreg_columns
    )
  )
# Add the versions of the regimen variables that include last known administration.
dft_all_cases %<>%
  mutate(
    hreg = purrr::map(
      .x = hreg,
      .f = \(x) {
        add_regimen_lastadm_vars(x)
      }
    )
  )
# Create a drug-keyed dataset:
dft_all_cases %<>%
  mutate(
    hdrug = purrr::map(
      .x = hreg,
      .f = create_drug_dat
    )
  )
# Trim whitespace from agent names.  Now a bigger problem in NSCLCv3.1
dft_all_cases %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(z) {
        mutate(z, agent = str_trim(agent))
      }
    )
  )
# Change "HCL" to Hydrochloride now that we have round 2 of NSCLC... just why?
dft_all_cases %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(z) {
        mutate(z, agent = str_replace(agent, "HCL", "Hydrochloride"))
      }
    )
  )
# Fix one drug names which cause particular problems down the line.
# "Etoposide" only has phosphate appended once and it causes some 
#    headaches on linking to the Warner materials to have both.  Easier to
#    change the data in this one case.
dft_all_cases %<>%
  mutate(
    hdrug = purrr::map(
      .x = hdrug,
      .f = \(x) {
        x %>%
          mutate(
            agent = if_else(
              agent %in% "Etoposide Phosphate",
              "Etoposide",
              agent
            )
          )
      }
    )
  )

# Now we can get the drug list:
dft_hdrug_all <- dft_all_cases %>%
  select(cohort, hdrug) %>%
  unnest(hdrug) 


