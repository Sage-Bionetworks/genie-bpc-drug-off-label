

library(readxl)
library(pdftools)
library(pdftables)
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)

path_guide <- here('data-raw', 'manual', 'guideline')

dft_nccn <- tibble(
  file = dir_ls(path_guide)
)

dft_nccn %<>%
  mutate(
    important_part = str_extract(file, pattern = "_[123].[0-9]{4}.*[0-9]{6}"),
    important_part = str_sub(important_part, start = 4),
    active_year = str_sub(important_part, end = 4),
    date = str_sub(important_part, start = 13, end = 18)
  ) %>%
  select(date, file, active_year) %>%
  mutate(date = lubridate::mdy(date)) %>%
  arrange(date)

dft_nccn %<>%
  mutate(
    pdf_vec = purrr::map(
      .x = file,
      .f = pdf_text
    )
  )

# mmmk, some of the early ones didn't work, but 2003 did and it seemed to have
#  zero hits.  No problem.

dft_nccn %<>%
  mutate(
    broad_string_matches = purrr::map(
      .x = pdf_vec,
      .f = \(z) {
        extract_pdf_strings(z, str_to_match = "[Cc]ape[a-zA-Z]+")
      }
    )
  )

# Unique strings discovered:
unique(Reduce(c, dft_nccn$broad_string_matches))

# The only one I dont want is "Capello" (an author)

dft_nccn %<>%
  mutate(
    string_matches = purrr::map(
      .x = broad_string_matches,
      .f = \(z) z[!(z %in% "Capello")]
    ),
    n_text_matches = purrr::map_dbl(
      .x = string_matches,
      .f = length
    )
  )

dft_nccn_sum <- select(
  dft_nccn,
  date, file, active_year, string_matches, n_text_matches
)

readr::write_rds(
  dft_nccn_sum,
  here('data', 'nccn_panc_capecitabine_sum.rds')
)

