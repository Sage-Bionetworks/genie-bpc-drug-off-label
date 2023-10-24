# Description: Creates any folders needed for project.
# Author: Alex Paynter

library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

fs::dir_create(here('output'))
