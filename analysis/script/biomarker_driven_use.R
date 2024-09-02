# An expansion on an analysis section.

library(fs); library(purrr); library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dft_hdrug_det <- readr::read_rds(
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)

dft_gen_mark <- readr::read_rds(
  here('data', 'no_ca_seq_filter', 'genomic_sum.rds')
)

dft_ind_mapped <- readr::read_rds(
  here('data', 'warner_materials', 'indications_mapped.rds')
)

dir_output <- here('data', 'stories', 'biomarker_driven_use')

# Trastuzumab first:
tras_string_list <- c(
  "Trastuzumab",
  "Trastuzumab deruxtecan",
  "Trastuzumab emtansine"
)
dft_cohort_any_tras <- dft_ind_mapped %>% 
  filter(!is.na(mapped_cohort)) %>%
  filter(regulator %in% "FDA") %>%
  mutate(
    tras = component %in% tras_string_list
  ) %>% 
  group_by(mapped_cohort) %>%
  summarize(has_any_tras = sum(tras, na.rm = T) > 0)
# On label indications exist for NSCLC and bladder, off label for everything else.

dft_hdrug_det %>% 
  filter(cohort %in% pull(filter(dft_cohort_any_tras, !has_any_tras), mapped_cohort)) %>% 
  count(cohort)


dft_tras <- dft_hdrug_det %>%
  # filter(cohort %in% pull(filter(dft_cohort_any_tras, !has_any_tras), mapped_cohort))
  # I'm going to go off script here a bit:  ALL of the NSCLC uses happened to be off label too.
  # So it's only breast cancer where this is approved and most were on label.
  filter(!cohort %in% "Breast") %>%
  group_by(cohort, record_id) %>%
  summarize(any_tras = any(agent %in% tras_string_list), .groups = "drop") 

dft_tras <- left_join(
  dft_tras,
  select(dft_gen_mark, record_id, any_ERBB2),
  by = "record_id"
) %>%
  replace_na(list(any_ERBB2 = FALSE))

readr::write_rds(
  x = dft_tras,
  file = here(dir_output, 'tras_data.rds')
)

library(lme4)
library(broom.mixed)
library(ggmosaic)
dft_tras_model <- glm(
  data = dft_tras,
  formula = any_tras ~ any_ERBB2,
  family = "binomial"
) %>%
  broom::tidy(., conf.int = T)

readr::write_rds(
  x = dft_tras_model,
  here(dir_output, 'tras_model.rds')
)

# for plotting:
dft_tras_stacked <- bind_rows(
  mutate(dft_tras, cohort = "All"),
  dft_tras
) %>%
  mutate(cohort = fct_inorder(cohort)) %>%
  mutate(erbb2_disp = if_else(
    any_ERBB2, "ERBB2 altered (mut/amp)", "ERBB2 not altered"
  ))

gg_tras_mosaic <- ggplot(
  data = dft_tras_stacked,
) +
  geom_mosaic(
    aes(x = product(cohort), fill = any_tras)
  ) + 
  theme_mosaic() +
  facet_wrap(~erbb2_disp) + 
  scale_fill_viridis_d(option = "magma", begin = 0, end = 0.5) + 
  coord_flip() + 
  guides(
    fill = guide_legend(title = "Trastuzumab used")
  ) + 
  theme(
    axis.title.y = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()
  )

readr::write_rds(
  x = gg_tras_mosaic,
  file = here(dir_output, 'tras_gg_mosaic.rds')
)


library(waffle)
dft_tras_stacked %>%
  count(cohort, any_ERBB2, any_tras) %>%
  mutate(lev = paste("ERBB2=", any_ERBB2, " tras=", any_tras)) %>%
  ggplot(data = .,
         aes(fill = lev, values = n)) + 
  geom_waffle(nrow = 100) +
  scale_fill_manual(
    values =  c("#ee99aa", "#994455", "#6699cc", "#004488")[c(1,2,3,4)]
  ) +
  facet_wrap(~cohort)

# A sloppy function structurally that I keep finding a need for:
# Plot a 2x2 table (epi style) as a ggplot2 object.
plot_two_by_two <- function(
    dat,
    var1,
    var2,
    add_odds_ratio = T,
    or_decimals = 1
) {
  nums <- tabyl(dat, !!sym(var1), !!sym(var2)) %>%
    adorn_totals(where = "both") %>%
    as_tibble(.) %>%
    # rearrange so the "true" or 1 (positive) values are top left (convention)
    select(1,3,2,Total) %>%
    slice(c(2,1,3))
  
  if (add_odds_ratio) {
    or_test <- fisher_test_helper(
      cell_11 = nums[[1, 2]],
      cell_10 = nums[[1, 3]],
      cell_01 = nums[[2, 2]],
      cell_00 = nums[[2, 3]],
      alpha = 0.05
    )
    
    or_str <- glue("OR = {formatC(or_test$estimate, format = 'f', digits = 2)} ({formatC(or_test$conf.low, format = 'f', digits = 2)}, {formatC(or_test$conf.high, format = 'f', digits = 2)})")
    
  }
  
  ft <- nums %>%
    rename(` ` = 1) %>%
    mutate(col1 = c(rep(var1, 2), "")) %>%
    relocate(col1, .before = 0L) %>%
    rename(`  ` = col1)
  
  ft <- ft %>%
    flextable(.) %>%
    add_header_row(top = T, values = c("", "", rep(var2, 2), "")) %>%
    merge_v(j = 1) %>%
    merge_at(part = 'header', i = 1, j = 3:4) %>%
    border_remove() %>%
    autofit(.) %>%
    align(j = 1:2, align = 'right') %>%
    align(j = 5, align = 'left') %>%
    align(i = 1, part = 'header', align = 'center') %>%
    align(i = 2, part = 'header', align = 'left')
  
  if (add_odds_ratio) {
    ft <- ft |> 
      add_body_row(top = F, values = c("", rep(or_str, 4))) |>
      merge_at(i = 4, j = 2:5) |>
      italic(i = 4) |>
      align(i = 4, align = "center")
  }
  
  ft <- ft %>%
    hline(i = 1:2, j = 3:4, border = fp_border_default()) %>%
    hline_top(j = 3:4, border = fp_border_default()) %>%
    # hline(i = 0) %>%
    hline(part = "header", i = 1, j = 3:4) %>%
    vline(i = 1:2, j = 1:4, part = "body") %>%
    align(i = 1:3, j = 3:4, align = "left")

    
  return(ft)
  
}


plot_two_by_two(
  dat = dft_tras,
  var1 = 'any_tras',
  var2 = 'any_ERBB2'
)
  


#   
# 
#   
# dfp_crc_biomarkers <- dft_hdrug_det %>% 
#   filter(
#     cohort %in% "CRC", 
#     !valid_ind_exists, 
#     agent %in% c("Trastuzumab", "Vemurafenib")
#   ) %>%
#   group_by(record_id, agent) %>%
#   arrange(regimen_number) %>% 
#   slice(1) %>%
#   ungroup(.) %>%
#   mutate(`Record [reg]` = glue("{record_id} [{regimen_number}]")) %>%
#   left_join(
#     .,
#     dft_gen_mark,
#     by = "record_id"
#   ) %>%
#   mutate(`Record [reg]` = glue("{record_id} [{regimen_number}]")) %>%
#   select(agent, `Record [reg]`, any_BRAF, any_ERBB2) %>%
#   arrange(agent, `Record [reg]`)