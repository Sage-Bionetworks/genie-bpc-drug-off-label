library(fs)
library(purrr)
library(here)
purrr::walk(.x = fs::dir_ls(here("R")), .f = source)

dir_out <- here('output', 'manu', 'fig1')

dft_hdrug_det <- readr::read_rds(
  here('data', 'linked_approvals', 'hdrug_determinations.rds')
)

dft_cohort_drug_sum <- dft_hdrug_det %>%
  group_by(cohort, agent) %>%
  summarize(
    n_on = sum(valid_ind_exists),
    n_off = sum(!valid_ind_exists),
    n_exposures = n(),
    .groups = 'drop'
  )

# Release time order:
cohort_order_lev <- c(
  'NSCLC',
  'CRC',
  'Breast',
  'Pancreas',
  'Prostate',
  'Bladder'
)

# Add a cohort label
dft_cohort_drug_sum %<>%
  mutate(.cohort_order = factor(cohort, levels = cohort_order_lev)) %>%
  group_by(cohort) %>%
  mutate(
    .cohort_off = sum(n_off),
    .cohort_exp = sum(n_exposures),
    .cohort_pct = .cohort_off / .cohort_exp * 100
  ) %>%
  mutate(
    cohort_lab = glue(
      '{cohort}: {.cohort_off}/{.cohort_exp} ({round(.cohort_pct,0)}%)'
    ),
  ) %>%
  ungroup(.) %>%
  arrange(.cohort_order) %>%
  mutate(cohort_lab = fct_inorder(cohort_lab)) %>%
  drop_dots

dft_cohort_drug_sum %<>%
  agent_plot_label_helper(
    n_agents_per_group = 5,
    group_vars = cohort_lab
  )

plot_one_cohort_top_agents <- function(
  dat,
  cohort_lab_var,
  agent_lab_var,
  x_scale_trans_fn = 'identity',
  x_scale_name = "Exposures",
  x_breaks = waiver(),
  on_lab = "On-label",
  off_lab = "Off-label"
) {
  dat_long <- dat %>%
    mutate(n_on = -n_on) %>%
    select(
      all_of(
        c(cohort_lab_var, agent_lab_var)
      ),
      agent_order,
      n_off,
      n_on
    ) %>%
    pivot_longer(
      cols = c(n_off, n_on),
      names_to = 'cat',
      values_to = 'n'
    )

  dat_long %<>%
    arrange(desc(cat)) %>%
    mutate(
      cat = case_when(
        cat %in% 'n_off' ~ off_lab,
        cat %in% 'n_on' ~ on_lab
      ),
      cat = fct_inorder(cat)
    )

  dat_long %<>%
    arrange(agent_order) %>%
    mutate('{agent_lab_var}' := fct_rev(fct_inorder(.data[[agent_lab_var]])))

  gg <- ggplot(
    dat_long,
    aes(
      fill = cat,
      groups = .data[[agent_lab_var]],
      y = .data[[agent_lab_var]],
      x = n
    )
  ) +
    geom_bar(position = 'stack', stat = 'identity', orientation = 'y') +
    scale_x_continuous(
      transform = x_scale_trans_fn,
      breaks = x_breaks,
      name = x_scale_name
    ) +
    scale_y_discrete(name = NULL) +
    scale_fill_d3(name = NULL) +
    theme_bw() +
    facet_wrap(vars(.data[[cohort_lab_var]])) +
    theme(
      legend.position = 'bottom'
    )

  gg
}

# Example to do one panel:
# plot_one_cohort_top_agents(
#   filter(dft_cohort_drug_sum, str_detect(cohort_lab, 'NSCLC')),
#   cohort_lab_var = 'cohort_lab',
#   agent_lab_var = 'agent_lab',
#   x_scale_trans_fn = transform_pseudo_log(sigma = 1, base = exp(1)),
#   x_breaks = round(c(-rev(10^(1:4)), -3, 0, 3, 10^(1:4)), 1)
# )

cohort_drug_nest <- dft_cohort_drug_sum %>%
  mutate(coh_copy = cohort_lab) %>%
  nest(.by = coh_copy)

fig1_combiner <- function(list_of_plots) {
  patchwork::wrap_plots(list_of_plots, nrow = 3, ncol = 2) +
    patchwork::plot_layout(guides = 'collect') &
    theme(legend.position = 'bottom')
}

fig1_on_off_counts_pseudolog <- cohort_drug_nest %>%
  mutate(
    plt = purrr::map(
      .x = data,
      .f = \(dat) {
        plot_one_cohort_top_agents(
          dat,
          cohort_lab_var = 'cohort_lab',
          agent_lab_var = 'agent_lab',
          # default params here worked pretty well:
          x_scale_trans_fn = transform_pseudo_log(),
          x_breaks = round(c(-rev(10^(1:4)), -3, 0, 3, 10^(1:4)), 1)
        )
      }
    )
  ) %>%
  pull(plt) %>%
  fig1_combiner(.)

fig1_on_off_counts_linear <- cohort_drug_nest %>%
  mutate(
    plt = purrr::map(
      .x = data,
      .f = \(dat) {
        plot_one_cohort_top_agents(
          dat,
          cohort_lab_var = 'cohort_lab',
          agent_lab_var = 'agent_lab',
        )
      }
    )
  ) %>%
  pull(plt) %>%
  fig1_combiner(.)

ggsave(
  here(dir_out, 'fig1_on_off_counts_pseudolog.pdf'),
  fig1_on_off_counts_pseudolog,
  height = 6,
  width = 12
)

ggsave(
  here(dir_out, 'fig1_on_off_counts_linear.pdf'),
  fig1_on_off_counts_linear,
  height = 6,
  width = 12
)

readr::write_rds(
  x = fig1_on_off_counts_pseudolog,
  here(dir_out, 'gg_on_off_counts_pseudolog.rds')
)

readr::write_rds(
  x = fig1_on_off_counts_linear,
  here(dir_out, 'gg_on_off_counts_linear.rds')
)
