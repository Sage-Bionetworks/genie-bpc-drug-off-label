
dft_drug_tracking <- readr::read_rds(
  here('data', 'cohort', 'drug_tracking_02.rds')
)

dft_drug_tracking %<>% 
  mutate(step = forcats::fct_inorder(step))

drug_sum_help <- function(dat) {
  dat %>%
    summarize(
      n_uses = n(),
      n_drug = length(unique(agent)),
      n_pts = length(unique(record_id)),
      n_cases = length(unique(paste0(record_id, "QZW", ca_seq)))
    )
}

dft_flow_all <- dft_drug_tracking %>%
  mutate(
    sum = purrr::map(
      .x = drug_key,
      .f = drug_sum_help
    )
  ) %>%
  select(-drug_key) %>%
  unnest(sum) %>%
  mutate(prop_lost = (lag(n_uses) - n_uses)/lag(n_uses))
```

## All cohort numbers

In the table below we start with the raw data, and each row is a processing step which builds on the previous.  The final row is the analysis data which we attempt classify as on or off label.

```{r, include = T}
dft_flow_all %>%
  select(step, n_uses, n_drug, n_pts) %>%
  # mutate(prop_lost = round(prop_lost, 3)) %>%
  flextable(.) %>%
  theme_booktabs(.) %>%
  autofit(.)
```

**Notes:**
  
  - A use is one person using one drug for one period of time.  `n_uses` counts this.  `n_drug` and `n_pts` counts the number of unique drugs and people with at least one observed use.  `prop_lost` is the proportion of the cohort in the previous step filtered out by this one.

## Cohort split numbers



```{r}
dft_flow_cohort <- dft_drug_tracking %>%
  mutate(
    sum = purrr::map(
      .x = drug_key,
      .f = \(x) {
        x %>% group_by(cohort) %>% drug_sum_help
      }
    )
  ) %>%
  select(-drug_key) %>%
  unnest(sum) %>%
  fix_cohort_names(.) %>%
  arrange(cohort) 

gg_cohort_drug_prop_loss <- dft_flow_cohort %>%
  group_by(cohort) %>%
  mutate(prop_lost = (lag(n_uses) - n_uses)/lag(n_uses)) %>%
  ungroup(.) %>%
  filter(!step %in% "Raw data") %>%
  mutate(cohort_r = fct_rev(fct_inorder(cohort))) %>%
  ggplot(
    data = .,
    aes(x = prop_lost, y = cohort_r, fill = cohort)
  ) + 
  geom_col() + 
  theme_bw() +
  theme(
    strip.text = element_text(hjust = 0),
    legend.position = "none",
    axis.title.y = element_blank()
  ) + 
  scale_x_continuous(
    expand = expansion(add = c(0, 0.01), mult = c(0, 0)),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = label_percent(),
    name = "Proportion of drug uses lost in step"
  ) +
  scale_fill_vibrant() + 
  facet_wrap(vars(step), ncol = 1) 


dfp_cohort_drug <- dft_flow_cohort %>%
  select(step, cohort, n_uses) %>%
  pivot_wider(
    names_from = 'cohort',
    values_from = 'n_uses'
  )


```


### Drug overview

First we focus on just the proportion of drug uses lost in each step, since that is really our main unit of analysis.

```{r}
#| include: true
#| fig-height: 7
#| fig-width: 4
gg_cohort_drug_prop_loss
```

**Notes:**
  
  - The reason for the bladder cancer cohort being so heavily affected leaps out:  Bladder is the most restricted cohort by literally every step.


```{r}
gg_use_prop_left <- dft_flow_cohort %>%
  group_by(cohort) %>%
  mutate(prop_remaining = n_uses / max(n_uses)) %>%
  ungroup(.) %>%
  mutate(step_rev = fct_rev(step)) %>%
  ggplot(
    data = .,
    aes(x = prop_remaining, y = step_rev, group = cohort, color = cohort)
  ) +
  geom_path() +
  theme_bw() +
  scale_x_continuous(
    expand = expansion(add = c(0, 0.01), mult = c(0, 0)),
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25),
    labels = label_percent(),
    name = "Proportion of drug uses remaining"
  ) + 
  scale_color_vibrant() + 
  labs(y = NULL)

gg_drug_left <- dft_flow_cohort %>%
  mutate(step_rev = fct_rev(step)) %>%
  ggplot(
    data = .,
    aes(x = n_drug, y = step_rev, group = cohort, color = cohort)
  ) +
  geom_path() +
  theme_bw() +
  scale_x_continuous(
    expand = expansion(add = c(0, 5), mult = c(0, 0)),
    limits = c(0, NA),
    n.breaks = 6,
    name = "Number of unique drugs remaining"
  ) + 
  scale_color_vibrant() + 
  labs(y = NULL)

gg_comb_left <- cowplot::plot_grid(
  gg_use_prop_left,
  gg_drug_left,
  ncol = 1
)


ggsave(
  gg_comb_left,
  height = 4, width = 8,
  filename = here('output', 'img',
                  '02_filtering_steps_cumulative_line.pdf')
)
```


Another way to look at this is the proportion of drugs remaining, as a portion of how many the raw data contain.  The following plot shows this, with the bottom pane showing the absolute number of unique drugs in each cohort.

```{r}
#| include: true
gg_comb_left
```