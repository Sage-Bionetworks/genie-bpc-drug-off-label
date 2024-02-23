check_test_cols_true_false <- function(
    dat
) {
  dat %>%
    select(matches("^test_")) %>%
    summarize(
      across(
        .cols = everything(),
        .fns = \(x) all(x %in% c(T,F))
      )
    ) %>%
    pivot_longer(
      cols = everything()
    ) %>%
    pull(value) %>%
    all(.)
}
i