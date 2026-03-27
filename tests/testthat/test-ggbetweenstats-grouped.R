# tests/testthat/test-grouped-aesthetics.R

test_that(
  "grouped_ggbetweenstats preserves aesthetics when grouping variable is dropped internally",
  {
    set.seed(123)

    n_per_cell <- 40

    fake_pk <- tidyr::expand_grid(
      dose_l = factor(
        c("Dose 5 mg", "Dose 10 mg"),
        levels = c("Dose 5 mg", "Dose 10 mg")
      ),
      day_l = factor(
        c("Day 1", "Day 7"),
        levels = c("Day 1", "Day 7")
      ),
      id = 1:n_per_cell
    ) %>%
      dplyr::mutate(
        subject = dplyr::row_number(),
        mu = dplyr::case_when(
          dose_l == "Dose 5 mg"  & day_l == "Day 1" ~ 90,
          dose_l == "Dose 10 mg" & day_l == "Day 1" ~ 125,
          dose_l == "Dose 5 mg"  & day_l == "Day 7" ~ 105,
          dose_l == "Dose 10 mg" & day_l == "Day 7" ~ 135
        ),
        Cmax = stats::rgamma(dplyr::n(), shape = 8, scale = mu / 8)
      ) %>%
      dplyr::select(subject, dose_l, day_l, Cmax)

    expect_no_error(
      p <- grouped_ggbetweenstats(
        data = fake_pk,
        x = day_l,
        y = Cmax,
        grouping.var = dose_l,
        type = "np",
        pairwise.display = "none",
        results.subtitle = TRUE,
        point.args = list(alpha = 0, size = 0),
        ggplot.component = list(
          ggplot2::aes(fill = dose_l, color = dose_l),
          ggplot2::scale_fill_manual(values = c("Dose 5 mg" = "orange", "Dose 10 mg" = "black")),
          ggplot2::scale_color_manual(values = c("Dose 5 mg" = "orange", "Dose 10 mg" = "black"))
        )
      )
    )

    expect_s3_class(p, "ggplot")

    set.seed(123)
    expect_doppelganger(
      title = "grouped aesthetics preserved after fix",
      fig = p
    )
  }
)
