library(ggplot2)
library(ggstatsplot)
# Données simulées

library(dplyr)

set.seed(123)

n_per_cell <- 40

# jeu de données plus simple et équilibré
fake_pk <- tidyr::expand_grid(
  dose_l = factor(c("Dose 5 mg", "Dose 10 mg"),
                  levels = c("Dose 5 mg", "Dose 10 mg")),
  day_l  = factor(c("Day 1", "Day 7"),
                  levels = c("Day 1", "Day 7")),
  id     = 1:n_per_cell
) %>%
  mutate(
    subject = row_number(),
    mu = case_when(
      dose_l == "Dose 5 mg"  & day_l == "Day 1" ~ 90,
      dose_l == "Dose 10 mg" & day_l == "Day 1" ~ 125,
      dose_l == "Dose 5 mg"  & day_l == "Day 7" ~ 105,
      dose_l == "Dose 10 mg" & day_l == "Day 7" ~ 135
    ),
    Cmax = rgamma(n(), shape = 8, scale = mu / 8)
  ) %>%
  select(subject, dose_l, day_l, Cmax)

fake_pk %>% count(dose_l, day_l)
# cas qui fonctionne
p_ok <- grouped_ggbetweenstats(
  data = fake_pk,
  x = dose_l,
  y = Cmax,
  grouping.var = day_l,
  type = "np",
  pairwise.display = "none",
  results.subtitle = TRUE,
  plot.type = "box",
  point.args = list(alpha = 0, size = 0),
  ggplot.component = list(
    aes(fill = dose_l, color = dose_l),
    scale_fill_manual(values = c("Dose 5 mg" = "orange", "Dose 10 mg" = "black")),
    scale_color_manual(values = c("Dose 5 mg" = "orange", "Dose 10 mg" = "black"))
  )
)

p_ok

# Cas problématique avec erreur
p_problem_error <- grouped_ggbetweenstats(
  data = fake_pk,
  x = day_l,
  y = Cmax,
  grouping.var = dose_l,
  type = "np",
  pairwise.display = "none",
  results.subtitle = TRUE,
  plot.type = "box",
  point.args = list(alpha = 0, size = 0),
  ggplot.component = list(
    #aes(fill = dose_l, color = dose_l),
    #scale_fill_manual(values = c("Day 1" = "orange", "Day 7" = "black")),
    #scale_color_manual(values = c("Day 1" = "orange", "Day 7" = "black"))
  )
)

p_problem_error
