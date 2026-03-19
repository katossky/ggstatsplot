# install.packages("ggstatsplot")
install.packages("pkgload")
install.packages("vdiffr")

library(ggstatsplot)
library(dplyr)
library(rlang)

devtools::load_all()
pkgload::load_all()

##### Reproduction de l'erreur d'origine

(df <- dplyr::tibble(
  grp = c(rep("c", 5), rep("a", 5), rep("b", 5)),
  val1 = runif(15),
  val2 = runif(15)
))

grouped_ggscatterstats(df, val1, val2, grouping.var = grp)
# Graphiques affichés dans l'ordre alphabétique.

##### Solution proposée par un répondant

test <- data.frame(
  id = c("b", "c", "a"),
  val = 1:3
)

# reordered
split(test, ~ id)

# not reordered
test$id <- factor(test$id, levels=unique(test$id))
split(test, ~ id)


##### Implémentation avec une fonction

.grouped_list <- function(data, grouping.var = NULL) {
  data <- as_tibble(data)

  if (quo_is_null(enquo(grouping.var))) {
    return(data)
  }

  data %>%
    mutate(
      across(
        {{ grouping.var }},
        ~ factor(.x, levels = unique(.x))
      )
    )

  data %>% split(f = new_formula(NULL, enquo(grouping.var)), drop = FALSE)
}

#> You can cite this package as:
#>      Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
#>      Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

##### Test final

(df <- dplyr::tibble(
  grp = c(rep("c", 5), rep("a", 5), rep("b", 5)),
  val1 = runif(15),
  val2 = runif(15)
))


grouped_ggscatterstats(df, val1, val2, grouping.var = grp)

grouped_ggscatterstats(df$c, val1, val2, grouping.var = grp)

df <- .grouped_list(df, grp)
df

