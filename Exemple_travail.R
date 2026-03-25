# Pour nos modifications
devtools::load_all()

# Problème :
df <- dplyr::tibble(
  grp = c(rep("c", 5), rep("a", 5), rep("b", 5)),
  val1 = runif(15),
  val2 = runif(15)
)

df
grouped_ggscatterstats(df, val1, val2, grouping.var = grp)



# exemple pb

test <- data.frame(
  id = c("b", "c", "a"),
  val = 1:3
)

# reordered
split(test, ~ id)

# la fonction split réordonne automatiquement les groupes quand ce n'est pas en factor


# not reordered
test$id <- factor(test$id, levels=unique(test$id))
split(test, ~ id)



# test de la solution :
df <- dplyr::tibble(
  grp = c(rep("c", 5), rep("a", 5), rep("b", 5)),
  val1 = runif(15),
  val2 = runif(15)
)
df


.grouped_list <- function(data, grouping.var = NULL) {

  data <- tibble::as_tibble(data)

  if (rlang::quo_is_null(rlang::enquo(grouping.var))) {
    return(data)
  }

  # Conserver l'ordre d'apparition des groupes avec factor
  data <- dplyr::mutate(
    data,
    dplyr::across(
      {{ grouping.var }},
      ~ factor(.x, levels = unique(.x))
    )
  )

  data %>% split(
    f = rlang::new_formula(NULL, rlang::enquo(grouping.var)),
    drop = TRUE) %>%
    # structure attendue
    list(data = ., title = names(.))

}


assignInNamespace(".grouped_list", .grouped_list, ns = "ggstatsplot")


.grouped_list(df, grp)
# la fonction renvoie l'ordre correct des var


# Graphique s'affiche correctement
grouped_ggscatterstats(df, val1, val2, grouping.var = grp)

