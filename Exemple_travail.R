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


# not reordered
test$id <- factor(test$id, levels=unique(test$id))
split(test, ~ id)


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


(df <- dplyr::tibble(
  grp = c(rep("c", 5), rep("a", 5), rep("b", 5)),
  val1 = runif(15),
  val2 = runif(15)
))


.grouped_list(df, grp)

grouped_ggscatterstats(df, val1, val2, grouping.var = grp)


