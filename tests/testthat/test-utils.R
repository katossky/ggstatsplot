# .grouped_list -----------------------------------------------------

test_that(
  ".grouped_list works with non-syntactic group names",
  {
    set.seed(123)
    expect_snapshot({
      sleep %>%
        rename("my non-syntactic name" = group) %>%
        .grouped_list(grouping.var = `my non-syntactic name`) %>%
        str()
    })
  }
)

test_that(".grouped_list preserves appearance order", {
  df <- tibble::tibble(
    grp = c("c", "a", "b", "a"),
    x = 1:4
  )

  res <- .grouped_list(df, grouping.var = grp)

  expect_equal(res$title, c("c", "a", "b"))
})


test_that(".grouped_list output works with grouped_ggscatterstats", {
  skip_if_not_installed("ggstatsplot")

  df <- tibble::tibble(
    grp = c("c", "a", "b", "a"),
    x = rnorm(4),
    y = rnorm(4)
  )

  expect_error(
    grouped_ggscatterstats(df, x, y, grouping.var = grp),
    NA
  )
})

# .is_palette_sufficient ------------------------------------

test_that(
  ".is_palette_sufficient is working",
  {
    expect_no_condition(.is_palette_sufficient("RColorBrewer", "Dark2", 2L))

    withr::local_options(list(warn = 0L))
    expect_snapshot(.is_palette_sufficient("RColorBrewer", "Dark2", 20L))
  }
)

# .eval_f ------------------------------------

test_that(
  ".eval_f works as expected",
  {
    f <- function() stop("Not working", call. = FALSE)
    expect_null(.eval_f(f))
  }
)
