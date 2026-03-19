getwd()
devtools::load_all()
install.packages("vdiffr")
library("vdiffr")

####### Proof of the bug #######

# EXAMPLE 1 : mtcars

# Default graph (points are visible, so no duplicate outliers)
ggbetweenstats(
  data = mtcars,
  x = cyl,
  y = mpg
)

# The disappearance of dots and violins is forced : this plot should show outliers for the '8' cylinder group, but it doesn't.
ggbetweenstats(
  data = mtcars,
  x = cyl,
  y = mpg,
  point.args = list(alpha = 0),
  violin.args = list(width = 0, linewidth = 0, colour = NA)
)


# EXAMPLE 2 : iris
ggbetweenstats(
  data = iris,
  x = Species,
  y = Sepal.Length
)

ggbetweenstats(
  data = iris,
  x = Species,
  y = Sepal.Length,
  point.args = list(alpha = 0),
  violin.args = list(width = 0, linewidth = 0, colour = NA)
)
