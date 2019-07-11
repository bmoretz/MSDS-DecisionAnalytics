library(mosaic)
library(data.table)
require(graphics)
library(nloptr)

# f(x, y) = x^2 + y, x^2 - y^2 = 1

f = makeFun(2 * (pi * r ^ 2 + 2 * pi * r * h) + 8 * (pi * r^2) ~ r & h)
g = makeFun(pi*r^2*h ~ r&h)
h = makeFun(h ~ r & h)
r = makeFun(4 * r ~ r & h)

plotFun(f(x, y) ~ x & y, x.lim = range(0, 125), y.lim = range(0, 5), filled = FALSE)
plotFun(g(x, y) ~ x & y, levels = 0, x.lim = range(0, 150), y.lim = range(0, 5), filled = FALSE, add = TRUE, col = "blue")
plotFun(h(x, y) ~ x & y, levels = 0, x.lim = range(0, 150), y.lim = range(0, 5), filled = FALSE, add = TRUE, lwd = 3, col = "red")
plotFun(r(x, y) ~ x & y, levels = 0, x.lim = range(0, 150), y.lim = range(0, 5), filled = FALSE, add = TRUE, lwd = 3, col = "red")

obj.fun <- function(x) {
  r <- x[1]
  h <- x[2]
  2 * pi * r ^ 2 + 4 * pi * r * h + 8 * (pi * r ^ 2)
}

eval_g_eq <- function(x) {
  constr <- list(pi * x[1] ** 2 * x[2] - 20,
               4 * x[1] - x[2]
  )
  return(list("constraints" = constr))
}

opt <- auglag(c(1, 5), fn = obj.fun, eval_g_eq = eval_g_eq, localsolver = "LBFGS",
              lower = c(1, 1), upper = c(5, 5))

opt