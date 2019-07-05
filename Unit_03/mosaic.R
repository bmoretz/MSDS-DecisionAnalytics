library(mosaic)

plotFun(a * sin(x ^ 2) ~ x, xlim = range(-5, 5), a = 2) # setting parameter value
plotFun(u ^ 2 ~ u, ulim = c(-4, 4)) # limits in terms of u
# Note roles of ylim and y.lim in this example
plotFun(y ^ 2 ~ y, ylim = c(-2, 20), y.lim = c(-4, 4))
# Combining plot elements to show the solution to an inequality
plotFun(x ^ 2 - 3 ~ x, xlim = c(-4, 4), grid = TRUE)
ladd(panel.abline(h = 0, v = 0, col = 'gray50'))
plotFun((x ^ 2 - 3) * (x ^ 2 > 3) ~ x, type = 'h', alpha = .1, lwd = 4, col = 'lightblue', add = TRUE)
plotFun(sin(x) ~ x,
   groups = cut(x, findZeros(sin(x) ~ x, within = 10)$x),
   col = c('blue', 'green'), lty = 2, lwd = 3, xlim = c(-10, 10))
plotFun(sin(x) ~ x,
   groups = cut(x, findZeros(sin(x) ~ x, within = 10)$x),
   col = c(1, 2), lty = 2, lwd = 3, xlim = c(-10, 10))
## plotFun( sin(2*pi*x/P)*exp(-k*t)~x+t, k=2, P=.3)
f <- rfun(~u & v)
plotFun(f(u = u, v = v) ~ u & v, u.lim = range(-3, 3), v.lim = range(-3, 3))
plotFun(u ^ 2 + v < 3 ~ u & v, add = TRUE, npts = 200)
if (require(mosaicData)) {
  # display a linear model using a formula interface
  model <- lm(wage ~ poly(exper, degree = 2), data = CPS85)
  fit <- makeFun(model)
  xyplot(wage ~ exper, data = CPS85)
  plotFun(fit(exper) ~ exper, add = TRUE, lwd = 3, col = "red")
  # Can also just give fit since it is a "function of one variable"
  plotFun(fit, add = TRUE, lwd = 2, col = 'white')
}
# Attempts to find sensible axis limits by default
plotFun(sin(k * x) ~ x, k = 0.01)
# Plotting a linear model with multiple predictors.
mod <- lm(length ~ width * sex, data = KidsFeet)
fitted.length <- makeFun(mod)
xyplot(length ~ width, groups = sex, data = KidsFeet, auto.key = TRUE)
plotFun(fitted.length(width, sex = "B") ~ width, add = TRUE, col = 1)
plotFun(fitted.length(width, sex = "G") ~ width, add = TRUE, col = 2)

# NOT RUN {
require(graphics)

f <- function(x, a)(x - a) ^ 2
xmin <- optimize(f, c(0, 1), tol = 0.0001, a = 1 / 3)
xmin

## See where the function is evaluated:
optimize(function(x) x ^ 2 * (print(x) - 1), lower = 0, upper = 10)

## "wrong" solution with unlucky interval and piecewise constant f():
f <- function(x) ifelse(x > -1, ifelse(x < 4, exp(-1 / abs(x - 1)), 10), 10)
fp <- function(x) { print(x); f(x) }

plot(f, -2, 5, ylim = 0:1, col = 2)
optimize(fp, c(-4, 20)) # doesn't see the minimum
optimize(fp, c(-7, 20)) # ok
# }

