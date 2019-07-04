## Attempt on a non-stiff equation
# y' = y^2 - y^3, y(0) = d, 0 <= t <= 2/d, d = 0.01
f <- function(t, y) y ^ 2 - y ^ 3
d <- 1 / 250
abm1 <- abm3pc(f, 0, 2 / d, d, n = 1 / d)
abm2 <- abm3pc(f, 0, 2 / d, d, n = 2 / d)
## Not run:
plot(abm1$x, abm1$y, type = "l", col = "blue")
lines(abm2$x, abm2$y, type = "l", col = "red")
grid()
## End(Not run)

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

f <- function(x) return(abs(x - 2) + 2 * abs(x - 1))
xmin <- optimize(f, interval = c(0, 3), tol = 0.0001)
xmin
## $minimum
## [1] 1.000009
##
## $objective
## [1] 1.000009
plot(f, 0, 3)


fn <- function(para) {
  # Vector of the parameters
  matrix.A <- matrix(para, ncol = 2)
  x <- matrix.A[, 1]
  y <- matrix.A[, 2]
  f.x <- (x ^ 2 + y - 11) ^ 2 + (x + y ^ 2 - 7) ^ 2
  return(f.x)
}
par <- c(1, 1)

xy <- as.matrix(expand.grid(seq(-5, 5, length = 101),
seq(-5, 5, length = 101)))
colnames(xy) <- c("x", "y")
df <- data.frame(fnxy = fn(xy), xy)
library(lattice)
wireframe(fnxy ~ x * y, data = df, shade = TRUE, drape = FALSE,
scales = list(arrows = FALSE),
screen = list(z = -240, x = -70, y = 0))

