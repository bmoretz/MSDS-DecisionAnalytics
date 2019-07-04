library(mosaic)

# f(x, y) = x^2 + y, x^2 - y^2 = 1

f = makeFun(2 * (pi * r ^ 2 + 2 * pi * r * h) + 8 * (pi * r^2) ~ r & h)
g = makeFun(pi*r^2*h ~ r&h)
h = makeFun(h ~ r & h)
r = makeFun(4 * r ~ r & h)

plotFun(f(x, y) ~ x & y, x.lim = range(0, 125), y.lim = range(0, 5), filled = FALSE)
plotFun(g(x, y) ~ x & y, levels = 0, x.lim = range(0, 150), y.lim = range(0, 5), filled = FALSE, add = TRUE, col = "blue")
plotFun(h(x, y) ~ x & y, levels = 0, x.lim = range(0, 150), y.lim = range(0, 5), filled = FALSE, add = TRUE, lwd = 3, col = "red")
plotFun(r(x, y) ~ x & y, levels = 0, x.lim = range(0, 150), y.lim = range(0, 5), filled = FALSE, add = TRUE, lwd = 3, col = "red")

obj.fun = function(r, h) {
  2*(pi*r^2 + 2*pi*r*h) + 8*(pi*r^2)
}

r.vals <- seq( from = 0, to = 5, by = 0.01)
h.vals <- seq(from = 0, to = 5, by = 0.01)

y_vals <- obj.fun(r.vals, h.vals)

plot(y_vals)

