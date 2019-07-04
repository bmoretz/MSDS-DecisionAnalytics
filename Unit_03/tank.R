library(mosaic)

# f(x, y) = x^2 + y, x^2 - y^2 = 1

f = makeFun(2 * (pi * r ^ 2 + 2 * pi * r * h) + 8 * (pi * r) ~ r & h)
g = makeFun(pi*r^2*h ~ r&h)
h = makeFun(h - 4 * r ~ r & h)

plotFun(f(x, y) ~ x & y, x.lim = range(0, 125), y.lim = range(0, 5), filled = FALSE)
#plotFun(g(x, y) ~ x & y, levels = 0, x.lim = range(0, 150), y.lim = range(0, 5), filled = FALSE, add = TRUE, col = "blue")
plotFun(h(x, y) ~ x & y, levels = 0, x.lim = range(0, 150), y.lim = range(0, 5), filled = FALSE, add = TRUE, lwd = 3, col = "red")
