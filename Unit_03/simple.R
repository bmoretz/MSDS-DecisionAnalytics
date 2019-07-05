library(mosaic)

# f(x, y) = x^2 + y, x^2 - y^2 = 1

f = makeFun(x ^ 2 + y ~ x & y)
g = makeFun(x ^ 2 - y ^ 2 ~ x & y)

plotFun(f(x, y) ~ x & y, x.lim = range(-10, 10), y.lim = range(-10, 10), filled = FALSE)
plotFun(g(x, y) ~ x & y, levels = 1, x.lim = range(-10, 10), y.lim = range(-10, 10), filled = FALSE, add = TRUE, col = 'red')
