#Minimum design weight of circular thin walled column
#Arora, pp.40-42, and exercise 3.23, p.85

library(nloptr)

#initialize variables
rho <- 7850 #density material (kg/m^3)
sigmaAllow <- 250e+6 #allowable stress (Pa)
E <- 210e+9 #Young's modulus (Pa)
l <- 5 #length of column (m)
P <- 50e+3 #axial load (N)

#objective function: mass = rho*(l*A) = 2*rho*l*pi*R*t
objFun <- function(x) {
  R <- x[1] #mean radius
  t <- x[2] #wall thickness
  2 * rho * l * pi * R * t
}

#constraints
constrFun <- function(x) {
  #initialize variables
  R <- x[1] #mean radius
  t <- x[2] #wall thickness
  A <- 2 * pi * R * t #material cross-sectional area
  I <- pi * R ^ 3 * t #moment of inertia (assuming R>>t)

  #define constraints
  z1 <- sigmaAllow - P / A #allowable stress
  z2 <- (pi ^ 2 * E * I) / (4 * l ^ 2) - P #buckling load
  z3 <- 50 - R / t #local buckling

  return(c(z1, z2, z3))
}

#Augmented Lagrange Multiplier (ALM) method
#note: for this ALM method to converge to the optimal design values
#the start values (starting design values) have to be in the infeasible region
#thus, first check infeasibility of the start values
constrFun(c(2e-2, 2e-2))
#apply ALM method
opt <- auglag(c(2e-2, 2e-2), fn = objFun, hin = constrFun, localsolver = "LBFGS",
              lower = c(.01, 5e-3), upper = c(1, 200e-3)) #.01<=R<=1 m; 5<=t<=200 mm
#check convergence: successful (integer>0) or failure (integer<0)
opt$convergence
#optimal solution
#(this solution is identical to the solution computed by Arora: R=53.6mm; t=5.0mm)
opt$par
#function value (is again identical to Arora's computed value of 66kg)
opt$value



#graphical representation of the optimization problem and its solution

#contour plot
x1values <- seq(0, .06, length.out = 75)
x2values <- seq(0, .03, length.out = 75)
lbgrid <- expand.grid(x1 = x1values, x2 = x2values)
lbgrid$cl <- apply(lbgrid, 1, objFun)
cl.matrix <- matrix(lbgrid$cl, nrow = length(x1values))
#plot contours
contour(x1values, x2values, cl.matrix, xlab = "R", ylab = "t",
        levels = c(20, 50, 66, 80, 110))

#include constraints:
#lower bound of R
abline(v = .01, lty = 2, col = "blue", lwd = 2)
#lower bound of t
abline(h = 5e-3, lty = 2, col = "blue", lwd = 2)
#allowable stress
constr1 <- function(x) { P / (sigmaAllow * 2 * pi * x) }
lines(x1values, constr1(x1values), lty = 2, col = "blue")
#buckling load
constr2 <- function(x) {(P * 4 * l ^ 2) / (pi ^ 3 * E * x ^ 3) }
lines(x1values, constr2(x1values), lty = 2, col = "blue")
#local buckling
constr3 <- function(x) { 50 / x }
lines(x1values, constr3(x1values), lty = 2, col = "blue")

#mark feasible region (FR)
text(.05, .020, "FR")

#include start value
points(2e-2, 2e-2, col = "blue")
#include optimal (final) solution
points(opt$par[1], opt$par[2], col = "red", lwd = 2)