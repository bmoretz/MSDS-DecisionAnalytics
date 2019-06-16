require(lpSolve)

# Let's consider a transportation problem of two origins a and b, and three
# destinations 1, 2 and 3. The cost c(i)(j) of transporting one unit from the 
# origin and the required demand in the destinations can be found in the
# following table:

#         1   2   3   capacity
#     a   8   6   3   70
#     b   2   4   9   40
# demand  40  35  25

obj.fun <- c(8, 6, 3, 2, 4, 9)

m <- 2
n <- 3

constr <- matrix(0, m + n, n * m)

for (i in 1:m) {
  for (j in 1:n) {
    constr[i, n * (i - 1) + j] <- 1
    constr[m + j, n * (i - 1) + j] <- 1
  }
}

constr.dir <- c(rep("<=", m), rep(">=", n))

rhs <- c(70, 40, 40, 35, 25)

optimum <- lp(direction = "min",
                 objective.in = obj.fun,
                 const.mat = constr,
                 const.dir = constr.dir,
                 const.rhs = rhs,
                 compute.sens = T)

# Print status: 0 = success, 2 = no feasible solution
if (optimum$status == 2) {
  print("Unsolvable")
} else if (optimum$status == 0) {

  print("Solution Found")

  # Display the optimum values
  best_sol <- optimum$solution
  print(best_sol)

  # Check the value of objective function at optimal point
  print(paste("Minimal Cost: ", optimum$objval, sep = ""))

  print(optimum$duals)

  # Sensibility Analysis Results
  print("Sensibility Analysis Results")
  print(optimum$duals.from)
  print(optimum$duals.to)
  print(optimum$sens.coef.from)
  print(optimum$sens.coef.to)

  rm(best_sol)
}

rm(optimum, obj.fun, constr, constr.dir, rhs)