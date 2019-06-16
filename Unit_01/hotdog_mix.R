require(lpSolve)

# Oliver Meyer hopes to follow his uncle Oscar's success in the hot dog business by creating a low-calorie, low-fat, low-cholesterol hot dog 
# made of at least 25% beef and 25% pork plus either chicken or turkey, or both. Oliver will market the 2 ounce dogs as all-meat with no fillers. 
# Each dog will must have no more than 6 grams of fat, 27 grams of cholesterol, and 100 calories. The cost of ingredients and their relevant information is shown. 
# Create a min-cost Hot Dog that meets all requirements at 2 ounces.

## Set the coefficients of the decision variables -> obj.fun
obj.fun <- c(0.76, 0.82, 0.64, 0.58) # Cost/lb

# Create constraint martix B
constr <- matrix(c(640, 1055, 780, 528, # Calories/lb
              32.5, 54, 25.6, 6.4, # Fat (g/lb)
              210, 205, 220, 172, # Cholestrerol (g/lb)
              1.0, 0.0, 0.0, 0, # 25% of total
              0, 1, 0, 0, # 25% of total
              1, 1, 1, 1), # total weight
              ncol = 4, byrow = TRUE)


# Direction of the constraints
constranints_direction <- c("<=", "<=", "<=", ">=", ">=", ">=")

min_weight_oz = 2
min_weight = min_weight_oz / 16

# Right hand side for the constraints
rhs <- c(100, 6, 27, min_weight * .25, min_weight * .25, min_weight)

# Solving Model

# Find the optimal solution
optimum <- lp(direction = "min",
               objective.in = obj.fun,
               const.mat = constr,
               const.dir = constranints_direction,
               const.rhs = rhs,
               all.int = F,
               compute.sens = T)

# Print status: 0 = success, 2 = no feasible solution
if (optimum$status == 2) {
  print("Unsolvable")
} else if (optimum$status == 0) {

  print("Solution Found")

  # Display the optimum values
  best_sol <- optimum$solution
  names(best_sol) <- c("Beef", "Pork", "Chicken", "Turkey")
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

rm(optimum, obj.fun, constr, constranints_direction, rhs)