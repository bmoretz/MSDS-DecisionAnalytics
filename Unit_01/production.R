require(lpSolve)

# A small business sells two products, named Product 1 and Product 2.
# Each tonne of Product 1 consumes 30 working hours, and each tonne of 
# Product 2 consumes 20 working hours. The business has a maximum of 2,700
# working hours for the period considered. As for machine hours,
# each tonne of Products 1 and 2 consumes 5 and 10 machine hours,
# respectively. There are 850 machine hours avaliable.

# Each tonne of Product 1 yields 20M pounds of profit, while product 2 yields 
# 60M pounds for each tonne sold. For technical reasons, the firm must product
# a minimum of 95 ttonnes in total between both products. We need to know
# how many tonnes of Product 1 and 2 must be produced to
# maximize total profit.

## Set the coefficients of the decision variables -> obj.fun
obj.fun <- c(20, 60)

# Create constraint martix B
constr <- matrix(c(30, 20,
              5, 10,
              1, 1), ncol = 2, byrow = TRUE)

# Direction of the constraints
constranints_direction <- c("<=", "<=", ">=")

# Right hand side for the constraints
rhs <- c(2700, 850, 95)

# Solving Model

# Find the optimal solution
optimum <- lp(direction = "max",
               objective.in = obj.fun,
               const.mat = constr,
               const.dir = constranints_direction,
               const.rhs = rhs,
               all.int = T)

# Print status: 0 = success, 2 = no feasible solution
print(ifelse(optimum$status == 0, "Solution Found", "Unsolvable"))

# Display the optimum values
best_sol <- optimum$solution
names(best_sol) <- c("Product 1 ", "Product 2")
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total Revenue: ", optimum$objval, sep = ""))

print(optimum$duals)

# Sensibility Analysis Results
print("Sensibility Analysis Results")
print(optimum$duals.from)
print(optimum$duals.to)
print(optimum$sens.coef.from)
print(optimum$sens.coef.to)

rm(optimum, obj.fun, constr, constranints_direction, rhs, best_sol)