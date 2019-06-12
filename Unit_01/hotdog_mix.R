require(lpSolve)

# Oliver Meyer hopes to follow his uncle Oscar's success in the hot dog business by creating a low-calorie, low-fat, low-cholesterol hot dog 
# made of at least 25% beef and 25% pork plus either chicken or turkey, or both. Oliver will market the 2 ounce dogs as all-meat with no fillers. 
# Each dog will must have no more than 6 grams of fat, 27 grams of cholesterol, and 100 calories. The cost of ingredients and their relevant information is shown. 
# Create a min-cost Hot Dog that meets all requirements at 2 ounces.

## Set the coefficients of the decision variables -> C

C <- c(0.76, 0.82, 0.64, 0.58) # Cost/lb

# Create constraint martix B
A <- matrix(c(640, 1055, 780, 528, # Calories/lb
              32.5, 54, 25.6, 6.4, # Fat (g/lb)
              210, 205, 220, 172,
              1, 0, 0, 0,
              0, 1, 0, 0,
              1, 1, 1, 1), # Cholestrerol (g/lb)
              nrow = 6, byrow = TRUE)

# Right hand side for the constraints
B <- c(100, 6, 27, 0.03125, 0.03125, 0.125)

# Direction of the constraints
constranints_direction <- c("<=", "<=", "<=", ">=", ">=",">=")

# Find the optimal solution
optimum <- lp(direction = "min",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)

# Print status: 0 = success, 2 = no feasible solution
print(ifelse(optimum$status == 0, "Solution Found", "Unsolvable"))

# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution
#names(best_sol) <- c("Tables", "Chairs", "Coffee Tables")
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total Revenue: ", optimum$objval, sep = ""))

#################
#   Output      #
#################

# [1] 0
# Tables Chairs Coffee Tables
# 0      8      2

# "Total Revenue: 110"

rm(optimum, constranints_direction, best_sol)