require(lpSolve)

# Creating a table takes 2 big blocks and 2 small blocks, while creating a chair takes 1 big block and 2 small blocks.
# A table produces $16 in revenue while a chair produces $10.
# What's the best combination of tables and chairs to produce the most revenue?

## Set the coefficients of the decision variables -> C
C <- c(16, 10)

# Create constraint martix B
A <- matrix(c(2, 1,
              2, 2), nrow = 2, byrow = TRUE)

# Right hand side for the constraints
B <- c(12, 18)

# Direction of the constraints
constranints_direction <- c("<=", "<=")

# Find the optimal solution
optimum <- lp(direction = "max",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)

# Print status: 0 = success, 2 = no feasible solution
print(optimum$status)

# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution
names(best_sol) <- c("Tables", "Chairs")
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total Revenue: ", optimum$objval, sep = ""))

#################
#   Output      #
#################

# [1] 0
# Tables Chairs 
# 3      6 

# "Total Revenue: 108"

rm(optimum, constranints_direction, best_sol)