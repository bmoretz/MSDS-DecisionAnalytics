require(lpSolve)

# Creating a table takes 2 big blocks and 2 small blocks, creating a chair takes 1 big block and 2 small blocks,
# while a coffee table takes 2 big blocks and 1 small block.
# A table produces $16 in revenue, a coffee table $15, while a chair produces $10.
# What's the best combination of tables, coffee tables and chairs to produce the most revenue?

## Set the coefficients of the decision variables -> C
C <- c(16, 10, 15)

# Create constraint martix B
A <- matrix(c(2, 1, 2,
              2, 2, 1), nrow = 2, byrow = TRUE)

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
print(ifelse(optimum$status == 0, "Solution Found", "Unsolvable"))

# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution
names(best_sol) <- c("Tables", "Chairs", "Coffee Tables")
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total Revenue: ", optimum$objval, sep = ""))

#################
#   Output      #
#################

# [1] 0
# Tables Chairs Coffee Tables
# 0      8 2

# "Total Revenue: 110"

rm(optimum, constranints_direction, best_sol)