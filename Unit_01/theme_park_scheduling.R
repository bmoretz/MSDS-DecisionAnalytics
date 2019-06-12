require(lpSolve)

# Maintenance at a major theme park in Central Florida is an ongoing process that occurs 24 hours per day.  
# Because it is a long drive from most residential areas to the park, employees do not like to work 
# shifts fewer than eight hours.  These 8-hour shifts start every four hours throughout the day, but the number of 
# maintenance workers needed at different times throughout the day varies.  The following table summarizes the 
# number of employees needed in each four-hour time period.

#		Time Period		Minimum # of Employees
#		12AM – 4AM			90
#		4AM – 8AM 			215
#		8AM – 12PM			250
#		12PM – 4PM			165
#		4PM – 8PM			  300
#		8PM – 12AM			125

# The maintenance supervisor wants to determine the minimum number of employees to schedule for each shift in order to meet staffing requirements.


## Set the coefficients of the decision variables -> C
C <- rep(1, 6)

# Create constraint martix B
A <- matrix(c(1, 0, 0, 0, 0, 1,
              1, 1, 0, 0, 0, 0,
              0, 1, 1, 0, 0, 0,
              0, 0, 1, 1, 0, 0,
              0, 0, 0, 1, 1, 0,
              0, 0, 0, 0, 1, 1), nrow = 6, byrow = TRUE)

# Right hand side for the constraints
B <- c(90, 215, 250, 165, 300, 125)

# Direction of the constraints
constranints_direction <- rep(">=", 6)

# Find the optimal solution
optimum <- lp(direction = "min",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)

# Print status: 0 = success, 2 = no feasible solution
print( ifelse(optimum$status==0, "Solution Found", "Unsolvable"))

# Display the optimum values for x_4p, x_3p and x_w
best_sol <- optimum$solution

names(best_sol) <- c("S0000", "S0400", "S0800", "S1200", "S1600", "S2000")
print(best_sol)

# Check the value of objective function at optimal point
print(paste("Total Hours Worked: ", optimum$objval, sep = ""))

#################
#   Output      #
#################

rm(optimum, constranints_direction, best_sol)