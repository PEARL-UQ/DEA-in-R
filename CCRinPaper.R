
# Illustration of Output-Oriented DEA in R ---

# Generate sample data
y1 = c(1, 2, 1, 0.5, 1.5, 2, 0) 
y2 = c(2, 1, 1, 1.5, 0.5, 0, 2) 
x1 = c(1, 1, 1, 1, 1, 1, 1)

Y = as.data.frame(cbind(y1, y2))
X = as.data.frame(x1)

# Define parameters
rts='CRS'
# rts='VRS'

M = length(Y)
N = length(X)
n = nrow(X)

# Choose between VRS and CRS
if (rts=='VRS'){
  Aeq = cbind(0, t(rep(1,n))) # Matrix for linear equality constraints;
  beq = 1                     # Vector for linear equality constraint;
}
if (rts=='CRS'){
  Aeq <- vector(mode = "numeric", length = 0)
  beq <- vector(mode = "numeric", length = 0)
}

library(lpSolve)
solution <- vector(mode = "numeric", length = 0)

# Estimate theta_i for each individual
for (i in 1:n){
xi = X[i,]
yi = Y[i,]

objx <- vector(mode = "numeric", length = 0)

f = t(-cbind(1, t(rep(0,n)))) # Objective function
Aineq = rbind(cbind(rep(0,N), t(X)),
              cbind(t(yi), -t(Y))) # Matrix for linear inequality constraints
bineq = t(cbind(xi, t(rep(0,M))))  # Vector for linear inequality constraints

direction = c(rep("<=",N+M),"=") # Set the directions including the equivalent constraint
A = rbind(Aineq,Aeq)
b = rbind(bineq,beq)
  
lp = lp (direction = "min", objective.in = f, const.mat = A,
    const.dir = direction, const.rhs = b) # Note that the bounds of every variable has been assumed in lp

solution = rbind(solution, lp[["solution"]][1]) # Collect estimates
}

solution