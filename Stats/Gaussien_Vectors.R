# ----------
# EXERCISE 1 
# ----------

# Below: Create a function to simulate a 2D Gaussian Vector X=(X1, X2)
# ------
# Inputs
# ------
#  mu : a vector of size 2 giving the mean of X
#  rho: a real number between [-1, 1] giving the correlation cor(X1,X2)
#  s  : a vector of size 2 containing the standard deviation of X. Default is (1,1).
#  n  : an integer giving the sample size. Default is 1000.
# ------
# Output
# ------
# A matrix of size nx2 containing a sample of size n from the Gaussian distribution of X

simu_VG <- function(mu, rho, s = c(1,1), n = 1000, plot = TRUE, ...){
  
  # construct the covariance matrix, such that :
  # cor(X1, X2) = rho, var(X1) = s[1]^2, var(X2) = s[2]^2
  
  Gamma <- matrix(c(s[1]^2, rho*s[1]*s[2], rho*s[1]*s[2], s[2]^2), 2, 2)
  
    # here simulate a sample of size n drawn from N(mu, Gamma) 
    
  X <- matrix(NA, n, 2)
  L <- t(chol(Gamma))
  for (i in 1:n){
    eps <- matrix(rnorm(2, 0, 1), 2, 1)
    X[i, ] <- mu + L%*%eps
  }
  
  # plot the results if argument 'plot' is equal to TRUE
  if (plot){
    par(mfrow = c(1,1))
    plot(X, asp=1, ...)  # asp = 1 --> same scale for the x and y axis
    abline(v = mu[1], h = mu[2])     
  }
  
  return(X)
}

# Run the function
X <- simu_VG(mu = c(1,2), rho = 0.8)



# ----------
# EXERCISE 2 
# ----------
# consider the points such that x1-h <= X1 <= x1+h
x1 <- 0
h <- 0.1

indices <- which((X[,1]>=x1-h) & (X[,1]<=x1+h) )  # the indices of X that fulfill the condition

X2 <- X[indices, 2]
points(X[indices, 1], X[indices, 2], col="blue")
abline(v = c(x1-h, x1+h))

# study the distribution of X2
# Is it a normal one ?
# What can you say of its mean ? Its variance ?

hist(X2, freq=FALSE)
qqnorm(X2); qqline(X2)

# ----------
# EXERCISE 3 
# ----------

x <- EuStockMarkets
plot(x)

period <- 251:500

DAX <- x[period, "DAX"]
CAC <- x[period, "CAC"]
rDAX <- diff(log(DAX))
rCAC <- diff(log(CAC))

par(mfrow=c(2,1))
title <- paste("daily return (%) over the period [", 
               period[1], "-", period[length(period)], "]", sep="")
plot(rDAX, type = "l", main = title)
plot(rCAC, type = "l", main = title)

# Is the vector (rDAX, rCAC) a Gaussian vector ?

plot(rDAX, rCAC, asp=1)

hist(rDAX, freq=FALSE)
qqnorm(rDAX); qqline(rDAX)
qqnorm(rCAC); qqline(rCAC)
CL1 <- rDAX + rCAC; qqnorm(CL1); qqline(CL1)
CL2 <- rDAX - rCAC; qqnorm(CL2); qqline(CL2)
