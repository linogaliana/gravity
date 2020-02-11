# Imagine 10 variables: 6 in selection, 4 in count
params <- rnorm(10)
X <- replicate(4, rnorm(200))
Z <- cbind(
  X,
  replicate(2, rnorm(200))
)
Y <- rpois(200, lambda = 3)


weights = rep(1, nrow(X))
offsetx = rep(0, nrow(X))
offsetz = rep(0, nrow(X))

fastZI(x, y, start = rep(0, ncol(x)), eps_f = 1e-08, eps_g = 1e-05, 
                   maxit = 300)
