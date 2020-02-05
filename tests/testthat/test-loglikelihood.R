testthat::context("Loglikelihood is well implemented in C++")

# Imagine 10 variables: 6 in selection, 4 in count
params <- rnorm(10)
X <- replicate(4, rnorm(200))
Z <- cbind(
  X,
  replicate(2, rnorm(200))
)
Y <- rnorm(200)

loglik_ZIP(params = params,
           x = X,
           z = Z,
           y = Y,
           link = "probit")
