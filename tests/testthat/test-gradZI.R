testthat::context("Gradients in C++ values are correct")

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



# ZERO-INFLATED POISSON -----------

testthat::test_that("Gradient for ZIP is correct",{
  testthat::expect_equal(
    grad_ZIP(params, X, Z, Y,
             weights = rep(1, nrow(X)),
             offsetx = rep(0, nrow(X)),
             offsetz = rep(0, nrow(X)),
             link = "logit"),
    grad_ZIP_R(params, X, Z, Y,
               link = "logit")
  )
})

testthat::test_that("Gradient for ZIP is correct",{
  testthat::expect_equal(
    grad_ZIP(params, X, Z, Y,
             weights = rep(1, nrow(X)),
             offsetx = rep(0, nrow(X)),
             offsetz = rep(0, nrow(X)),
             link = "probit"),
    grad_ZIP_R(params, X, Z, Y,
               weights = rep(1, nrow(X)),
               offsetx = rep(0, nrow(X)),
               offsetz = rep(0, nrow(X)),
               link = "probit")
  )
})
