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

a <- grad_ZIP(params, X, Z, Y,
             weights = weights,
             offsetx = offsetx,
             offsetz = offsetz,
             link = "logit")
b <-  grad_ZIP_R(params, X, Z, Y,
                 link = "logit")

max((a-b)[Y>0])
max((a-b)[Y==0])

testthat::test_that("Gradient for ZIP is correct",{
  testthat::expect_equal(
    grad_ZIP(params, X, Z, Y,
             weights = rep(1, nrow(X)),
             offsetx = rep(0, nrow(X)),
             offsetz = rep(0, nrow(X)),
             link = "logit") - 
    grad_ZIP_R(params, X, Z, Y,
               link = "logit")
  )
})

testthat::test_that("[Only positive terms] Gradient for ZIP is correct",{
  testthat::expect_equal(
    grad_ZIP(params, X[Y>0,], Z[Y>0,], Y[Y>0],
             weights = rep(1, nrow(X))[Y>0],
             offsetx = rep(0, nrow(X))[Y>0],
             offsetz = rep(0, nrow(X))[Y>0],
             link = "logit") - 
    grad_ZIP_R(params, X[Y>0,], Z[Y>0,], Y[Y>0],
               link = "logit")
  )
})