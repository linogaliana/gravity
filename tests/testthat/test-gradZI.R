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


testthat::test_that("Gradient for ZINB is correct",{
  testthat::expect_equal(
    grad_ZINB_R(c(params,2), X, Z, Y,
                weights = rep(1, nrow(X)),
                offsetx = rep(0, nrow(X)),
                offsetz = rep(0, nrow(X)),
                link = "probit"),
    grad_ZINB(c(params,2), X, Z, Y,
                weights = rep(1, nrow(X)),
                offsetx = rep(0, nrow(X)),
                offsetz = rep(0, nrow(X)),
                link = "probit"),
    check.names = FALSE
  )
})



testthat::test_that("Gradient for ZINB is correct",{
  testthat::expect_equal(
    grad_ZINB_R(c(params,2), X, Z, Y,
                weights = rep(1, nrow(X)),
                offsetx = rep(0, nrow(X)),
                offsetz = rep(0, nrow(X)),
                link = "logit"),
    grad_ZINB(c(params,2), X, Z, Y,
              weights = rep(1, nrow(X)),
              offsetx = rep(0, nrow(X)),
              offsetz = rep(0, nrow(X)),
              link = "logit"),
    check.names = FALSE
  )
})


# TRY ON REAL DATA -----------


fm_zinb2 <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin", x = TRUE)

testthat::test_that(
  "With real data, we find equal gradients",{
    testthat::expect_equal(
      grad_ZIP(params, X, Z, Y,
               weights = rep(1, nrow(X)),
               offsetx = rep(0, nrow(X)),
               offsetz = rep(0, nrow(X)),
               link = "logit"),
      grad_ZIP_R(params, X, Z, Y,
                 weights = rep(1, nrow(X)),
                 offsetx = rep(0, nrow(X)),
                 offsetz = rep(0, nrow(X)),
                 link = "logit")
    )
  }
)

testthat::test_that(
  "With real data, we find equal gradients",{
    testthat::expect_equal(
      as.numeric(grad_ZINB(c(params,1), X, Z, Y,
               weights = rep(1, nrow(X)),
               offsetx = rep(0, nrow(X)),
               offsetz = rep(0, nrow(X)),
               link = "logit")),
      as.numeric(grad_ZINB_R(c(params,1), X, Z, Y,
                 weights = rep(1, nrow(X)),
                 offsetx = rep(0, nrow(X)),
                 offsetz = rep(0, nrow(X)),
                 link = "logit"))
    )
  }
)


# WITH OPTIM



optim(fn = gravity:::loglik_ZINB, gr = gravity:::grad_ZINB,
      par = c(params,1), 
      method = pscl::zeroinfl.control()$method,
      hessian = pscl::zeroinfl.control()$hessian,
      control = pscl::zeroinfl.control, x = X,
      z = Z, y = Y, weights = rep(1, nrow(X)),
      offsetx = rep(0, nrow(X)),
      offsetz = rep(0, nrow(X)),
      link = "logit")

optim(fn = gravity:::loglik_ZIP, gr = gravity:::grad_ZIP,
      par = params, 
      method = pscl::zeroinfl.control()$method,
      hessian = pscl::zeroinfl.control()$hessian,
      control = pscl::zeroinfl.control, x = X,
      z = Z, y = Y, weights = rep(1, nrow(X)),
      offsetx = rep(0, nrow(X)),
      offsetz = rep(0, nrow(X)),
      link = "logit")


optim(fn = gravity:::loglik_ZINB, gr = gravity:::grad_ZINB,
      par = c(params,1), 
      method = pscl::zeroinfl.control()$method,
      hessian = pscl::zeroinfl.control()$hessian,
      control = pscl::zeroinfl.control, x = X,
      z = Z, y = Y, weights = rep(1, nrow(X)),
      offsetx = rep(0, nrow(X)),
      offsetz = rep(0, nrow(X)),
      link = "probit")

optim(fn = gravity:::loglik_ZIP, gr = gravity:::grad_ZIP,
      par = params, 
      method = pscl::zeroinfl.control()$method,
      hessian = pscl::zeroinfl.control()$hessian,
      control = pscl::zeroinfl.control, x = X,
      z = Z, y = Y, weights = rep(1, nrow(X)),
      offsetx = rep(0, nrow(X)),
      offsetz = rep(0, nrow(X)),
      link = "probit")


# speed

# microbenchmark::microbenchmark(
#   grad_ZINB(c(params,2), X, Z, Y,
#             weights = rep(1, nrow(X)),
#             offsetx = rep(0, nrow(X)),
#             offsetz = rep(0, nrow(X)),
#             link = "logit"),
#   grad_ZINB_R(c(params,2), X, Z, Y,
#               weights = rep(1, nrow(X)),
#               offsetx = rep(0, nrow(X)),
#               offsetz = rep(0, nrow(X)),
#               link = "logit"),
#   times=50L
# )
# 
# 
# microbenchmark::microbenchmark(
#   grad_ZINB(c(params,2), X, Z, Y,
#             weights = rep(1, nrow(X)),
#             offsetx = rep(0, nrow(X)),
#             offsetz = rep(0, nrow(X)),
#             link = "probit"),
#   grad_ZINB_R(c(params,2), X, Z, Y,
#               weights = rep(1, nrow(X)),
#               offsetx = rep(0, nrow(X)),
#               offsetz = rep(0, nrow(X)),
#               link = "probit"),
#   times=50L
# )
