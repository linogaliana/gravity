testthat::context("Loglikelihood is well implemented in C++")

# Imagine 10 variables: 6 in selection, 4 in count
params <- rnorm(10)
X <- replicate(4, rnorm(200))
Z <- cbind(
  X,
  replicate(2, rnorm(200))
)
Y <- rpois(200L, lambda = 2)

# ZERO INFLATED POISSON REGRESSION ---------------------

testthat::test_that(
  "Log-likelihood in C++ consistent with R implementation",{
    
    testthat::expect_equal(
      loglik_ZIP(params = params,
                 x = X,
                 z = Z,
                 y = Y,
                 weights = rep(1, nrow(X)),
                 offsetx = rep(0, nrow(X)),
                 offsetz = rep(0, nrow(Z)),
                 link = "probit"),
      loglik_ZIP_R(params = params,
                   X = X,
                   Z = Z,
                   Y = Y,
                   link = "probit")
    )
    
    testthat::expect_equal(
      loglik_ZIP(params = params,
                 x = X,
                 z = Z,
                 y = Y,
                 weights = rep(1, nrow(X)),
                 offsetx = rep(0, nrow(X)),
                 offsetz = rep(0, nrow(Z)),
                 link = "logit"),
      loglik_ZIP_R(params = params,
                   X = X,
                   Z = Z,
                   Y = Y,
                   weights = rep(1, nrow(X)),
                   offsetx = rep(0, nrow(X)),
                   offsetz = rep(0, nrow(Z)),
                   link = "logit")
    )
    
  }
)




testthat::test_that(
  "Log-likelihood in C++ consistent with R implementation",{
    
    testthat::expect_equal(
      loglik_ZIP(params = params,
                 x = X,
                 z = Z,
                 y = Y,
                 weights = rep(1, nrow(X)),
                 offsetx = rep(0, nrow(X)),
                 offsetz = rep(0, nrow(Z)),
                 link = "probit"),
      loglik_ZIP_R(params = params,
                   X = X,
                   Z = Z,
                   Y = Y,
                   weights = rep(1, nrow(X)),
                   offsetx = rep(0, nrow(X)),
                   offsetz = rep(0, nrow(Z)),
                   link = "probit")
    )
    
    testthat::expect_equal(
      loglik_ZIP(params = params,
                 x = X[Y>0,],
                 z = Z[Y>0,],
                 y = Y[Y>0],
                 weights = rep(1, nrow(X[Y>0,])),
                 offsetx = rep(0, nrow(X[Y>0,])),
                 offsetz = rep(0, nrow(Z[Y>0,])),
                 link = "logit"),
      loglik_ZIP_R(params = params,
                   X = X[Y>0,],
                   Z = Z[Y>0,],
                   Y = Y[Y>0],
                   weights = rep(1, nrow(X[Y>0,])),
                   offsetx = rep(0, nrow(X[Y>0,])),
                   offsetz = rep(0, nrow(Z[Y>0,])),
                   link = "logit")
    )
  }
)



# ZERO INFLATED NEGATIVE BINOMIAL REGRESSION ---------------------


testthat::test_that(
  "Log-likelihood in C++ consistent with R implementation",{
    
    testthat::expect_equal(
      loglik_ZINB(params = c(params,1),
                  x = X,
                  z = Z,
                  y = Y,
                  weights = rep(1, nrow(X)),
                  offsetx = rep(0, nrow(X)),
                  offsetz = rep(0, nrow(Z)),
                  link = "probit"),
      loglik_ZINB_R(params = c(params,1),
                    X = X,
                    Z = Z,
                    Y = Y,
                    weights = rep(1, nrow(X)),
                    offsetx = rep(0, nrow(X)),
                    offsetz = rep(0, nrow(Z)),
                    link = "probit")
    )
    
    testthat::expect_equal(
      loglik_ZINB(params = c(params,1),
                  x = X,
                  z = Z,
                  y = Y,
                  weights = rep(1, nrow(X)),
                  offsetx = rep(0, nrow(X)),
                  offsetz = rep(0, nrow(Z)),
                  link = "logit"),
      loglik_ZINB_R(params = c(params,1),
                    X = X,
                    Z = Z,
                    Y = Y,
                    weights = rep(1, nrow(X)),
                    offsetx = rep(0, nrow(X)),
                    offsetz = rep(0, nrow(Z)),
                    link = "logit")
      
    )
    
  }
)


# WITH OPTIM --------------------


# optim(
#   par = c(params,1),
#   fn = loglik_ZINB,
#   x = X,
#   z = Z,
#   y = Y,
#   weights = rep(1, nrow(X)),
#   offsetx = rep(0, nrow(X)),
#   offsetz = rep(0, nrow(Z)),
#   link = "logit"
# )
# 
# optim(
#   par = c(params,1),
#   fn = loglik_ZINB_R,
#   X = X,
#   Z = Z,
#   Y = Y,
#   weights = rep(1, nrow(X)),
#   offsetx = rep(0, nrow(X)),
#   offsetz = rep(0, nrow(Z)),
#   link = "logit"
# )
# 
# 
# optim(
#   par = c(params,1),
#   fn = loglik_ZINB,
#   x = X,
#   z = Z,
#   y = Y,
#   weights = rep(1, nrow(X)),
#   offsetx = rep(0, nrow(X)),
#   offsetz = rep(0, nrow(Z)),
#   link = "probit"
# )
# 
# optim(
#   par = c(params,1),
#   fn = loglik_ZINB_R,
#   X = X,
#   Z = Z,
#   Y = Y,
#   weights = rep(1, nrow(X)),
#   offsetx = rep(0, nrow(X)),
#   offsetz = rep(0, nrow(Z)),
#   link = "probit"
# )
# 
# 
# optim(
#   par = c(params),
#   fn = loglik_ZIP,
#   x = X,
#   z = Z,
#   y = Y,
#   weights = rep(1, nrow(X)),
#   offsetx = rep(0, nrow(X)),
#   offsetz = rep(0, nrow(Z)),
#   link = "probit"
# )
# 
# optim(
#   par = c(params),
#   fn = loglik_ZIP_R,
#   X = X,
#   Z = Z,
#   Y = Y,
#   weights = rep(1, nrow(X)),
#   offsetx = rep(0, nrow(X)),
#   offsetz = rep(0, nrow(Z)),
#   link = "probit"
# )




# microbenchmark::microbenchmark(
#   loglik_ZIP(params = params,
#              x = X,
#              z = Z,
#              y = Y,
#              link = "probit"),
#   loglik_ZIP_R(params = params,
#                X = X,
#                Z = Z,
#                Y = Y,
#                link = "probit"),
#   times = 50L
# )
# 
# microbenchmark::microbenchmark(
#   loglik_ZIP(params = params,
#              x = X,
#              z = Z,
#              y = Y,
#              link = "logit"),
#   loglik_ZIP_R(params = params,
#                X = X,
#                Z = Z,
#                Y = Y,
#                link = "logit"),
#   times = 50L
# )



# microbenchmark::microbenchmark(
#   loglik_ZINB(params = c(params,1),
#               x = X,
#               z = Z,
#               y = Y,
#               weights = rep(1, nrow(X)),
#               offsetx = rep(0, nrow(X)),
#               offsetz = rep(0, nrow(Z)),
#               link = "logit"),
#   loglik_ZINB_R(params = c(params,1),
#                 X = X,
#                 Z = Z,
#                 Y = Y,
#                 weights = rep(1, nrow(X)),
#                 offsetx = rep(0, nrow(X)),
#                 offsetz = rep(0, nrow(Z)),
#                 link = "logit")  
# )


# microbenchmark::microbenchmark(
#   loglik_ZINB(params = c(params,1),
#               x = X,
#               z = Z,
#               y = Y,
#               weights = rep(1, nrow(X)),
#               offsetx = rep(0, nrow(X)),
#               offsetz = rep(0, nrow(Z)),
#               link = "probit"),
#   loglik_ZINB_R(params = c(params,1),
#                 X = X,
#                 Z = Z,
#                 Y = Y,
#                 weights = rep(1, nrow(X)),
#                 offsetx = rep(0, nrow(X)),
#                 offsetz = rep(0, nrow(Z)),
#                 link = "probit")  
# )
