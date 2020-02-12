context("fastzeroinfl is consistent with existing pscl::fastzeroinfl function")


# I - ZERO INFLATED POISSON --------

# A/ logit

fit_pscl  <- pscl::zeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine,
                            link = "logit")
fit_speed  <- gravity:::fastzeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine,
                                     link = "logit")
fit_speedb  <- gravity:::fastzeroinfl2(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine,
                                       link = "logit")


testthat::test_that(
  "Coefficients are equal when using fastglm rather than glm",
  testthat::expect_equal(
    fit_pscl$coefficients,
    fit_speed$coefficients
  )
)

testthat::test_that(
  "Coefficients are equal when using C++ rather than R ML",
  testthat::expect_equal(
    fit_pscl$coefficients$count,
    fit_speedb$coefficients$count,
    tolerance = 0.05
  )
)

testthat::test_that(
  "Coefficients are equal when using C++ rather than R ML",
  testthat::expect_equal(
    fit_pscl$coefficients$zero,
    fit_speedb$coefficients$zero,
    tolerance = 0.05
  )
)


# B/ probit

fit_pscl  <- pscl::zeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine,
                            link = "probit")
fit_speed  <- gravity:::fastzeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine,
                                     link = "probit")
fit_speedb  <- gravity:::fastzeroinfl2(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine,
                                       link = "probit")


testthat::test_that(
  "Coefficients are equal when using fastglm rather than glm",
  testthat::expect_equal(
    fit_pscl$coefficients,
    fit_speed$coefficients
  )
)

testthat::test_that(
  "Coefficients are equal when using C++ rather than R ML",
  testthat::expect_equal(
    fit_pscl$coefficients$count,
    fit_speedb$coefficients$count,
    tolerance = 0.05
  )
)

testthat::test_that(
  "Coefficients are equal when using C++ rather than R ML",
  testthat::expect_equal(
    fit_pscl$coefficients$zero,
    fit_speedb$coefficients$zero,
    tolerance = 0.05
  )
)


microbenchmark::microbenchmark(
  pscl::zeroinfl(Days ~ Sex + Age, data = MASS::quine,
                 dist = "poisson"),
  gravity:::fastzeroinfl(Days ~ Sex + Age, data = MASS::quine,
                         dist = "poisson"),
  gravity:::fastzeroinfl2(Days ~ Sex + Age, data = MASS::quine,
                          dist = "poisson"),
  times = 50L
)

# NEGATIVE BINOMIAL ------------

data("bioChemists", package = "pscl")


fm_zinb2 <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
fm_zinb2a <- gravity::fastzeroinfl(art ~ . | ., data = bioChemists, dist = "negbin")
fm_zinb2b <- gravity:::fastzeroinfl2(art ~ . | ., data = bioChemists, dist = "negbin")



# MORE TESTS --------------------



# coefficients ==============-

test_that("pscl::zeroinfl and gravity::fastzeroinfl coef are equal [at rounding error]", {
  expect_equal(coef(fit_pscl), coef(fit_speed),
               tolerance = 1e-4)
})


# We list output from glm

## residuals ==================

test_that("residuals are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(as.numeric(fit_pscl$residuals),
               as.numeric(fit_speed$residuals))
})

## fitted.values ==============

test_that("fitted.values are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(as.numeric(fit_pscl$fitted.values),
               as.numeric(fit_speed$fitted.values))
})

## rank ==========-

test_that("fitted.values are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(fit_pscl$rank,
               fit_speed$rank)
})


## family ====================-

test_that("distributions are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(fit_pscl$link,
               fit_speed$link)
  expect_equal(fit_pscl$dist,
               fit_speed$dist)
})




## aic ========================-

test_that("aic are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(as.numeric(AIC(fit_pscl)),
               as.numeric(AIC(fit_speed))
  )
})

## df.null ==============-

test_that("df.null are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(as.numeric(fit_pscl$df.null),
               as.numeric(fit_speed$df.null)
  )
})

## weights ====================

# test_that("weights are the same fastzeroinfl and zeroinfl theta", {
#   expect_equal(as.numeric(fit_pscl$weights),
#                as.numeric(fit_speed$weights)
#   )
# })


## df.residual ====================-

test_that("df.residual are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(as.numeric(fit_pscl$df.residual),
               as.numeric(fit_speed$df.residual)
  )
})

## y ======================-

test_that("y are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(as.numeric(fit_pscl$y),
               as.numeric(fit_speed$y)
  )
})



## model ====================-

test_that("model are the same fastzeroinfl and zeroinfl theta", {
  expect_equal(fit_pscl$model,
               fit_speed$model
  )
})

## converged ====================

test_that("converged are the same fastzeroinfl and zeroinfl theta", {
  expect_true(fit_pscl$converged)
  expect_true(fit_speed$converged)
})

## formula ====================-

test_that("formula", {
  expect_equal(fit_pscl$formula,
               fit_speed$formula
  )
})

## terms ======================-

test_that("terms", {
  expect_identical(fit_pscl$terms,
                   fit_speed$terms
  )
})





# microbenchmark

data("bioChemists", package = "pscl")
microbenchmark::microbenchmark(
  fit_pscl2  <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "negbin"),
  fit_speed2  <- gravity:::fastzeroinfl(art ~ . | ., data = bioChemists, dist = "negbin"),
  fit_speedb2  <- gravity:::fastzeroinfl2(art ~ . | ., data = bioChemists, dist = "negbin")
)

microbenchmark::microbenchmark(
  fit_pscl2  <- pscl::zeroinfl(art ~ . | ., data = bioChemists, dist = "poisson"),
  fit_speed2  <- gravity:::fastzeroinfl(art ~ . | ., data = bioChemists, dist = "poisson"),
  fit_speedb2  <- gravity:::fastzeroinfl2(art ~ . | ., data = bioChemists, dist = "poisson")
)

