context("fastzeroinfl is consistent with existing pscl::fastzeroinfl function")


fit_pscl  <- pscl::zeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine)
fit_speed  <- gravity:::fastzeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine)
fit_speedb  <- gravity:::fastzeroinfl2(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine)


fit_pscl2  <- pscl::zeroinfl(Days ~ Sex + Age, data = MASS::quine,
                             dist = "negbin")
fit_speed2  <- gravity:::fastzeroinfl(Days ~ Sex + Age, data = MASS::quine,
                                      dist = "negbin")
fit_speed3  <- gravity:::fastzeroinfl2(Days ~ Sex + Age, data = MASS::quine,
                                     dist = "negbin")


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
