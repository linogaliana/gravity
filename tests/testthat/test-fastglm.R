
fit_mass  <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine,
                          control = glm.control(maxit = 12))
fit_speed <- gravity:::fastglm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine,
                                  control = glm.control(maxit = 12))


# coefficients ==============-

test_that("fastglm.nb and glm.nb coef [at rounding error]", {
  expect_equal(coef(fit_mass), coef(fit_speed),
               tolerance = 1e-4)
})

# We list output from glm

## coefficients ==============-

test_that("theta is the same fastglm.nb and glm.nb theta", {
  expect_equal(fit_mass$theta, fit_speed$theta)
})

## residuals ==================

test_that("residuals are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$residuals),
               as.numeric(fit_speed$residuals))
})

## fitted.values ==============

test_that("fitted.values are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$fitted.values),
               as.numeric(fit_speed$fitted.values))
})

## rank ==========-

test_that("fitted.values are the same fastglm.nb and glm.nb theta", {
  expect_equal(fit_mass$rank,
               fit_speed$rank)
})


## family ====================-

test_that("fitted.values are the same fastglm.nb and glm.nb theta", {
  expect_equal(fit_mass$family,
               fit_speed$family)
})


## linear.predictors ==========-

test_that("linear.predictors are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$linear.predictors),
               as.numeric(fit_speed$linear.predictors)
               )
})

## deviance ====================

test_that("deviance are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$deviance),
               as.numeric(fit_speed$deviance)
  )
})

## aic ========================-

test_that("aic are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$aic),
               as.numeric(fit_speed$aic)
  )
})

## null.deviance ==============-

test_that("aic are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$null.deviance),
               as.numeric(fit_speed$null.deviance)
  )
})

## iter ========================

test_that("aic are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$iter),
               as.numeric(fit_speed$iter)
  )
})

## weights ====================

test_that("weights are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$weights),
               as.numeric(fit_speed$weights)
  )
})


## prior.weights ==================-

test_that("prior.weights are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$prior.weights),
               as.numeric(fit_speed$prior.weights)
  )
})


## df.residual ====================-

test_that("df.residual are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$df.residual),
               as.numeric(fit_speed$df.residual)
  )
})

## y ======================-

test_that("y are the same fastglm.nb and glm.nb theta", {
  expect_equal(as.numeric(fit_mass$y),
               as.numeric(fit_speed$y)
  )
})


## x ========================-

test_that("x are the same fastglm.nb and glm.nb theta", {
  expect_equal(fit_mass$x,
               fit_speed$x
  )
})

## model ====================-

test_that("model are the same fastglm.nb and glm.nb theta", {
  expect_equal(fit_mass$model,
               fit_speed$model
  )
})

## converged ====================

test_that("converged are the same fastglm.nb and glm.nb theta", {
  expect_true(fit_mass$converged)
  expect_true(fit_speed$converged)
})

## formula ====================-

test_that("formula", {
  expect_equal(fit_mass$formula,
               fit_speed$formula
  )
})

## terms ======================-

test_that("terms", {
  expect_identical(fit_mass$terms,
               fit_speed$terms
  )
})



# set init.theta ----------

fit_mass  <- MASS::glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine,
                          control = glm.control(maxit = 12),
                          init.theta = 2)
fit_speed <- gravity:::fastglm.nb(Days ~ Sex/(Age + Eth*Lrn), data = MASS::quine,
                                  control = glm.control(maxit = 12),
                                  init.theta = 2)

test_that("fastglm.nb and glm.nb coef [at rounding error]", {
  expect_equal(coef(fit_mass), coef(fit_speed),
               tolerance = 1e-4)
})


