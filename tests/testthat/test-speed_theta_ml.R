context("speed_theta_ml produces same result than MASS::theta.ml")


# We fit a GLM object
fit <- fastglm::fastglm(
  x = as.matrix(cars[,"dist"]), y = cars[,"speed"],
  family = poisson(link = "log"),
  method = 2L
)

mu <- fit$fitted.values

th_mass <- as.vector(MASS::theta.ml(cars[,"speed"], mu))
th <- as.vector(gravity::speed_theta_ml(cars[,"speed"], mu))



# OUTPUT CONSISTENT WITH MASS FUNCTION ----------------------------------------

test_that("speed_theta_ml produces same output than MASS::theta.ml", {
  expect_equal(
    th_mass,
    th
  )
})



