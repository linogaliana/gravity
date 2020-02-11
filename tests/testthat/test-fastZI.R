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


fit_pscl  <- pscl::zeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine)
fit_speed  <- gravity:::fastzeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine)
fit_speedb  <- gravity:::fastzeroinfl2(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine)


fastZI(X, Z, Y, link = "probit")
fastZI(X, Z, Y, link = "logit")
