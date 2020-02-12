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

gravity:::fastZI(X, Z, Y)
gravity:::fastZI(X, Z, Y, link = "logit")


fit_pscl  <- pscl::zeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine, x = TRUE)
fit_speed  <- gravity:::fastzeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine)
fit_speedb  <- gravity:::fastzeroinfl2(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine)


fastZI(x=as.matrix(fit_pscl$x$count), z = as.matrix(fit_pscl$x$zero),
       y = as.numeric(fit_pscl$y), link = "probit")

fastZI(as.matrix(fit_pscl$x$count), as.matrix(fit_pscl$x$zero),
       as.numeric(fit_pscl$y), link = "logit")
