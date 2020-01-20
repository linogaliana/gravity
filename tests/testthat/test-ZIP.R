testthat::context("Zero inflated Poisson with Rcpp equivalent to R based approach")


fit_pscl  <- pscl::zeroinfl(Days ~ Sex + Age + Eth*Lrn, data = MASS::quine,
                            x = TRUE)

X <- fit_pscl$x$count
Z <- fit_pscl$x$zero
Y <- fit_pscl$y
weights <- fit_pscl$weights
offsetx <- 0
offsetz <- 0


Y0 <- Y <= 0
Y1 <- Y > 0

linkstr <- "probit"
linkobj <- make.link(linkstr)
linkinv <- linkobj$linkinv

model_count <- glm.fit(X, Y, family = poisson())
model_zero <- glm.fit(Z, as.integer(Y0),
                      family = binomial(link = linkstr))

start <- list(count = model_count$coefficients, zero = model_zero$coefficients)

kx <- NCOL(X)
kz <- NCOL(Z)


ziPoisson <- function(parms) {
  mu <- as.vector(exp(X %*% parms[1:kx]))
  phi <- as.vector(linkinv(Z %*% parms[(kx + 1):(kx + 
                                                   kz)]))
  loglik0 <- log(phi + exp(log(1 - phi) - mu))
  loglik1 <- log(1 - phi) + dpois(Y, lambda = mu, log = TRUE)
  loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[Y1] * 
                                                   loglik1[Y1])
  loglik
}

gradPoisson <- function(parms) {
  eta <- as.vector(X %*% parms[1:kx] + offsetx)
  mu <- exp(eta)
  etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + 
                      offsetz)
  muz <- linkinv(etaz)
  clogdens0 <- -mu
  dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
                                              clogdens0)
  wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + 
                                          log(1 - muz) + clogdens0 + log(mu)))
  wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                      (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
  colSums(cbind(wres_count * weights * X, wres_zero * 
                  weights * Z))
}

ziPoisson(c(start$count, 
            start$zero))

gradPoisson(c(start$count, 
              start$zero))
