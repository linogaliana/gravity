grad_ZIP_R <- function(params, X, Z, Y,
                        weights = NULL,
                        offsetx = NULL,
                        offsetz = NULL,
                        link = c("probit","logit")){
  
  link <- match.arg(link)
  
  if (missing(weights)) weights <- rep(1,nrow(X))
  if (missing(offsetx)) offsetx <- rep(0,nrow(X))
  if (missing(offsetz)) offsetz <- rep(0,nrow(Z))
  
  
  kx <- ncol(X)
  kz <- ncol(Z)
  
  linkobj <- make.link(link)
  linkinv <- linkobj$linkinv
  
  eta <- as.vector(X %*% params[1:kx] + offsetx)
  mu <- exp(eta)
  etaz <- as.vector(Z %*% params[(kx + 1):(kx + kz)] + 
                      offsetz)
  muz <- linkinv(etaz)
  
  
  clogdens0 <- -mu
  
  Y1 <- (Y>0)
  
  dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
                                              clogdens0)
  wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + 
                                          log(1 - muz) + clogdens0 + log(mu)))
  
  
  wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                      (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
  colSums(cbind(wres_count * weights * X, wres_zero * 
                  weights * Z))
}



grad_ZINB_R <- function(params, X, Z, Y,
                       weights = NULL,
                       offsetx = NULL,
                       offsetz = NULL,
                       link = c("probit","logit")){
  
  link <- match.arg(link)
  
  if (missing(weights)) weights <- rep(1,nrow(X))
  if (missing(offsetx)) offsetx <- rep(0,nrow(X))
  if (missing(offsetz)) offsetz <- rep(0,nrow(Z))
  
  
  kx <- ncol(X)
  kz <- ncol(Z)
  
  linkobj <- make.link(link)
  linkinv <- linkobj$linkinv
  
  eta <- as.vector(X %*% params[1:kx] + offsetx)
  mu <- exp(eta)
  etaz <- as.vector(Z %*% params[(kx + 1):(kx + kz)] + 
                      offsetz)
  muz <- linkinv(etaz)

  Y1 <- (Y>0)
  
  theta <- exp(params[(kx + kz) + 1])
  clogdens0 <- dnbinom(0, size = theta, mu = mu, log = TRUE)
  dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
                                              clogdens0)
  wres_count <- ifelse(Y1, Y - mu * (Y + theta)/(mu + 
                                                   theta), -exp(-log(dens0) + log(1 - muz) + clogdens0 + 
                                                                  log(theta) - log(mu + theta) + log(mu)))
  wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                      (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
  wres_theta <- theta * ifelse(Y1, digamma(Y + theta) - 
                                 digamma(theta) + log(theta) - log(mu + theta) + 
                                 1 - (Y + theta)/(mu + theta), exp(-log(dens0) + 
                                                                     log(1 - muz) + clogdens0) * (log(theta) - log(mu + 
                                                                                                                     theta) + 1 - theta/(mu + theta)))
  colSums(cbind(wres_count * weights * X, wres_zero * 
                  weights * Z, wres_theta))
}

