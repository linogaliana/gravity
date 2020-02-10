loglik_ZIP_R <- function(params, X, Z, Y,
                         weights = NULL,
                         offsetx = NULL,
                         offsetz = NULL,
                         link = c("probit","logit")) {
  
  link <- match.arg(link)
  
  if (missing(weights)) weights <- rep(1,nrow(X))
  if (missing(offsetx)) offsetx <- rep(0,nrow(X))
  if (missing(offsetz)) offsetz <- rep(0,nrow(Z))
  
  
  kx <- ncol(X)
  kz <- ncol(Z)

  linkinv <- make.link(link)$linkinv

  mu <- as.vector(exp(X %*% params[1:kx] + offsetx))
  phi <- as.vector(linkinv(Z %*% params[(kx + 1):(kx + 
                                                   kz)] + offsetz))
  loglik0 <- log(phi + exp(log(1 - phi) - mu))
  loglik1 <- log(1 - phi) + dpois(Y, lambda = mu, log = TRUE)
  
  Y0 <- (Y==0)
  
  loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[!Y0] * 
                                                   loglik1[!Y0])
  loglik
}



loglik_ZINB_R <- function(params, X, Z, Y,
                         weights = NULL,
                         offsetx = NULL,
                         offsetz = NULL,
                         link = c("probit","logit")){
  
  if (missing(weights)) weights <- rep(1,nrow(X))
  if (missing(offsetx)) offsetx <- rep(0,nrow(X))
  if (missing(offsetz)) offsetz <- rep(0,nrow(Z))
  
  link <- match.arg(link)
  
  kx <- ncol(X)
  kz <- ncol(Z)
  
  linkinv <- make.link(link)$linkinv
  
  mu <- as.vector(exp(X %*% params[1:kx] + offsetx))
  phi <- as.vector(linkinv(Z %*% params[(kx + 1):(kx + 
                                                   kz)] + offsetz))
  theta <- exp(params[(kx + kz) + 1])
  loglik0 <- log(phi + exp(log(1 - phi) + suppressWarnings(dnbinom(0, 
                                                                   size = theta, mu = mu, log = TRUE))))
  loglik1 <- log(1 - phi) + suppressWarnings(dnbinom(Y, 
                                                     size = theta, mu = mu, log = TRUE))
  
  Y0 <- (Y==0)
  
  loglik <- sum(weights[Y0] * loglik0[Y0]) + sum(weights[!Y0] * 
                                                   loglik1[!Y0])
  loglik
}
