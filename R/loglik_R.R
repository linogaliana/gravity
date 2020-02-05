loglik_ZIP_R <- function(params, X, Z, Y,
                         weights = rep(1,nrow(X)),
                         offsetx = rep(0,nrow(X)),
                         offsetz = rep(0,nrow(Z)),
                         link = c("probit","logit")) {
  
  link <- match.arg(link)
  
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
