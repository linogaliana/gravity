fastZI <- function(x, z, y, weights = NULL, offsetx = NULL,
                   offsetz = NULL, 
                   start = rep(0, ncol(X) + ncol(Z)),
                   link = "probit",
                   eps_f = 1e-08, eps_g = 1e-05, 
                    maxit = 300, ...){
  
  
  if (missing(weights)) weights <- rep(1,nrow(X))
  if (missing(offsetx)) offsetx <- rep(0,nrow(X))
  if (missing(offsetz)) offsetz <- rep(0,nrow(Z))  
  
  
  return(
    fastZI_(x,
          z,
          y,
          weights = weights,
          offsetx = offsetx,
          offsetz = offsetz,
          start = start,
          eps_f = eps_f, eps_g = eps_g, 
          maxit = maxit,
          link = link)
  )
  
}
  


