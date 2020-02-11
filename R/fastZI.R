fastZI <- function(x, y, start = rep(0, ncol(x)), eps_f = 1e-08, eps_g = 1e-05, 
                    maxit = 300){
  
  
  return(
    fastZI_(x,
          z,
          y,
          weights,
          offsetx,
          offsetz,
          start = start, eps_f = eps_f, eps_g = eps_g, 
          maxit = maxit)
  )
  
}
  


