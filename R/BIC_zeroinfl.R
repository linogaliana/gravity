#' Bayesian information criterion implementation for \code{zeroinfl} objects
#' 
#' @param object A fitted model object for which there exists a logLik
#'  method to extract the corresponding log-likelihood, or
#'  an object inheriting from class logLik.
#' @param ... Optionally more fitted model objects
#' 
#' 
#' Formula \eqn{-2l + k\log(npar)} where *npar*
#'  represents the number of parameters in the fitted model
#'  and \eqn{l} represents model log-likelihood
#' 
#' @import stats
#' @export
#' 

BIC.zeroinfl <- function(object, ...){
  
  if (inherits(object,"light.zeroinfl")) return(
    BIC.light.zeroinfl(object, ...)
  )
  
  # We fit 2 objects coefficients + theta parameter
  k <- length(object$coefficients$count) + length(object$coefficients$zero)
  if (!is.null(object$theta)) k <- k + 1
  return(
    -2*object$loglik + k*log(object$n)
  )
  
}

BIC.fastzeroinfl <- BIC.zeroinfl
