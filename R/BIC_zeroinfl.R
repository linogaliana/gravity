#' Bayesian information criterion implementation for \code{zeroinfl} objects
#' 
#' @inheritParams stats::AIC
#' 
#' Formula \eqn{-2*l + k*\log(npar)} where \eqn{npar} 
#'  represents the number of parameters in the fitted model
#'  and \eqn{l} represents model log-likelihood
#' 
#' @export

BIC.zeroinfl <- function(object){
  # We fit 2 objects coefficients + theta parameter
  k <- length(object$coefficients$count) + length(object$coefficients$zero)
  if (!is.null(object$theta)) k <- k + 1
  return(
    -2*object$loglik + k*log(object$n)
  )
}
