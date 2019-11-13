
#' Fit a negative binomial generalized linear model
#' 
#' A modification of the function \link[MASS]{glm.nb} to
#'  reduce time and memory consumption
#'  
#' @inheritParams MASS::glm.nb
#' 
#' @importFrom fastglm fastglm
#' @seealso \link{fastglm.nb} ; \link[MASS]{theta.ml} ; \link[MASS]{glm.nb}
#' 
#' @importFrom "stats" ".getXlevels" "glm.control" "is.empty.model" "model.extract" "model.matrix" "model.offset" "model.response" "model.weights"
#' @importFrom MASS negative.binomial
#' @import RcppEigen
#' @export

fastglm.nb <- function(formula, data, weights, subset, na.action, start = NULL, 
                        etastart, mustart, control = glm.control(...), method = "glm.fit", 
                        model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, ..., 
                        init.theta = NULL, link = log){
  
  loglik <- function(n, th, mu, y, w) sum(w * (lgamma(th + 
                                                        y) - lgamma(th) - lgamma(y + 1) + th * log(th) + y * 
                                                 log(mu + (y == 0)) - (th + y) * log(th + mu)))
  link <- substitute(link)
  fam0 <- if (is.null(init.theta)) {
    do.call("poisson", list(link = link))
  } else{
    do.call(MASS::negative.binomial, list(theta = init.theta, 
                                      link = link))
  }
  mf <- Call <- match.call()
  
  m <- match(c("formula", "data", "subset", "weights", "na.action", 
               "etastart", "mustart", "offset"), names(mf), 0)
  
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval.parent(mf)
  mf <- mf
  
  Terms <- attr(mf, "terms")
  
  
  
  Y <- model.response(mf, "numeric")
  
  ## null model support
  X <- if (!is.empty.model(Terms)) model.matrix(Terms, mf, contrasts) else matrix(,NROW(Y),0)
  
  w <- model.weights(mf)
  if(!length(w)){
    w <- rep(1, nrow(mf))
  }else if(any(w < 0)){
    stop("negative weights not allowed")
  }
  
  offset <- model.offset(mf)
  
  
  mustart <- model.extract(mf, "mustart")
  etastart <- model.extract(mf, "etastart")
  n <- length(Y)
  
  start = NULL
  
  if(control$trace > 1) message("Initial fit:")
  
  fit <- fastglm::fastglm(
    x = X, y = Y, family = fam0, weights = w,
    start = start,
    etastart = etastart, 
    mustart = mustart,
    offset = offset, maxit = control$maxit,
    method = 2L
  )
  
  class(fit) <- c("fastglm","glm", "lm")
  
  mu <- fit$fitted.values
  
  th <- as.vector(speed_theta_ml(Y, mu, limit = control$maxit, 
                                 eps = control$epsilon,
                                 trace = control$trace > 2))
  
  
  if (control$trace > 1) 
    message(gettextf("Initial value for 'theta': %f", signif(th)), 
            domain = NA)
  fam <- do.call(MASS::negative.binomial, list(theta = th, link = link))
  iter <- 0
  d1 <- sqrt(2 * max(1, fit$df.residual))
  d2 <- del <- 1
  g <- fam$linkfun
  Lm <- loglik(n, th, mu, Y, w)
  Lm0 <- Lm + 2 * d1
  
  
  while ((iter <- iter + 1) <= control$maxit && (abs(Lm0 - 
                                                     Lm)/d1 + abs(del)/d2) > control$epsilon){
    
    eta <- g(mu)
    
    
    fit <- fastglm::fastglm(x = X, y = Y,
                            weights = w, etastart = eta,
                            offset = offset, family = fam,
                            maxit = control$maxit,
                            method = 2L)
    
    t0 <- th
    
    th <- speed_theta_ml(Y, mu, limit = control$maxit, 
                         eps = control$epsilon,
                         trace = control$trace > 2)
    
    fam <- do.call(MASS::negative.binomial, list(theta = th, 
                                             link = link))
    mu <- fit$fitted.values
    del <- t0 - th
    Lm0 <- Lm
    Lm <- loglik(n, th, mu, Y, w)
    
    if (control$trace) {
      Ls <- loglik(n, th, Y, Y, w)
      Dev <- 2 * (Ls - Lm)
      message(sprintf("Theta(%d) = %f, 2(Ls - Lm) = %f", 
                      iter, signif(th), signif(Dev)), domain = NA)
    }
    gc()
  }
  
  
  if (!is.null(attr(th, "warn"))) 
    fit$th.warn <- attr(th, "warn")
  if (iter > control$maxit) {
    warning("alternation limit reached")
    fit$th.warn <- gettext("alternation limit reached")
  }
  if (length(offset) && attr(Terms, "intercept")) {
    null.deviance <- if (length(Terms)) 
      fastglm::fastglm(x = X[, "(Intercept)", drop = FALSE],
                       y = Y, 
                       weights = w,
                       offset = offset, family = fam,
                       control = list(maxit = control$maxit, 
                                      epsilon = control$epsilon, trace = control$trace > 
                                        1),
                       intercept = TRUE,
                       method = 2L)$deviance
    else fit$deviance
    fit$null.deviance <- null.deviance
  }
  class(fit) <- c("negbin", "glm", "lm","fastglm")
  fit$terms <- Terms
  fit$formula <- as.vector(attr(Terms, "formula"))
  Call$init.theta <- signif(as.vector(th), 10)
  Call$link <- link
  fit$call <- Call
  if (model) 
    fit$model <- mf
  fit$na.action <- attr(mf, "na.action")
  if (x) 
    fit$x <- X
  if (!y) 
    fit$y <- NULL
  fit$theta <- as.vector(th)
  fit$SE.theta <- attr(th, "SE")
  fit$twologlik <- as.vector(2 * Lm)
  fit$aic <- -fit$twologlik + 2 * fit$rank + 2
  fit$contrasts <- attr(X, "contrasts")
  fit$xlevels <- .getXlevels(Terms, mf)
  fit$method <- method
  fit$control <- control
  fit$offset <- offset
  fit  
}
