
#' Strip regression objects to reduce memory needs
#' 
#' Remove elements that are more related
#'  to prediction than model to reduce
#'  memory needs to store a glm output
#'  
#' 
#' @param object A regression object or summary.
#'  Accepted classes are \link[stats]{glm} or
#'  \link[pscl]{zeroinfl}
#'  
#' @return Initial object stripped from
#'  heavy elements. Initial class is returned
#'  with a new one indicating the object has
#'  been stripped.
#' 
#' @export

strip <- function(object, ...){
  UseMethod("strip")
}

#' @rdname strip
#' @method strip glm
#' @S3method strip glm

strip.glm <- function(object) {
  
  if (!inherits(object, "glm")) stop("object' should be a glm object") 
  
  object$y = c()
  object$model = c()
  
  object$coefficients <- summary(object)$coefficients
  
  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  # object$qr$qr = c()  
  # object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()
  
  
  object$family$variance = c()
  #object$family$dev.resids = c()
  #object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()
  
  
  class(object) <- c(class(object), "light.glm")
  
  return(object)
}

#' @rdname strip
#' @method strip lm
#' @S3method strip lm

strip.lm <- function(object) {
  
  if (!inherits(object, "lm")) stop("object' should be a lm object") 
  
  object$coefficients <- summary(object)$coefficients
  
  object$y = c()
  object$model = c()
  
  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  # object$qr$qr = c()  
  # object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()
  
  
  object$family$variance = c()
  #object$family$dev.resids = c()
  #object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()
  
  
  class(object) <- c(class(object), "light.glm")
  
  return(object)
}


#' @rdname strip
#' @method strip summary.glm
#' @S3method strip summary.glm

strip.summary.glm <- function(object){
  
  if (!inherits(object, "summary.glm")) stop("object' should be the summary of a glm object") 
  
  object$deviance.resid <- NULL
  
  class(object) <- c(class(object), "light.summary.glm")
  
  return(object)
}

#' @rdname strip
#' @method strip summary.selection
#' @S3method strip summary.selection

strip.selection <- function(object) {
  
  if (!inherits(object, "selection")) stop("object' should be a selection object") 
  
  object$coefficients <- summary(object)$coefficients
  
  object$y = c()
  object$model = c()
  
  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  # object$qr$qr = c()  
  # object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()
  
  
  object$family$variance = c()
  #object$family$dev.resids = c()
  #object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()
  
  
  class(object) <- c(class(object), "light.glm")
  
  return(object)
}


#' @rdname strip
#' @method strip summary.lm
#' @S3method strip summary.lm

strip.summary.lm <- function(object){
  
  if (!inherits(object, "summary.lm")) stop("object' should be the summary of a lm object") 
  
  object$deviance.resid <- NULL
  
  class(object) <- c(class(object), "light.summary.lm")
  
  return(object)
}

#' @rdname strip
#' @method strip summary.selection
#' @S3method strip summary.selection

strip.summary.selection <- function(object){
  
  if (!inherits(object, "summary.selection")) stop("object' should be the summary of a selection object") 
  
  object$deviance.resid <- NULL
  
  class(object) <- c(class(object), "light.summary.lm")
  
  return(object)
}

#' @rdname strip
#' @method strip zeroinfl
#' @S3method strip zeroinfl

strip.zeroinfl <- function(object) {
  
  if (!inherits(object, "zeroinfl")) stop("object' should be a zeroinfl object") 
  
  object$coefficients <- summary(object)$coefficients
  
  object$y = c()
  object$model = c()
  
  object$residuals = c()
  object$fitted.values = c()
  object$effects = c()
  # object$qr$qr = c()  
  # object$linear.predictors = c()
  object$weights = c()
  object$prior.weights = c()
  object$data = c()
  
  
  object$family$variance = c()
  #object$family$dev.resids = c()
  #object$family$aic = c()
  object$family$validmu = c()
  object$family$simulate = c()
  attr(object$terms,".Environment") = c()
  attr(object$formula,".Environment") = c()
  
  class(object) <- c(class(object), "light.zeroinfl")
  
  return(object)
}

#' @rdname strip
#' @method strip summary.zeroinfl
#' @S3method strip summary.zeroinfl

strip.summary.zeroinfl <- function(object){
  
  if (!inherits(object, "summary.zeroinfl")) stop("object' should be the summary of a zeroinfl object") 
  
  object$weights <- NULL
  object$residuals <- NULL
  object$fitted.values <- NULL
  #object$model <- NULL
  class(object) <- c(class(object), "light.summary.zeroinfl")
  
  return(object)
}


#' Trim a big object to reduce memory need
#' 
#' This function returns as little information
#'  from obj needed
#'  
#' @return A list with following elements
#' \itemize{
#'   \item object - \link{strip} object
#'   \item summary - \link{strip} summary object. For
#'    \link[pscl]{zeroinfl} objects, contain elements
#'    \code{obj_trim}, \code{se_count}, \code{se_selection}
#' }
#' @param obj An object whose class is 
#'  accepted by \link{strip} method
#' @export

trim_big_object <- function(obj){
  
  trim_summary_zeroinfl <- function(obj){
    
    obj.summary <- strip(
      summary(obj)
    )
    obj_trim <- list(
      "obj_trim" = strip(obj),
      "se_count" = obj.summary$coefficients$count[,"Std. Error"],
      "se_selection" = obj.summary$coefficients$zero[,"Std. Error"]
    )
    
    return(obj_trim)
  }
  
  trim_summary_selection <- function(obj){
    
    obj.summary <- strip(
      summary(obj)
    )
    idx_outcome <- obj$param$index$betaO
    idx_selection <- obj$param$index$betaS
    obj_trim <- list(
      "obj_trim" = strip(obj),
      "se_count" = obj.summary$estimate$count[idx_outcome,"Std. Error"],
      "se_selection" = obj.summary$estimate$zero[idx_selection,"Std. Error"],
      "pvalues_count" = obj.summary$estimate$count[idx_outcome,"Pr(>|t|)"],
      "pvalues_selection" = obj.summary$estimate$zero[idx_selection,"Pr(>|t|)"]
    )
    
    return(obj_trim)
  }
  
  # FIX TO BE ABLE TO USE STARGAZER
  if (obj$call[1] == str2lang("gravity::fastzeroinfl()")) obj$call[1] <- str2lang("zeroinfl()")

  if (inherits(obj, "zeroinfl")){
    list(
      "object" = strip(obj),
      "summary" = trim_summary_zeroinfl(obj)
    )
  } else{
    list(
      "object" = strip(obj),
      "summary" = strip(summary(obj))
    )
  }
}
