
#' Strip regression objects to reduce memory needs
#' 
#' Remove elements that are more related
#'  to prediction than model to reduce
#'  memory needs to store a glm output
#' 
#' @param object A regression object
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
  
  return(object)
}


#' @rdname strip
#' @method strip summary.glm
#' @S3method strip summary.glm

strip.summary.glm <- function(object){
  
  if (!inherits(object, "summary.glm")) stop("object' should be the summary of a glm object") 
  
  object$deviance.resid <- NULL
  return(object)
}


#' @rdname strip
#' @method strip zeroinfl
#' @S3method strip zeroinfl

strip.zeroinfl <- function(object) {
  
  if (!inherits(object, "zeroinfl")) stop("object' should be a zeroinfl object") 
  
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
  
  return(object)
}
