
#' @export
listcoeff <- function(object, ...){
  UseMethod("listcoeff")
}

#' @rdname listcoeff
listcoeff.light.zeroinfl <- function(object, ...){
  
  if (!inherits(object, "light.zeroinfl")) stop("Object is not light.zeroinfl")
  
  allcoeffs <- object$coefficients
  
  allcoeffs <- c(rownames(allcoeffs$count),
                   rownames(allcoeffs$zero))
  allcoeffs <- unique(allcoeffs)
  
  allcoeffs <- gsub(pattern = "\\_", replacement = "\\\\_", allcoeffs)

  return(allcoeffs)
}

