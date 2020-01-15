# estimated_variance <- function(object){
#   UseMethod("estimated_variance")
# }

# estimated_variance_nb <- function(object = MASS::glm.nb(art ~ . , data = bioChemists)){
#   
#   y <- object$y
#   X <- model.matrix(object)
# 
#   alpha <- 1/object$theta
#   mu <- object$fitted.values
#   beta <- object$coefficients
#   
#   essai <- sapply(1:length(beta), function(r){
#     sum(
#       X[,r]*mu*(1+alpha*y)/(1+alpha*mu)^2
#     )
#   })
#   sqrt(1/essai)
#   
#   summary(object)$coefficients
# 
# } 

# estimated_variance_ZIP <- function(object = pscl::zeroinfl(art ~ . | ., data = bioChemists)){
#   
#   y <- object$y
#   X <- model.matrix(object)
#   Z <- X
#   
#   beta <- object$coefficients$count
#   gamma <- object$coefficients$count
#   
#   if (is.null(object$offset$count)){
#     mu <- exp(X %*% beta)
#   } else{
#     mu <- exp(log(offset$count) + X %*% beta)
#   }
#   
#   if (is.null(object$offset$zero)){
#     lambda <- exp(Z %*% gamma)
#   } else{
#     lambda <- exp(log(offset$zero) + Z %*% gamma)
#   }
#   
#   se_beta <- sapply(1:length(beta), function(r){
#     l1 <- X[,r]*X[,r]*mu*((mu-1)*lambda*exp(mu)-1)/(lambda*exp(mu)+1)^2
#     l2 <- mu*X[,r]*X[,r]
#     derivl <- sum(l1[y==0]) - sum(l2[y==0]) 
#   })
#   se_gamma <- sapply(1:length(gamma), function(r){
#     l1 <- Z[,r]*Z[,r]*lambda*exp(mu)/(lambda*exp(mu)+1)^2
#     l2 <- Z[,r]*Z[,r]*lambda/(lambda+1)^2
#     derivl <- sum(l1[y==0]) - sum(l2) 
#   })
#   
#   1/(-se_beta*nrow(X))
#   
#   summary(object)$coefficients
#   
# } 
