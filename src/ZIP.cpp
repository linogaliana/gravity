// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]

#include <RcppNumerical.h>

using namespace Numer;

typedef Eigen::Map<Eigen::MatrixXd> MapMat;
typedef Eigen::Map<Eigen::VectorXd> MapVec;

class ZIP: public MFuncGrad
{
private:
  const MapMat X;
  const MapMat Z;
  const MapVec Y;
public:
  ZIP(const MapMat x_, const MapMat z_, const MapVec y_) : X(x_), Z(z_), Y(y_) {}

  

  double f_grad(Constvec& beta, Constvec& gamma,
                Refvec grad)
  {
    // Negative log likelihood
    //   sum(log(1 + exp(X * beta))) - y' * X * beta
    
    
    // 
    Eigen::VectorXd eta = X*beta + offsetx ;
    Eigen::VectorXd mu = exp(eta);
    Eigen::VectorXd etaz = Z*gamma + offsetz ;
    Eigen::VectorXd muz = linkinv(etaz);
    Eigen::VectorXd clogdens0 = -mu;
    Eigen::VectorXd Y1 = (Y==0) ;

    
    Eigen::VectorXd dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + clogdens0) ;
    
   wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + 
     log(1 - muz) + clogdens0 + log(mu)))
   wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                       (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
   
    
    Eigen::VectorXd xbeta = X * beta;
    const double yxbeta = Y.dot(xbeta);
    // X * beta => exp(X * beta)
    xbeta = xbeta.array().exp();
    const double f = (xbeta.array() + 1.0).log().sum() - yxbeta;
    
    // Gradient
    //   X' * (p - y), p = exp(X * beta) / (1 + exp(X * beta))
    
    // exp(X * beta) => p
    xbeta.array() /= (xbeta.array() + 1.0);
    grad.noalias() = X.transpose() * (xbeta - Y);
    
    return f;
  }
};


gradPoisson <- function(parms) {
  eta <- as.vector(X %*% parms[1:kx] + offsetx)
  mu <- exp(eta)
  etaz <- as.vector(Z %*% parms[(kx + 1):(kx + kz)] + 
    offsetz)
  muz <- linkinv(etaz)
  clogdens0 <- -mu
  dens0 <- muz * (1 - as.numeric(Y1)) + exp(log(1 - muz) + 
    clogdens0)
  wres_count <- ifelse(Y1, Y - mu, -exp(-log(dens0) + 
    log(1 - muz) + clogdens0 + log(mu)))
  wres_zero <- ifelse(Y1, -1/(1 - muz) * linkobj$mu.eta(etaz), 
                      (linkobj$mu.eta(etaz) - exp(clogdens0) * linkobj$mu.eta(etaz))/dens0)
  colSums(cbind(wres_count * weights * X, wres_zero * 
    weights * Z))
}


// [[Rcpp::export]]
Rcpp::NumericVector logistic_reg(Rcpp::NumericMatrix x, Rcpp::NumericVector y)
{
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapVec yy = Rcpp::as<MapVec>(y);
  // Negative log likelihood
  LogisticReg nll(xx, yy);
  // Initial guess
  Eigen::VectorXd beta(xx.cols());
  beta.setZero();
  
  double fopt;
  int status = optim_lbfgs(nll, beta, fopt);
  if(status < 0)
    Rcpp::stop("fail to converge");
  
  return Rcpp::wrap(beta);
}

