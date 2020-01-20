// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]

#include <RcppNumerical.h>

using namespace Numer;

typedef Eigen::Map<Eigen::MatrixXd> MapMat;
typedef Eigen::Map<Eigen::VectorXd> MapVec;


double gradPoisson_cpp(NumericVector params,
                       Rcpp::NumericMatrix x,
                       Rcpp::NumericMatrix z,
                       Rcpp::NumericVector y,
                       str link = "probit"){
  
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapVec yy = Rcpp::as<MapVec>(y);
  const MapMat zz = Rcpp::as<MapMat>(z);
  
  int kx = x.ncol();
  
  Rcpp::NumericMatrix offsetx(x.nrow());
  Rcpp::NumericMatrix offsetz(z.nrow());
  
  Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
  Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.len())];
  
  Rcpp::NumericVector eta = xx * beta + offsetx ;
  Rcpp::NumericVector mu = exp(eta) ;
  Rcpp::NumericVector etaz = zz * gamma + offsetz ;

  if (link == "probit"){
    
  } else{
    
  }  
  
    
  double eta = 
  
}