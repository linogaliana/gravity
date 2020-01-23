// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]

#include <Rcpp.h>
#include <math.h>
#include <RcppNumerical.h>

using namespace Numer;




//' @title logit and inverse logit functions
//' 
//' @description
//' transform \code{x} either via the logit, or inverse logit.
//'
//' @details
//' The loogit and inverse logit functions are part of R via the
//' logistic distribution functions in the stats package.  
//' Quoting from the documentation for the logistic distribution
//'
//' "\code{qlogis(p)} is the same as the \code{logit} function, \code{logit(p) =
//' log(p/1-p)}, and \code{plogis(x)} has consequently been called the 'inverse
//' logit'."
//'
//' See the examples for benchmarking these functions.  The \code{logit} and
//' \code{invlogit} functions are faster than the \code{qlogis} and \code{plogis}
//' functions.
//'
//' @seealso \code{\link[stats]{qlogis}}
//'
//' @examples
//' library(qwraps2)
//' library(rbenchmark)
//' 
//' # compare logit to qlogis
//' p <- runif(1e5)
//' identical(logit(p), qlogis(p)) 
//' benchmark(logit(p), qlogis(p))
//' 
//' # compare invlogit to plogis
//' x <- runif(1e5, -1000, 1000)
//' identical(invlogit(x), plogis(x))
//' benchmark(invlogit(x), plogis(x))
//'
//' @param x a numeric vector
//' @export
//' @rdname logit
// [[Rcpp::export]]
Rcpp::NumericVector logit(Rcpp::NumericVector x) {
  int n = x.size();
  Rcpp::NumericVector result(n);
  
  for(int i = 0; i < n; ++i) { 
    result[i] = log( x[i] / (1.0 - x[i]) );
  } 
  return result;
}

//' @export
//' @rdname logit
// [[Rcpp::export]]
Rcpp::NumericVector invlogit(Rcpp::NumericVector x) { 
  int n = x.size();
  Rcpp::NumericVector result(n);
  
  for (int i=0; i < n; ++i) { 
    result[i] = 1.0 / (1.0 + exp (-1.0 * x[i]));
  }
  return result;
}

//' @export
//' @rdname logit
// [[Rcpp::export]]
Rcpp::NumericVector invprobit(Rcpp::NumericVector x,
                              double eps = std::numeric_limits<double>::epsilon()){ 
  
  int n = x.size();
  Rcpp::NumericVector result(n);
  
  for (int i=0; i < n; ++i) {
    double thresh = -R::qnorm(eps) ;
    result[i] = R::pnorm(
      pmin(pmax(x[i], -thresh),
           thresh)
      ) ;
  }
  return result;
}




typedef Eigen::Map<Eigen::MatrixXd> MapMat;
typedef Eigen::Map<Eigen::VectorXd> MapVec;





double loglik_ZIP(Rcpp::NumericVector params,
                  Rcpp::NumericMatrix x,
                  Rcpp::NumericMatrix z,
                  Rcpp::NumericVector y,
                  char link = "probit"){
  
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapVec yy = Rcpp::as<MapVec>(y);
  const MapMat zz = Rcpp::as<MapMat>(z);
  
  int kx = x.ncol();
  
  Rcpp::NumericVector offsetx(x.nrow());
  Rcpp::NumericVector offsetz(z.nrow());
  Rcpp::NumericVector weights(y.length(), 1.0); 
    
  Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
  Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.length())];
  
  Eigen::VectorXd xbeta = xx * beta;
  Rcpp::NumericVector muz = zz * gamma ; // + offsetz ;
  
  Rcpp::NumericVector mu = xbeta ; // + offsetx ;
  
  
  double phi;
  
  if (link == "logit"){
    phi = invlogit(muz + offsetz) ;
  } else{
    phi = invprobit(muz + offsetz) ;
  }
  
  double loglik0 = log(phi + exp(log(1 - phi) - mu));
  double loglik1 = log(1 - phi) + Rcpp::dpois(y, lambda = mu, log = true);
  
  double loglik;
  for (int i = 0; i < y.length(); i++){
    if (y[i]>0){
      loglik += sum(weights[i]*loglik1[i]);
    } else{
      loglik += sum(weights[i]*loglik0[i]);
    }
  }
  
  
  return loglik;  
    
}


// double gradPoisson_cpp(NumericVector params,
//                        Rcpp::NumericMatrix x,
//                        Rcpp::NumericMatrix z,
//                        Rcpp::NumericVector y,
//                        str link = "probit"){
//   
//   const MapMat xx = Rcpp::as<MapMat>(x);
//   const MapVec yy = Rcpp::as<MapVec>(y);
//   const MapMat zz = Rcpp::as<MapMat>(z);
//   
//   int kx = x.ncol();
//   
//   Rcpp::NumericMatrix offsetx(x.nrow());
//   Rcpp::NumericMatrix offsetz(z.nrow());
//   
//   Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
//   Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.len())];
//   
//   Rcpp::NumericVector eta = xx * beta + offsetx ;
//   Rcpp::NumericVector mu = exp(eta) ;
//   Rcpp::NumericVector etaz = zz * gamma + offsetz ;
//   
//   if (link == "probit"){
//     
//     
//   } else{
//     
//   }  
//   
//   
// }