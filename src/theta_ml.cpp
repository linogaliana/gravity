#include <Rcpp.h>
using namespace Rcpp;

//' Score for negative binomial maximum-likelihood estimates
//' 
//' @param n Number of observations
//' @param th \eqn{\theta} parameter
//' @param mu Predicted values
//' @param y Observed values
//' 
//' @return Score value
// [[Rcpp::export]]
double score(int n, double th,
             NumericVector mu,
             NumericVector y){
  
  return sum((Rcpp::digamma(th + 
             y) - Rcpp::digamma(NumericVector(y.length(),th)) + log(th) + 1 - log(th + mu) - (y + 
             th)/(mu + th))) ; 
  
}

//' Information iteration value
//' 
//' @inheritParams score
// [[Rcpp::export]]
double info(int n, double th,
            NumericVector mu,
            NumericVector y){
  
  return sum((-Rcpp::trigamma(th + 
             y) + Rcpp::trigamma(NumericVector(y.length(),th)) - 1/th + 2/(mu + th) - (y + th)/pow(mu + 
             th, 2)));
  
}

//' Maximum likelihood estimation for negative binomial models
//' 
//' Simplified C++ function for \link[MASS]{theta.ml}
//' 
//' @inheritParams MASS::theta.ml
//' @importFrom MASS theta.ml
//' 
//' @return A maximum-likelihood estimator for \eqn{\theta}
//' 
//' @seealso \link{fastglm.nb} ; \link[MASS]{theta.ml} ; \link[MASS]{glm.nb}
//' 
//' @references <Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer>
//' 
//' @export
//' 
// [[Rcpp::export]]
double speed_theta_ml(NumericVector y, NumericVector mu,
                      int limit = 10, double eps = 1e-6,
                      bool trace = true){
  
  
  // We do not allow weights for the moment
  int n = y.length();
  
  double t0 = n/sum(pow(y/mu - 1, 2));
  int it = 0 ;
  double i = 0;
  double del = 1;
  
  if (trace){
    // Rcout << "theta.ml: iter" << it << "'theta = " << std::setprecision(6) << t0 << "'\n";
    Rcout << "theta.ml: iter" << it << " 'theta = " << t0 << "'\n";
  }
  
  while( (it < limit - 1) & (std::abs(del) > eps) ) {
    t0 = std::abs(t0) ;
    del = score(n, t0, mu, y) ;
    double i = info(n, t0, mu, y);
    del /= i;
    t0 += del ;
    it += 1 ;
    if (trace){
      Rcout << "theta.ml: iter" << it << " 'theta = " << t0  << "'\n";
    }    
  }
  
  
  if (t0 < 0){
    t0 = 0 ;
    Rcout << "estimate truncated at zero";
    // t0.attr("warn") = "estimate truncated at zero";
  }
  
  if (it == limit){
    Rcout << "iteration limit reached";
    // t0.attr("warn") = "iteration limit reached";
  }
  
  // t0.attr("SE") = sqrt(1/i);
  
  return(t0);
}

