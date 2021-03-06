// [[Rcpp::depends(Rcpp)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]

#include <Rcpp.h>
#include <math.h>
#include <RcppNumerical.h>

using namespace Numer;
using namespace Rcpp;




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

// [[Rcpp::export]]
Rcpp::NumericVector probit(Rcpp::NumericVector x) {
  Rcpp::NumericVector result = Rcpp::qnorm(x);
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
Rcpp::NumericVector invprobit(Rcpp::NumericVector x) {
  
  double eps = std::numeric_limits<double>::epsilon();
  double thresh = -R::qnorm(eps, 0.0, 1.0, 1, 0) ;
  
  Rcpp::NumericVector result = Rcpp::pnorm(
    Rcpp::clamp(-thresh, x, thresh)
  );
  //NB: clamp faster than pmin(pmax(x[i], -thresh), thresh)
  return result;
}



typedef Eigen::Map<Eigen::MatrixXd> MapMat;
typedef Eigen::Map<Eigen::VectorXd> MapVec;

//' @export
// [[Rcpp::export]]
double loglik_ZIP(Rcpp::NumericVector params,
                  Rcpp::NumericMatrix x,
                  Rcpp::NumericMatrix z,
                  Rcpp::NumericVector y,
                  Rcpp::NumericVector weights,
                  Rcpp::NumericVector offsetx,
                  Rcpp::NumericVector offsetz,
                  Rcpp::String link = "probit"){
  
  int n = x.nrow();
  
  // Rcpp::NumericVector offsetx(n);
  // Rcpp::NumericVector offsetz(n);
  // Rcpp::NumericVector weights(n, 1.0);
  
  Rcpp::IntegerVector yy = Rcpp::as<IntegerVector>(y);
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapMat zz = Rcpp::as<MapMat>(z);
  const MapVec offx = Rcpp::as<MapVec>(offsetx);
  const MapVec offz = Rcpp::as<MapVec>(offsetz);
  //const MapVec w = Rcpp::as<MapVec>(weights);
  
  int kx = x.ncol();
  
  Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
  Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.length())];
  
  const MapVec beta2 = Rcpp::as<MapVec>(beta);
  const MapVec gamma2 = Rcpp::as<MapVec>(gamma);
  
  const Eigen::VectorXd mu = xx*beta2 + offx ;
  const Eigen::VectorXd muz = zz*gamma2 + offz ;
  
  Rcpp::NumericVector phi;
  
  if (link == "logit"){
    phi = invlogit(wrap(muz)) ;
  } else{
    phi = invprobit(wrap(muz)) ;
  }
  
  Rcpp::NumericVector mu2 = exp(wrap(mu)) ;
  
  // Rcpp::NumericVector loglik0 = log(phi + exp(log(1 - phi) - mu2));
  Rcpp::NumericVector loglik0 = log(phi + exp(log(1 - phi) - mu2));
  Rcpp::NumericVector loglik1(n);
  
  double loglik;
  for (int i = 0; i < n; i++){
    if (y[i]>0){
      loglik1[i] += log(1 - phi[i]) + R::dpois(yy[i], mu2[i], true);
      loglik += weights[i]*loglik1[i];
    } else{
      loglik += weights[i]*loglik0[i];
    }
  }
  
  
  return loglik ;
}


//' @export
// [[Rcpp::export]]
double loglik_ZINB(Rcpp::NumericVector params,
                   Rcpp::NumericMatrix x,
                   Rcpp::NumericMatrix z,
                   Rcpp::NumericVector y,
                   Rcpp::NumericVector weights,
                   Rcpp::NumericVector offsetx,
                   Rcpp::NumericVector offsetz,
                   Rcpp::String link = "probit"){
  
  int n = x.nrow();
  
  // Rcpp::NumericVector offsetx(n);
  // Rcpp::NumericVector offsetz(n);
  // Rcpp::NumericVector weights(n, 1.0);
  
  Rcpp::IntegerVector yy = Rcpp::as<IntegerVector>(y);
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapMat zz = Rcpp::as<MapMat>(z);
  const MapVec offx = Rcpp::as<MapVec>(offsetx);
  const MapVec offz = Rcpp::as<MapVec>(offsetz);
  //const MapVec w = Rcpp::as<MapVec>(weights);
  
  int kx = x.ncol();
  
  Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
  Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.length()-1)];
  double theta = exp(params[params.length()-1]);
  
  const MapVec beta2 = Rcpp::as<MapVec>(beta);
  const MapVec gamma2 = Rcpp::as<MapVec>(gamma);
  
  const Eigen::VectorXd mu = xx*beta2 + offx ;
  const Eigen::VectorXd muz = zz*gamma2 + offz ;
  
  Rcpp::NumericVector phi;
  
  if (link == "logit"){
    phi = invlogit(wrap(muz)) ;
  } else{
    phi = invprobit(wrap(muz)) ;
  }
  
  Rcpp::NumericVector mu2 = exp(wrap(mu)) ;
  
  // Rcpp::NumericVector loglik0 = log(phi + exp(log(1 - phi) - mu2));
  Rcpp::NumericVector loglik0(n);
  Rcpp::NumericVector loglik1(n);
  
  
  double loglik;
  for (int i = 0; i < n; i++){
    if (y[i]>0){
      loglik1[i] += log(1 - phi[i]) + R::dnbinom_mu(yy[i], theta, mu2[i], true);
      loglik += weights[i]*loglik1[i];
    } else{
      double mu3 = R::dnbinom_mu(0, theta,
                                 mu2[i], true) ;
      loglik0[i] += log(phi[i] + exp(log(1 - phi[i]) + mu3)) ;
      loglik += weights[i]*loglik0[i];
    }
  }
  
  
  return loglik ;
}

// [[Rcpp::export]]
Rcpp::NumericVector dmudeta_probit(Rcpp::NumericVector eta){
  double eps = std::numeric_limits<double>::epsilon();
  return Rcpp::pmax(
    Rcpp::dnorm(eta), eps
  ) ;
}


// [[Rcpp::export]]
Rcpp::NumericVector dmudeta_logit(Rcpp::NumericVector eta){
  
  Rcpp::NumericVector opexp = 1 + exp(eta);
  
  return (opexp-1)/pow(opexp, 2);
}

// [[Rcpp::export]]
Rcpp::NumericVector grad_ZIP(Rcpp::NumericVector params,
                             Rcpp::NumericMatrix x,
                             Rcpp::NumericMatrix z,
                             Rcpp::NumericVector y,
                             Rcpp::NumericVector weights,
                             Rcpp::NumericVector offsetx,
                             Rcpp::NumericVector offsetz,
                             Rcpp::String link = "probit"){
  
  int n = x.nrow();
  
  // Rcpp::NumericVector offsetx(n);
  // Rcpp::NumericVector offsetz(n);
  // Rcpp::NumericVector weights(n, 1.0);
  
  Rcpp::IntegerVector yy = Rcpp::as<IntegerVector>(y);
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapMat zz = Rcpp::as<MapMat>(z);
  const MapVec offx = Rcpp::as<MapVec>(offsetx);
  const MapVec offz = Rcpp::as<MapVec>(offsetz);
  //const MapVec w = Rcpp::as<MapVec>(weights);
  
  int kx = x.ncol();
  int kz = z.ncol();
  
  Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
  Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.length())];
  
  const MapVec beta2 = Rcpp::as<MapVec>(beta);
  const MapVec gamma2 = Rcpp::as<MapVec>(gamma);
  
  const Eigen::VectorXd eta_eig = xx*beta2 + offx ;
  const Eigen::VectorXd etaz_eig = zz*gamma2 + offz ;
  
  Rcpp::NumericVector muz;
  Rcpp::NumericVector dmudeta;
  
  // Get back in Rcpp classes
  Rcpp::NumericVector etaz = wrap(etaz_eig);
  Rcpp::NumericVector mu = exp(wrap(eta_eig)) ;
  
  if (link == "logit"){
    muz = invlogit(etaz) ;
    dmudeta = dmudeta_logit(etaz) ;
  } else{
    muz = invprobit(etaz) ;
    dmudeta = dmudeta_probit(etaz) ;
  }
  
  
  
  Rcpp::NumericVector clogdens0 = - mu ;
  
  Rcpp::NumericVector wres_count(n) ;
  Rcpp::NumericVector wres_zero(n) ;
  
  Rcpp::NumericVector dens0 = exp(log(1.0-muz) + clogdens0) ;
  
  Rcpp::NumericMatrix term1(n, kx) ;
  Rcpp::NumericMatrix term2(n, kz) ;
  
  
  for (int i = 0; i < n; i++){
    
    if (y[i]> 0.0){
      wres_count[i] += y[i];
      wres_count[i] -= mu[i];
      wres_zero[i] -= dmudeta[i]/(1 - muz[i]);
    } else{
      dens0[i] += muz[i];
      wres_count[i] -= exp(-log(dens0[i]) +
        log(1 - muz[i]) + clogdens0[i] + log(mu[i])
      );
      wres_zero[i] = dmudeta[i] - exp(clogdens0[i])*dmudeta[i];
      wres_zero[i] /= dens0[i];
    }
    
    term1(i,_) = wres_count[i]*weights[i]*x(i,_);
    term2(i,_) = wres_zero[i]*weights[i]*z(i,_) ;
  }
  
  
  // cbind
  Rcpp::NumericMatrix out = no_init_matrix(n, kx + kz);
  for (int j = 0; j < kx + kz; j++) {
    if (j < kx) {
      out(_, j) = term1(_, j);
    } else {
      out(_, j) = term2(_, j - kx);
    }
  }
  
  
  return Rcpp::colSums(out) ;
}


// [[Rcpp::export]]
Rcpp::NumericVector grad_ZIP_probit(Rcpp::NumericVector params,
                                    Rcpp::NumericMatrix x,
                                    Rcpp::NumericMatrix z,
                                    Rcpp::NumericVector y,
                                    Rcpp::NumericVector weights,
                                    Rcpp::NumericVector offsetx,
                                    Rcpp::NumericVector offsetz){
  
  int n = x.nrow();
  
  // Rcpp::NumericVector offsetx(n);
  // Rcpp::NumericVector offsetz(n);
  // Rcpp::NumericVector weights(n, 1.0);
  
  Rcpp::IntegerVector yy = Rcpp::as<IntegerVector>(y);
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapMat zz = Rcpp::as<MapMat>(z);
  const MapVec offx = Rcpp::as<MapVec>(offsetx);
  const MapVec offz = Rcpp::as<MapVec>(offsetz);
  //const MapVec w = Rcpp::as<MapVec>(weights);
  
  int kx = x.ncol();
  int kz = z.ncol();
  
  Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
  Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.length())];
  
  const MapVec beta2 = Rcpp::as<MapVec>(beta);
  const MapVec gamma2 = Rcpp::as<MapVec>(gamma);
  
  const Eigen::VectorXd eta_eig = xx*beta2 + offx ;
  const Eigen::VectorXd etaz_eig = zz*gamma2 + offz ;
  
  Rcpp::NumericVector muz;
  Rcpp::NumericVector dmudeta;
  
  // Get back in Rcpp classes
  Rcpp::NumericVector etaz = wrap(etaz_eig);
  Rcpp::NumericVector mu = exp(wrap(eta_eig)) ;
  
  
  muz = invprobit(etaz) ;
  dmudeta = dmudeta_probit(etaz) ;
  
  
  Rcpp::NumericVector clogdens0 = - mu ;
  
  Rcpp::NumericVector wres_count(n) ;
  Rcpp::NumericVector wres_zero(n) ;
  
  Rcpp::NumericVector dens0 = exp(log(1.0-muz) + clogdens0) ;
  
  Rcpp::NumericMatrix term1(n, kx) ;
  Rcpp::NumericMatrix term2(n, kz) ;
  
  
  for (int i = 0; i < n; i++){
    
    if (y[i]> 0.0){
      wres_count[i] += y[i];
      wres_count[i] -= mu[i];
      wres_zero[i] -= dmudeta[i]/(1 - muz[i]);
    } else{
      dens0[i] += muz[i];
      wres_count[i] -= exp(-log(dens0[i]) +
        log(1 - muz[i]) + clogdens0[i] + log(mu[i])
      );
      wres_zero[i] = dmudeta[i] - exp(clogdens0[i])*dmudeta[i];
      wres_zero[i] /= dens0[i];
    }
    
    term1(i,_) = wres_count[i]*weights[i]*x(i,_);
    term2(i,_) = wres_zero[i]*weights[i]*z(i,_) ;
  }
  
  
  // cbind
  Rcpp::NumericMatrix out = no_init_matrix(n, kx + kz);
  for (int j = 0; j < kx + kz; j++) {
    if (j < kx) {
      out(_, j) = term1(_, j);
    } else {
      out(_, j) = term2(_, j - kx);
    }
  }
  
  
  return Rcpp::colSums(out) ;
}


// [[Rcpp::export]]
Rcpp::NumericVector grad_ZIP_logit(Rcpp::NumericVector params,
                                   Rcpp::NumericMatrix x,
                                   Rcpp::NumericMatrix z,
                                   Rcpp::NumericVector y,
                                   Rcpp::NumericVector weights,
                                   Rcpp::NumericVector offsetx,
                                   Rcpp::NumericVector offsetz){
  
  int n = x.nrow();
  
  // Rcpp::NumericVector offsetx(n);
  // Rcpp::NumericVector offsetz(n);
  // Rcpp::NumericVector weights(n, 1.0);
  
  Rcpp::IntegerVector yy = Rcpp::as<IntegerVector>(y);
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapMat zz = Rcpp::as<MapMat>(z);
  const MapVec offx = Rcpp::as<MapVec>(offsetx);
  const MapVec offz = Rcpp::as<MapVec>(offsetz);
  //const MapVec w = Rcpp::as<MapVec>(weights);
  
  int kx = x.ncol();
  int kz = z.ncol();
  
  Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
  Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.length())];
  
  const MapVec beta2 = Rcpp::as<MapVec>(beta);
  const MapVec gamma2 = Rcpp::as<MapVec>(gamma);
  
  const Eigen::VectorXd eta_eig = xx*beta2 + offx ;
  const Eigen::VectorXd etaz_eig = zz*gamma2 + offz ;
  
  Rcpp::NumericVector muz;
  Rcpp::NumericVector dmudeta;
  
  // Get back in Rcpp classes
  Rcpp::NumericVector etaz = wrap(etaz_eig);
  Rcpp::NumericVector mu = exp(wrap(eta_eig)) ;
  
  
  muz = invlogit(etaz) ;
  dmudeta = dmudeta_logit(etaz) ;
  
  
  
  Rcpp::NumericVector clogdens0 = - mu ;
  
  Rcpp::NumericVector wres_count(n) ;
  Rcpp::NumericVector wres_zero(n) ;
  
  Rcpp::NumericVector dens0 = exp(log(1.0-muz) + clogdens0) ;
  
  Rcpp::NumericMatrix term1(n, kx) ;
  Rcpp::NumericMatrix term2(n, kz) ;
  
  
  for (int i = 0; i < n; i++){
    
    if (y[i]> 0.0){
      wres_count[i] += y[i];
      wres_count[i] -= mu[i];
      wres_zero[i] -= dmudeta[i]/(1 - muz[i]);
    } else{
      dens0[i] += muz[i];
      wres_count[i] -= exp(-log(dens0[i]) +
        log(1 - muz[i]) + clogdens0[i] + log(mu[i])
      );
      wres_zero[i] = dmudeta[i] - exp(clogdens0[i])*dmudeta[i];
      wres_zero[i] /= dens0[i];
    }
    
    term1(i,_) = wres_count[i]*weights[i]*x(i,_);
    term2(i,_) = wres_zero[i]*weights[i]*z(i,_) ;
  }
  
  
  // cbind
  Rcpp::NumericMatrix out = no_init_matrix(n, kx + kz);
  for (int j = 0; j < kx + kz; j++) {
    if (j < kx) {
      out(_, j) = term1(_, j);
    } else {
      out(_, j) = term2(_, j - kx);
    }
  }
  
  
  return Rcpp::colSums(out) ;
}


// [[Rcpp::export]]
Rcpp::NumericVector grad_ZINB(Rcpp::NumericVector params,
                              Rcpp::NumericMatrix x,
                              Rcpp::NumericMatrix z,
                              Rcpp::NumericVector y,
                              Rcpp::NumericVector weights,
                              Rcpp::NumericVector offsetx,
                              Rcpp::NumericVector offsetz,
                              Rcpp::String link = "probit"){
  
  int n = x.nrow();
  
  // Rcpp::NumericVector offsetx(n);
  // Rcpp::NumericVector offsetz(n);
  // Rcpp::NumericVector weights(n, 1.0);
  
  Rcpp::IntegerVector yy = Rcpp::as<IntegerVector>(y);
  const MapMat xx = Rcpp::as<MapMat>(x);
  const MapMat zz = Rcpp::as<MapMat>(z);
  const MapVec offx = Rcpp::as<MapVec>(offsetx);
  const MapVec offz = Rcpp::as<MapVec>(offsetz);
  //const MapVec w = Rcpp::as<MapVec>(weights);
  
  int kx = x.ncol();
  int kz = z.ncol();
  
  Rcpp::NumericVector beta = params[Rcpp::Range(0, kx-1)];
  Rcpp::NumericVector gamma = params[Rcpp::Range(kx, params.length())];
  double theta = exp(params[params.length()-1]);
  
  const MapVec beta2 = Rcpp::as<MapVec>(beta);
  const MapVec gamma2 = Rcpp::as<MapVec>(gamma);
  
  const Eigen::VectorXd eta_eig = xx*beta2 + offx ;
  const Eigen::VectorXd etaz_eig = zz*gamma2 + offz ;
  
  Rcpp::NumericVector muz;
  Rcpp::NumericVector dmudeta;
  
  // Get back in Rcpp classes
  Rcpp::NumericVector etaz = wrap(etaz_eig);
  Rcpp::NumericVector mu = exp(wrap(eta_eig)) ;
  
  if (link == "logit"){
    muz = invlogit(etaz) ;
    dmudeta = dmudeta_logit(etaz) ;
  } else{
    muz = invprobit(etaz) ;
    dmudeta = dmudeta_probit(etaz) ;
  }
  
  Rcpp::NumericVector clogdens0(n);
  Rcpp::NumericVector dens0 = 1-muz;
  Rcpp::NumericVector wres_count(n) ;
  Rcpp::NumericVector wres_zero(n) ;
  Rcpp::NumericVector wres_theta(n) ;
  Rcpp::NumericMatrix term1(n, kx) ;
  Rcpp::NumericMatrix term2(n, kz) ;
  
  
  for (int i = 0; i < n; i++){
    
    clogdens0[i] = R::dnbinom_mu(0, theta, mu[i], true);
    dens0[i] *= exp(clogdens0[i]);
    
    if (y[i]> 0.0){
      
      wres_count[i] += y[i]-mu[i]*(y[i]+theta)/(mu[i]+theta) ;
      wres_zero[i] -= dmudeta[i]/(1 - muz[i]) ; 
      wres_theta[i] += R::digamma(y[i] + theta) - R::digamma(theta) +
        log(theta) - log(mu[i] + theta) + 1.0 - (y[i] + theta)/(mu[i] + theta);
      
    } else{
      
      dens0[i] += muz[i];
      wres_count[i] -= exp(-log(dens0[i]) +
        log(1 - muz[i]) + clogdens0[i] + 
        log(theta) - log(mu[i] + theta) + log(mu[i])
      );
      wres_zero[i] += (dmudeta[i] - exp(clogdens0[i])*dmudeta[i])/dens0[i] ; 
      
      wres_theta[i] += exp(-log(dens0[i]) + log(1-muz[i]) + clogdens0[i]);
      wres_theta[i] *= log(theta) - log(mu[i] + theta) + 1.0 - theta/(mu[i] + theta) ; 
      
    }
    
    wres_theta[i] *= theta;
    
    term1(i,_) = wres_count[i]*weights[i]*x(i,_);
    term2(i,_) = wres_zero[i]*weights[i]*z(i,_) ;    
    
  }
  
  // cbind
  Rcpp::NumericMatrix out = no_init_matrix(n, kx + kz + 1);
  
  for (int j = 0; j < kx + kz + 1; j++) {
    if (j < kx) {
      out(_, j) = term1(_, j);
    } else if (j < kx + kz){
      out(_, j) = term2(_, j - kx);
    } else{
      out(_, j) = wres_theta ;
    }
  }
  
  
  return Rcpp::colSums(out) ;
  
}


class ZIP_binomial: public MFuncGrad
{
private:
  const MapMat X;
  const MapMat Z;
  const MapVec Y;
  const MapVec w;
  const MapVec ox;
  const MapVec oz;
public:
  ZIP_binomial(const MapMat x_,
               const MapMat z_, 
               const MapVec y_,
               const MapVec weights_,
               const MapVec offsetx_,
               const MapVec offsetz_) : X(x_), Z(z_), 
               Y(y_), w(weights_), ox(offsetx_), oz(offsetz_) {}
  
  double f_grad(Constvec& theta, Refvec grad){
    
    Rcpp::NumericVector gradient = grad_ZIP_probit(wrap(theta), wrap(X), wrap(Z), wrap(Y),
                                            wrap(w),
                                            wrap(ox),
                                            wrap(oz)) ;
    
    const double f = sum(gradient);
    grad.noalias() = Rcpp::as<MapVec>(gradient);
    
    return f ;
    
  }
};


class ZIP_logistic: public MFuncGrad
{
private:
  const MapMat X;
  const MapMat Z;
  const MapVec Y;
  const MapVec w;
  const MapVec ox;
  const MapVec oz;
public:
  ZIP_logistic(const MapMat x_,
               const MapMat z_, 
               const MapVec y_,
               const MapVec weights_,
               const MapVec offsetx_,
               const MapVec offsetz_) : X(x_), Z(z_), 
               Y(y_), w(weights_), ox(offsetx_), oz(offsetz_) {}
  
  double f_grad(Constvec& theta, Refvec grad){
    
    Rcpp::NumericVector gradient = grad_ZIP_logit(wrap(theta), wrap(X), wrap(Z), wrap(Y),
                                            wrap(w),
                                            wrap(ox),
                                            wrap(oz)) ;
    
    const double f = sum(gradient);
    grad.noalias() = Rcpp::as<MapVec>(gradient);
    
    return f ;
    
  }
};

// [[Rcpp::export]]
Rcpp::List fastZIP_binomial_(Rcpp::NumericMatrix x,
                             Rcpp::NumericMatrix z,
                             Rcpp::NumericVector y,
                             Rcpp::NumericVector weights,
                             Rcpp::NumericVector offsetx,
                             Rcpp::NumericVector offsetz,
                             Rcpp::NumericVector start,
                             double eps_f, double eps_g, int maxit){
  
  const MapMat xx   = Rcpp::as<MapMat>(x) ;
  const MapMat zz   = Rcpp::as<MapMat>(z) ;
  const MapVec yy   = Rcpp::as<MapVec>(y) ;
  const MapVec ww   = Rcpp::as<MapVec>(weights) ;
  const MapVec oox  = Rcpp::as<MapVec>(offsetx) ;
  const MapVec ooz  = Rcpp::as<MapVec>(offsetz) ;
  
  ZIP_binomial zeroinfl(xx, zz, yy,
                        ww, oox,
                        ooz);
  
  // Initial guess
  Rcpp::NumericVector b = Rcpp::clone(start);
  MapVec beta(b.begin(), b.length());
  
  double fopt;
  int status = optim_lbfgs(zeroinfl, beta, fopt, maxit, eps_f, eps_g);
  
  if(status < 0)
    Rcpp::warning("algorithm did not converge");
  
  return Rcpp::List::create(
    Rcpp::Named("coefficients") = b,
    Rcpp::Named("loglikelihood") = -fopt,
    Rcpp::Named("converged")  = (status >= 0)    
  ) ;
}


// [[Rcpp::export]]
Rcpp::List fastZIP_logistic_(Rcpp::NumericMatrix x,
                             Rcpp::NumericMatrix z,
                             Rcpp::NumericVector y,
                             Rcpp::NumericVector weights,
                             Rcpp::NumericVector offsetx,
                             Rcpp::NumericVector offsetz,
                             Rcpp::NumericVector start,
                             double eps_f, double eps_g, int maxit){
  
  const MapMat xx   = Rcpp::as<MapMat>(x) ;
  const MapMat zz   = Rcpp::as<MapMat>(z) ;
  const MapVec yy   = Rcpp::as<MapVec>(y) ;
  const MapVec ww   = Rcpp::as<MapVec>(weights) ;
  const MapVec oox  = Rcpp::as<MapVec>(offsetx) ;
  const MapVec ooz  = Rcpp::as<MapVec>(offsetz) ;
  
  ZIP_logistic zeroinfl(xx, zz, yy,
                        ww, oox,
                        ooz);
  
  // Initial guess
  Rcpp::NumericVector b = Rcpp::clone(start);
  MapVec beta(b.begin(), b.length());
  
  double fopt;
  int status = optim_lbfgs(zeroinfl, beta, fopt, maxit, eps_f, eps_g);
  
  if(status < 0)
    Rcpp::warning("algorithm did not converge");
  
  return Rcpp::List::create(
    Rcpp::Named("coefficients") = b,
    Rcpp::Named("loglikelihood") = -fopt,
    Rcpp::Named("converged")  = (status >= 0)    
  ) ;
}