// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// score
double score(int n, double th, NumericVector mu, NumericVector y);
RcppExport SEXP _gravity_score(SEXP nSEXP, SEXP thSEXP, SEXP muSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type th(thSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(score(n, th, mu, y));
    return rcpp_result_gen;
END_RCPP
}
// info
double info(int n, double th, NumericVector mu, NumericVector y);
RcppExport SEXP _gravity_info(SEXP nSEXP, SEXP thSEXP, SEXP muSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< double >::type th(thSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(info(n, th, mu, y));
    return rcpp_result_gen;
END_RCPP
}
// speed_theta_ml
double speed_theta_ml(NumericVector y, NumericVector mu, int limit, double eps, bool trace);
RcppExport SEXP _gravity_speed_theta_ml(SEXP ySEXP, SEXP muSEXP, SEXP limitSEXP, SEXP epsSEXP, SEXP traceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mu(muSEXP);
    Rcpp::traits::input_parameter< int >::type limit(limitSEXP);
    Rcpp::traits::input_parameter< double >::type eps(epsSEXP);
    Rcpp::traits::input_parameter< bool >::type trace(traceSEXP);
    rcpp_result_gen = Rcpp::wrap(speed_theta_ml(y, mu, limit, eps, trace));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_gravity_score", (DL_FUNC) &_gravity_score, 4},
    {"_gravity_info", (DL_FUNC) &_gravity_info, 4},
    {"_gravity_speed_theta_ml", (DL_FUNC) &_gravity_speed_theta_ml, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_gravity(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
