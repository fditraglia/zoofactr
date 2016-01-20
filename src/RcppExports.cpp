// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// draw_normal
arma::colvec draw_normal(arma::colvec mu, arma::mat Sigma_inv);
RcppExport SEXP zoofactr_draw_normal(SEXP muSEXP, SEXP Sigma_invSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::colvec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Sigma_inv(Sigma_invSEXP);
    __result = Rcpp::wrap(draw_normal(mu, Sigma_inv));
    return __result;
END_RCPP
}
// density_normal
arma::vec density_normal(arma::mat x, arma::colvec mu, arma::mat Sigma_inv, bool logret);
RcppExport SEXP zoofactr_density_normal(SEXP xSEXP, SEXP muSEXP, SEXP Sigma_invSEXP, SEXP logretSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Sigma_inv(Sigma_invSEXP);
    Rcpp::traits::input_parameter< bool >::type logret(logretSEXP);
    __result = Rcpp::wrap(density_normal(x, mu, Sigma_inv, logret));
    return __result;
END_RCPP
}
// draw_wishart
arma::mat draw_wishart(int v, arma::mat S);
RcppExport SEXP zoofactr_draw_wishart(SEXP vSEXP, SEXP SSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type v(vSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type S(SSEXP);
    __result = Rcpp::wrap(draw_wishart(v, S));
    return __result;
END_RCPP
}
// log_mv_gamma
double log_mv_gamma(int p, double a);
RcppExport SEXP zoofactr_log_mv_gamma(SEXP pSEXP, SEXP aSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< int >::type p(pSEXP);
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    __result = Rcpp::wrap(log_mv_gamma(p, a));
    return __result;
END_RCPP
}
// density_wishart
double density_wishart(arma::mat X, int v, arma::mat S, bool logret);
RcppExport SEXP zoofactr_density_wishart(SEXP XSEXP, SEXP vSEXP, SEXP SSEXP, SEXP logretSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type v(vSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type S(SSEXP);
    Rcpp::traits::input_parameter< bool >::type logret(logretSEXP);
    __result = Rcpp::wrap(density_wishart(X, v, S, logret));
    return __result;
END_RCPP
}
// log1p_arma
arma::vec log1p_arma(arma::vec x);
RcppExport SEXP zoofactr_log1p_arma(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::vec >::type x(xSEXP);
    __result = Rcpp::wrap(log1p_arma(x));
    return __result;
END_RCPP
}
// density_t
arma::vec density_t(arma::mat x, int nu, arma::colvec mu, arma::mat Sigma_inv, bool logret);
RcppExport SEXP zoofactr_density_t(SEXP xSEXP, SEXP nuSEXP, SEXP muSEXP, SEXP Sigma_invSEXP, SEXP logretSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nu(nuSEXP);
    Rcpp::traits::input_parameter< arma::colvec >::type mu(muSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Sigma_inv(Sigma_invSEXP);
    Rcpp::traits::input_parameter< bool >::type logret(logretSEXP);
    __result = Rcpp::wrap(density_t(x, nu, mu, Sigma_inv, logret));
    return __result;
END_RCPP
}
// draw_gamma
arma::vec draw_gamma(double a, arma::vec rate);
RcppExport SEXP zoofactr_draw_gamma(SEXP aSEXP, SEXP rateSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type rate(rateSEXP);
    __result = Rcpp::wrap(draw_gamma(a, rate));
    return __result;
END_RCPP
}
// vech
arma::colvec vech(arma::mat M);
RcppExport SEXP zoofactr_vech(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type M(MSEXP);
    __result = Rcpp::wrap(vech(M));
    return __result;
END_RCPP
}
// devech
arma::mat devech(arma::colvec v, int dim);
RcppExport SEXP zoofactr_devech(SEXP vSEXP, SEXP dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::colvec >::type v(vSEXP);
    Rcpp::traits::input_parameter< int >::type dim(dimSEXP);
    __result = Rcpp::wrap(devech(v, dim));
    return __result;
END_RCPP
}
// samplerTest
List samplerTest(arma::mat X, arma::mat Y, arma::mat G0, arma::vec g0, arma::mat R0, int r0, int n_draws, int burn_in);
RcppExport SEXP zoofactr_samplerTest(SEXP XSEXP, SEXP YSEXP, SEXP G0SEXP, SEXP g0SEXP, SEXP R0SEXP, SEXP r0SEXP, SEXP n_drawsSEXP, SEXP burn_inSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G0(G0SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type g0(g0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type R0(R0SEXP);
    Rcpp::traits::input_parameter< int >::type r0(r0SEXP);
    Rcpp::traits::input_parameter< int >::type n_draws(n_drawsSEXP);
    Rcpp::traits::input_parameter< int >::type burn_in(burn_inSEXP);
    __result = Rcpp::wrap(samplerTest(X, Y, G0, g0, R0, r0, n_draws, burn_in));
    return __result;
END_RCPP
}
// logML_SUR
double logML_SUR(arma::mat X, arma::mat Y, arma::mat G0, arma::vec g0, arma::mat R0, int r0, int n_draws, int burn_in);
RcppExport SEXP zoofactr_logML_SUR(SEXP XSEXP, SEXP YSEXP, SEXP G0SEXP, SEXP g0SEXP, SEXP R0SEXP, SEXP r0SEXP, SEXP n_drawsSEXP, SEXP burn_inSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type G0(G0SEXP);
    Rcpp::traits::input_parameter< arma::vec >::type g0(g0SEXP);
    Rcpp::traits::input_parameter< arma::mat >::type R0(R0SEXP);
    Rcpp::traits::input_parameter< int >::type r0(r0SEXP);
    Rcpp::traits::input_parameter< int >::type n_draws(n_drawsSEXP);
    Rcpp::traits::input_parameter< int >::type burn_in(burn_inSEXP);
    __result = Rcpp::wrap(logML_SUR(X, Y, G0, g0, R0, r0, n_draws, burn_in));
    return __result;
END_RCPP
}
// defaultSUR
List defaultSUR(arma::mat X, arma::mat Y, double coef_scale, double cov_scale);
RcppExport SEXP zoofactr_defaultSUR(SEXP XSEXP, SEXP YSEXP, SEXP coef_scaleSEXP, SEXP cov_scaleSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type Y(YSEXP);
    Rcpp::traits::input_parameter< double >::type coef_scale(coef_scaleSEXP);
    Rcpp::traits::input_parameter< double >::type cov_scale(cov_scaleSEXP);
    __result = Rcpp::wrap(defaultSUR(X, Y, coef_scale, cov_scale));
    return __result;
END_RCPP
}
