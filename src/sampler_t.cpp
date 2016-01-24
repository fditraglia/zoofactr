#include <RcppArmadillo.h>
#include "distributions.h"
#include "matrix_tools.h"

using namespace Rcpp;

class SURt {
  public:
    SURt(const arma::mat&, const arma::mat&, const arma::mat&,
                 const arma::vec&, const arma::mat&, int, int, int, int);
    double logML();
    arma::mat g_draws, Omega_inv_draws, lambda_draws;
//  private: //uncomment later after everything has been thoroughly tested
    int r1, a1, T, D, K, p, n_vech, j, r0copy, nucopy;
    arma::vec G0_inv_g0, gbar_lambda, g, g0copy, lambda;
    arma::mat R0_inv, G0_inv;
    arma::mat Xcopy, Ycopy, G0copy, R0copy;
    arma::mat resid, R_Tlambda, G_Tlambda_inv, Omega_inv, R_Tlambda_draws;
};
//Class constructor
SURt::SURt(const arma::mat& X, const arma::mat& Y,
                           const arma::mat& G0, const arma::vec& g0,
                           const arma::mat& R0, int r0, int nu,
                           int n_draws = 1000,
                           int burn_in = 1000){
  T = Y.n_rows;
  D = Y.n_cols;
  K = X.n_cols;
  p = K * D;
  n_vech = (D + 1) * D / 2;

  Omega_inv_draws.zeros(n_vech, n_draws);
  g_draws.zeros(p, n_draws);
  RT_draws.zeros(n_vech, n_draws);

  r0copy = r0;
  G0copy = G0;
  g0copy = g0;
  r0copy = r0;
  R0copy = R0;
  Xcopy = X;
  Ycopy = Y;

  r1 = r0 + T;
  a1 = 0.5 * (nu + D); // Gamma shape parameter for lambda draws
  R0_inv = inv_sympd(R0);
  G0_inv = inv_sympd(G0);
  G0_inv_g0 = solve(symmatu(G0), g0);

  //Starting values
  Omega_inv = arma::eye(D, D);
  lambda = arma::ones(T);

  for(int i = 0; i < (n_draws + burn_in); i++){
    //draw gamma
    G_Tlambda_inv = G0_inv + kron(Omega_inv, X.t() * arma::diagmat(lambda) * X);
    gbar_lambda = solve(G_Tlambda_inv, G0_inv_g0 +
      vectorise(X.t() * arma::diagmat(lambda) * Y * Omega_inv));
    g = draw_normal(gbar_lambda, G_Tlambda_inv);
    //construct matrix of errors given gamma: each row is a time period
    resid = Y - X * reshape(g, K, D);
    //draw Omega_inv
    R_Tlambda = inv_sympd(R0_inv + resid.t() * arma::diagmat(lambda) * resid);
    Omega_inv = draw_wishart(r1, R_Tlambda);
    //draw lambda
    lambda = draw_gamma(a1, 0.5 * (nu + arma::sum(arma::pow(resid, 2), 2)));

    if(i >= burn_in){
      j = i - burn_in;
      R_Tlambda_draws.col(j) = vech(R_Tlambda); //Store this for ML calculation
      Omega_inv_draws.col(j) = vech(Omega_inv);
      g_draws.col(j) = g;
    }
  }
}
//Member function to calculate marginal likelihood
double SURt::logML(){
  //Posterior means for gamma and Omega inverse
  arma::vec gstar = mean(g_draws, 1);
  arma::mat Omega_inv_star = devech(mean(Omega_inv_draws, 1), D);

  //Contribution of prior - identical to model with normal likelihood
  double prior1 = as_scalar(density_normal(gstar, g0copy,
                                           G0copy, true));
  double prior2 = density_wishart(Omega_inv_star, r0copy,
                                  R0copy, true);

  //Residuals evaluated at posterior mean of gamma: each row is a time period
  arma::mat resid_star =  Ycopy - Xcopy * reshape(gstar, K, D);

  //Contribution of likelihood: transpose residuals since density_t expects
  //                            each column to be one observation (time period)
  double like = sum(density_t(resid_star.t(), nucopy, arma::zeros<arma::vec>(D),
                                   Omega_inv_star, true));

  //First Term of Posterior Contribution: Omega inverse
  arma::vec post1_terms(R_Tlambda_draws.n_cols);
  arma::mat R_Tlambda_g(D, D);
  for(int i = 0; i < R_Tlambda_draws.n_cols; i++){
    R_Tlambda_g = devech(R_Tlambda_draws.col(i), D);
    post1_terms(i) = density_wishart(Omega_inv_star, r1, R_Tlambda_g);
  }
  double post1 = log(mean(post1_terms));

  //Second term of Posterior Constribution: gamma (Reduced run!)
  arma::vec post2_terms;
  // Implement with same number of draws and burn-in as original samples
  //      In the reduced run hold Omega inverse fixed at prior mean
  //      Also, store only the gamma_bar and G_Tlambda since these are used
  //      in the ML calculation.
  double post2 = log(mean(post2_terms));


  return (prior1 + prior2) + like - (post1 + post2);
}


// [[Rcpp::export]]
List samplerTest(arma::mat X, arma::mat Y, arma::mat G0, arma::vec g0,
                 arma::mat R0, int r0, int nu,
                 int n_draws, int burn_in){
  SURt draws(X, Y, G0, g0, R0, r0, nu, n_draws, burn_in);
  List out = List::create(Named("g_draws") = draws.g_draws,
          Named("Omega_inv_draws") = draws.Omega_inv_draws,
          Named("logML") = draws.logML());
  return out;
}

// [[Rcpp::export]]
double logML_SUR(arma::mat X, arma::mat Y, arma::mat G0, arma::vec g0,
                 arma::mat R0, int r0, int nu,
                 int n_draws = 5000, int burn_in = 1000){
  SURt draws(X, Y, G0, g0, R0, r0, nu, n_draws, burn_in);
  return draws.logML();
}

// [[Rcpp::export]]
List defaultSUR(arma::mat X, arma::mat Y, double coef_scale = 10,
              double cov_scale = 10){
// If X ~ Wishart_d(v, S) then E[X] = v * S
// and E[X^{-1}] = S^{-1} / (v - d - 1)
  int d = Y.n_cols;
  int p = X.n_cols * d;
  arma::vec g0 = arma::zeros(p);
  arma::mat G0 = pow(coef_scale, 2) * arma::eye(p, p);
  int r0 = d + 2;
  int nu = 5;
  arma::mat R0 = arma::eye(d, d) / (pow(cov_scale, 2) * r0);
  SURt draws(X, Y, G0, g0, R0, r0, nu, 5000, 1000);
  List out = List::create(Named("g_draws") = draws.g_draws,
          Named("Omega_inv_draws") = draws.Omega_inv_draws);
  return out;
}
