#include <RcppArmadillo.h>
#include "distributions.h"
#include "matrix_tools.h"

using namespace Rcpp;

class SURnormal {
  public:
    SURnormal(const arma::mat&, const arma::mat&, const arma::mat&,
                 const arma::vec&, const arma::mat&, int, int, int);
    double logML();
    arma::mat g_draws, Omega_inv_draws;
//  private:
    int r1, T, D, K, p, n_vech, j;
    arma::vec G0_inv_g0, gbar, g;
    arma::mat XX, XY, R0_inv, G0_inv;
    arma::mat resid, RT, GT_inv, Omega_inv, RT_draws;
    //Private copies of prior and other input parameters with same names
    //as the corresponding function arguments using an initialization list
    const int r0, n_draws, burn_in;
    const arma::mat X, Y, G0, R0;
    const arma::vec g0;
};
//Class constructor
SURnormal::SURnormal(const arma::mat& X, const arma::mat& Y,
                     const arma::mat& G0, const arma::vec& g0,
                     const arma::mat& R0, int r0, int n_draws = 1000,
                     int burn_in = 1000): r0(r0), n_draws(n_draws),
                     burn_in(burn_in), X(X), Y(Y), G0(G0), R0(R0), g0(g0){
  T = Y.n_rows;
  D = Y.n_cols;
  K = X.n_cols;
  p = K * D;
  n_vech = (D + 1) * D / 2;

  Omega_inv_draws.zeros(n_vech, n_draws);
  g_draws.zeros(p, n_draws);
  RT_draws.zeros(n_vech, n_draws);

  XX = X.t() * X;
  XY = X.t() * Y;
  r1 = r0 + T;
  R0_inv = inv_sympd(R0);
  G0_inv = inv_sympd(G0);
  G0_inv_g0 = solve(symmatu(G0), g0);

  Omega_inv = arma::eye(D, D);

  for(int i = 0; i < (n_draws + burn_in); i++){

    GT_inv = G0_inv + kron(Omega_inv, XX);
    gbar = solve(GT_inv, G0_inv_g0 + vectorise(XY * Omega_inv));
    g = draw_normal(gbar, GT_inv);
    resid = Y - X * reshape(g, K, D);
    RT = inv_sympd(R0_inv + resid.t() * resid);
    Omega_inv = draw_wishart(r1, RT);

    if(i >= burn_in){
      j = i - burn_in;
      RT_draws.col(j) = vech(RT);
      Omega_inv_draws.col(j) = vech(Omega_inv);
      g_draws.col(j) = g;
    }
  }
}
//Member function to calculate marginal likelihood
double SURnormal::logML(){
  arma::vec gstar = mean(g_draws, 1);
  arma::mat Omega_inv_star = devech(mean(Omega_inv_draws, 1), D);

  double prior1 = as_scalar(density_normal(gstar, g0, G0_inv, true));
  double prior2 = density_wishart(Omega_inv_star, r0, R0, true);

  arma::mat resid_star =  Y - X * reshape(gstar, K, D);
  double like = sum(density_normal(resid_star.t(), arma::zeros<arma::vec>(D),
                                   Omega_inv_star, true));

  arma::mat GT_inv_star = G0_inv + kron(Omega_inv_star, XX);
  arma::vec gbar_star = solve(GT_inv_star, G0_inv_g0 + vectorise(XY *
                                Omega_inv_star));
  double post1 = as_scalar(density_normal(gstar, gbar_star,
                                          GT_inv_star, true));

  arma::vec post2_terms(RT_draws.n_cols);
  for(int i = 0; i < n_draws; i++){
    post2_terms(i) = density_wishart(Omega_inv_star, r1,
                devech(RT_draws.col(i), D));
  }
  double post2 = log(mean(post2_terms));

  return (prior1 + prior2) + like - (post1 + post2);
}


// [[Rcpp::export]]
List samplerTest_normal(arma::mat X, arma::mat Y, arma::mat G0, arma::vec g0,
                 arma::mat R0, int r0,
                 int n_draws, int burn_in){
  SURnormal draws(X, Y, G0, g0, R0, r0, n_draws, burn_in);
  List out = List::create(Named("g_draws") = draws.g_draws,
          Named("Omega_inv_draws") = draws.Omega_inv_draws,
          Named("logML") = draws.logML());
  return out;
}

// [[Rcpp::export]]
double logML_SUR_normal(arma::mat X, arma::mat Y, arma::mat G0, arma::vec g0,
                 arma::mat R0, int r0,
                 int n_draws = 5000, int burn_in = 1000){
  SURnormal draws(X, Y, G0, g0, R0, r0, n_draws, burn_in);
  return draws.logML();
}

// [[Rcpp::export]]
List defaultSUR_normal(arma::mat X, arma::mat Y, double coef_scale = 10,
              double cov_scale = 10){
// If X ~ Wishart_d(v, S) then E[X] = v * S
// and E[X^{-1}] = S^{-1} / (v - d - 1)
  int d = Y.n_cols;
  int p = X.n_cols * d;
  arma::vec g0 = arma::zeros(p);
  arma::mat G0 = pow(coef_scale, 2) * arma::eye(p, p);
  int r0 = d + 2;
  arma::mat R0 = arma::eye(d, d) / (pow(cov_scale, 2) * r0);
  SURnormal draws(X, Y, G0, g0, R0, r0, 5000, 1000);
  List out = List::create(Named("g_draws") = draws.g_draws,
          Named("Omega_inv_draws") = draws.Omega_inv_draws);
  return out;
}
