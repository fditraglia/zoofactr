#include <RcppArmadillo.h>
#include "distributions.h"
#include "matrix_tools.h"

using namespace Rcpp;

class SURidentical {
  public:
    SURidentical(const arma::mat&, const arma::mat&, const arma::mat&,
                 const arma::vec&, const arma::mat&, int, int, int);
    double logML();
    arma::mat g_draws, Omega_inv_draws;
//  private:
    int r1, T, D, K, p, n_vech, j, r0copy;
    arma::vec G0_inv_g0, gbar, g, g0copy;
    arma::mat XX, XY, R0_inv, G0_inv;
    arma::mat Xcopy, Ycopy, G0copy, R0copy;
    arma::mat resid, RT, GT_inv, Omega_inv, RT_draws;
};
//Class constructor
SURidentical::SURidentical(const arma::mat& X, const arma::mat& Y,
                           const arma::mat& G0, const arma::vec& g0,
                           const arma::mat& R0, int r0,
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
double SURidentical::logML(){
  arma::vec gstar = mean(g_draws, 1);
  arma::mat Omega_inv_star = devech(mean(Omega_inv_draws, 1), D);

  double prior1 = as_scalar(density_normal(gstar, g0copy,
                                           inv_sympd(G0copy), true));
  double prior2 = density_wishart(Omega_inv_star, r0copy,
                                  R0copy, true);

  arma::mat resid_star =  Ycopy - Xcopy * reshape(gstar, K, D);
  double like = sum(density_normal(resid_star.t(), arma::zeros<arma::vec>(D),
                                   Omega_inv_star, true));

  arma::mat GT_inv_star = G0_inv + kron(Omega_inv_star, XX);
  arma::vec gbar_star = solve(GT_inv_star, G0_inv_g0 + vectorise(XY *
                                Omega_inv_star));
  double post1 = as_scalar(density_normal(gstar, gbar_star,
                                          GT_inv_star, true));

  arma::vec post2_terms(RT.n_cols);
  arma::mat RT_g(D, D);
  for(int i = 0; i < RT.n_cols; i++){
    RT_g = devech(RT_draws.col(i), D);
    post2_terms(i) = density_wishart(Omega_inv_star, r1, RT_g);
  }
  double post2 = log(mean(post2_terms));

  return (prior1 + prior2) + like - (post1 + post2);
}


// [[Rcpp::export]]
List samplerTest(arma::mat X, arma::mat Y, arma::mat G0, arma::vec g0,
                 arma::mat R0, int r0,
                 int n_draws, int burn_in){
  SURidentical draws(X, Y, G0, g0, R0, r0, n_draws, burn_in);
  List out = List::create(Named("g_draws") = draws.g_draws,
          Named("Omega_inv_draws") = draws.Omega_inv_draws,
          Named("logML") = draws.logML());
  return out;
}

// [[Rcpp::export]]
double logML_SUR(arma::mat X, arma::mat Y, arma::mat G0, arma::vec g0,
                 arma::mat R0, int r0,
                 int n_draws = 5000, int burn_in = 1000){
  SURidentical draws(X, Y, G0, g0, R0, r0, n_draws, burn_in);
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
  arma::mat R0 = arma::eye(d, d) / (pow(cov_scale, 2) * r0);
  SURidentical draws(X, Y, G0, g0, R0, r0, 5000, 1000);
  List out = List::create(Named("g_draws") = draws.g_draws,
          Named("Omega_inv_draws") = draws.Omega_inv_draws);
  return out;
}

/*** R
# setwd("~/factor-choice/")
# dat <- read.csv("data_value.csv")
# nT <- nrow(dat)
# x1 <- rep(1, nT)
# x2 <- dat$Mkt.RF
# x3 <- dat$SMB
# x4 <- dat$HML
# M <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
# set.seed(1234)
# errors <- t(chol(M) %*% rbind(rnorm(nT), rnorm(nT)))
# y1 <- x2 + errors[,1]
# y2 <- 0.5 + x2 + 0.2 * x3 + 0.2 * x4 + errors[,2]
# Y <- cbind(y1, y2)
# X <- cbind(x1, x2, x3, x4)
# rm(x1, x2, x3, x4, y1, y2, nT, dat)
# r0 <- 10
# g0 <- rep(0, ncol(X) * ncol(Y))
# G0 <- diag(ncol(X) * ncol(Y))
# R0 <- diag(ncol(Y))
# gibbs <- samplerTest(X, Y, G0, g0, R0, r0, 4000, 1000)
# solve(devech(rowMeans(gibbs$Omega_inv_draws), 2))
# matrix(rowMeans(gibbs$g_draws), ncol(X), ncol(Y))
*/
