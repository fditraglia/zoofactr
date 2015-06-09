#include <RcppArmadillo.h>
using namespace Rcpp;


// [[Rcpp::export]]
arma::colvec draw_normal(arma::colvec mu, arma::mat Sigma_inv){
/*-------------------------------------------------------
# RETURNS:
#  A draw from a MV Normal(mu, Sigma_inv) distribution
#--------------------------------------------------------
# ARGUMENTS:
#  mu           mean vector
#  Sigma_inv    precision matrix (inverse of cov matrix)
#--------------------------------------------------------
# NOTE: parameterized using precision matrix!
#-------------------------------------------------------*/
  RNGScope scope;
  int p = Sigma_inv.n_cols;
  arma::colvec x = rnorm(p);
  arma::mat R = chol(Sigma_inv);
  return mu + solve(trimatu(R), x);
}

// [[Rcpp::export]]
arma::vec density_normal(arma::mat x, arma::colvec mu, arma::mat Sigma_inv,
                         bool logret = false){
/*-------------------------------------------------------
# RETURNS:
#  MV Normal(mu, Sigma_inv) probability density function
#--------------------------------------------------------
# ARGUMENTS:
#  x            matrix of points at which density is
#                 is to be evaluated: each column is a
#                 point, each row is a coordinate
#  mu           mean vector
#  Sigma_inv    precision matrix (inverse of cov matrix)
#  logret       if true, return log of density
#--------------------------------------------------------
# NOTE: Parameterized using precision matrix
#-------------------------------------------------------*/
 int p = Sigma_inv.n_cols;
 arma::mat R = chol(Sigma_inv);
 double first = -0.5 * p * log(2.0 * arma::datum::pi);
 double second = sum(log(diagvec(R)));
 x.each_col() -= mu;
 arma::vec third = -0.5 * sum(pow(R * x, 2)).t();
 arma::vec logdensity = first + second + third;
 if(logret)
   return logdensity;
 else
   return exp(logdensity);
}

// [[Rcpp::export]]
arma::mat draw_wishart(int v, arma::mat S){
/*-------------------------------------------------------
# RETURNS:
#  A draw from the Wishart(v, S) distribution.
#--------------------------------------------------------
# ARGUMENTS:
#  v     degrees of freedom
#  S     scale matrix
#--------------------------------------------------------
# NOTES:
#  (1) Employs Bartlett Decomp. (Smith & Hocking, 1972)
#  (2) Output is identical to rwish from MCMCpack R
#      package provided the same seed is used.
#-------------------------------------------------------*/
  RNGScope scope;
  int p = S.n_rows;
  arma::mat L = chol(S, "lower");
  arma::mat A(p,p, arma::fill::zeros);
  for(int i = 0; i < p; i++){
    int df = v - (i + 1) + 1; //zero-indexing
    A(i,i) = sqrt(R::rchisq(df));
  }
  for(int row = 1; row < p; row++){
    for(int col = 0; col < row; col++){
      A(row, col) = R::rnorm(0,1);
    }
  }
  arma::mat LA = trimatl(trimatl(L) * trimatl(A));
  return LA * LA.t();
}


// [[Rcpp::export]]
double log_mv_gamma(int p, double a){
/*-------------------------------------------------------
# RETURNS:
#  Natural logarithm of p-dimensional MV Gamma function
#--------------------------------------------------------
# ARGUMENTS:
#  p     dimension of MV Gamma function
#  a     argument of MV Gamma function
#--------------------------------------------------------
# NOTES: The multivariate Gamma function appears in the
#        normalizing constant for the Wishart distribution
#-------------------------------------------------------*/
  double lgamma_sum = 0.0;
  for(int j = 1; j <= p; j++){
    lgamma_sum += R::lgammafn(a + 0.5 * (1 - j));
  }
  return 0.25 * p * (p - 1) * log(arma::datum::pi) + lgamma_sum;
}


// [[Rcpp::export]]
double density_wishart(arma::mat X, int v, arma::mat S,
                       bool logret = false){
/*-------------------------------------------------------
# RETURNS:
#  Wishart(v, S) density evaluated at X
#--------------------------------------------------------
# ARGUMENTS:
#  v        degrees of freedom
#  S        scale matrix
#  logret   if true, return log of density
#-------------------------------------------------------*/
  int p = S.n_rows;
  double X_val, X_sign;
  log_det(X_val, X_sign, X);
  double term1 = 0.5 * (v - p - 1) * X_val;
  double term2 = -0.5 * trace(solve(trimatl(S), trimatl(X)));
  double term3 = -0.5 * v * p * log(2.0);
  double S_val, S_sign;
  log_det(S_val, S_sign, S);
  double term4 = -0.5 * v * S_val;
  double term5 = -1.0 * log_mv_gamma(p, 0.5 * v);
  double logdensity = term1 + term2 + term3 + term4 + term5;
  if(logret)
    return logdensity;
  else
    return exp(logdensity);
}
