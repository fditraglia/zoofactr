#include <RcppArmadillo.h>
using namespace Rcpp;


//' Simulate one draw from a multivariate normal distribution
//'
//' @param mu A numeric vector, the mean of the distribution.
//' @param Sigma_inv A numeric matrix, the precision matrix (inverse of the
//' of the variance-covariance matrix) of the distribution
//' @return A column vector containing the draw.
//' @details Uses the Cholesky decomposition of Sigma_inv.
//' @examples
//' M <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
//' draw_normal(c(0, 0), solve(M))
// [[Rcpp::export]]
arma::colvec draw_normal(arma::colvec mu, arma::mat Sigma_inv){
  RNGScope scope;
  int p = Sigma_inv.n_cols;
  arma::colvec x = rnorm(p);
  arma::mat R = chol(Sigma_inv);
  return mu + solve(trimatu(R), x);
}

//' Multivariate normal probability density function
//'
//' @param x A numeric matrix, each column of which is a point at which the
//' density is to be evaluated.
//' @param mu A numeric vector, the mean of the distribution.
//' @param Sigma_inv A numeric matrix, the precision matrix (inverse of the
//' of the variance-covariance matrix) of the distribution.
//' @param logret, a logical value indicating whether to return the log density.
//' @return A column vector whose jth element is the density of a multivariate
//' normal distribution with mean mu and precision matrix Sigma_inv evaluated at
//' the jth column of x. If logret is true, the natural logarithm of the density
//' is returned.
//' @examples
//' M <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
//' m <- c(0, 0)
//' density_normal(cbind(c(0, 0), c(2, 2)), m, solve(M))
//' density_normal(cbind(c(0, 0), c(2, 2)), m, solve(M), TRUE)
// [[Rcpp::export]]
arma::vec density_normal(arma::mat x, arma::colvec mu, arma::mat Sigma_inv,
                         bool logret = false){
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

//' Simulate one draw from the Wishart distribution
//'
//' @param v An integer, the degrees of freedom of the distribution.
//' @param S A numeric matrix, the scale matrix of the distribution.
//' @return A column vector containing the draw.
//' @details Employs the Bartlett Decomposition (Smith & Hocking 1972).
//' Output exactly matches that of rwish from the MCMCpack package if the same
//' random seed is used.
//' @examples
//' M <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
//' draw_wishart(10, M)
// [[Rcpp::export]]
arma::mat draw_wishart(int v, arma::mat S){
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


//' Natural logarithm of the p-dimensional MV Gamma function
//'
//' @param p An integer, the dimesion of the MV Gamma function.
//' @param a A real number, the argument of the MV Gamma function.
//' @details Used to calculate the normalizing constant for the density of the
//' Wishart distribution.
// [[Rcpp::export]]
double log_mv_gamma(int p, double a){
  double lgamma_sum = 0.0;
  for(int j = 1; j <= p; j++){
    lgamma_sum += R::lgammafn(a + 0.5 * (1 - j));
  }
  return 0.25 * p * (p - 1) * log(arma::datum::pi) + lgamma_sum;
}


//' Wishart probability density function
//'
//' @param x A numeric matrix, the point at which the density is to be
//' evaluated.
//' @param v An integer, the degrees of freedom of the distribution.
//' @param S A numeric matrix, the scale matrix of the distribution.
//' of the variance-covariance matrix) of the distribution.
//' @param logret, a logical value indicating whether to return the log density.
//' @return A column real number: the value of the probability density function
//' by the default or the natural logarithm if logret is TRUE.
//' @examples
//' M <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
//' density_wishart(M, 10, M)
//' density_wishart(M, 10, M, TRUE)
// [[Rcpp::export]]
double density_wishart(arma::mat X, int v, arma::mat S,
                       bool logret = false){
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
