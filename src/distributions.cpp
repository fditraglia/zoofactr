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
//' Defaults to FALSE.
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
//' Defaults to FALSE.
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
  double term2 = -0.5 * trace(solve(S, X));
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

//' Armadillo wrapper for R's log1p function.
//'
//' @param x A numeric vector.
//' @return A numeric vector whose values are log(1 + x).
//' @details This is simply a wrapper to the vectorized Rcpp sugar function
//' log1p which uses the same implementation as R. It takes an Armadillo vector
//' as input and returns and Armadillo vector as output. It is intended for use
//' in C++ code that uses only Armadillo types.
//' @examples
//' log1p_arma(1/10^(0:5))
// [[Rcpp::export]]
arma::vec log1p_arma(arma::vec x){
  NumericVector x_Rcpp = NumericVector(x.begin(), x.end());
  NumericVector out_Rcpp = Rcpp::log1p(x_Rcpp);
  arma::vec out(out_Rcpp.begin(), out_Rcpp.size(), false);
  return out;
}


//' Multivariate Student-t probability density function
//'
//' @param nu A positive integer, the degrees of freedom of the distribution.
//' @param x A numeric matrix, each column of which is a point at which the
//' density is to be evaluated.
//' @param mu A numeric vector, the location parameter of the distribution.
//' @param Sigma_inv A numeric matrix, the inverse of the scale matrix the
//' of the variance-covariance matrix) of the distribution.
//' @param logret, a logical value indicating whether to return the log density.
//' Defaults to FALSE.
//' @return A column vector whose jth element is the density of a multivariate
//' Student-t with the specified parameters evaluated at  the jth column of x.
//' If logret is true, the natural logarithm of the density is returned.
//' @examples
//' M <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
//' m <- c(0, 0)
//' df <- 20
//' density_t(cbind(c(0, 0), c(2, 2)), df, m, solve(M))
//' density_t(cbind(c(0, 0), c(2, 2)), df, m, solve(M), TRUE)
// [[Rcpp::export]]
arma::vec density_t(arma::mat x, double nu, arma::colvec mu, arma::mat Sigma_inv,
                         bool logret = false){
 int p = Sigma_inv.n_cols;
 arma::mat R = chol(Sigma_inv);
 double t1 = R::lgammafn(0.5 * (nu + p));
 double t2 = -1.0 * R::lgammafn(0.5 * nu);
 double t3 = -0.5 * p * log(nu * arma::datum::pi);
 double t4 = sum(log(diagvec(R)));
 x.each_col() -= mu;
 arma::vec t5 = -0.5 * (nu + p) * log1p_arma(sum(pow(R * x, 2)).t() / nu);
 arma::vec logdensity = t1 + t2 + t3 + t4 + t5;
 if(logret)
   return logdensity;
 else
   return exp(logdensity);
}


//' Simulate draws from the Gamma distribution.
//'
//' @param a A positive number, the shape parameter of the distribution.
//' @param rate A numeric vector of positive values containing the rates
//' rates parameters for each draw.
//' @return A vector of random draws. The jth element is a Gamma(a, rate(j))
//' variate.
//' @details Note that the rate parameter is the reciprocal of the scale
//' parameter which appears as an argument in the underlying C function rgamma
//' from Rmath.h called here. The function draw_gamma is vectorized for its
//' second argument, the vector of rate parameters, and uses the length of this
//' parameter to determine how many draws to make. This function is intended
//' for use in a Gibbs sampler that uses the scale mixture of normals
//' representation of the Student-t distribution. In each step of the sampler
//' we draw an auxiliary parameter (a Gamma variate) for each observation in the
//' sample. Each of these draws has the same shape parameter but a different
//' rate parameter.
//' @examples
//' draw_gamma(1, 1:10)
// [[Rcpp::export]]
arma::vec draw_gamma(double a, arma::vec rate){
  RNGScope scope;
  int n = rate.n_rows;
  arma::vec out(n);
  for(int i = 0; i < n; i++)
     out(i) = R::rgamma(a, 1 / rate(i)); //zero-indexing
  return out;
}
