#include <RcppArmadillo.h>
#include <stdexcept>
using namespace Rcpp;

//' Half-vectorization of a symmetric matrix
//'
//' @param M An n by n symmetric matrix.
//' @return A column vector containing the vech of M, i.e.
//' the n * (n + 1) / 2 unique elements of M.
//' @details Throws an error if M is not square, but does not test if M is
//' symmetric. Elements above the main diagonal are simply ignored.
//' @examples
//' M <- matrix(c(11, 12, 13, 14,
//'               12, 22, 23, 24,
//'               13, 23, 33, 34,
//'               14, 24, 34, 44), 4, 4, byrow = TRUE)
//' vech(M)
// [[Rcpp::export]]
arma::colvec vech(arma::mat M){
  int m = M.n_rows;
  int n = M.n_cols;
  if(m != n){
    throw std::invalid_argument("received non-square matrix");
  }
  arma::colvec out((n + 1) * n / 2, arma::fill::zeros);
  int out_index = 0;
  for(int col = 0; col < n; col++){
    for(int row = col; row < n; row++){
      out(out_index) = M(row, col);
      out_index++;
    }
  }
  return out;
}


//' Convert Half-vectorization to symmetric matrix
//'
//' @param v A numeric vector with n * (n + 1) / 2 elements.
//' @param dim An integer indicating the dimension of the resulting square,
//' symmetric matrix.
//' @return The n by n symmetric matrix whose half-vectorization is v.
//' @details Throws an error if dim does not correspond to the length of v.
//' @examples
//' v <- c(11:14, 22:24, 33:34, 44)
//' devech(v, 4)
// [[Rcpp::export]]
arma::mat devech(arma::colvec v, int dim){
  if(v.n_elem != ((dim + 1) * dim / 2)){
    throw std::invalid_argument("dim and length v disagree");
  }
  arma::mat out(dim, dim, arma::fill::zeros);
  int v_index = 0;
  for(int col = 0; col < dim; col++){
    for(int row = col; row < dim; row++){
      out(row, col) = v(v_index);
      v_index++;
    }
  }
  return symmatl(out);
}
