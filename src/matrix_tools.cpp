#include <RcppArmadillo.h>
#include <stdexcept>
using namespace Rcpp;

// [[Rcpp::export]]
arma::colvec vech(arma::mat A){
  int m = A.n_rows;
  int n = A.n_cols;
  if(m != n){
    throw std::invalid_argument("received non-square matrix");
  }
  arma::colvec out((n + 1) * n / 2, arma::fill::zeros);
  int out_index = 0;
  for(int col = 0; col < n; col++){
    for(int row = col; row < n; row++){
      out(out_index) = A(row, col);
      out_index++;
    }
  }
  return out;
}


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
