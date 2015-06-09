#ifndef MATRIX_TOOLS_H
#define MATRIX_TOOLS_H

arma::colvec vech(arma::mat A);

arma::mat devech(arma::colvec v, int dim);

#endif