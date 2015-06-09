#ifndef DISTRIBUTIONS_H
#define DISTRIBUTIONS_H

arma::colvec draw_normal(arma::colvec mu, arma::mat Sigma_inv);

arma::vec density_normal(arma::mat x, arma::colvec mu, 
                      arma::mat Sigma_inv, bool logret = false);

arma::mat draw_wishart(int v, arma::mat S);

double log_mv_gamma(int p, double a);

double density_wishart(arma::mat X, int v, arma::mat S, 
                       bool logret = false);

#endif