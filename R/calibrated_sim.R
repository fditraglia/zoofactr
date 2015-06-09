# Requires rmvtnorm package

SURidentical <- function(Factors, Portfolios, inData, alpha = TRUE){
  Y <- as.matrix(inData[,c(Portfolios)])
  X <- as.matrix(inData[,c(Factors)])
  if(alpha){
    X <- cbind(rep(1.0, nrow(X)), X)
    colnames(X)[1] <- "alpha"
  }
  reg <- lm.fit(x = X, y = Y)
  FactorLoadings <- reg$coefficients
  FactorResiduals <- reg$residuals
  return(list(Gamma = FactorLoadings, Sigma = cov(FactorResiduals)))
}

# Calibrated simulation
SURsim <- function(Factors, Portfolios, inData, alpha = TRUE,
                   FactorLoadings = NULL, nu = Inf){
  n_Obs <- nrow(inData)
  SURresults <- SURidentical(Factors, Portfolios, inData, alpha)
  if(is.null(FactorLoadings)){
    FactorLoadings <- SURresults$Gamma
  }
  FactorData <- as.matrix(inData[,Factors])
  if(alpha){
    FactorData <- cbind(rep(1, n_Obs), FactorData)
  }
  errorCov <- SURresults$Sigma
  sim_errors <- mvtnorm::rmvt(n_Obs, sigma = errorCov, df = nu)
  sim_returns <- FactorData %*% FactorLoadings + sim_errors
  return(list(x = FactorData, y = sim_returns))
}
