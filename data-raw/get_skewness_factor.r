#' @import tidyr

#-----------------------------------------------------
# Conditional Skewness
# Source: code
# (http://faculty.fuqua.duke.edu/~charvey/Research/Published_Papers/P56_Conditional_skewness_in.pdf)
#-----------------------------------------------------

# cleaning the data
crspData <- read.csv("data/crsp/crsp.csv", stringsAsFactors = FALSE)
crspData <- cleanCRSPData(crspData)
# separating data about returns and market value
returns <- spread(crspData[, c('month', 'TICKEREXC', 'RET')], TICKEREXC, RET)
marketValue <- spread(crspData[, c('month', 'TICKEREXC', 'marketValue')],
                      TICKEREXC, marketValue)
# get ff5 data for the market returns and risk free
# alternatively, could use one of the indices provided by CRSP
load("data/ff5.rda")
rm(crspData)
sk <- computeSkewness(returns, marketValue, ff5, windowLength = 60,
                      rfName = 'RF', mktRtName = 'Mkt.RF')
devtools::use_data(sk, overwrite = TRUE)


computeSkewness <- function(returns, marketValue, marketData, windowLength = 60,
                            rfName = 'RF', mktRtName = 'Mkt.RF') {
  excessReturns <- computeExcessReturns(returns, marketData, rfName = rfName)
  month <- excessReturns[, 'month']
  mkExcessReturns <- marketData[month == month, mktRtName]
  marketValue <- marketValue[month == month, ]
  bigT <- nrow(excessReturns)
  skewness <- data.frame(SPlus = double(), SMinus = double(), SK = double())
  for (tNdx in (windowLength + 1):bigT) {
    timeWindow <- (tNdx - (windowLength)):(tNdx - 1)
    relevExRet <- excessReturns[timeWindow, ]
    usedTickersNdx <- colSums(is.na(relevExRet)) == 0
    usedTickersNdx["month"] <- FALSE
    relevExRet <- as.matrix(relevExRet[ , usedTickersNdx])
    relevMkExRet <- as.matrix(mkExcessReturns[timeWindow])
    idioSkew <- computeSkewnessIndividual(relevExRet, relevMkExRet)
    skewness[tNdx, ] <- computeSkewnessFactor(idioSkew,
                                              marketValue[tNdx - 1, usedTickersNdx + 1],
                                              excessReturns[tNdx, usedTickersNdx],
                                              cutoff = c(0.3, 0.7))
  }
  sk <- data.frame(month = month, skewness)
  return(sk)
}

computeSkewnessIndividual <- function(excessReturnToday, mkExcessReturnToday) {
  mX <- cbind(mkExcessReturnToday, 1)
  marketBetaHat <- solve(t(mX) %*% mX) %*% t(mX) %*% excessReturnToday
  residuals <- excessReturnToday - mX %*% marketBetaHat
  mkResidual <- mkExcessReturnToday - mean(mkExcessReturnToday)
  mkReisualSquared <- kronecker(as.matrix(mkResidual) ^ 2, t(rep(1, ncol(residuals))))
  numerator <- apply(residuals * mkReisualSquared, 2, mean)
  denominator <- sqrt(apply(residuals ^ 2, 2, mean)) * mean(mkResidual ^ 2)
  skewness <-  numerator / denominator
  return(skewness)
}

computeSkewnessFactor <- function(idioSkew, mkValue, exReturns,
                                  cutoff = c(0.3, 0.7)) {
  portfolioNdx <- cut(idioSkew, breaks = quantile(idioSkew, c(0, cutoff, 1)),
                      include.lowest = TRUE, labels = FALSE)
  sMinusNdx <- which(portfolioNdx == 1)
  sPlusNdx <- which(portfolioNdx == max(portfolioNdx))
  sMinusExReturn <- sum(mkValue[sMinusNdx] * exReturns[sMinusNdx], na.rm = TRUE) /
                     sum(mkValue[sMinusNdx], na.rm = TRUE)
  sPlusExReturn <- sum(mkValue[sPlusNdx] * exReturns[sPlusNdx], na.rm = TRUE) /
                     sum(mkValue[sPlusNdx], na.rm = TRUE)
  answer <- data.frame(SPlus = sPlusExReturn, SMinus = sMinusExReturn,
                       SK = sMinusExReturn - sPlusExReturn)
  return(answer)
}

computeExcessReturns <- function(returns, riskFreeData, rfName = 'RF') {
  tickerNdx <- 2 : ncol(returns)
  returns <- merge(returns, riskFreeData, by = 'month')
  excessReturns <- returns[, tickerNdx] - returns[, rfName]
  excessReturns$month <- returns$month
  return(excessReturns)
}

cleanCRSPData <- function(crspData) {
  crspData <- na.omit(crspData)
  warning("NA values removed")
  crspData$PRC <- abs(crspData$PRC)
  crspData <- crspData[which(crspData$RET != 'C'), ]
  crspData$RET <- as.numeric(crspData$RET) * 100
  crspData <- crspData[which(crspData$TICKER != ''), ]
  crspData$TICKEREXC <- paste0(crspData$TICKER, crspData$EXCHCD)
  duplicateNdx <- !duplicated(crspData[, c('date', 'TICKEREXC')])
  crspData <- crspData[duplicateNdx, ]
  crspData$marketValue <- crspData$PRC * crspData$SHROUT
  crspData$month <- as.numeric(substr(as.character(crspData$date), 1, 6))
  return(crspData)
}


