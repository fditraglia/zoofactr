test_that("cleanCRSPData produces correct results", {
  crspSample <- data.frame(date = c(19860228,
                                    19860331,
                                    19860228,
                                    19860331),
                           EXCHCD = c(1, 1, 2, 1),
                           TICKER = c('A', 'A', 'B', 'A'),
                           PRC = c(-1, -1, 2, 1.2),
                           RET = c('C', 0.03, 0.04, 0.05),
                           SHROUT = c(100, 200, 300, 400),
                           stringsAsFactors = FALSE)
  actual <- cleanCRSPData(crspSample)
  expected <-  data.frame(date = c(19860331,
                                   19860228),
                          EXCHCD = c(1, 2),
                          TICKER = c('A', 'B'),
                          PRC = c(1, 2),
                          RET = c(0.03, 0.04) * 100,
                          SHROUT = c(200, 300),
                          TICKEREXC = c('A1', 'B2'),
                          marketValue = c(200 * 1, 300 * 2),
                          month = c(198603, 198602),
                          stringsAsFactors = FALSE)
  expect_equal(actual, expected, check.attributes=FALSE)
})

test_that("computeExcessReturns", {
  returns <- data.frame(month = c(198603, 198602),
                        A = c(5, 2), B = c(3, -2))
  riskFreeData <- data.frame(month = c(198603, 198601),
                             RF = c(1, 0.5))
  expected <- data.frame(A = 5 - 1, B = 3 - 1, month = 198603)
  actual <- computeExcessReturns(returns, riskFreeData, rfName = 'RF')
  expect_equal(actual, expected)
})

test_that("computeSkewnessFactor", {
  idioSkew <- 1:4
  names <- letters[1:4]
  mkValue <- seq(100, 400, by = 100)
  exReturns <- 1:4
  cutoff <- c(0.25, 0.5)
  names(mkValue) <- names
  names(exReturns) <- names
  SPlus <- (3 * 300 + 4 * 400) / (300 + 400)
  SMinus <- 1 * 100 / 100
  SK <- SMinus - SPlus
  expected <- data.frame(SPlus = SPlus, SMinus = SMinus, SK = SK)
  actual <- computeSkewnessFactor(idioSkew, mkValue, exReturns, cutoff)
  expect_equal(actual, expected)
})
