#' @import xlsx
#-----------------------------------------------------
# Liquidity Factor (liq_data_1962_2014.txt)
# LIQ
# Source: Lubos Pastor's Website
# (faculty.chicagobooth.edu/lubos.pastor/research/)
#-----------------------------------------------------
liq <- read.delim("http://faculty.chicagobooth.edu/lubos.pastor/research/liq_data_1962_2014.txt", comment.char = "%", na.strings = "-99")
names(liq) <- c("month", "LIQagg", "LIQinnov", "LIQv")
devtools::use_data(liq)

#-----------------------------------------------------
# Momentum Factor (F-F_Momentum_Factor.zip)
# MOM
# Type: Returns
# Source: Ken French's Website
# (mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp)
#-----------------------------------------------------
url_prefix <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
file_prefix <- "F-F_Momentum_Factor"
temp <- tempfile()
download.file(paste0(url_prefix, file_prefix, "_TXT.zip"), temp)
mom <- readLines(unz(temp, paste0(file_prefix, ".TXT")))
unlink(temp)
monthly_obs <- grepl("[[:digit:]]{6}", substr(mom, 1, 6))
first_line <- min(which(monthly_obs)) - 1
last_line <- max(which(monthly_obs))
mom <- mom[first_line:last_line]
mom <- read.table(textConnection(mom), na.strings = c("-99.99", "-999"))
mom <- data.frame(month = rownames(mom), mom, stringsAsFactors = FALSE)
rownames(mom) <- NULL
names(mom)[2] <- "MOM"
rm(monthly_obs, first_line, last_line, url_prefix, file_prefix, temp)
devtools::use_data(mom)

#-----------------------------------------------------
# Fama/French 5 Factors (2x3)
# FF5
# Type: Returns (unadjusted except for market)
# Source: Ken French's Website
# (mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp)
#-----------------------------------------------------
url_prefix <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
file_prefix <- "F-F_Research_Data_5_Factors_2x3"
temp <- tempfile()
download.file(paste0(url_prefix, file_prefix, "_TXT.zip"), temp)
ff5 <- readLines(unz(temp, paste0(file_prefix, ".txt")))
unlink(temp)
monthly_obs <- grepl("[[:digit:]]{6}", substr(ff5, 1, 6))
first_line <- min(which(monthly_obs)) - 1
last_line <- max(which(monthly_obs))
ff5 <- ff5[first_line:last_line]
ff5 <- read.table(textConnection(ff5), na.strings = c("-99.99", "-999"))
ff5 <- data.frame(month = rownames(ff5), ff5, stringsAsFactors = FALSE)
rownames(ff5) <- NULL
rm(monthly_obs, first_line, last_line, url_prefix, file_prefix, temp)
devtools::use_data(ff5)

#-----------------------------------------------------
# Betting Against Beta
# BAB
# Type: Returns (unadjusted?)
# Source: AQR website
# (www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly)
#-----------------------------------------------------
url <- "http://www.aqr.com/library/data-sets/betting-against-beta-equity-factors-monthly/data"
temp <- tempfile()
download.file(url, temp, mode = 'wb')
bab <- read.xlsx(temp, 1, stringsAsFactors = FALSE)
unlink(temp)
header_line <- which(bab[ , 1] == 'DATE')
usa_ndx <- which(bab[header_line, ] == "USA")
non_empty_ndx <- which(!is.na(bab[ , usa_ndx]) &
                       bab[ , usa_ndx] != "")
first_line <- min(non_empty_ndx[non_empty_ndx > header_line])
last_line <- max(non_empty_ndx)
bab <- data.frame(bab[first_line : last_line, c(1, usa_ndx)])
colnames(bab) <- c("month", "BAB")
bab[ , "month"] <- as.Date(as.numeric(bab[ , "month"]) - 25569,
                            origin = "1970-01-01")
bab[ , "month"] <- paste0(format(bab[ , "month"], "%Y"),
                          format(bab[ , "month"], "%m"))
bab[, "BAB"] <- as.numeric(bab[, "BAB"])

rm(header_line, usa_ndx, non_empty_ndx, first_line, last_line, url, temp)
devtools::use_data(bab)
rm(bab)

#-----------------------------------------------------
# Gross Profitability
# GP
# Type: Returns (unadjusted?)
# Source: Robert Novy-Marx website
# (http://rnm.simon.rochester.edu/data_lib/index.html)
#-----------------------------------------------------
url_prefix <- "http://rnm.simon.rochester.edu/data_lib/OSoV/"
file_prefix <- "Ind_adj_fac_ret"
temp <- tempfile()
download.file(paste0(url_prefix, file_prefix, ".xlsx"), temp, mode = 'wb')
gp <- read.xlsx(temp, 1, stringsAsFactors = FALSE)
unlink(temp)
gp <- gp[ , c("Date", "PMU.")]
colnames(gp) <- c("month", "GP")
rm(url_prefix, file_prefix, temp)
devtools::use_data(gp)
rm(gp)

#-----------------------------------------------------
# Quality Minus Junk
# QMJ
# Type: Returns (unadjusted?)
# Source: AQR website
# (https://www.aqr.com/library/data-sets/quality-minus-junk-factors-monthly)
#-----------------------------------------------------
url <- "http://www.aqr.com/library/data-sets/quality-minus-junk-factors-monthly/data"
temp <- tempfile()
download.file(url, temp, mode = 'wb')
qmj <- read.xlsx(temp, 1, stringsAsFactors = FALSE)
unlink(temp)
header_line <- which(qmj[ , 1] == 'DATE')
usa_ndx <- which(qmj[header_line, ] == "USA")
non_empty_ndx <- which(!is.na(qmj[ , usa_ndx]) &
                         qmj[ , usa_ndx] != "")
first_line <- min(non_empty_ndx[non_empty_ndx > header_line])
last_line <- max(non_empty_ndx)
qmj <- data.frame(qmj[first_line : last_line, c(1, usa_ndx)])
colnames(qmj) <- c("month", "QMJ")
qmj[ , "month"] <- as.Date(as.numeric(qmj[ , "month"]) - 25569,
                           origin = "1970-01-01")
qmj[ , "month"] <- paste0(format(qmj[ , "month"], "%Y"),
                          format(qmj[ , "month"], "%m"))
qmj[, "QMJ"] <- as.numeric(qmj[, "QMJ"])

rm(header_line, usa_ndx, non_empty_ndx, first_line, last_line, url, temp)
devtools::use_data(qmj)
rm(qmj)

#-----------------------------------------------------
# Profitability and Investment
# HXZ: ME - size, IA - investment, ROE - profitability
# Type: Returns adjusted by the risk free rate
# Source: provided by Lu Zhang
# (zhanglu@fisher.osu.ed)
#-----------------------------------------------------
file_dir <- "data/"
file_name <- "HXZ q-Factors (monthly 1967 to 2014).csv"
hxz <- read.csv(paste0(file_dir, file_name), header = TRUE)
hxz$Month <- paste0(hxz$Year, sprintf("%02d", hxz$Month))
hxz$Year <- NULL
colnames(hxz)[1] <- "month"
colnames(hxz)[4] <- "IA"

rm(file_dir, file_name)
devtools::use_data(hxz)
rm(hxz)

#-----------------------------------------------------
# Common Idiosyncratic Volatility
# CIV: CIV and CIVinnvov
# Type: Returns (unadjusted?)
# Source: provided by Brian Kelly
# (bryan.kelly@chicagobooth.edu)
#-----------------------------------------------------
file_dir <- "data/"
file_name <- "CIV_time_series.csv"
civ <- read.csv(paste0(file_dir, file_name), header = TRUE)
colnames(civ) <- c("month", "CIV", "CIVinnov")

rm(file_dir, file_name)
devtools::use_data(civ)
rm(civ)

#-----------------------------------------------------
# Default risk
# DSV
# Type: Macro indicator
# Source: Maria Vassalou's site
# http://maria-vassalou.com/research/data/
#-----------------------------------------------------
url <- "http://maria-vassalou.com/wp-content/uploads/2013/05/DSV.xls"
temp <- tempfile()
download.file(url, temp, mode = 'wb')
dsv <- read.xlsx(temp, 1, stringsAsFactors = FALSE)
unlink(temp)
colnames(dsv) <- c("month", "DSV")

rm(url, temp)
devtools::use_data(dsv)
rm(dsv)

#-----------------------------------------------------
# CAY(Consumption-wealth ratio) and Consumption Growth
# CAY: CAY and dC
# Type: Macro indicators
# Source: Martin Lettau's site
# http://faculty.haas.berkeley.edu/lettau/data/
#-----------------------------------------------------
cay <- readLines("http://faculty.haas.berkeley.edu/lettau/data/cay_q_03Q1.txt")
monthly_obs <- grepl("[[:digit:]]{6}", substr(cay, 2, 7))
first_line <- min(which(monthly_obs)) - 1
last_line <- max(which(monthly_obs))
cay <- cay[first_line:last_line]
cay <- read.table(textConnection(cay), na.strings = c("-99.99", "-999"),
                  header = TRUE, fill = TRUE)
colnames(cay) <- c('month', 'c', 'a', 'y', 'cay')
consGrowth = diff(cay[ , "c"])
cay <- data.frame(month = cay$month[-1], DC = consGrowth, CAY = cay$cay[-1])
mom <- data.frame(month = rownames(mom), mom, stringsAsFactors = FALSE)

rm(monthly_obs, first_line, last_line)
devtools::use_data(cay)
rm(cay)

#-----------------------------------------------------
# Standartized Unexpected Earnings
# SUE
# Type: Returns
# Source: Lakshmanan Shivakumar's website
# https://www.london.edu/faculty-and-research/faculty/profiles/shivakumar-l
#-----------------------------------------------------
url <- "https://www.london.edu/-/media/files/faculty%20and%20research/faculty%20profiles/accounting/lakshmanan%20shivakumar/sue_portfolio_pmn_returns_nyseamex_2003.xls"
temp <- tempfile()
download.file(url, temp, mode = 'wb')
sue <- read.xlsx(temp, 1, stringsAsFactors = FALSE)
unlink(temp)
colnames(sue) <- c("month", paste0("P", 1:10), "SUE")
sue <- sue[, c("month", "SUE")]

rm(url, temp)
devtools::use_data(sue)
rm(sue)

#-----------------------------------------------------
# Net Payout Yield
# NPY: NPYAll and NPYNonFin
# Type: Returns
# Source: Michael Roberts' website
# http://finance.wharton.upenn.edu/~mrrobert/styled-9/styled-13/index.html
#-----------------------------------------------------
url <- "http://finance.wharton.upenn.edu/~mrrobert/resources/Data/Post2Web-PayoutPaperDataTS-23-Sep-2011.xlsx"
temp <- tempfile()
download.file(url, temp, mode = 'wb')
npy <- read.xlsx(temp, 2, stringsAsFactors = FALSE)
unlink(temp)
header_line <- which(npy[ , 1] == 'year')
header <- npy[header_line, ]
first_line <- header_line + 2
last_line <- max(which(!is.na(npy[ , 1]) & npy[, 1] != ''))
npy <- data.frame(npy[first_line : last_line, ])
colnames(npy) <- header
npy$month <- as.numeric(npy$month)
npy$month <- as.numeric(npy$year) * 100 + as.numeric(npy$month)
npy <- npy[which(npy[, 'lcrspnpy'] != '.'), ]
npy <- npy[, c(2, which(header == 'lcrspnpy'))]
colnames(npy) <- c('month', 'NPYAll', 'NPYNonFin')
npy$NPYAll <- as.numeric(npy$NPYAll)
npy$NPYNonFin <- as.numeric(npy$NPYNonFin)

devtools::use_data(npy)
rm(npy, header, header_line, first_line, last_line)

#-----------------------------------------------------
# Market Mispricing
# UMO
# Type: Returns
# Source: Dangling Jiang's website
# https://sites.google.com/site/danlingjiang/data-library
#-----------------------------------------------------
file_dir <- "data/"
file_name <- "UMO.xlsx"
umo <- read.xlsx(paste0(file_dir, file_name), 2, stringsAsFactors = FALSE)
umo$month <- umo$year * 100 + umo$month
umo <- umo[, c("month", "UMO")]

rm(file_dir, file_name)
devtools::use_data(umo)
rm(umo)

#-----------------------------------------------------
# Market Sentiment
# SENT: SENTorth (orthogonalized components) and SENT
# Type: Macro
# Description: Composite sentiment index based on various sentiment measures
# Source: Jeffrey wurgler's website
# http://people.stern.nyu.edu/jwurgler/
#-----------------------------------------------------
url <- "http://people.stern.nyu.edu/jwurgler/data/Investor_Sentiment_Data_v23_POST.xlsx"
temp <- tempfile()
download.file(url, temp, mode = 'wb')
sent <- read.xlsx(temp, 2, stringsAsFactors = FALSE)
unlink(temp)

header_line <- which(sent[ , 1] == 'yearmo')
header <- sent[header_line, ]
notNA <- which(sent[ , 2] != '.')
first_line <- min(notNA[notNA > header_line])
last_line <- max(which(!is.na(sent[ , 1]) & sent[, 2] != ''))
sent <- data.frame(sent[first_line : last_line, ])
header <- c("month", "SENTorth", "SENT")
colnames(sent) <- c("month", "SENTorth", "SENT")
sent <- sent[ , header]
sent$month <- as.numeric(sent$month)
sent$SENTorth <- as.numeric(sent$SENTorth)
sent$SENT <- as.numeric(sent$SENT)

devtools::use_data(sent)
rm(sent, header, header_line, first_line, last_line, notNA, temp, url)

#-----------------------------------------------------
# Private Information
# PIN
# Type: Returns
# Description: Return on a zero-investment portfolio long in high PIN stocks
# and short in low PIN stocks; PIN (private information) is the probability of information-
# based trade
# Source: Søren Hvidkjær's website
# https://sites.google.com/site/hvidkjaer/data
#-----------------------------------------------------
pin <- read.delim("https://sites.google.com/site/hvidkjaer/data/data-files/pinret.txt?attredirects=0",
                  comment.char = "%", na.strings = "-99", sep = " ")
names(pin) <- c("month", "PIN")
devtools::use_data(pin)
rm(pin)

#-----------------------------------------------------
# Liquidity
# LIQSadka: LIQfix and LIQvar
# Type: Macro
# Description: Market-wide liquidity constructed first by decomposing firm-level
# liquidity into variable and fixed price effects then averaging the variable
# component
# Source: Ronnie Sadka's website
# https://www2.bc.edu/ronnie-sadka/
#-----------------------------------------------------
url <- "http://www2.bc.edu/~sadka/Sadka-LIQ-factors-1983-2012-WRDS.xlsx"
temp <- tempfile()
download.file(url, temp, mode = 'wb')
liqSadka <- read.xlsx(temp, 1, stringsAsFactors = FALSE)
unlink(temp)

liqSadka <- liqSadka[1:3, ]
names(liqSadka) <- c("month", "LIQfix", "LIQvar")
devtools::use_data(liqSadka)
rm(liqSadka, header, header_line, first_line, last_line, notNA, temp, url)

#-----------------------------------------------------
# Consumption Volatility Risk
# CVR
# Type: Return
# Description: Filtered consumption growth volatility from a Markov
# regime-switching model based on historical consumption data
# Source: Lars-Alexander Kuehn's website
# http://berlin.tepper.cmu.edu/
#-----------------------------------------------------
url <- "http://berlin.tepper.cmu.edu/pdf/Data_Website.xlsx"
temp <- tempfile()
download.file(url, temp, mode = 'wb')
cvr <- read.xlsx(temp, 3, stringsAsFactors = FALSE)
unlink(temp)

header_line <- which(cvr[ , 1] == 'Year')
header <- cvr[header_line, ]
first_line <- header_line + 1
last_line <- max(which(!is.na(cvr[ , 1]) & cvr[, 1] != ''))
cvr <- data.frame(cvr[first_line : last_line, ])
colnames(cvr) <- header
cvr$month <- as.numeric(cvr$Year) * 100 + as.numeric(cvr$Month)
cvr <- cvr[, c("month", "CVR (H-L)")]
colnames(cvr) <- c("month", "CVR")
cvr$CVR <- as.numeric(cvr$CVR)

devtools::use_data(cvr)
rm(cvr, header, header_line, first_line, last_line, temp, url)
