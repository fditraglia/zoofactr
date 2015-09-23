#' @import xlsx
#-----------------------------------------------------
# Liquidity Factor (liq_data_1962_2014.txt)
# Source: Lubos Pastor's Website
# (faculty.chicagobooth.edu/lubos.pastor/research/)
#-----------------------------------------------------
liq <- read.delim("http://faculty.chicagobooth.edu/lubos.pastor/research/liq_data_1962_2014.txt", comment.char = "%", na.strings = "-99")
names(liq) <- c("month", "LIQagg", "LIQinnov", "LIQv")
devtools::use_data(liq)

#-----------------------------------------------------
# Momentum Factor (F-F_Momentum_Factor.zip)
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
mom <- data.frame(month = rownames(mom), mom)
rownames(mom) <- NULL
names(mom)[2] <- "MOM"
rm(monthly_obs, first_line, last_line, url_prefix, file_prefix, temp)
devtools::use_data(mom)

#-----------------------------------------------------
# Fama/French 5 Factors (2x3)
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
ff5 <- data.frame(month = rownames(ff5), ff5)
rownames(ff5) <- NULL
rm(monthly_obs, first_line, last_line, url_prefix, file_prefix, temp)
devtools::use_data(ff5)

#-----------------------------------------------------
# Betting Against Beta
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
colnames(gp) <- c("month", "PMU")
rm(url_prefix, file_prefix, temp)
devtools::use_data(gp)
rm(gp)

#-----------------------------------------------------
# Quality Minus Junk
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
# Source: provided by Lu Zhang
# (zhanglu@fisher.osu.ed)
#-----------------------------------------------------
file_dir <- "data/"
file_name <- "HXZ q-Factors (monthly 1967 to 2014).csv"
hxz <- read.csv(paste0(file_dir, file_name), header = TRUE)
hxz$Month <- paste0(hxz$Year, sprintf("%02d", hxz$Month))
hxz$Year <- NULL
colnames(hxz)[1] <- "month"

rm(file_dir, file_name)
devtools::use_data(hxz)
rm(hxz)
