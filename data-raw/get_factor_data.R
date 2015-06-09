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
download.file(paste0(url_prefix, file_prefix, ".zip"), temp)
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
download.file(paste0(url_prefix, file_prefix, ".zip"), temp)
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

rm(ff5, liq, mom)
