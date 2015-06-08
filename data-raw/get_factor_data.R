# Helper function for downloading monthly data from Ken French's Website
read.French <- function(zipfile, textfile){
  temp <- tempfile()
  prefix <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
  zip_url <- paste0(prefix, zipfile)
  temp <- tempfile()
  download.file(zip_url, temp)
  out <- readLines(unz(temp, textfile))
  unlink(temp)
  monthly_obs <- grepl("[[:digit:]]{6}", substr(out, 1, 6))
  first_line <- min(which(monthly_obs)) - 1
  last_line <- max(which(monthly_obs))
  out <- out[first_line:last_line]
  out <- read.table(textConnection(out), na.strings = c("-99.99", "-999"))
  out <- data.frame(month = rownames(out), out)
  rownames(out) <- NULL
  return(out)
}

#-----------------------------------------------------
# Liquidity Factor (liq_data_1962_2014.txt)
# Source: Lubos Pastor's Website
# (faculty.chicagobooth.edu/lubos.pastor/research/)
#-----------------------------------------------------
liq <- read.delim("http://faculty.chicagobooth.edu/lubos.pastor/research/liq_data_1962_2014.txt", comment.char = "%")
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
#devtools::use_data(mom)

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
#devtools::use_data(ff5)


#-----------------------------------------------------
# Ten Portfolios Formed on Size
# Source: Ken French's Website
#-----------------------------------------------------
url_prefix <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
file_prefix <- "Portfolios_Formed_on_ME.txt"
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
#devtools::use_data(ff5)

 <- read.French("F-F_Research_Data_5_Factors_2x3.zip",
                   "F-F_Research_Data_5_Factors_2x3.txt")
size_portfolios_url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_ME.zip"
temp <- tempfile()
download.file(size_portfolios_url, temp)
size_portfolios_value <- read.table(unz(temp,
                  "Portfolios_Formed_on_ME.txt"),
                  skip = 13, nrows = 1065,
                  na.strings = c("-99.99", "-999"))

size_portfolios_equal <- read.table(unz(temp,
                  "Portfolios_Formed_on_ME.txt"),
                  skip = 1082, nrows = 1065,
                  na.strings = c("-99.99", "-999"))
unlink(temp)
size_names <- c("month", "Lo10", "Dec2", "Dec3",
                "Dec4", "Dec5", "Dec6", "Dec7",
                "Dec8", "Dec9", "Hi10")
size_portfolios_value <- size_portfolios_value[,-c(2:10)]
names(size_portfolios_value) <- size_names
size_portfolios_equal <- size_portfolios_equal[,-c(2:10)]
names(size_portfolios_equal) <- size_names
rm(size_names, temp, size_portfolios_url)

#-----------------------------------------------------
# 10 Industry Portfolios
# Source: Ken French's Website
#-----------------------------------------------------
industry_portfolios_url <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/10_Industry_Portfolios.zip"
temp <- tempfile()
download.file(industry_portfolios_url, temp)
industry_portfolios_value <- read.table(unz(temp,
                  "10_Industry_Portfolios.txt"),
                  skip = 12, nrows = 1062,
                  na.strings = c("-99.99", "-999"))

industry_portfolios_equal <- read.table(unz(temp,
                  "10_Industry_Portfolios.txt"),
                  skip = 1078, nrows = 1062,
                  na.strings = c("-99.99", "-999"))
unlink(temp)
industry_names <- c("month", "NoDur", "Durbl",
                    "Manuf", "Enrgy", "HiTec",
                    "Telcm", "Shops", "Hlth",
                    "Utils", "Other")
names(industry_portfolios_equal) <- industry_names
names(industry_portfolios_value) <- industry_names
rm(industry_names, temp, industry_portfolios_url)

#-----------------------------------------------------
# Merge the Data
#-----------------------------------------------------
all_factors <- merge(fama_french, momentum, by = "month")
all_factors <- merge(all_factors, liquidity, by = "month")

portfolios_value <- merge(industry_portfolios_value, size_portfolios_value,
                          by = "month")
portfolios_equal <- merge(industry_portfolios_equal, size_portfolios_equal,
                          by = "month")

rm(fama_french, industry_portfolios_equal, industry_portfolios_value,
   size_portfolios_equal, size_portfolios_value, momentum, liquidity)

portequal <- merge(all_factors, portfolios_equal, by = "month")
portvalue <- merge(all_factors, portfolios_value, by = "month")

rm(all_factors, portfolios_equal, portfolios_value)


#-----------------------------------------------------
# Output Merge Data
#-----------------------------------------------------
devtools::use_data(portvalue)
devtools::use_data(portequal)

rm(portequal, portvalue)


