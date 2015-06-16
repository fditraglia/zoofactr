#-----------------------------------------------------
# Ten Portfolios Formed on Size
# Source: Ken French's Website
#-----------------------------------------------------
url_prefix <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
file_prefix <- "Portfolios_Formed_on_ME"
temp <- tempfile()
download.file(paste0(url_prefix, file_prefix, ".zip"), temp)
size_raw <- readLines(unz(temp, paste0(file_prefix, ".txt")))
unlink(temp)
rm(temp, url_prefix, file_prefix)

size_raw <- size_raw[1:(min(which(grepl("Annual", size_raw))) - 1)]

value_first <- which(grepl("Value Weighted Returns -- Monthly", size_raw)) + 1
equal_first <- which(grepl("Equal Weighted Returns -- Monthly", size_raw)) + 1
monthly_obs <- which(grepl("[[:digit:]]{6}", substr(size_raw, 1, 6)))
value_last <- max(monthly_obs[monthly_obs < equal_first])
equal_last <- max(which(size_raw != ""))

clean_colnames <- function(raw_colnames){
  out <- trimws(raw_colnames, "left")
  out <- unlist(strsplit(out, "  "))
  out <- gsub(" ", "", out)
  return(out)
}

size_value <- size_raw[(value_first + 1):value_last]
size_value <- read.table(textConnection(size_value),
                          na.strings = c("-99.99", "-999"))
names(size_value) <- c("month", clean_colnames(size_raw[value_first]))

size_equal <- size_raw[(equal_first + 1):equal_last]
size_equal <- read.table(textConnection(size_equal),
                          na.strings = c("-99.99", "-999"))
names(size_equal) <- c("month", clean_colnames(size_raw[equal_first]))

size <- list(equal = size_equal, value = size_value)

rm(size_raw, size_equal, size_value, equal_first, equal_last,
   value_first, value_last, monthly_obs)

devtools::use_data(size)
rm(size, clean_colnames)

#-----------------------------------------------------
# 10 Industry Portfolios
# Source: Ken French's Website
#-----------------------------------------------------
url_prefix <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"
file_prefix <- "10_Industry_Portfolios"
temp <- tempfile()
download.file(paste0(url_prefix, file_prefix, ".zip"), temp)
industry_raw <- readLines(unz(temp, paste0(file_prefix, ".txt")))
unlink(temp)
rm(temp, url_prefix, file_prefix)

industry_raw <- industry_raw[1:(min(which(grepl("Annual", industry_raw))) - 1)]

value_first <- which(grepl("Average Value Weighted Returns -- Monthly",
                           industry_raw)) + 1
equal_first <- which(grepl("Average Equal Weighted Returns -- Monthly",
                           industry_raw)) + 1
monthly_obs <- which(grepl("[[:digit:]]{6}", substr(industry_raw, 1, 6)))
value_last <- max(monthly_obs[monthly_obs < equal_first])
equal_last <- max(which(industry_raw != ""))

industry10_equal <- industry_raw[equal_first:equal_last]
industry10_equal <- read.table(textConnection(industry10_equal), header = TRUE,
                               na.strings = c("-99.99", "-999"))

industry10_value <- industry_raw[value_first:value_last]
industry10_value <- read.table(textConnection(industry10_value), header = TRUE,
                               na.strings = c("-99.99", "-999"))

industry10_value <- data.frame(month = rownames(industry10_value),
                               industry10_value)
rownames(industry10_value) <- NULL
industry10_equal <- data.frame(month = rownames(industry10_equal),
                               industry10_equal)
rownames(industry10_equal) <- NULL


industry10 <- list(equal = industry10_equal, value = industry10_value)
rm(equal_first, equal_last, industry_raw, industry10_value, industry10_equal,
   monthly_obs, value_last, value_first)

devtools::use_data(industry10)
rm(industry10)
