library(dplyr)
library(readr)
library(naniar)
library(readxl)

#
# Read in data from downloaded CSV's: Table LD03 - Total College and University Enrollment, All
# Students, by Student Origin (county)
# https://research.schev.edu/info/Reports.Guide-to-the-Data-for-Localities-Reports  =============
#

ALL <- read.csv("./Data/SCHEV data/LocalityData_Enrollment-ALL.csv", skip=1,
                stringsAsFactors = FALSE) 
PR_4YR <- read.csv("./Data/SCHEV data/LocalityData_Enrollment-PRIVATE_NONPROFIT_4YR.csv", skip=1, 
                   stringsAsFactors = FALSE)
PUB_4YR <- read.csv("./Data/SCHEV data/LocalityData_Enrollment-PUBLIC_4YR.csv", skip=1, 
                    stringsAsFactors = FALSE)
PUB_2YR <- read.csv("./Data/SCHEV data/LocalityData_Enrollment-PUBLIC_2YR.csv", skip=1, 
                    stringsAsFactors = FALSE)

#
# format column names so data can be joined =================================
#

colnames(ALL)[3:7] <- c("2015_16", "2016_17", "2017_18", "2018_19", "2019_20")
colnames(ALL)[3:7] <- paste("ALL", colnames(ALL)[3:7], sep="_")

colnames(PR_4YR)[3:7] <- c("2015_16", "2016_17", "2017_18", "2018_19", "2019_20")
colnames(PR_4YR)[3:7] <- paste("PR_4YR", colnames(PR_4YR)[3:7], sep="_")

colnames(PUB_4YR)[3:7] <- c("2015_16", "2016_17", "2017_18", "2018_19", "2019_20")
colnames(PUB_4YR)[3:7] <- paste("PUB_4YR", colnames(PUB_4YR)[3:7], sep="_")

colnames(PUB_2YR)[3:7] <- c("2015_16", "2016_17", "2017_18", "2018_19", "2019_20")
colnames(PUB_2YR)[3:7] <- paste("PUB_2YR", colnames(PUB_2YR)[3:7], sep="_")

#
# format numeric data as integers (instead of strings)  ================================
#

ALL[,3:7] <- lapply(ALL[,3:7], function(x) gsub(",", "", x))
ALL[,3:7] <- sapply(ALL[,3:7], as.numeric)

PR_4YR[,3:7] <- lapply(PR_4YR[,3:7], function(x) gsub(",", "", x))
PR_4YR[,3:7] <- sapply(PR_4YR[,3:7], as.numeric)

PUB_4YR[,3:7] <- lapply(PUB_4YR[,3:7], function(x) gsub(",", "", x))
PUB_4YR[,3:7] <- sapply(PUB_4YR[,3:7], as.numeric)

PUB_2YR[,3:7] <- lapply(PUB_2YR[,3:7], function(x) gsub(",", "", x))
PUB_2YR[,3:7] <- sapply(PUB_2YR[,3:7], as.numeric)


#
# find totals per year of in-state students for each higher ed type  ===========================
#

totals_ALL <- apply(ALL[ ,3:7], 2, sum)
totals_PR_4YR <- apply(PR_4YR[ ,3:7], 2, sum)
totals_PUB_4YR <- apply(PUB_4YR[ ,3:7], 2, sum)
totals_PUB_2YR <- apply(PUB_2YR[ ,3:7], 2, sum)

#
# calculate percentage of in-state students for each county and higher ed type: county/total * 100
#

ALL[, 8:12] = (sweep(ALL[, 3:7], 2, totals_ALL, "/"))*100
colnames(ALL)[8:12] <- paste("perc", colnames(ALL)[8:12], sep="_")

PR_4YR[, 8:12] = (sweep(PR_4YR[, 3:7], 2, totals_PR_4YR, "/"))*100
colnames(PR_4YR)[8:12] <- paste("perc", colnames(PR_4YR)[8:12], sep="_")

PUB_4YR[, 8:12] = (sweep(PUB_4YR[, 3:7], 2, totals_PUB_4YR, "/"))*100
colnames(PUB_4YR)[8:12] <- paste("perc", colnames(PUB_4YR)[8:12], sep="_")

PUB_2YR[, 8:12] = (sweep(PUB_2YR[, 3:7], 2, totals_PUB_2YR, "/"))*100
colnames(PUB_2YR)[8:12] <- paste("perc", colnames(PUB_2YR)[8:12], sep="_")


#
# check for missingness in data
#

miss_var_summary(ALL)
miss_var_summary(PR_4YR)
miss_var_summary(PUB_4YR)
miss_var_summary(PUB_2YR)

#
# merge higher-ed types together  ------------------------------------------
#

temp1 <- merge(ALL, PR_4YR, by=c("Locality", "FIPS"))
temp2 <- merge(PUB_4YR, PUB_2YR, by=c("Locality", "FIPS"))
higher_ed <- merge(temp1, temp2, by=c("Locality", "FIPS"))

#
#  format FIPS columns --------------------------------------------
#

higher_ed <- higher_ed %>% mutate(
  STATEFP = substr(FIPS, start=1, stop=2),
  COUNTYFP = substr(FIPS, start=4, stop=6)
)

higher_ed$FIPS = NULL
higher_ed <- higher_ed[c(1, 42, 43, 2:41)]

miss_var_summary(higher_ed)  # no missing data

write_rds(higher_ed, "./Data/schev.rds")


