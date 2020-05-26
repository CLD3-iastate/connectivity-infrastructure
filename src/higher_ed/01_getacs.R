library(tidycensus)
library(acs)
library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
library(naniar)
library(readr)

census_api_key(CENSUS_API_KEY) # My key

#
# Select tables --------------------------------------------------------------------------------------
#

tables <- c(
  "B14006", # POVERTY STATUS IN THE PAST 12 MONTHS BY SCHOOL ENROLLMENT BY LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER
  "B11003" # FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN UNDER 18 YEARS
  #"B28001", # TYPES OF COMPUTERS IN HOUSEHOLD 
  #"B28002" # PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD 
)         


#
# Get tables for counties --------------------------------------------------------------------------------------
#

# # Iowa (FIPS 19)
# dataia <- get_acs(geography = "county", state = 19, table = tables[1], year = 2018, survey = "acs5", 
#                   cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
# for(i in 2:length(tables)){
#   tmp <- get_acs(geography = "county", state = 19, table = tables[i],year = 2018, survey = "acs5", 
#                  cache_table = TRUE, output = "wide", geometry = FALSE)
#   dataia <- left_join(dataia, tmp)
# }
# remove(tmp)
# 
# # Oregon (FIPS 41)
# dataor <- get_acs(geography = "county", state = 41, table = tables[1], year = 2018, survey = "acs5", 
#                   cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
# for(i in 2:length(tables)){
#   tmp <- get_acs(geography = "county", state = 41, table = tables[i],year = 2018, survey = "acs5", 
#                  cache_table = TRUE, output = "wide", geometry = FALSE)
#   dataor <- left_join(dataor, tmp)
# }
# remove(tmp)

# Virginia (FIPS 51)
datava <- get_acs(geography = "county", state = 51, table = tables[1], year = 2018, survey = "acs5", 
                  cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(tables)){
  tmp <- get_acs(geography = "county", state = 51, table = tables[i],year = 2018, survey = "acs5", 
                 cache_table = TRUE, output = "wide", geometry = FALSE)
  datava <- left_join(datava, tmp)
}
remove(tmp)

# Join
data <- datava #rbind(dataia, dataor, datava)


#
# Calculate variables --------------------------------------------------------------------------------------
#

data <- data %>% transmute(
  STATEFP = STATEFP, 
  COUNTYFP = COUNTYFP, 
  COUNTYNS = COUNTYNS, 
  AFFGEOID = AFFGEOID, 
  GEOID = GEOID, 
  LSAD = LSAD, 
  NAME.x = NAME.x, 
  NAME.y = NAME.y,
  NAME = NAME,
  ALAND = ALAND, 
  AWATER = AWATER, 
  geometry = geometry,
  # share HHs with no computing device
  #dev_none = B28001_011E / B28001_001E * 100,
  # share HHs with no internet access
  #int_none = B28002_013E / B28002_001E * 100,
  # percent students in grades 9-12 under poverty level
  poverty = B14006_008E / B14006_001E * 100
 )


#
# Check missingness, split, write --------------------------------------------------------------------------------------
#

miss_var_summary(data)

write_rds(data, "./Data/acs.Rds")