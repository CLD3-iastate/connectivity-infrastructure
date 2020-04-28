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

census_api_key("548d39e0315b591a0e9f5a8d9d6c1f22ea8fafe0") # Teja's key

#
# Select tables --------------------------------------------------------------------------------------
#

tables <- c(
  "B28001", # TYPES OF COMPUTERS IN HOUSEHOLD 
  "B28003", # PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
  "B28002", # PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD 
  "B28004", # HOUSEHOLD INCOME IN THE LAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS) BY PRESENCE AND TYPE OF INTERNET SUBSCRIPTION 
  "B28005", # AGE BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD 
  "B28007", # LABOR FORCE STATUS BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD 
  "B28008"  # PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD 
)         


#
# Get tables for counties --------------------------------------------------------------------------------------
#

# Iowa (FIPS 19)
dataia <- get_acs(geography = "county", state = 19, table = tables[1], year = 2018, survey = "acs5", 
                  cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(tables)){
  tmp <- get_acs(geography = "county", state = 19, table = tables[i],year = 2018, survey = "acs5", 
                 cache_table = TRUE, output = "wide", geometry = FALSE)
  dataia <- left_join(dataia, tmp)
}
remove(tmp)

# Oregon (FIPS 41)
dataor <- get_acs(geography = "county", state = 41, table = tables[1], year = 2018, survey = "acs5", 
                  cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(tables)){
  tmp <- get_acs(geography = "county", state = 41, table = tables[i],year = 2018, survey = "acs5", 
                 cache_table = TRUE, output = "wide", geometry = FALSE)
  dataor <- left_join(dataor, tmp)
}
remove(tmp)

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
data <- rbind(dataia, dataor, datava)


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
  # share HHs with dialup internet only
  int_dialup = B28002_003E / B28002_001E * 100,
  # share HHs with smartphone only
  dev_phone = B28001_006E / B28001_001E * 100,
  # share HHs with no computing device
  dev_none = B28001_011E / B28001_001E * 100,
  # share HHs with no internet access
  int_none = B28002_013E / B28002_001E * 100,
  # share HHs with cellular internet only
  int_cell = B28002_006E / B28002_001E * 100,
  # share HHs with satellite internet only
  int_sat = B28002_010E / B28002_001E * 100,
  # share HHs with broadband internet
  int_bband = B28002_004E / B28002_001E * 100,
  # share HHs making <10k with no internet subscription
  int_none_10kless = B28004_005E / B28004_001E * 100,
  # share HHs making 10-19.9k with no internet subscription
  int_none_1019k = B28004_009E / B28004_001E * 100,
  # share HHs making 20-34.9k with no internet subscription
  int_none_2034k = B28004_013E / B28004_001E * 100,
  # share HHs making 35-49.9k with no internet subscription
  int_none_3549k = B28004_017E / B28004_001E * 100,
  # share HHs making 50-74.9k with no internet subscription
  int_none_5074k = B28004_021E / B28004_001E * 100,
  # share HHs making 75+k with no internet subscription
  int_none_75kovr = B28004_025E / B28004_001E * 100,
  # share of children (<18) without a computer
  nocomp_chd = B28005_007E / B28005_002E * 100,
  # share of working age (18-64) without a computer
  nocomp_adult = B28005_013E / B28005_008E * 100,
  # share of older adults (65+) without a computer
  nocomp_old = B28005_019E / B28005_014E * 100,
  # share persons in labor force without computer (no computer employed + no computer unemployed : total in labor force)
  nocomp_labor = (B28007_008E + B28007_014E) / B28007_002E * 100,
  # share persons employed without computer
  nocomp_emp = B28007_008E / B28007_003E * 100,
  # share persons unemployed without computer
  nocomp_unemp = B28007_014E / B28007_009E * 100
)


#
# Check missingness, split, write --------------------------------------------------------------------------------------
#

miss_var_summary(data)

write_rds(data, "./rivanna_data/working/appdata.Rds")