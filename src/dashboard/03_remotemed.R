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
# Select variables -------------------------------------------------------------------------------------------------------------
#

# a)
# B28002 PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
# share of households with no internet access
# _001 (total), _013 (no internet access)

# B28002_001
# B28002_013

# b)
# B28003	PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD
# no computer, _001 (total), _006 (no computer)

# B28003_001
# B28003_006

# c)
# B27020 HEALTH INSURANCE COVERAGE STATUS AND TYPE BY CITIZENSHIP STATUS , Civilian noninstitutionalized population
# no insurance, _001 (total), _006, _012, _017 (no coverage by citizenship status)

# B27020_001
# B27020_006
# B27020_012
# B27020_017

# Select variables
vars <- c(
  # presence and types of internet
  "B28002_001", "B28002_013",
  # presence of computer 
  "B28003_001", "B28003_006",
  # no insurance
  "B27020_001",
  "B27020_006",
  "B27020_012",
  "B27020_017"
)                 


#
# Get variables for counties from ACS--------------------------------------------------------------------------------------
#

# 19 (iowa), 51 (virginia), 41 (oregon)
state_fips <- c(19, 41, 51)

acsdata <- get_acs(geography = "county", state = state_fips[1], variables = vars, year = 2018, survey = "acs5", 
                   cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
for(i in 2:length(state_fips)){
  tmp <- get_acs(geography = "county", state = state_fips[i], variables = vars, year = 2018, survey = "acs5", 
                 cache_table = TRUE, output = "wide", geometry = TRUE, keep_geo_vars = TRUE)
  acsdata <- rbind(acsdata, tmp)
}

any(is.na(acsdata))


#
# Get variables for counties from County Health Rankings --------------------------------------------------------------------------------------
#

chrdata <- read.csv("./rivanna_data/working/countyhealthrankings/mentalhealth.csv", header = TRUE,
                    colClasses = c("GEOID" = "character", "state" = "character", "county" = "character",
                                   "avgnum_poormenth" = "numeric", "pct_unins" = "numeric",
                                   "menthprov_per100k" = "numeric"), stringsAsFactors = FALSE)


# avgnum_poormenth = average number of poor mental health days, 2017, Behavioral Risk Factor Surveillance Survey (https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-outcomes/quality-of-life/poor-mental-health-days)
# pct_unins = percent uninsured, 2017, Small Area Health Insurance Estimates (https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/clinical-care/access-to-care/uninsured)
# menthprov_per100k = number of mental health providers per 100,000 population, 2019, CMS National Provider Identification (https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/clinical-care/access-to-care/mental-health-providers)

# Discard uninsured, use ACS
chrdata$pct_unins <- NULL


#
# Join --------------------------------------------------------------------------------------
#

setdiff(chrdata$GEOID, acsdata$GEOID)
setdiff(acsdata$GEOID, chrdata$GEOID)

data <- left_join(acsdata, chrdata, by = "GEOID")


#
# Calculate --------------------------------------------------------------------------------------
#

data <- data %>% transmute(
  STATEFP = STATEFP, 
  COUNTYFP = COUNTYFP, 
  COUNTYNS = COUNTYNS, 
  AFFGEOID = AFFGEOID, 
  GEOID = GEOID, 
  LSAD = LSAD, 
  NAME.x = NAME.x, 
  ALAND = ALAND, 
  AWATER = AWATER, 
  geometry = geometry,
  # households without internet (no internet / all)
  nointernet = B28002_013E /B28002_001E * 100,
  # presence of computer (no computer / all)
  nocomputer = B28003_006E / B28003_001E * 100,
  # percent uninsured
  pct_unins = (B27020_006E + B27020_012E + B27020_017E) / B27020_001E * 100,
  # average number of poor mental health days
  avgnum_poormenth = avgnum_poormenth,
  # mental health providers per 100k population
  menthprov_per100k = menthprov_per100k
)

any(is.na(data))
miss_var_summary(data) # menthprov_per100k data missing for 14 counties total

# Filter by state
vadata <- data %>% filter(STATEFP == 51)
iadata <- data %>% filter(STATEFP == 19)
ordata <- data %>% filter(STATEFP == 41)

miss_var_summary(vadata) # 4
miss_var_summary(ordata) # 1
miss_var_summary(iadata) # 9 


#
# Quintiles --------------------------------------------------------------------------------------
#

# Find quintiles (note: providers per 100k will have to be reverse-coded)
vadata$nointernetQuint <- cut(vadata$nointernet, quantile(vadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
vadata$nocomputerQuint <- cut(vadata$nocomputer, quantile(vadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
vadata$menthdaysQuint <- cut(vadata$avgnum_poormenth, quantile(vadata$avgnum_poormenth, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
vadata$uninsQuint <- cut(vadata$pct_unins, quantile(vadata$pct_unins, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
vadata$menthprovQuint <- cut(vadata$menthprov_per100k, quantile(vadata$menthprov_per100k, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
vadata$menthprovQuint <- case_when(vadata$menthprovQuint == 5 ~ 1,
                                   vadata$menthprovQuint == 4 ~ 2,
                                   vadata$menthprovQuint == 3 ~ 3,
                                   vadata$menthprovQuint == 2 ~ 4,
                                   vadata$menthprovQuint == 1 ~ 5,
                                   is.na(vadata$menthprovQuint) ~ NA_real_)

ordata$nointernetQuint <- cut(ordata$nointernet, quantile(ordata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$nocomputerQuint <- cut(ordata$nocomputer, quantile(ordata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$menthdaysQuint <- cut(ordata$avgnum_poormenth, quantile(ordata$avgnum_poormenth, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$uninsQuint <- cut(ordata$pct_unins, quantile(ordata$pct_unins, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$menthprovQuint <- cut(ordata$menthprov_per100k, quantile(ordata$menthprov_per100k, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$menthprovQuint <- case_when(ordata$menthprovQuint == 5 ~ 1,
                                   ordata$menthprovQuint == 4 ~ 2,
                                   ordata$menthprovQuint == 3 ~ 3,
                                   ordata$menthprovQuint == 2 ~ 4,
                                   ordata$menthprovQuint == 1 ~ 5,
                                   is.na(ordata$menthprovQuint) ~ NA_real_)

iadata$nointernetQuint <- cut(iadata$nointernet, quantile(iadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$nocomputerQuint <- cut(iadata$nocomputer, quantile(iadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$menthdaysQuint <- cut(iadata$avgnum_poormenth, quantile(iadata$avgnum_poormenth, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$uninsQuint <- cut(iadata$pct_unins, quantile(iadata$pct_unins, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$menthprovQuint <- cut(iadata$menthprov_per100k, quantile(iadata$menthprov_per100k, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$menthprovQuint <- case_when(iadata$menthprovQuint == 5 ~ 1,
                                   iadata$menthprovQuint == 4 ~ 2,
                                   iadata$menthprovQuint == 3 ~ 3,
                                   iadata$menthprovQuint == 2 ~ 4,
                                   iadata$menthprovQuint == 1 ~ 5,
                                   is.na(iadata$menthprovQuint) ~ NA_real_)

# Get cutoffs for table
vaqnoint <- quantile(vadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqnocomp <- quantile(vadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqpoorment <- quantile(vadata$avgnum_poormenth, prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqnumprov <- quantile(desc(vadata$menthprov_per100k), prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqunins <- quantile(vadata$pct_unins, prob = seq(0, 1, length = 6), na.rm = TRUE)

vaquintcuts <- bind_rows(vaqnoint, vaqnocomp, vaqpoorment, vaqnumprov, vaqunins)
vaquintcuts$id <- c("No internet", "No computer", "Num poor mental health", "Num mental health prov", "Pct uninsured")
vaquintcuts

iaqnoint <- quantile(iadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqnocomp <- quantile(iadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqpoorment <- quantile(iadata$avgnum_poormenth, prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqnumprov <- quantile(desc(iadata$menthprov_per100k), prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqunins <- quantile(iadata$pct_unins, prob = seq(0, 1, length = 6), na.rm = TRUE)

iaquintcuts <- bind_rows(iaqnoint, iaqnocomp, iaqpoorment, iaqnumprov, iaqunins)
iaquintcuts$id <- c("No internet", "No computer", "Num poor mental health", "Num mental health prov", "Pct uninsured")
iaquintcuts

orqnoint <- quantile(ordata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
orqnocomp <- quantile(ordata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
orqpoorment <- quantile(ordata$avgnum_poormenth, prob = seq(0, 1, length = 6), na.rm = TRUE)
orqnumprov <- quantile(desc(ordata$menthprov_per100k), prob = seq(0, 1, length = 6), na.rm = TRUE)
orqunins <- quantile(ordata$pct_unins, prob = seq(0, 1, length = 6), na.rm = TRUE)

orquintcuts <- bind_rows(orqnoint, orqnocomp, orqpoorment, orqnumprov, orqunins)
orquintcuts$id <- c("No internet", "No computer", "Num poor mental health", "Num mental health prov", "Pct uninsured")
orquintcuts

# Did they place in 4 or 5th quintile? 
# This handles NAs. Value ends up being NA.
vadata <- vadata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            menthdaysTop = ifelse(menthdaysQuint >= 4, 1, 0),
                            menthprovTop = ifelse(menthprovQuint >= 4, 1, 0),
                            uninsTop = ifelse(uninsQuint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop)

iadata <- iadata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            menthdaysTop = ifelse(menthdaysQuint >= 4, 1, 0),
                            menthprovTop = ifelse(menthprovQuint >= 4, 1, 0),
                            uninsTop = ifelse(uninsQuint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop)

ordata <- ordata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            menthdaysTop = ifelse(menthdaysQuint >= 4, 1, 0),
                            menthprovTop = ifelse(menthprovQuint >= 4, 1, 0),
                            uninsTop = ifelse(uninsQuint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop)

# Vulnerability  
# This handles NAs. Value ends up being NA.
vadata <- vadata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 5 ~ "Very High",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 4 ~ "High",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 3 ~ "Medium",      
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 2 ~ "Low",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 1 ~ "Very Low",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 0 ~ "None"))
vadata$vulnerability <- factor(vadata$vulnerability, levels = c("None", "Very Low", "Low", "Medium", "High", "Very High"), ordered = TRUE)

iadata <- iadata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 5 ~ "Very High",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 4 ~ "High",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 3 ~ "Medium",      
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 2 ~ "Low",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 1 ~ "Very Low",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 0 ~ "None"))
iadata$vulnerability <- factor(iadata$vulnerability, levels = c("None", "Very Low", "Low", "Medium", "High", "Very High"), ordered = TRUE)

ordata <- ordata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 5 ~ "Very High",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 4 ~ "High",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 3 ~ "Medium",      
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 2 ~ "Low",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 1 ~ "Very Low",
  nointernetTop + nocomputerTop + menthdaysTop + menthprovTop + uninsTop == 0 ~ "None"))
ordata$vulnerability <- factor(ordata$vulnerability, levels = c("None", "Very Low", "Low", "Medium", "High", "Very High"), ordered = TRUE)

# Write out
write_rds(iadata, "./rivanna_data/working/dashboard/ia_med.Rds")
write_rds(vadata, "./rivanna_data/working/dashboard/va_med.Rds")
write_rds(ordata, "./rivanna_data/working/dashboard/or_med.Rds")

# Plot Iowa
ggplot() +
  geom_sf(data = iadata, size = 0.2, aes(fill = vulnerability)) +
  labs(title = "Iowa County-Level Mental Telehealth Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates, County Health Rankings 2020.\n
       \n
       Vulnerability calculated using information on % households with no internet access, % households with\n
       no computer, average number of poor mental health dates, number of mental health providers per 100,000\n
       population, and % uninsured population under age 65. Number of mental health providers per 100,000 was\n
       reverse-coded for consistency with higher values indicating greater vulnerability.\n
       \n
       Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 5 indicators\n
       considered, high if on 4 indicators, medium if on 3, very low if on 2, low if on 1, and no vulnerability\n
       if they did not place in the 4th or 5th quintile on any indicators.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis(name = "Relative\nVulnerability", guide = "legend", discrete = T,
                     na.value = "#f0f0f0") 

# Plot Virginia
ggplot() +
  geom_sf(data = vadata, size = 0.2, aes(fill = vulnerability)) +
  labs(title = "Virginia County-Level Mental Telehealth Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates, County Health Rankings 2020.\n
       \n
       Vulnerability calculated using information on % households with no internet access, % households with\n
       no computer, average number of poor mental health dates, number of mental health providers per 100,000\n
       population, and % uninsured population under age 65. Number of mental health providers per 100,000 was\n
       reverse-coded for consistency with higher values indicating greater vulnerability.\n
       \n
       Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 5 indicators\n
       considered, high if on 4 indicators, medium if on 3, very low if on 2, low if on 1, and no vulnerability\n
       if they did not place in the 4th or 5th quintile on any indicators.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis(name = "Relative\nVulnerability", guide = "legend", discrete = T,
                     na.value = "#f0f0f0") 

# Plot Oregon
ggplot() +
  geom_sf(data = ordata, size = 0.2, aes(fill = vulnerability)) +
  labs(title = "Oregon County-Level Mental Telehealth Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates, County Health Rankings 2020.\n
       \n
       Vulnerability calculated using information on % households with no internet access, % households with\n
       no computer, average number of poor mental health dates, number of mental health providers per 100,000\n
       population, and % uninsured population under age 65. Number of mental health providers per 100,000 was\n
       reverse-coded for consistency with higher values indicating greater vulnerability.\n
       \n
       Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 5 indicators\n
       considered, high if on 4 indicators, medium if on 3, very low if on 2, low if on 1, and no vulnerability\n
       if they did not place in the 4th or 5th quintile on any indicators.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis(name = "Relative\nVulnerability", guide = "legend", discrete = T,
                     na.value = "#f0f0f0") 

