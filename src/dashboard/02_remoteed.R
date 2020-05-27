library(tidycensus)
library(acs)
library(dplyr)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)
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
# B28005 AGE BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD 
# share of children (<18) without a computer, 002 (all population <18 in households), 007 (no computer)

# B28005_002
# B28005_007

# c)
# B14001	SCHOOL ENROLLMENT BY LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER 
# share of enroled in K-12 (assuming this is <18yrs), _001 (total population), _005, _006, _007 (grades 1-12)

# B14001_001
# B14001_005
# B14001_006
# B14001_007

# Select variables
vars <- c(
  # presence and types of internet
  "B28002_001", "B28002_013",
  # presence of computer by age
  "B28005_002", "B28005_007",
  # school enrolment
  "B14001_001", "B14001_005", "B14001_006", "B14001_007"
)                 


#
# Get variables for counties --------------------------------------------------------------------------------------
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
# Calculate --------------------------------------------------------------------------------------
#

data <- acsdata %>% transmute(
  STATEFP = STATEFP, 
  COUNTYFP = COUNTYFP, 
  COUNTYNS = COUNTYNS, 
  AFFGEOID = AFFGEOID, 
  GEOID = GEOID, 
  LSAD = LSAD, 
  NAME.x = NAME.x, 
  NAME.y = NAME.y,
  ALAND = ALAND, 
  AWATER = AWATER, 
  geometry = geometry,
  # households without internet (no internet / all)
  nointernet = B28002_013E /B28002_001E * 100,
  # presence of computer by age (no computer / all <18)
  nocomputer = B28005_007E / B28005_002E * 100,
  # school enrolment (enroled in K12 / all)
  ink12 = (B14001_005E + B14001_006E + B14001_007E) / B14001_001E * 100
)

any(is.na(data))

# Filter by state
vadata <- data %>% filter(STATEFP == 51)
iadata <- data %>% filter(STATEFP == 19)
ordata <- data %>% filter(STATEFP == 41)


#
# Quintiles --------------------------------------------------------------------------------------
#

# Find quintiles:
vadata$nointernetQuint <- cut(vadata$nointernet, quantile(vadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
vadata$nocomputerQuint <- cut(vadata$nocomputer, quantile(vadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
vadata$ink12Quint <- cut(vadata$ink12, quantile(vadata$ink12, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)

ordata$nointernetQuint <- cut(ordata$nointernet, quantile(ordata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$nocomputerQuint <- cut(ordata$nocomputer, quantile(ordata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$ink12Quint <- cut(ordata$ink12, quantile(ordata$ink12, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)

iadata$nointernetQuint <- cut(iadata$nointernet, quantile(iadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$nocomputerQuint <- cut(iadata$nocomputer, quantile(iadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$ink12Quint <- cut(iadata$ink12, quantile(iadata$ink12, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)

# Get cutoffs for table
vaqnoint <- quantile(vadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqnocomp <- quantile(vadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqink12 <- quantile(vadata$ink12, prob = seq(0, 1, length = 6), na.rm = TRUE)

vaquintcuts <- bind_rows(vaqnoint, vaqnocomp, vaqink12)
vaquintcuts$id <- c("No internet", "No computer", "In K12")
vaquintcuts

iaqnoint <- quantile(iadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqnocomp <- quantile(iadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqink12 <- quantile(iadata$ink12, prob = seq(0, 1, length = 6), na.rm = TRUE)

iaquintcuts <- bind_rows(iaqnoint, iaqnocomp, iaqink12)
iaquintcuts$id <- c("No internet", "No computer", "In K12")
iaquintcuts

orqnoint <- quantile(ordata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
orqnocomp <- quantile(ordata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
orqink12 <- quantile(ordata$ink12, prob = seq(0, 1, length = 6), na.rm = TRUE)

orquintcuts <- bind_rows(orqnoint, orqnocomp, orqink12)
orquintcuts$id <- c("No internet", "No computer", "In K12")
orquintcuts

# Did they place in 4 or 5th quintile?
vadata <- vadata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            ink12Top = ifelse(ink12Quint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + ink12Top)

iadata <- iadata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            ink12Top = ifelse(ink12Quint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + ink12Top)

ordata <- ordata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            ink12Top = ifelse(ink12Quint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + ink12Top)

# Vulnerability  
vadata <- vadata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + ink12Top == 3 ~ "High",
  nointernetTop + nocomputerTop + ink12Top == 2 ~ "Medium",
  nointernetTop + nocomputerTop + ink12Top == 1 ~ "Low",      
  nointernetTop + nocomputerTop + ink12Top == 0 ~ "None"))
vadata$vulnerability <- factor(vadata$vulnerability, levels = c("None", "Low", "Medium", "High"), ordered = TRUE)

iadata <- iadata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + ink12Top == 3 ~ "High",
  nointernetTop + nocomputerTop + ink12Top == 2 ~ "Medium",
  nointernetTop + nocomputerTop + ink12Top == 1 ~ "Low",      
  nointernetTop + nocomputerTop + ink12Top == 0 ~ "None"))
iadata$vulnerability <- factor(iadata$vulnerability, levels = c("None", "Low", "Medium", "High"), ordered = TRUE)

ordata <- ordata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + ink12Top == 3 ~ "High",
  nointernetTop + nocomputerTop + ink12Top == 2 ~ "Medium",
  nointernetTop + nocomputerTop + ink12Top == 1 ~ "Low",      
  nointernetTop + nocomputerTop + ink12Top == 0 ~ "None"))
ordata$vulnerability <- factor(ordata$vulnerability, levels = c("None", "Low", "Medium", "High"), ordered = TRUE)

# Write out
write_rds(iadata, "./rivanna_data/working/dashboard/ia_edu.Rds")
write_rds(vadata, "./rivanna_data/working/dashboard/va_edu.Rds")
write_rds(ordata, "./rivanna_data/working/dashboard/or_edu.Rds")

# Plot Iowa
ggplot() +
  geom_sf(data = iadata, size = 0.2, aes(fill = vulnerability)) +
  labs(title = "Iowa County-Level K-12 Remote Education Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.\n
       \n
       Vulnerability calculated using information on % households with no internet access subscription,\n
       % children (age <18) without a computer, and % population enroled in K-12.\n
       \n
       Counties are considered high vulnerability if they placed in 4th or 5th quintile on all 3 indicators\n
       considered, medium if on 2 indicators, low if on 1, and no vulnerability if they did not\n
       place in the 4th or 5th quintile on any indicators.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis(name = "Relative\nVulnerability", guide = "legend", discrete = T) 

# Plot Virginia
ggplot() +
  geom_sf(data = vadata, size = 0.2, aes(fill = vulnerability)) +
  labs(title = "Virginia County-Level K-12 Remote Education Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.\n
       \n
       Vulnerability calculated using information on % households with no internet access subscription,\n
       % children (age <18) without a computer, and % population enroled in K-12.\n
       \n
       Counties are considered high vulnerability if they placed in 4th or 5th quintile on all 3 indicators\n
       considered, medium if on 2 indicators, low if on 1, and no vulnerability if they did not\n
       place in the 4th or 5th quintile on any indicators.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis(name = "Relative\nVulnerability", guide = "legend", discrete = T)

# Plot Oregon
ggplot() +
  geom_sf(data = ordata, size = 0.2, aes(fill = vulnerability)) +
  labs(title = "Oregon County-Level Remote Education Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.\n
       \n
       Vulnerability calculated using information on % households with no internet access subscription,\n
       % children (age <18) without a computer, and % population enroled in K-12.\n
       \n
       Counties are considered high vulnerability if they placed in 4th or 5th quintile on all 3 indicators\n
       considered, medium if on 2 indicators, low if on 1, and no vulnerability if they did not\n
       place in the 4th or 5th quintile on any indicators.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis(name = "Relative\nVulnerability", guide = "legend", discrete = T) 