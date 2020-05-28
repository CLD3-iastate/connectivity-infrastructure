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

# Digital connectivity
#a) percent households with no internet access or relying solely on cellular data to access the internet (Int1):
#b) percent households with no computing devices or relying solely on mobile devices to access the internet (Int2); 
#c) percent population with access to no providers or access to up to 10/1 advertised speeds only (Int3).

# a)
# B28002 PRESENCE AND TYPES OF INTERNET SUBSCRIPTIONS IN HOUSEHOLD
# _001 (total), _003 (only dial up), _006 (cellular data plan with no other type of internet subscription), _010 (only satellite), _013 (no internet access)

# B28002_001
# B28002_003
# B28002_006
# B28002_010
# B28002_013

# b)
# B28007 LABOR FORCE STATUS BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD 
# _002 (total in the civilian labor force: employed and unemployed), _008 (no computer employed) + _014 (no computer unemployed)

# B28007_002
# B28007_008
# B28007_014

# Work
#a) Industry: industries that overall engage less in working from home. 
# occupations: share of workers employed in service, natural, construction, maintenance, production, transportation, material moving, 
#             and military specific occupations (Occ) 
# b)industry: share of workers employed in construction, manufacturing, wholesale, retail, transportation and warehousing, utilities, 
#           and government, including armed forces (Ind).

# b) B08124 MEANS OF TRANSPORTATION TO WORK BY OCCUPATION
# _001 (total) - _043 (already work at home), 
# _010, _012, _013, _014, _017, _019, _020, _021, _024, _026, _027, _028, _031, _033, _034, _035, _038, _040, _041, _042

# B08124_001
# B08124_043
# B08124_010
# B08124_012
# B08124_013
# B08124_014
# B08124_017
# B08124_019
# B08124_020
# B08124_021
# B08124_024
# B08124_026
# B08124_027
# B08124_028
# B08124_031
# B08124_033
# B08124_034
# B08124_035
# B08124_038
# B08124_040
# B08124_041
# B08124_042

# a) B08126 MEANS OF TRANSPORTATION TO WORK BY INDUSTRY 
# _001 (total) - _091 (already work at home)
# B08126_001
# B08126_091
# B08126_018
# B08126_019
# B08126_020
# B08126_021
# B08126_022
# B08126_029
# B08126_030
# B08126_033
# B08126_034
# B08126_035
# B08126_036
# B08126_037
# B08126_044
# B08126_045
# B08126_048
# B08126_049
# B08126_050
# B08126_051
# B08126_052
# B08126_059
# B08126_060
# B08126_063
# B08126_064
# B08126_065
# B08126_066
# B08126_067
# B08126_074
# B08126_075
# B08126_078
# B08126_079
# B08126_080
# B08126_081
# B08126_082
# B08126_089
# B08126_090

# Select variables
vars <- c(
  # presence and types of internet
  "B28002_001", "B28002_003", "B28002_006", "B28002_010", "B28002_013",
  # presence of computer
  "B28007_002", "B28007_008", "B28007_014",
  # occupations
  "B08124_001", "B08124_043", "B08124_010", "B08124_012", "B08124_013", "B08124_014", 
  "B08124_017", "B08124_019", "B08124_020", "B08124_021", "B08124_024", "B08124_026", 
  "B08124_027", "B08124_028", "B08124_031", "B08124_033", "B08124_034", "B08124_035",
  "B08124_038", "B08124_040", "B08124_041", "B08124_042",
  # industries
  "B08126_001", "B08126_091", "B08126_018", "B08126_019", "B08126_020", "B08126_021", 
  "B08126_022", "B08126_029", "B08126_030", "B08126_033", "B08126_034", "B08126_035", "B08126_036", "B08126_037", 
  "B08126_044", "B08126_045", "B08126_048", "B08126_049", "B08126_050", "B08126_051", 
  "B08126_052", "B08126_059", "B08126_060", "B08126_063", "B08126_064", "B08126_065", 
  "B08126_066", "B08126_067", "B08126_074", "B08126_075", "B08126_078", "B08126_079", 
  "B08126_080", "B08126_081", "B08126_082", "B08126_089", "B08126_090"
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
  # internet access (only dial up/only cellular/only satellite/none : total)
  nointernet = (B28002_003E + B28002_006E + B28002_010E + B28002_013E) / B28002_001E * 100,
  # presence of computer for those in labor force (no computer employed + no computer unemployed : total in labor force)
  nocomputer = (B28007_008E + B28007_014E) / B28007_002E * 100,
  # occupations (not-wfh-friendly occupations / total occupations that don't already WFH)
  occup = (B08124_010E + B08124_012E + B08124_013E + B08124_014E + B08124_017E +
             B08124_019E + B08124_020E + B08124_021E + B08124_024E + B08124_026E +
             B08124_027E + B08124_028E + B08124_031E + B08124_033E + B08124_034E +
             B08124_035E + B08124_038E + B08124_040E + B08124_041E + B08124_042E) / (B08124_001E - B08124_043E) * 100,
  # industries (not-wfh-friendly industries / total in industries that don't already WFH)
  industr = (B08126_018E + B08126_019E + B08126_020E + B08126_021E + B08126_022E + B08126_029E +
               B08126_030E + B08126_033E + B08126_034E + B08126_035E + B08126_036E + B08126_037E +
               B08126_044E + B08126_045E + B08126_048E + B08126_049E + B08126_050E + B08126_051E +
               B08126_052E + B08126_059E + B08126_060E + B08126_063E + B08126_064E + B08126_065E +
               B08126_066E + B08126_067E + B08126_074E + B08126_075E + B08126_078E + B08126_079E +
               B08126_080E + B08126_081E + B08126_082E + B08126_089E + B08126_090E) / (B08126_001E - B08126_091E) * 100
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
vadata$occupQuint <- cut(vadata$occup, quantile(vadata$occup, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
vadata$industrQuint <- cut(vadata$industr, quantile(vadata$industr, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)

ordata$nointernetQuint <- cut(ordata$nointernet, quantile(ordata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$nocomputerQuint <- cut(ordata$nocomputer, quantile(ordata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$occupQuint <- cut(ordata$occup, quantile(ordata$occup, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
ordata$industrQuint <- cut(ordata$industr, quantile(ordata$industr, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)

iadata$nointernetQuint <- cut(iadata$nointernet, quantile(iadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$nocomputerQuint <- cut(iadata$nocomputer, quantile(iadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$occupQuint <- cut(iadata$occup, quantile(iadata$occup, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)
iadata$industrQuint <- cut(iadata$industr, quantile(iadata$industr, prob = seq(0, 1, length = 6), na.rm = TRUE), labels = FALSE, include.lowest = TRUE, right = FALSE)

# Get cutoffs for table
vaqint <- quantile(vadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqcomp <- quantile(vadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqoccup <- quantile(vadata$occup, prob = seq(0, 1, length = 6), na.rm = TRUE)
vaqind <- quantile(vadata$industr, prob = seq(0, 1, length = 6), na.rm = TRUE)

vaquintcuts <- bind_rows(vaqint, vaqcomp, vaqoccup, vaqind)
vaquintcuts$id <- c("Internet", "Computer", "Occupation", "Industry")
vaquintcuts

iaqint <- quantile(iadata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqcomp <- quantile(iadata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqoccup <- quantile(iadata$occup, prob = seq(0, 1, length = 6), na.rm = TRUE)
iaqind <- quantile(iadata$industr, prob = seq(0, 1, length = 6), na.rm = TRUE)

iaquintcuts <- bind_rows(iaqint, iaqcomp, iaqoccup, iaqind)
iaquintcuts$id <- c("Internet", "Computer", "Occupation", "Industry")
iaquintcuts

orqint <- quantile(ordata$nointernet, prob = seq(0, 1, length = 6), na.rm = TRUE)
orqcomp <- quantile(ordata$nocomputer, prob = seq(0, 1, length = 6), na.rm = TRUE)
orqoccup <- quantile(ordata$occup, prob = seq(0, 1, length = 6), na.rm = TRUE)
orqind <- quantile(ordata$industr, prob = seq(0, 1, length = 6), na.rm = TRUE)

orquintcuts <- bind_rows(orqint, orqcomp, orqoccup, orqind)
orquintcuts$id <- c("Internet", "Computer", "Occupation", "Industry")
orquintcuts

# Did they place in 4 or 5th quintile?
vadata <- vadata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            occupTop = ifelse(occupQuint >= 4, 1, 0),
                            industrTop = ifelse(industrQuint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + occupTop + industrTop)

iadata <- iadata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            occupTop = ifelse(occupQuint >= 4, 1, 0),
                            industrTop = ifelse(industrQuint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + occupTop + industrTop)

ordata <- ordata %>% mutate(nointernetTop = ifelse(nointernetQuint >= 4, 1, 0),
                            nocomputerTop = ifelse(nocomputerQuint >= 4, 1, 0),
                            occupTop = ifelse(occupQuint >= 4, 1, 0),
                            industrTop = ifelse(industrQuint >= 4, 1, 0),
                            scoreTop = nointernetTop + nocomputerTop + occupTop + industrTop)

# Vulnerability  
vadata <- vadata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + occupTop + industrTop == 4 ~ "Very High",
  nointernetTop + nocomputerTop + occupTop + industrTop == 3 ~ "High",
  nointernetTop + nocomputerTop + occupTop + industrTop == 2 ~ "Medium",       
  nointernetTop + nocomputerTop + occupTop + industrTop == 1 ~ "Low",   
  nointernetTop + nocomputerTop + occupTop + industrTop == 0 ~ "None"))
vadata$vulnerability <- factor(vadata$vulnerability, levels = c("None", "Low", "Medium", "High", "Very High"), ordered = TRUE)

vadata <- vadata %>% mutate(accessibility = case_when(
  nointernetTop + nocomputerTop + occupTop + industrTop == 4 ~ "Very Low",
  nointernetTop + nocomputerTop + occupTop + industrTop == 3 ~ "Low",
  nointernetTop + nocomputerTop + occupTop + industrTop == 2 ~ "Medium",       
  nointernetTop + nocomputerTop + occupTop + industrTop == 1 ~ "High",   
  nointernetTop + nocomputerTop + occupTop + industrTop == 0 ~ "Very High"))
vadata$accessibility <- factor(vadata$accessibility, levels = c("Very High", "High", "Medium", "Low", "Very Low"), ordered = TRUE)

iadata <- iadata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + occupTop + industrTop == 4 ~ "Very High",
  nointernetTop + nocomputerTop + occupTop + industrTop == 3 ~ "High",
  nointernetTop + nocomputerTop + occupTop + industrTop == 2 ~ "Medium",       
  nointernetTop + nocomputerTop + occupTop + industrTop == 1 ~ "Low",   
  nointernetTop + nocomputerTop + occupTop + industrTop == 0 ~ "None"))
iadata$vulnerability <- factor(iadata$vulnerability, levels = c("None", "Low", "Medium", "High", "Very High"), ordered = TRUE)

iadata <- iadata %>% mutate(accessibility = case_when(
  nointernetTop + nocomputerTop + occupTop + industrTop == 4 ~ "Very Low",
  nointernetTop + nocomputerTop + occupTop + industrTop == 3 ~ "Low",
  nointernetTop + nocomputerTop + occupTop + industrTop == 2 ~ "Medium",       
  nointernetTop + nocomputerTop + occupTop + industrTop == 1 ~ "High",   
  nointernetTop + nocomputerTop + occupTop + industrTop == 0 ~ "Very High"))
iadata$accessibility <- factor(iadata$accessibility, levels = c("Very High", "High", "Medium", "Low", "Very Low"), ordered = TRUE)

ordata <- ordata %>% mutate(vulnerability = case_when(
  nointernetTop + nocomputerTop + occupTop + industrTop == 4 ~ "Very High",
  nointernetTop + nocomputerTop + occupTop + industrTop == 3 ~ "High",
  nointernetTop + nocomputerTop + occupTop + industrTop == 2 ~ "Medium",       
  nointernetTop + nocomputerTop + occupTop + industrTop == 1 ~ "Low",   
  nointernetTop + nocomputerTop + occupTop + industrTop == 0 ~ "None"))
ordata$vulnerability <- factor(ordata$vulnerability, levels = c("None", "Low", "Medium", "High", "Very High"), ordered = TRUE)
 
ordata <- ordata %>% mutate(accessibility = case_when(
  nointernetTop + nocomputerTop + occupTop + industrTop == 4 ~ "Very Low",
  nointernetTop + nocomputerTop + occupTop + industrTop == 3 ~ "Low",
  nointernetTop + nocomputerTop + occupTop + industrTop == 2 ~ "Medium",       
  nointernetTop + nocomputerTop + occupTop + industrTop == 1 ~ "High",   
  nointernetTop + nocomputerTop + occupTop + industrTop == 0 ~ "Very High"))
ordata$accessibility <- factor(ordata$accessibility, levels = c("Very High", "High", "Medium", "Low", "Very Low"), ordered = TRUE)

# Write out
write_rds(iadata, "./rivanna_data/working/dashboard/ia_work.Rds")
write_rds(vadata, "./rivanna_data/working/dashboard/va_work.Rds")
write_rds(ordata, "./rivanna_data/working/dashboard/or_work.Rds")

# Plot Iowa
ggplot() +
  geom_sf(data = iadata, size = 0.2, aes(fill = vulnerability)) +
  labs(title = "Iowa County-Level Telework Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.\n
       \n
       Vulnerability calculated using information on % households with no broadband internet subscription,\n
       % persons in labor force with no computer available, % persons employed in telework unfriendly\n
       occupations not working remotely, and % persons employed in telework unfriendly industries not\n
       working remotely.\n
       \n
       Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 4 indicators\n
       considered, high if on 3 indicators, medium if on 2, low if on 1, and no vulnerability if they did not\n
       place in the 4th or 5th quintile on any indicators. Quintiles were calculated using Iowa data only; the\n
       resulting map shows within-state comparisons.") +
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
  labs(title = "Virginia County-Level Telework Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.\n
       \n
       Vulnerability calculated using information on % households with no broadband internet subscription,\n
       % persons in labor force with no computer available, % persons employed in telework unfriendly\n
       occupations not working remotely, and % persons employed in telework unfriendly industries not\n
       working remotely.\n
       \n
       Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 4 indicators\n
       considered, high if on 3 indicators, medium if on 2, low if on 1, and no vulnerability if they did not\n
       place in the 4th or 5th quintile on any indicators. Quintiles were calculated using Iowa data only; the\n
       resulting map shows within-state comparisons.") +
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
  labs(title = "Oregon County-Level Telework Vulnerability", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.\n
                    \n
       Vulnerability calculated using information on % households with no broadband internet subscription,\n
       % persons in labor force with no computer available, % persons employed in telework unfriendly\n
       occupations not working remotely, and % persons employed in telework unfriendly industries not\n
       working remotely.\n
       \n
       Counties are considered very high vulnerability if they placed in 4th or 5th quintile on all 4 indicators\n
       considered, high if on 3 indicators, medium if on 2, low if on 1, and no vulnerability if they did not\n
       place in the 4th or 5th quintile on any indicators. Quintiles were calculated using Iowa data only; the\n
       resulting map shows within-state comparisons.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis(name = "Relative\nVulnerability", guide = "legend", discrete = T) 