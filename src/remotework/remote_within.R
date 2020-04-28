vadata <- data %>% filter(STATEFP == 51)
iadata <- data %>% filter(STATEFP == 19)
ordata <- data %>% filter(STATEFP == 41)


#
# Quintiles --------------------------------------------------------------------------------------
#

# Find quintiles:
vadata <- vadata %>% mutate(internetQuint = ntile(internet, 5),
                            computerQuint = ntile(computer, 5),
                            occupQuint = ntile(occup, 5),
                            industrQuint = ntile(industr, 5))

ordata <- ordata %>% mutate(internetQuint = ntile(internet, 5),
                            computerQuint = ntile(computer, 5),
                            occupQuint = ntile(occup, 5),
                            industrQuint = ntile(industr, 5))

iadata <- iadata %>% mutate(internetQuint = ntile(internet, 5),
                            computerQuint = ntile(computer, 5),
                            occupQuint = ntile(occup, 5),
                            industrQuint = ntile(industr, 5))

# Did they place in 4 or 5th quintile?
vadata <- vadata %>% mutate(internetTop = ifelse(internetQuint >= 4, 1, 0),
                            computerTop = ifelse(computerQuint >= 4, 1, 0),
                            occupTop = ifelse(occupQuint >= 4, 1, 0),
                            industrTop = ifelse(industrQuint >= 4, 1, 0),
                            scoreTop = internetTop + computerTop + occupTop + industrTop)

iadata <- iadata %>% mutate(internetTop = ifelse(internetQuint >= 4, 1, 0),
                            computerTop = ifelse(computerQuint >= 4, 1, 0),
                            occupTop = ifelse(occupQuint >= 4, 1, 0),
                            industrTop = ifelse(industrQuint >= 4, 1, 0),
                            scoreTop = internetTop + computerTop + occupTop + industrTop)

ordata <- ordata %>% mutate(internetTop = ifelse(internetQuint >= 4, 1, 0),
                            computerTop = ifelse(computerQuint >= 4, 1, 0),
                            occupTop = ifelse(occupQuint >= 4, 1, 0),
                            industrTop = ifelse(industrQuint >= 4, 1, 0),
                            scoreTop = internetTop + computerTop + occupTop + industrTop)

# Vulnerability  
vadata <- vadata %>% mutate(vulnerability = case_when(
  internetTop + computerTop + occupTop + industrTop == 4 ~ "Very High",
  internetTop + computerTop + occupTop + industrTop == 3 ~ "High",
  internetTop + computerTop + occupTop + industrTop == 2 ~ "Medium",       
  internetTop + computerTop + occupTop + industrTop == 1 ~ "Low",   
  internetTop + computerTop + occupTop + industrTop == 0 ~ "None"))
vadata$vulnerability <- factor(vadata$vulnerability, levels = c("None", "Low", "Medium", "High", "Very High"), ordered = TRUE)

iadata <- iadata %>% mutate(vulnerability = case_when(
  internetTop + computerTop + occupTop + industrTop == 4 ~ "Very High",
  internetTop + computerTop + occupTop + industrTop == 3 ~ "High",
  internetTop + computerTop + occupTop + industrTop == 2 ~ "Medium",       
  internetTop + computerTop + occupTop + industrTop == 1 ~ "Low",   
  internetTop + computerTop + occupTop + industrTop == 0 ~ "None"))
iadata$vulnerability <- factor(iadata$vulnerability, levels = c("None", "Low", "Medium", "High", "Very High"), ordered = TRUE)

ordata <- ordata %>% mutate(vulnerability = case_when(
  internetTop + computerTop + occupTop + industrTop == 4 ~ "Very High",
  internetTop + computerTop + occupTop + industrTop == 3 ~ "High",
  internetTop + computerTop + occupTop + industrTop == 2 ~ "Medium",       
  internetTop + computerTop + occupTop + industrTop == 1 ~ "Low",   
  internetTop + computerTop + occupTop + industrTop == 0 ~ "None"))
ordata$vulnerability <- factor(ordata$vulnerability, levels = c("None", "Low", "Medium", "High", "Very High"), ordered = TRUE)

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
  scale_fill_viridis(name = "Vulnerability", guide = "legend", discrete = T) 

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
  scale_fill_viridis(name = "Vulnerability", guide = "legend", discrete = T)

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
  scale_fill_viridis(name = "Vulnerability", guide = "legend", discrete = T) 