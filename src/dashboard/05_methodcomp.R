library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(ggthemes)
library(viridis)
library(scales)

# Read in data
data_med <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_med.Rds")
data_work <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_work.Rds")
data_edu <- read_rds("~/Git/gates/rivanna_data/working/dashboard/data_edu.Rds")

# Create "average quintile" scores
data_edu <- data_edu %>% mutate(avgquint = (nointernetQuint + nocomputerQuint + ink12Quint)/3)
data_work <- data_work %>% mutate(avgquint = (nointernetQuint + nocomputerQuint + occupQuint + industrQuint)/4)
data_med <- data_med %>% mutate(avgquint = (nointernetQuint + nocomputerQuint + menthdaysQuint + uninsQuint + menthprovQuint)/5)

# Rescale
data_edu <- data_edu %>% mutate(r_avgquint = rescale(avgquint),
                                r_scoretop = rescale(scoreTop))
data_work <- data_work %>% mutate(r_avgquint = rescale(avgquint),
                                r_scoretop = rescale(scoreTop))
data_med <- data_med %>% mutate(r_avgquint = rescale(avgquint),
                                  r_scoretop = rescale(scoreTop))

# Compare
hist(data_edu$avgquint)
hist(data_edu$scoreTop)

hist(data_work$avgquint)
hist(data_work$scoreTop)

hist(data_med$avgquint)
hist(data_med$scoreTop)

# Plot
# OREGON TELEWORK
ggplot() +
  geom_sf(data = data_work[data_work$STATEFP == 41, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Oregon Telework Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_work[data_work$STATEFP == 41, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Oregon Telework Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# IOWA TELEWORK
ggplot() +
  geom_sf(data = data_work[data_work$STATEFP == 19, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Iowa Telework Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_work[data_work$STATEFP == 19, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Iowa Telework Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# VIRGINIA TELEWORK
ggplot() +
  geom_sf(data = data_work[data_work$STATEFP == 51, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Virginia Telework Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_work[data_work$STATEFP == 51, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Virginia Telework Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 


# IOWA REMOTE ED
ggplot() +
  geom_sf(data = data_edu[data_edu$STATEFP == 19, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Iowa Remote Ed Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_edu[data_edu$STATEFP == 19, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Iowa Remote Ed Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25))

# OREGON REMOTE ED
ggplot() +
  geom_sf(data = data_edu[data_edu$STATEFP == 41, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Oregon Remote Ed Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_edu[data_edu$STATEFP == 41, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Oregon Remote Ed Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25))

# VIRGINIA REMOTE ED
ggplot() +
  geom_sf(data = data_edu[data_edu$STATEFP == 51, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Virginia Remote Ed Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_edu[data_edu$STATEFP == 51, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Virginia Remote Ed Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25))


# VIRGINIA TELEHEALTH
ggplot() +
  geom_sf(data = data_med[data_med$STATEFP == 51, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Virginia Telehealth Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_med[data_med$STATEFP == 51, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Virginia Telehealth Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# OREGON TELEHEALTH
ggplot() +
  geom_sf(data = data_med[data_med$STATEFP == 41, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Oregon Telehealth Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_med[data_med$STATEFP == 41, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Oregon Telehealth Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# IOWA TELEHEALTH
ggplot() +
  geom_sf(data = data_med[data_med$STATEFP == 19, ], size = 0.2, aes(fill = r_scoretop)) +
  labs(title = "Iowa Telehealth Vulnerability: Top Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 

# Plot
ggplot() +
  geom_sf(data = data_med[data_med$STATEFP == 19, ], size = 0.2, aes(fill = r_avgquint)) +
  labs(title = "Iowa Telehealth Vulnerability: Average Quintile", 
       caption = "Data source: American Community Survey 2014-18 (5-year) estimates.") +
  theme_map() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 8, lineheight = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 11),
        legend.position = "left",
        plot.caption.position = "plot",
        plot.title.position = "plot") +
  scale_fill_viridis_c(name = "Relative\nVulnerability", limits = c(0, 1), breaks = seq(0, 1, 0.25)) 








