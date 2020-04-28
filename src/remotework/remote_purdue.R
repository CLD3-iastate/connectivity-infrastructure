library(readr)
library(tigris)
library(sf)
library(dplyr)
library(stringr)

# Read in and filter
purdue <- read_csv("./Git/fairfax/rivanna_data/original/purdue/purdue.csv")

purdueIowa <- purdue %>% filter(State == "Iowa")
purdueVirginia <- purdue %>% filter(State == "Virginia")
purdueOregon <- purdue %>% filter(State == "Oregon")

# Virginia has consolidated independent cities. Edited that manually. 

# Retrieve geographies and convert
iowaCounties <- counties("Iowa", cb = FALSE, year = 2018, refresh = TRUE)
virginiaCounties <- counties("Virginia", cb = FALSE, year = 2018, refresh = TRUE)
oregonCounties <- counties("Oregon", cb = FALSE, year = 2018, refresh = TRUE)

iowaCounties <- st_as_sf(iowaCounties)
virginiaCounties <- st_as_sf(virginiaCounties)
oregonCounties <- st_as_sf(oregonCounties)

# Join
piowa <- left_join(purdueIowa, iowaCounties, by = c("Name" = "NAME"))
pvirginia <- left_join(purdueVirginia, virginiaCounties, by = c("Name" = "NAME"))
poregon <- left_join(purdueOregon, oregonCounties, by = c("Name" = "NAME"))

piowa <- st_as_sf(piowa)
pvirginia <- st_as_sf(pvirginia)
poregon <- st_as_sf(poregon)

# Prep
piowa$Vulnerability <- factor(piowa$Vulnerability, levels = c("No", "Low", "Moderate", "High"), ordered = TRUE)
pvirginia$Vulnerability <- factor(pvirginia$Vulnerability, levels = c("No", "Low", "Moderate", "High"), ordered = TRUE)
poregon$Vulnerability <- factor(poregon$Vulnerability, levels = c("No", "Low", "Moderate", "High"), ordered = TRUE)

# Plot
iowa <- ggplot() +
  geom_sf(data = piowa, size = 0.2, aes(fill = Vulnerability)) +
  labs(title = "Iowa") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "plain", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "none") +
  scale_fill_viridis(name = "Vulnerability", guide = "legend", discrete = T) 

oregon <- ggplot() +
  geom_sf(data = poregon, size = 0.2, aes(fill = Vulnerability)) +
  labs(title = "Oregon") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "plain", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "none") +
  scale_fill_viridis(name = "Vulnerability", guide = "legend", discrete = T) 

virginia <- ggplot() +
  geom_sf(data = pvirginia, size = 0.2, aes(fill = Vulnerability)) +
  labs(title = "Virginia") +
  theme_map() +
  theme(plot.title = element_text(size = 13, face = "plain", hjust = 0.5),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.box.just = "center") +
  guides(fill = guide_legend(title.hjust = 2)) + 
  scale_fill_viridis(name = "Vulnerability", guide = "legend", discrete = T)

prow <- plot_grid(
  iowa + theme(legend.position = "none"),
  oregon + theme(legend.position = "none"),
  virginia + theme(legend.position = "none"),
  align = "v",
  hjust = -1,
  ncol = 3)

legend_b <- get_legend(virginia + theme(legend.direction = "horizontal", 
                                        legend.justification = "right", 
                                        legend.box.just = "bottom",
                                        legend.margin = margin(c(1,10,5,5))))

test <- plot_grid(prow, legend_b, ncol = 1, rel_heights = c(3.5, 0.5))
annotate_figure(test, 
                top = text_grob("County-Level Telework Vulnerability", face = "bold", size = 14),
                bottom = text_grob("Data source: Purdue Center for Regional Development; American Community Survey 2014-18, Federal Communications Commission 2018.\n
                    \n
       Vulnerability calculated using information on % households with no internet access or relying only on cellular data for internet access;\n
       % households with no computing devices or relying only on mobile devices for internet access; % population with access to no providers or\n
       access <10/1 advertised speeds only; % workers employed in service, natural, construction, maintenance, production, transportation, material\n
       moving, and military specific occupations; and % workers employed in construction, manufacturing, wholesale, retail, transportation and\n 
       warehousing, utilities, and government, including armed forces.\n
       \n
       Counties are considered high vulnerability if they placed in 4th or 5th quintile on all 5 indicators considered, moderate if on 4 indicators, low if\n
       on 1, and no vulnerability if they did not place in the 4th or 5th quintile on any indicators. Quintiles were calculated using national data.", 
                                   just = "right", x = 0.99, lineheight = 0.5, size = 9))



