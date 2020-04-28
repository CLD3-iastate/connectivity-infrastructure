library(readxl)
library(dplyr)

coltypes <- c(rep("text", 3), rep("numeric", 144))
data <- read_excel("./rivanna_data/original/USDA Food Research Atlas 2015.xlsx", sheet = 3, col_names = TRUE,
                   col_types = coltypes, progress = TRUE)

data <- data %>% filter(State == "Iowa" | State == "Virginia" | State == "Oregon")
