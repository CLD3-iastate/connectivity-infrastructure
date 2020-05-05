library(readr)
library(dplyr)


#
# Prepare data ------------------------------------------------------------------------------------
#

# Mental health
ia_med <- read_rds("./rivanna_data/working/dashboard/ia_med.Rds")
va_med <- read_rds("./rivanna_data/working/dashboard/va_med.Rds")
or_med <- read_rds("./rivanna_data/working/dashboard/or_med.Rds")

ia_med$NAME.x <- paste0(ia_med$NAME.x, " County, Iowa")
va_med$NAME.x <- paste0(va_med$NAME.x, " County, Virginia")
or_med$NAME.x <- paste0(or_med$NAME.x, " County, Oregon")

data_med <- rbind(ia_med, va_med)
data_med <- rbind(data_med, or_med)

data_med <- data_med %>% select(-COUNTYFP, -COUNTYNS, -AFFGEOID, -LSAD, -ALAND, -AWATER)
data_med <- data_med %>% rename(name = NAME.x)

data_med$nointernetTop <- factor(data_med$nointernetTop, levels = c(0, 1), labels = c("No", "Yes"))
data_med$nocomputerTop <- factor(data_med$nocomputerTop, levels = c(0, 1), labels = c("No", "Yes"))
data_med$menthdaysTop <- factor(data_med$menthdaysTop, levels = c(0, 1), labels = c("No", "Yes"))
data_med$menthprovTop <- factor(data_med$menthprovTop, levels = c(0, 1), labels = c("No", "Yes"))
data_med$uninsTop <- factor(data_med$uninsTop, levels = c(0, 1), labels = c("No", "Yes"))

data_med <- st_transform(data_med, 4326)


# Remote work
ia_work <- read_rds("./rivanna_data/working/dashboard/ia_work.Rds")
va_work <- read_rds("./rivanna_data/working/dashboard/va_work.Rds")
or_work <- read_rds("./rivanna_data/working/dashboard/or_work.Rds")

data_work <- rbind(ia_work, va_work)
data_work <- rbind(data_work, or_work)

data_work <- data_work %>% select(-COUNTYFP, -COUNTYNS, -AFFGEOID, -LSAD, -NAME.x, -ALAND, -AWATER)
data_work <- data_work %>% rename(name = NAME.y)

data_work$nointernetTop <- factor(data_work$nointernetTop, levels = c(0, 1), labels = c("No", "Yes"))
data_work$nocomputerTop <- factor(data_work$nocomputerTop, levels = c(0, 1), labels = c("No", "Yes"))
data_work$occupTop <- factor(data_work$occupTop, levels = c(0, 1), labels = c("No", "Yes"))
data_work$industrTop <- factor(data_work$industrTop, levels = c(0, 1), labels = c("No", "Yes"))

data_work <- st_transform(data_work, 4326)


# Remote education
ia_edu <- read_rds("./rivanna_data/working/dashboard/ia_edu.Rds")
va_edu <- read_rds("./rivanna_data/working/dashboard/va_edu.Rds")
or_edu <- read_rds("./rivanna_data/working/dashboard/or_edu.Rds")

data_edu <- rbind(ia_edu, va_edu)
data_edu <- rbind(data_edu, or_edu)

data_edu <- data_edu %>% select(-COUNTYFP, -COUNTYNS, -AFFGEOID, -LSAD, -NAME.x, -ALAND, -AWATER)
data_edu <- data_edu %>% rename(name = NAME.y)

data_edu$nointernetTop <- factor(data_edu$nointernetTop, levels = c(0, 1), labels = c("No", "Yes"))
data_edu$nocomputerTop <- factor(data_edu$nocomputerTop, levels = c(0, 1), labels = c("No", "Yes"))
data_edu$ink12Top <- factor(data_edu$ink12Top, levels = c(0, 1), labels = c("No", "Yes"))

data_edu <- st_transform(data_edu, 4326)


#
# Write out ------------------------------------------------------------------------------------
#

data_med <- write_rds(data_med, "./rivanna_data/working/dashboard/data_med.Rds")
data_work <- write_rds(data_work, "./rivanna_data/working/dashboard/data_work.Rds")
data_edu <- write_rds(data_edu, "./rivanna_data/working/dashboard/data_edu.Rds")