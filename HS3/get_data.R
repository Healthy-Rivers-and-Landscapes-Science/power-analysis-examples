##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 03/27/2026
# Description: get Feather River data
#########################################################
# library
library(EDIutils)
library(readxl)
library(tidyverse)

# get data
temp <- read_data_entity_names(packageId = "edi.1802.2")
temp_dat <- read_data_entity(packageId = "edi.1802.2", entityId = temp$entityId[1])
data <- readr::read_csv(file = temp_dat)

# view data
head(data)
str(data)
summary(data)

# add month, year and water year
data$month <- format(as.Date(data$date), "%m")
data$year <- format(as.Date(data$date), "%Y")

dates.posix <- as.POSIXlt(data$date)
offset <- ifelse(dates.posix$mon >= 10 - 1, 1, 0)

#water year
data$water_year <- dates.posix$year + 1900 + offset

# add site size and restoration status data (will be added to edi soon)
site_dat <-  read_excel("HS3/Copy of FR_Redd Survey Locations.xlsx")


# redd density by date and location
redd_density <- data %>%
  group_by(location, date) %>%
  summarize(total_num_redd = sum(number_redds, na.rm = TRUE),
            sample = n())

redd_density <- merge(site_dat[,-1], redd_density, by = "location", all=TRUE)

redd_density$redd_density <- redd_density$total_num_redd/redd_density$size_ft2
