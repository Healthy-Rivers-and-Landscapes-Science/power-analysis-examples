##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 04/14/2026
# Description: get tidal wetlands data, code adapted from Rosie Hartman's power_wetlandbugs.R exploration code
# sub-setting decisions from Tidal Wetland subgroup's discussion on 4/10
#########################################################

## library
#data manipulation
library(tidyverse)
#mapping
library(sf)
#install.packages("devtools")
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")
library(deltamapr)

## data
load("HTW3/AllWetlandBugs_2010onwards.RData")
load("HTW3/wetlandsites.RData")

samples = select(Allbugs_Oct2025, Latitude, Longitude, Source) %>%
  distinct()  %>%
  mutate(Longitude = case_when(Longitude >0 ~ Longitude*-1,
                               TRUE ~ Longitude)) %>%
  filter(!is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

## sub-setting
# cache slough only
prioritysites = allsites  %>%
  mutate(Region = "Cache Slough",
         site_type = case_when(site_type == "Planned" ~ "Restored",
                               TRUE ~ site_type)) %>%
  st_transform(crs = st_crs(WW_Delta))

# now some surrounding sloughs for comparison
External = filter(WW_Delta, HNAME %in% c(
  "CACHE SLOUGH", "PROSPECT SLOUGH",
  "SACTO. R DEEP WATER SH CHAN")) %>%
  select("Project_na" = HNAME) %>%
  mutate(site_type = "channel", Region = c("Cache Slough", "Cache Slough", "Cache Slough"
  )) %>%
  bind_rows(prioritysites) %>%
  filter(Region == "Cache Slough")

bugsexternal = Allbugs_Oct2025 %>%
  filter(!is.na(Longitude)) %>%
  mutate(Longitude = case_when(Longitude >0 ~ Longitude*-1,
                               TRUE ~ Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F) %>%
  st_transform(crs = st_crs(WW_Delta)) %>%
  st_join(External) %>%
  filter(!is.na(Project_na)) %>%
  st_drop_geometry()

# FRP only
unique(bugsexternal$Source)
dat <- subset(bugsexternal, Source == "FRP")

# spring only
dat_season = dat %>% mutate(Month = month(Date),
                            Season  = case_when(Month %in% c(3,4,5) ~ "Spring",
                                                Month %in% c(6,7,8) ~ "Summer",
                                                Month %in% c(9,10,11) ~ "Fall",
                                                Month %in% c(12,1,2) ~ "Winter"))

dat_spring <- subset(dat_season, Season == "Spring")



# meso-zooplankton most relevant as smelt diets
# lump CPUE and Eurytemora only

# Prey taxa included (from Stacy's manuscript)
# Acanthocyclops
# Eurytemora
# Acartiella
# Gammarus
# Bosmina
# Harpacticoid copepods
# Calanoid copepods
# Hyperacanthomysis
# Ceriodaphnia
# Ilyocryptus
# Chironomidae larvae
# Limnoithona
# Chydoridae
# Neomysis kadiakensis
# Neomysis mercedis
# Corophium 	Pseudodiaptomus
# Cumacea 	Sididae
# Cyclopoid copepods 	Sinocalanus
# Daphnia 	Tortanus

taxa_check <- unique(dat_spring[,c(29:36)])
genus_dat = filter(dat_spring, Genus %in% c("Acanthocyclops", "Eurytemora", "Acartiella", "Gammarus",
                                                "Bosmina", "Hyperacanthomysis", "Ceriodaphnia", "Ilyocryptus", "Limnoithona",
                                                "Corophium", "Pseudodiaptomus", "Sinocalanus", "Daphnia", "Tortanus", "Neomysis"))

order_dat = filter(dat_spring, Order %in% c("Harpacticoida", "Calanoida", "Cyclopoida", "Cumacea"))

family_dat = filter(dat_spring, Family %in% c("Chironomidae", "Chydoridae", "Sididae"))

prey_dat <- rbind(genus_dat, order_dat, family_dat)

eury_dat <- subset(dat_spring, Genus == "Eurytemora")

# log cpue
sample_check <- unique(prey_dat[,c(2, 5, 14)]) # lump using SampleID

log_prey_dat <- prey_dat %>%
  group_by(SampleID) %>%
  summarise(total_value = sum(CPUE)) %>%
  mutate(logCPUE = log(total_value+1))#, logCPUE = if_else(logCPUE == -Inf, 0, logCPUE))

log_prey_dat_combo <- merge(log_prey_dat, unique(prey_dat[,c(2, 5, 8, 9, 11:16, 19, 21, 24, 41:44)]), by = "SampleID", all = TRUE)

log_eury_dat <- eury_dat %>% mutate(logCPUE = log(CPUE+1))#, logCPUE = if_else(logCPUE == -Inf, 0, logCPUE))

# add restoration date, Stacy recommends to drop Lindsey and LICB
# Prospect Island is still under construction
# site meta data will be added to edi package, but for now, adding here

site_metadata <- read.csv("HTW3/site_metadata.csv")

log_prey_dat_site <- merge(log_prey_dat_combo, site_metadata[,-c(1,3)], by = "Project_na", all.y = TRUE)

log_eury_dat_site <- merge(log_eury_dat, site_metadata[,-c(1,3)], by = "Project_na", all.y = TRUE)
