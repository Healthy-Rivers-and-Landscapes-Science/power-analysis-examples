# new get data script, after discussion about needing raw data, given distribution/modeling/outcome of data diagnostics

# library
library(EDIutils)
library(readxl)
library(tidyverse)

# get data
temp <- read_data_entity_names(packageId = "edi.269.6")
# zoop
temp_dat <- read_data_entity(packageId = "edi.269.6", entityId = temp$entityId[3])
data <- readr::read_csv(file = temp_dat)
# org_lookup
temp_dat <- read_data_entity(packageId = "edi.269.6", entityId = temp$entityId[1])
data_org <- readr::read_csv(file = temp_dat)
# zero dates
temp_dat <- read_data_entity(packageId = "edi.269.6", entityId = temp$entityId[2])
sampling_dat <- readr::read_csv(file = temp_dat)

# site meta data will be added to edi package, but for now, adding here
site_metadata <- read.csv("HTW3/site_metadata_updated.csv")

# view data
head(data)
str(data)
summary(data)

unique(data$SiteType) # awesome!

colSums(is.na(data[, c("Count", "AdjCount")])) # none, great!

# spring only
dat_season = data %>% mutate(Month = month(Date),
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
# Corophium
# Pseudodiaptomus
# Cumacea
# Sididae
# Cyclopoid copepods
# Sinocalanus
# Daphnia
# Tortanus

org_dat_spring <- merge(dat_spring, data_org, by = "CommonName", all.x = TRUE)

taxa_check <- unique(org_dat_spring[,c(30:35)])

genus_dat = filter(org_dat_spring, Genus %in% c("Acanthocyclops", "Eurytemora", "Acartiella", "Gammarus",
                                            "Bosmina", "Hyperacanthomysis", "Ceriodaphnia", "Ilyocryptus", "Limnoithona",
                                            "Corophium", "Pseudodiaptomus", "Sinocalanus", "Daphnia", "Tortanus", "Neomysis"))

unique(genus_dat$Genus)

order_dat = filter(org_dat_spring, Order %in% c("Harpacticoida", "Calanoida", "Cyclopoida", "Cumacea"))

unique(order_dat$Order)

family_dat = filter(org_dat_spring, Family %in% c("Chironomidae", "Chydoridae", "Sididae"))

unique(family_dat$Family)

prey_dat <- rbind(genus_dat, order_dat, family_dat)

eury_dat <- subset(org_dat_spring, Genus == "Eurytemora")

# add zero catch
keep_list <- c("Browns Island","Decker Island","Ryer Island","Tule Red","Webb Tract Islands and Berms","Winter Island")
subset_dates <- sampling_dat[sampling_dat$Location %in% keep_list, ]

pairs1 <- (unique(subset_dates[,c(2,3)]))
pairs2 <- (unique(prey_dat[,c(5,7)]))
pairs3 <- (unique(eury_dat[,c(5,7)]))

pairs2$prey <- 1
pairs3$eury <- 1
merged_df <- merge(pairs1, pairs2, by = c("Date", "Location"), all = TRUE)
merged_df2 <- merge(pairs1, pairs3, by = c("Date", "Location"), all = TRUE)

merged_df[is.na(merged_df)] <- 0
merged_df_corrected <- subset(merged_df, prey == 0)
colnames(merged_df_corrected)[3] <- "AdjCount"

merged_df2[is.na(merged_df2)] <- 0
merged_df2_corrected <- subset(merged_df2, eury == 0)
colnames(merged_df2_corrected)[3] <- "AdjCount"

prey_dat_zero <- bind_rows(prey_dat, merged_df_corrected)
eury_dat_zero <- bind_rows(eury_dat, merged_df2_corrected)

# add restoration date,
# Stacy's recommended comparisons - Tule Red/Ryer, Winter Island/Browns Island, Decker Island/Webb Tract Islands and Berms (WTIB)

prey_dat_rest <- merge(prey_dat_zero, site_metadata, by = "Location", all.y = TRUE)
unique(prey_dat_rest[,c(1,6,38:40)])

eury_dat_rest <- merge(eury_dat_zero, site_metadata, by = "Location", all.y = TRUE)

# need to make before/after for channel/reference specific to the completion dates for each restored site
rest.sum <- subset(site_metadata, year_complete != "NA")
nrow(rest.sum)

prey_dat_rest_mod <- prey_dat_rest
ncol(prey_dat_rest)
head(prey_dat_rest_mod)
ncol(prey_dat_rest_mod)

prey_dat_rest_mod[,41:43] <- NA
colnames(prey_dat_rest_mod)[41:43] <- rest.sum$Location

for(i in rest.sum$Location){
  #browser()
  prey_dat_rest_mod[,i] <- ifelse(prey_dat_rest_mod$Date <= rest.sum[rest.sum$Location == i, "consutruction_date"], "before", "after")
}

# add day of year
prey_dat_rest_mod$day_of_year <- yday(prey_dat_rest_mod$Date)
prey_dat_rest_mod$year <- year(prey_dat_rest_mod$Date)

# 1
comp_1 <- prey_dat_rest_mod[prey_dat_rest_mod$Location %in% c("Ryer Island", "Tule Red"), ]
colnames(comp_1)[42] <- "test"

# 2
comp_2 <- prey_dat_rest_mod[prey_dat_rest_mod$Location %in% c("Browns Island", "Winter Island"), ]
colnames(comp_2)[43] <- "test"
# 3
comp_3 <-  prey_dat_rest_mod[prey_dat_rest_mod$Location %in% c("Decker Island", "Webb Tract Islands and Berms"), ]
colnames(comp_3)[41] <- "test"

full_comp <- rbind(comp_1[,-c(41,43)], comp_2[,-c(41,42)], comp_3[,-c(42,43)])
