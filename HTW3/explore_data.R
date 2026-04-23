##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 04/14/2026
# Description: explore data from get_data.R, as discussed with Tidal Wetland subgroup on 4/10
#########################################################
# variables to consider - site type, time (day of year, year), conductivity
# comparisons - before/after, channel/reference/restored

#library
library(lubridate)

ggplot(log_prey_dat_site, aes(x = Project_na, y = logCPUE, fill = site_type)) +
  geom_boxplot() +
  facet_grid(~Year)


# summarize before/after
restored <- log_prey_dat_site[!is.na(log_prey_dat_site$year_complete), ]

restored$BA <- ifelse(restored$Date <= restored$consutruction_date, "before",
                        ifelse (restored$Date > restored$consutruction_date, "after",
                                NA))

chcek_BA <- unique(restored[,c(5,21,22)]) #looks good

ggplot(restored, aes(x = Project_na, y = logCPUE, fill = BA)) +
  geom_boxplot() +
  facet_grid(~Year)

BA_summary <- restored %>%
  group_by(Project_na, BA) %>%
  summarise(mean = mean(logCPUE), sd = sd(logCPUE), min = min(logCPUE), max = max(logCPUE))

# add day of year
restored$day_of_year <- yday(restored$Date)

ggplot(restored, aes(x = day_of_year, y = logCPUE, color = Project_na)) +
  geom_boxplot() +
  facet_grid(~BA)

# need to make before/after for channel/reference specific to the completion dates for each restored site

head(site_metadata)

rest.sum <- subset(site_metadata, site_type=="Restored" & year_complete != "NA")
nrow(rest.sum)

log_prey_dat_mod <- log_prey_dat_site
ncol(log_prey_dat_site)
head(log_prey_dat_mod)
ncol(log_prey_dat_mod)

log_prey_dat_mod[,22:31] <- NA
colnames(log_prey_dat_mod)[22:31] <- rest.sum$Project_na

for(i in rest.sum$Project_na){
  #browser()
  log_prey_dat_mod[,i] <- ifelse(log_prey_dat_mod$Date <= rest.sum[rest.sum$Project_na == i, "consutruction_date"], "before", "after")
}

# Stacy's recommended comparisons - Tule Red/Ryer, Winter Island/Browns Island, Decker Island/Webb Tract Islands and Berms (WTIB)
log_prey_dat_mod$day_of_year <- yday(log_prey_dat_mod$Date)

# 1
comp_1 <- subset(log_prey_dat_mod, Project_na == c("Ryer", "Tule Red"))
colnames(comp_1)[23] <- "test"

# 2
comp_2 <- subset(log_prey_dat_mod, Project_na == c("Browns", "Winter"))
colnames(comp_2)[26] <- "test"
# 3
comp_3 <- subset(log_prey_dat_mod, Project_na == c("Decker", "Web Tract Berms"))
colnames(comp_3)[22] <- "test"

ggplot(comp_3, aes(x = day_of_year, y = logCPUE, color = Project_na)) +
  geom_point() +
  facet_grid(~test)
