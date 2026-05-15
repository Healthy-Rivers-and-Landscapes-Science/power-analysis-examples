##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 05/01/2026
# Description: explore data from get_data.R, as discussed with Tidal Wetland subgroup on 4/10
#########################################################
# variables to consider - site type, time (day of year, year), conductivity
# comparisons - before/after, channel/reference/restored

#library
library(lubridate)
library(MASS)
library(lattice)
library(car)

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

full_comp <- rbind(comp_1[,c(1,4,7:12,14:17,23,32)], comp_2[,c(1,4,7:12,14:17,26,32)], comp_3[,c(1,4,7:12,14:17,22,32)])

ggplot(full_comp, aes(x = day_of_year, y = logCPUE, color = Project_na)) +
  geom_point(aes(shape = test)) +
  geom_line(aes(linetype = site_type)) +
  facet_wrap(~Year)

ggplot(comp_1, aes(x = day_of_year, y = logCPUE, color = site_type)) +
  geom_point(aes(shape = test)) +
  #geom_line(aes(linetype = site_type)) +
  facet_grid(~Year)

ggplot(comp_1, aes(x = Project_na, y = logCPUE, fill = site_type)) +
  geom_boxplot() +
  facet_grid(~Year)

# outliers
boxplot(full_comp$logCPUE)
dotchart(full_comp$logCPUE)

#distribution
hist(full_comp$logCPUE, breaks = 100) # poisson or negative binomial - need to check clumpyness

histogram( ~ logCPUE | Year, data = full_comp) # not great
histogram( ~ logCPUE | site_type, data = full_comp)
histogram( ~ logCPUE | test, data = full_comp)

# homogeneity of variance
model_interest <- lm(logCPUE ~  site_type + day_of_year + test, data = full_comp)

model_full <- lm(logCPUE ~  site_type + day_of_year + test +
                   Secchi + Temperature + SalSurf + TurbidityNTU + TowType +
                 Tide + DO  + pH, data = full_comp)

plot(resid(model_interest))
plot(resid(model_full))# okay, not great

bwplot(logCPUE ~  site_type | test, data = full_comp)
bwplot(logCPUE ~  site_type | Year, data = full_comp) # year may be problematic
bwplot(logCPUE ~  test | Year, data = full_comp)
bwplot(logCPUE ~  test | TowType, data = full_comp) # not balanced
bwplot(logCPUE ~  site_type | TowType, data = full_comp)

bwplot(day_of_year ~  test | site_type, data = full_comp) # day of year is not great either (early season sampling in "before" compared to "after" restoration years)
bwplot(Temperature ~  test | site_type, data = full_comp) # better

# zeros
dat_rmnas <- full_comp[!is.na(full_comp$logCPUE),]

plot(table(round(dat_rmnas$logCPUE * dat_rmnas$logCPUE)),
     type = "h")

#  0   1   2   3   4   5   6   7   8   9  10  12  ...
#164  14   8  11   2   5   4   2   5   3   2   2

# collinearity
vif(model_interest)
vif(model_full) #day_of_year, Temperature (3), SalSurf (2), TowType (1)

model_vif_check <- lm(logCPUE ~  site_type + Temperature + test +
                   Secchi + TurbidityNTU +
                   Tide + DO  + pH, data = full_comp)

vif(model_vif_check)

plot(full_comp$Temperature, full_comp$SalSurf)
plot(full_comp$Temperature, full_comp$day_of_year)

#
#model <- glm.nb(logCPUE ~  site_type + day_of_year + test, data = full_comp)
model <- glm(logCPUE ~  site_type + day_of_year + test, data = full_comp, family = poisson(link = "log"))
summary(model)

mean(full_comp$logCPUE) #poisson, conditional variance is roughly equal to the conditional mean
sd(full_comp$logCPUE)

# possibile covariates -
#Secchi
#Temperature
#SalSurf
#TurbidityNTU
#Tide
#DO
#pH

model_full <- lm(logCPUE ~  site_type + day_of_year + test +
                   Secchi + Temperature + SalSurf + TurbidityNTU + TowType +
                   Tide + DO  + pH, data = full_comp)
