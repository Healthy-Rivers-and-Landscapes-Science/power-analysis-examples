# visualize data
# library
library(lattice)
library(MASS)

# add zero for CPUE
full_comp$CPUE[is.na(full_comp$CPUE)] <- 0

ggplot(full_comp, aes(x = day_of_year, y = AdjCount, color = Location)) +
  geom_point(aes(shape = test)) +
  geom_line(aes(linetype = site_type)) +
  facet_wrap(~year)

ggplot(comp_3, aes(x = day_of_year, y = AdjCount, color = site_type)) +
  geom_point(aes(shape = test)) +
  #geom_line(aes(linetype = site_type)) +
  facet_grid(~year)

ggplot(comp_3, aes(x = Location, y = AdjCount, fill = site_type)) +
  geom_boxplot() +
  facet_grid(~year)

# outliers
boxplot(full_comp$AdjCount)
dotchart(full_comp$AdjCount) # one outlier (Tule Red, 2019)

#distribution
hist(full_comp$AdjCount, breaks = 100) # poisson or negative binomial - need to check clumpyness

histogram( ~ AdjCount | year, data = full_comp) # not great
histogram( ~ AdjCount | site_type, data = full_comp)
histogram( ~ AdjCount | test, data = full_comp)

# homogeneity of variance
model_interest <- lm(AdjCount ~  site_type + day_of_year + test, data = full_comp)

#model_full <- lm(AdjCount ~  site_type + day_of_year + test +
#                   effort + Temperature + SalSurf + TurbidityNTU + TowType +
#                   Tide + DO  + pH, data = full_comp)

plot(resid(model_interest)) # not great
#plot(resid(model_full))

bwplot(AdjCount ~  site_type | test, data = full_comp)
bwplot(AdjCount ~  site_type | year, data = full_comp)
bwplot(AdjCount ~  test | year, data = full_comp)

bwplot(day_of_year ~  test | site_type, data = full_comp) # early season sampling in "before" compared to "after" restoration years

# zeros
dat_rmnas <- full_comp[!is.na(full_comp$AdjCount),]

plot(table(round(dat_rmnas$AdjCount * dat_rmnas$AdjCount)),
     type = "h")

#  0         1         2         3         4         5         6         7         8
#152        23         7         4        41         6        27         4        12   ...


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

mean(full_comp$AdjCount) #poisson, conditional variance is roughly equal to the conditional mean
sd(full_comp$AdjCount) #Negative Binomial, variance significantly exceeds the mean (overdispersion),

mean(full_comp$CPUE)
sd(full_comp$CPUE)

# possibile covariates -
#Secchi
#Temperature
#SalSurf
#TurbidityNTU
#Tide
#DO
#pH

full_comp <- subset(full_comp, CPUE < 40000)
comp_1 <- subset(comp_1, CPUE < 40000)

model_full <- lm(logCPUE ~  site_type + day_of_year + test +
                   Secchi + Temperature + SalSurf + TurbidityNTU + TowType +
                   Tide + DO  + pH, data = full_comp)

nb_model <- glm.nb(AdjCount ~ site_type + day_of_year + test + effort, data = full_comp)
nb_model <- glm.nb(CPUE ~ site_type * test , data = comp_1)

model <- glmer.nb(AdjCount ~ site_type * test + (1 | effort), data = comp_1)
model <- glm.nb(AdjCount ~ site_type * test + offset(effort), data = comp_3)

model <- glmer.nb(CPUE ~ site_type * test + (1 | day_of_year), data = comp_1)


summary(model)
anova(model)

ggplot(comp_3) +
  geom_point(aes(x= year, y = CPUE, color = test, shape = site_type)) + theme_bw() +
  facet_grid(~Location)

ggplot(comp_1) +
  geom_line(aes(x= year, y = CPUE, color = Location, linetype = site_type), lwd =1)+ theme_bw()+
  labs(x = "Year", y = "Density", color = "Site Name", linetype = "Site Type",
       title = "Example Redd Survey Data") +
  guides(color = "none")

mod <- lmerTest::lmer(CPUE ~ site_type*test + (1|day_of_year), data=comp_3) # Random effect for site controls for site-level differences. type*time interaction tests whether the change over time differs between control and impact sites.
summary(mod)
anova(mod) #
