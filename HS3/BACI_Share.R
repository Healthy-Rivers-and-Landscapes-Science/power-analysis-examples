# BACI Analysis code by Kiera McNeely (Cramer Fish Sciences) 4/28/2026

# The code below was developed using Feather River data spawning ground (ground survey) data from 2020-2023.
# "The analysis examines whether restoration efforts significantly affected redd density by comparing control (unrestored) and impact (restored) sites using data from before and after restoration occurred.


# Libraries ----
library(ggplot2) # Plotting
library(lmerTest) # Random effect model with p-value

# Data formatting ----
# Note: Data format will probably differ than the Feather River dataset so this code has been removed. 
# The goal is to have a data frame that includes site id, year, type (Impact or Control), time (Before or After restoration), redd density for entire season.
example_df <- data.frame(siteid = rep(c("site1", "site2", "site3", "site4"), 4),
                         year = c(rep(2020,4), rep(2021,4), rep(2022,4), rep(2023,4)),
                         type = rep(c("Control", "Control", "Impact", "Impact"), 4),
                         time = c(rep("Before",8), rep("After",8)),
                         density = c(0.8, 0.9, 0.7, 0.85,
                                     0.75, 0.8, 0.7, 0.78,
                                     0.82, 0.88, 1.8, 1.7,
                                     0.74, 0.80, 1.9, 2.0))

# BACI Analysis ----
## Exploratory Plots ----
ggplot(example_df) +
  geom_point(aes(x= year, y = density, color = time, shape = type)) + theme_bw()

ggplot(example_df) + 
  geom_line(aes(x= year, y = density, color = siteid, linetype = type), lwd =1)+ theme_bw()+
  labs(x = "Year", y = "Density", color = "Site Name", linetype = "Site Type",
       title = "Example Redd Survey Data") +
  guides(color = "none")
# Both plots show similar densities between control and impact sites before restoration, and a clear increase in impact sites after restoration, suggesting a significant interaction.


## Model ----
mod <- lmerTest::lmer(density ~ type*time + (1|siteid), data=example_df) # Random effect for site controls for site-level differences. type*time interaction tests whether the change over time differs between control and impact sites.
summary(mod) 
anova(mod) # significant type:time interaction (p<0.05) indicates a restoration effect.


