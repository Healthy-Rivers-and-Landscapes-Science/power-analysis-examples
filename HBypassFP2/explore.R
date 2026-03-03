##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 03/02/2026
# Description: Explore EDI 996 data (Fish Food on Floodplain Farm Fields)
# Notes: Assumes edi.996.4.r has been run already
#########################################################

# library
library(dplyr)
library(ggplot2)

head(dt3[,c(2:51)]) # zoop data
str(dt3)

study_design <- unique(dt4[,c(1,4:6)]) # look at site descriptions


dt3$zoop_total <- rowSums(dt3[,c(2:51)]) # sum zoop columns
dt3$concentration <- dt3$zoop_total/dt3$volume_sampled # and divide by volume sampled

conc_summary <- dt3 %>% # summarize by site
  group_by(location) %>%
  summarise(
    mean_conc = mean(concentration, na.rm = TRUE),
    sd_conc = sd(concentration, na.rm = TRUE),
    n_size = n(),
    .groups = "drop" # drops the grouping structure after summarizing
  )

conc_summary_details <- merge(conc_summary, study_design, by = "location") # add sampling design

ggplot(conc_summary_details, aes(x = habitat_type, y = habitat_type_2, size = mean_conc, color = sd_conc)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.7) +
  scale_size(name = "mean") +
  labs(title = "bubble plot of zoop concentration",
       x = "habitat type",
       y = "habitat type 2",
       color = "sd") +
  theme_minimal()

## could consider most relevant comparisons
## could choose relevant taxa rather than summing all
## still need distance between sites
## plot zoop vs covariates
