##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 2026-04-03
# Description: All-subsets model comparison and validity testing
#              for zooplankton concentration (Fish Food on Floodplain Farm Fields)
# Notes: Assumes data_prep.R has been run already (loads zoop_data into environment)
#########################################################

# library
library(dplyr)
library(MuMIn)   # dredge(), AICc()
library(car)     # vif()
library(lme4)
library(lmtest)  # bptest()
library(ggplot2)
library(GGally)

# -------------------------------------------------------------------------
# 1. Prepare modelling data
# -------------------------------------------------------------------------
# Drop rows with NAs in any predictor or the response so all candidate models
# are fit to the same observations (required for valid AICc comparison).
# distance is scaled (mean = 0, sd = 1) to reduce the artificial collinearity
# between the linear and quadratic terms. All other predictors are kept in
# their original units.

model_vars <- c("concentration", "treatment", "distance",
                "do_mg_l", "temp_c", "flow_cfs", "mgmt_ac","year")

model_data <- zoop_data %>%
  select(all_of(model_vars)) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  mutate(
    treatment  = factor(treatment, levels = c("control", "treatment")),
    distance_s = as.numeric(scale(distance))
  )

cat("Observations available for modelling:", nrow(model_data), "\n")
cat("Predictors: treatment, distance_s, I(distance_s^2), do_mg_l, temp_c, flow_cfs, mgmt_ac\n\n")

# -------------------------------------------------------------------------
# 2. Assess correlation and plot
# -------------------------------------------------------------------------
pcor <- ggpairs(subset(model_data, distance %in% c(0,1,6)), columns = c(1,4:6), ggplot2::aes(colour=as.factor(distance)))

png(file="Fig_Correlation.png",width = 7, height = 4, units = "in", res=400)
pcor
dev.off()

# na.action = na.fail is required by dredge() — it prevents fitting models
# on different subsets of data if NAs were present (already handled above).

# -------------------------------------------------------------------------
# 2. Fit global model for control vs. 0, 1, and 6 mi downstream (separately)
# -------------------------------------------------------------------------
model_data0 <- subset(model_data, distance %in% c(-0.3,0))
global_model0 <- glm(
  concentration ~ treatment + do_mg_l + temp_c + flow_cfs,
  data      = model_data0,
  na.action = na.fail
)

cat("Global model summary: 0 mi:\n")
print(summary(global_model0))

model_data1 <- subset(model_data, distance %in% c(-0.3,1))
global_model1 <- glm(
  concentration ~ treatment + do_mg_l + temp_c + flow_cfs,
  data      = model_data1,
  na.action = na.fail
)

cat("Global model summary: 1 mi:\n")
print(summary(global_model1))

model_data6 <- subset(model_data, distance %in% c(-0.3,6))
global_model6 <- glm(
  concentration ~ treatment + do_mg_l + temp_c + flow_cfs,
  data      = model_data6,
  na.action = na.fail
)

cat("Global model summary: 6 mi:\n")
print(summary(global_model6))

# -------------------------------------------------------------------------
# 3. All-subsets comparison via AICc
# -------------------------------------------------------------------------
# Do model comparison for control vs. treatment at each of 3 distances (0, 1, and 6 mi)
all_subsets <- dredge(global_model0, rank   = "AICc")
print(all_subsets)
summary_table0 <- cbind("distance"=0,all_subsets)

all_subsets <- dredge(global_model1, rank   = "AICc")
print(all_subsets)
summary_table1 <- cbind("distance"=1,all_subsets)

all_subsets <- dredge(global_model6, rank   = "AICc")
print(all_subsets)
summary_table6 <- cbind("distance"=6,all_subsets)

summary_table <- rbind(summary_table0,summary_table1,summary_table6)

write.csv(summary_table, "model_comparison_summary.csv", row.names = FALSE)
cat("\nSaved to model_comparison_summary.csv\n")
