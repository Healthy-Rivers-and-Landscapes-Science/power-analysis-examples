##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 2026-04-17
# Description: determine range of flows for Yolo
#########################################################
# get data from inundation package
devtools::install_github("goertler/inundation")

library(inundation)
library(dplyr)

inun <- calc_inundation()

#subset to Jan-June HRL assets deployment focus
inun$month <- as.numeric(format(inun$date, "%m"))

inun_explore <- subset(inun, month <= 6 & inundation == 0)

# range in cms
summary(inun_explore$sac)*0.0283
summary(inun_explore$yolo_dayflow)*0.0283
