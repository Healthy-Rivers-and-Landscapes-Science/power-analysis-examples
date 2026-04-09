##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 2026-04-03
# Description: Data preparation for zooplankton power analysis
#              using EDI 996.4 data (Fish Food on Floodplain Farm Fields)
# Notes: Assumes edi.996.4.r has been run already (loads dt3 into environment)
#########################################################

# library
library(dplyr)
library(ggplot2)

# -------------------------------------------------------------------------
# 1. Load lookup table; filter dt3 to 2019/2021 and treated locations
# -------------------------------------------------------------------------

lookup <- read.csv("F4F_LocationLookup.csv", stringsAsFactors = FALSE)

# Identify locations with a non-NA treatment value
treated_locations <- lookup$location[!is.na(lookup$treatment)]

# Extract year from dt3$date and filter to 2019 and 2021 only,
# then filter to locations that appear in treated_locations
zoop_data <- dt3 %>%
  filter(format(date, "%Y") %in% c("2019", "2021")) %>%
  filter(location %in% treated_locations)

# -------------------------------------------------------------------------
# 2. Join treatment and distance from lookup table
# -------------------------------------------------------------------------

zoop_data <- zoop_data %>%
  left_join(
    lookup %>% select(location, treatment, distance),
    by = "location"
  )

# -------------------------------------------------------------------------
# 3. Join USGS flow data by date; rename value column to flow_cfs
# -------------------------------------------------------------------------

flow <- read.csv("USGS_WLKflowdata_2016-2026.csv", stringsAsFactors = FALSE)

# Parse the date from the "time" column and keep only what is needed for the join
flow <- flow %>%
  mutate(date = as.Date(time)) %>%
  select(date, flow_cfs = value)

zoop_data <- zoop_data %>%
  left_join(flow, by = "date")

# -------------------------------------------------------------------------
# 4. Create mgmt_ac column (managed water volume by year)
# -------------------------------------------------------------------------

zoop_data <- zoop_data %>%
  mutate(mgmt_ac = case_when(
    format(date, "%Y") == "2019" ~ 5435,
    format(date, "%Y") == "2021" ~ 8775
  ))

# -------------------------------------------------------------------------
# 5. Create concentration column (total zooplankton per unit volume)
# -------------------------------------------------------------------------

zoop_data <- zoop_data %>%
  mutate(concentration = rowSums(across(2:52), na.rm = TRUE) / 1000, # Original data is count per cubic m of water sampled (which = 1000 L)
         year = format(date, "%Y"))

# -------------------------------------------------------------------------
# Quick check
# -------------------------------------------------------------------------

cat("Rows in filtered dataset:", nrow(zoop_data), "\n")
cat("Years present:", paste(sort(unique(format(zoop_data$date, "%Y"))), collapse = ", "), "\n")
cat("Locations present:", paste(sort(unique(as.character(zoop_data$location))), collapse = ", "), "\n")
cat("Treatment values:", paste(sort(unique(zoop_data$treatment)), collapse = ", "), "\n")
cat("Distance values:", paste(sort(unique(zoop_data$distance)), collapse = ", "), "\n")
cat("Flow NAs:", sum(is.na(zoop_data$flow_cfs)), "\n")
cat("Concentration range:", round(min(zoop_data$concentration, na.rm = TRUE), 2),
    "to", round(max(zoop_data$concentration, na.rm = TRUE), 2), "\n")

# -------------------------------------------------------------------------
# Sample sizes by treatment and distance
# -------------------------------------------------------------------------

sample_size_summary <- zoop_data %>%
  group_by(treatment, distance) %>%
  summarise(n = n(), .groups = "drop")

cat("\nSample sizes by treatment and distance:\n")
print(sample_size_summary)

write.csv(sample_size_summary, "sample_size_summary.csv", row.names = FALSE)

# -------------------------------------------------------------------------
# Plot zoop concentration by distance
# -------------------------------------------------------------------------
p <- ggplot(zoop_data, aes(x=distance, y=concentration, fill=year)) +
  geom_jitter(width = 0.05, aes(color=year)) +
  labs(x= "\nDistance from outfall (mi)",y="Zoop concentration (count/L)\n") +
  scale_x_continuous(breaks=c(-0.3,0,0.5,1,2,3,4,6)) +
  theme_classic() + theme(panel.background = element_rect(color="black"),
                          axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),
                          legend.position="right",legend.text=element_text(size=10), plot.margin=unit(c(0.3,0.6,0.4,0.4),"cm"))
p


png(file="Fig_Zoop concentration by distance.png",width = 7, height = 4, units = "in", res=400)
p
dev.off()

