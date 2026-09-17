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
library(RColorBrewer)
library(tidyr)

# -------------------------------------------------------------------------
# 1. Load lookup table; filter dt3 to 2019/2021 and treated locations
# -------------------------------------------------------------------------

lookup <- read.csv("F4F_LocationLookup.csv", stringsAsFactors = FALSE)

# Identify locations with a non-NA treatment value
treated_locations <- lookup$location[!is.na(lookup$treatment)]

# Extract year from dt3$date and filter to 2019 and 2021 only,
# then filter to locations that appear in treated_locations
zoop_data1 <- dt3 %>%
  filter(format(date, "%Y") %in% c("2019", "2021")) %>%
  filter(location %in% c(treated_locations,"RRCAN"))

# -------------------------------------------------------------------------
# 2. Join treatment and distance from lookup table
# -------------------------------------------------------------------------

zoop_data1 <- zoop_data1 %>%
  left_join(
    lookup %>% select(location, treatment, distance),
    by = "location"
  )

zoop_data1$treatment[zoop_data1$location=="RRCAN"] <- "treatment"
zoop_data1$distance[zoop_data1$location=="RRCAN"] <- -0.1

# -------------------------------------------------------------------------
# 3. Join USGS flow data by date; rename value column to flow_cfs
# -------------------------------------------------------------------------

flow <- read.csv("USGS_WLKflowdata_2016-2026.csv", stringsAsFactors = FALSE)

# Parse the date from the "time" column and keep only what is needed for the join
flow <- flow %>%
  mutate(date = as.Date(time)) %>%
  select(date, flow_cfs = value)

zoop_data1 <- zoop_data1 %>%
  left_join(flow, by = "date")

# -------------------------------------------------------------------------
# 4. Create mgmt_ac column (managed water volume by year)
# -------------------------------------------------------------------------

zoop_data1 <- zoop_data1 %>%
  mutate(mgmt_ac = case_when(
    format(date, "%Y") == "2019" ~ 5435,
    format(date, "%Y") == "2021" ~ 8775
  ))

# -------------------------------------------------------------------------
# 5. Create concentration column (total zooplankton per unit volume)
# -------------------------------------------------------------------------

zoop_data1 <- zoop_data1 %>%
  mutate(concentration = rowSums(across(2:52), na.rm = TRUE) / 1000, # Original data is count per cubic m of water sampled (which = 1000 L)
         year = format(date, "%Y"))

# -------------------------------------------------------------------------
# Quick check
# -------------------------------------------------------------------------

cat("Rows in filtered dataset:", nrow(zoop_data1), "\n")
cat("Years present:", paste(sort(unique(format(zoop_data1$date, "%Y"))), collapse = ", "), "\n")
cat("Locations present:", paste(sort(unique(as.character(zoop_data1$location))), collapse = ", "), "\n")
cat("Treatment values:", paste(sort(unique(zoop_data1$treatment)), collapse = ", "), "\n")
cat("Distance values:", paste(sort(unique(zoop_data1$distance)), collapse = ", "), "\n")
cat("Flow NAs:", sum(is.na(zoop_data1$flow_cfs)), "\n")
cat("Concentration range:", round(min(zoop_data1$concentration, na.rm = TRUE), 2),
    "to", round(max(zoop_data1$concentration, na.rm = TRUE), 2), "\n")

# -------------------------------------------------------------------------
# Sample sizes by treatment and distance
# -------------------------------------------------------------------------

sample_size_summary <- zoop_data1 %>%
  group_by(treatment, distance) %>%
  summarise(n = n(), .groups = "drop")

cat("\nSample sizes by treatment and distance:\n")
print(sample_size_summary)

# -------------------------------------------------------------------------
# Mean concentration by site and year
# -------------------------------------------------------------------------
# Define site distances and labels
sitedist <- c(-0.3, -0.1, 0, 1, 6)
sitelabs <- c("Upstream", "Canal", "Outfall", "1 mi ds", "6 mi ds")

# Subset to target distances and convert to factors with custom labels
dat_sub <- zoop_data1[zoop_data1$distance %in% sitedist, ]
dat_sub$distance <- factor(dat_sub$distance, levels = sitedist, labels = sitelabs)
dat_sub$year <- factor(dat_sub$year)

# Create summary table
summary_table <- dat_sub %>%
  group_by(year, distance) %>%
  summarise(
    mean_val = mean(concentration, na.rm = TRUE),
    sd_val = sd(concentration, na.rm = TRUE)
  )

write.csv(summary_table, "summary_table.csv")

# Create summary table of flows
#0.02832 is conversion factor from cfs to cms
summary_table <- dat_sub %>%
  group_by(year) %>%
  summarise(
    mean_val = mean(flow_cfs*0.02832, na.rm = TRUE),
    min_val = min(flow_cfs*0.02832, na.rm = TRUE),
    max_val = max(flow_cfs*0.02832, na.rm = TRUE)
  )

# Plot
p <- ggplot(dat_sub, aes(x = distance, y = concentration, fill = year)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.8) +
  scale_y_continuous(limits=c(0,50)) +
  labs(x = "\nSite",y = "Zoop concentration (count/L)\n", fill = "Year") +
  theme_classic() + theme(panel.background = element_rect(color="black"),
                          axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),
                          legend.position="right",legend.text=element_text(size=10),legend.key = element_blank(), plot.margin=unit(c(0.3,0.6,0.4,0.4),"cm"))

png(file="Fig_Zoop concentration by distance and year.png",width = 7, height = 4, units = "in", res=400)
p
dev.off()


# -------------------------------------------------------------------------
# River dilution: Mean concentration by distance and year
# -------------------------------------------------------------------------
# Parameters
Qr_values <- c(100, 200, 300, 400, 500) # main river flows (m3/s), based on range of flows observed in 2 years of pilot data
Qa <- 30 # added stream flow (m3/s)
Cr <- 0.5 # river concentration (approximate annual mean from pilot data)
Ca <- 20 # added stream concentration (approximate annual mean from pilot data in canal)

# Mixing lengths (m)
L_values <- 3*round(6 * (Qr_values^0.65),0) # Generic equation calculating length to near complete mixing
L_values <- as.data.frame(cbind(Qr_values, L_values))

# Distance sequence (m)
x <- seq(0, 5000, length.out = 100)

# Make df for storing concentration curve results
df <- expand_grid(Qr = Qr_values, x = x, Cx = NA)

# Loop through flows and plot
for (i in 1:nrow(df)) {
  Cf <- (df$Qr[i]*Cr + Qa*Ca) / (df$Qr[i] + Qa) # final concentration
  L <- L_values[which(L_values$Qr_values==df$Qr[i]),2] # mixing length
  df$Cx[i] <- Cf + (Cr - Cf) * exp(-df$x[i] / L)
}

# Create plot
my_colors <- brewer.pal(11, "Spectral")[c(1, 3, 8, 10, 11)]  # every other color

p1 <- ggplot(df, aes(x = x, y = Cx, group = Qr, color = factor(Qr))) +
  geom_line(linewidth=0.75) +
  scale_color_manual(values = my_colors, name = "River flow (cms)") +
  scale_y_continuous(limits=c(0,8), breaks=seq(0,8,by=1)) +
  scale_x_continuous(limits=c(0,3000),breaks=seq(0,3000,by=250), labels = c(0,"",0.5,"",1,"",1.5,"",2,"",2.5,"",3)) +
  labs(x = "\nDistance downstream (km)",y = "Zoop concentration (count/L)\n") +
  theme_classic() + theme(panel.background = element_rect(color="black"),
                          axis.text=element_text(size=11),axis.title=element_text(size=12,face="bold"),
                          legend.position=c(0.18,0.78),legend.text=element_text(size=10),legend.key = element_blank(), plot.margin=unit(c(0.3,0.6,0.4,0.4),"cm"))

png(file="Fig_Zoop concentration by distance and flow.png",width = 6, height = 5, units = "in", res=400)
p1
dev.off()


