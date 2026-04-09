##########################################################
# Created by: Pascale Goertler (pascale.goertler@water.ca.gov)
# Last updated: 2026-04-03
# Description: Simulation-based power analysis for zooplankton concentration
#              using the simr package
# Notes: Assumes data_prep.R has been run already (loads zoop_data)
#########################################################

# library
library(dplyr)
library(lme4)
library(simr)
library(ggplot2)
library("pwrss")

set.seed(15)
attach(zoop_data)

# -------------------------------------------------------------------------
# 0. Simulation parameters
# -------------------------------------------------------------------------

n_per_loc    <- c(1, 2, 3, 5, 10, 20, 30, 50)  # sample sizes per location (treatment and control) to test
target_dists <- c(0, 1, 6)              # downstream distances (miles)
multipliers  <- c(0.2, 0.5, 1, 2, 5)    # fold-increase relative to control
nsim         <- 5000

# -------------------------------------------------------------------------
# 1. Simple power analysis: control vs. outfall
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# 1.1. Prepare model data and fit base model
# -------------------------------------------------------------------------
model_vars <- c("concentration", "treatment", "flow_cfs", "distance")

# Include control site and all target treatment distances
model_data <- zoop_data %>%
  select(all_of(model_vars)) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  filter(distance %in% c(-0.3, target_dists)) %>%
  mutate(treatment = factor(treatment, levels = c("control", "treatment")))

# Base model formula (refit per distance inside loop below)
base_model_formula <- concentration ~ treatment + flow_cfs

# -------------------------------------------------------------------------
# 1.2. Simulation-based power analysis
# -------------------------------------------------------------------------
# For each target distance, the data are subset to control (distance = -0.3)
# plus that treatment distance. The generative model draws control observations
# from N(ctrl_mean, residual_sd) and treatment observations from
# N(ctrl_mean * multiplier, residual_sd), where residual_sd comes from
# base_model. Each simulated dataset has n_per_loc observations per group.
# Power = proportion of nsim simulations where the treatment p-value < 0.05.

# Helper: run nsim simulations for one (n, multiplier) combination
run_power_sim <- function(ctrl_mean, treat_mean, n, residual_sd,
                          mean_flow, sd_flow, nsim) {
  sig_count <- 0L
  for (i in seq_len(nsim)) {
    sim_data <- data.frame(
      concentration = c(rnorm(n, ctrl_mean,  residual_sd),
                        rnorm(n, treat_mean, residual_sd)),
      treatment     = factor(rep(c("control", "treatment"), each = n),
                             levels = c("control", "treatment")),
      flow_cfs      = rnorm(2 * n, mean_flow, sd_flow)
    )
    m <- glm(concentration ~ treatment + flow_cfs, data = sim_data)
    p <- summary(m)$coefficients["treatmenttreatment", "Pr(>|t|)"]
    sig_count <- sig_count + (p < 0.05)
  }
  sig_count / nsim
}

# Loop over distances, then over all (n_per_loc x multiplier) combinations
power_results <- lapply(target_dists, function(dist) {

  # Subset to control + this treatment distance, then refit base_model
  sub_data <- model_data %>%
    filter(distance %in% c(-0.3, dist)) %>%
    mutate(treatment = factor(treatment, levels = c("control", "treatment")))

  dist_model <- glm(base_model_formula, data = sub_data)

  residual_sd <- sigma(dist_model)
  ctrl_mean   <- mean(sub_data$concentration[sub_data$treatment == "control"],
                      na.rm = TRUE)
  mean_flow   <- mean(sub_data$flow_cfs, na.rm = TRUE)
  sd_flow     <- sd(sub_data$flow_cfs,   na.rm = TRUE)

  cat(sprintf(
    "dist = %g mi | n = %d | control mean = %.3f | residual SD = %.3f | mean flow = %.1f cfs\n",
    dist, nrow(sub_data), ctrl_mean, residual_sd, mean_flow
  ))

  grid <- expand.grid(n_per_loc = n_per_loc, multiplier = multipliers)

  grid$power <- mapply(function(n, mult) {
    run_power_sim(
      ctrl_mean   = ctrl_mean,
      treat_mean  = ctrl_mean * (1 + mult),
      n           = n,
      residual_sd = residual_sd,
      mean_flow   = mean_flow,
      sd_flow     = sd_flow,
      nsim        = nsim
    )
  }, grid$n_per_loc, grid$multiplier)

  grid$distance <- dist
  grid
})

power_df <- bind_rows(power_results)

# Save results
write.csv(power_df, "power_analysis_simple_results.csv", row.names = FALSE)
cat("\nSaved results to power_analysis_simple_results.csv\n")

# -------------------------------------------------------------------------
# 3. Plot
# -------------------------------------------------------------------------

power_df$distance   <- factor(power_df$distance,
                               levels = target_dists,
                               labels = paste0(target_dists, " mi"))
power_df$multiplier <- factor(power_df$multiplier,
                               levels = multipliers,
                               labels = paste0(multipliers, "x"))

ggplot(power_df, aes(x = n_per_loc, y = power,
                     colour = multiplier, group = multiplier)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.80, linetype = "dashed", colour = "grey40") +
  facet_wrap(~ distance, ncol = 3,
             labeller = labeller(distance = label_both)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
  labs(
    x       = "Sample size per location (n)",
    y       = "Power",
    colour  = "Concentration\nmultiplier",
    title   = "Power analysis: control vs. treatment site by distance",
  ) +
  theme_bw() +
  theme(legend.position = "right")

ggsave("power_analysis_simple.png", width = 10, height = 5, dpi = 150)
cat("Saved plot to power_analysis_simple.png\n")

