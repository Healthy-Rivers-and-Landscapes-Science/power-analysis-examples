# BACI Bootstrap Power Analysis 
# By Kiera McNeely 5/26/2026 
# Cramer Fish Sciences

# libraries
library(dplyr) # data manipulation
library(ggplot2) # plot
library(lmerTest) # LMER 


# There are a few methods to run this analysis depending on the restoration schedule for your system.

# Method 1: If restoration was completed in the same year across all sites, a single BACI analysis
# can be run for the entire system to examine the interaction between the control and impact groups.

# Method 2: If restoration was completed in different years across sites, the system cannot be
# compared in a single analysis. Data will need to be paired (one impact site and one control site)
# and a separate BACI analysis run for each project site.

# The key to this analysis is knowing the restoration schedule and formatting the data correctly.
# First, determine which method applies to your system. Then subset the data to include either all
# sites (Method 1) or the project site of interest with its paired control (Method 2).

# The formatted dataset should contain the following columns:
#   - site_id: unique identifier for each subsite
#   - type: whether the site is "Control" or "Impact"
#   - time: whether the survey occurred "Before" or "After" restoration
#   - density: redd density accounting for sampling effort (# redds / (survey area * number of surveys))
#   - binary_type: numeric coding of type (0 = Control, 1 = Impact)
#   - binary_time: numeric coding of time (0 = Before, 1 = After)

test_data <- data.frame(site_id  = c(seq(1, 25, by = 1), seq(1, 25, by = 1), seq(26, 50, by = 1), seq(26, 50, by = 1)),
                        type = c(rep("Control", 50), rep("Impact", 50)),
                        binary_type = c(rep(0, 50), rep(1, 50)),
                        time = c(rep("Before", 25), rep("After", 25), rep("Before", 25), rep("After", 25)),
                        binary_time = c(rep(0, 25), rep(1, 25), rep(0, 25), rep(1, 25)),
                        density  = c(runif(25, min = 0,   max = 0.3), runif(25, min = 0, max = 0.3), 
                                     runif(25, min = 0,   max = 0.3), runif(25, min = 0.25, max = 0.65)))


# Plots -----
ggplot(test_data %>% mutate(time = factor(time, levels = c("Before", "After"))))+
  geom_boxplot(aes(x = type, y = density, fill = type))+
  facet_wrap(~time) +
  theme_bw() +
  labs(x = "Site Type", y = "Redd Density", title = "Test Redd Data")
# from this plot we would expect a significant interaction due to the increase in impact from before and after and the consistency of the control from before and after.



# Bootstrap Power Analysis -----
# need to subset by impact sites and control sites, then pull all data from those sites
impact_sites <- unique(test_data$site_id[test_data$type=="Impact"])  # List of impact sites to pull from for bootstrap
control_sites <- unique(test_data$site_id[test_data$type=="Control"]) # List of control sites to pull from for bootstrap
n_sites <- c(5, 10, 15, 20, 25) # samples to pull from each treatment
boot_itr <- 1000 # number of iterations

boot_baci <- function(impact_sites, control_sites, boot_itr, n_sites, data){ # need list of impact sites, control sites, data to filter from,
  
  # create results table
  total_rows <- length(n_sites)*boot_itr # pre-allocate results table
  boot_res <- data.frame(n = numeric(total_rows),         
                         effect_size = numeric(total_rows),
                         pvalue = numeric(total_rows))
  counter <- 1 # counter for storing results
  
  
  for(n_site in n_sites){# repeat bootstrap for each n value
    for(i in 1:boot_itr){
      
      # resample data: Get the sites to use in the bootstrap, need to pull data from those sites for all years.
      imp <- sample(impact_sites, size = n_site, replace = T) 
      con <- sample(control_sites, size = n_site, replace = T)
      all_sites <- c(imp,con)
      
      mod_data <- NULL
      for(s in all_sites){
        keep <- data[data$site_id == s,] # get the data for this site for all the years to use in the BACI analysis
        mod_data <- rbind(mod_data,keep)
      }
      
      # BACI
      mod <- lmerTest::lmer(density ~ binary_time*binary_type + (1|site_id), data=mod_data) # model with random effect for site
      mod_sum <- summary(mod)
      p_value <- mod_sum$coefficients[4, 5] # pull the p-value
      eff_size <- ifelse(p_value <= 0.05, mod_sum$coefficients[4, 1], NA) #grab coefficient, ONLY when significant      
      
      # Save results
      res <- c(n_site, eff_size, p_value)
      boot_res[counter,] <- res
      counter <- counter +1
      
    } #/bootstrap
  } #/sample size
  
  # summary of results for each n (sample size group)
  summary <- boot_res %>% group_by(n) %>% summarize(mean_eff_size = mean(effect_size, na.rm=T),
                                                    mean_power = mean(pvalue < 0.05, na.rm=T)) #Accept Alternative if p-value is less than 0.05.
  return(summary)
}

## Results ----
set.seed(2026) # set before calling the function for reproducibility
baci_power <- boot_baci(impact_sites, control_sites, boot_itr, n_sites, data = test_data) ## singular boundary is from sampling with replacement, this is ok.

baci_power
# Effect size is the difference of the difference, which is the effect of the restoration. It's how much the impact group changed related to the control group.




