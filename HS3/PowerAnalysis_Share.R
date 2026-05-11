## Power analysis code By Kiera McNeely (Cramer Fish Sciences) 3/11/2026

# The code below was developed using Lower American River (LAR) spawning ground (ground survey) data from 2022 - 2024 from restored and unrestored sites. 
# The analysis examines how many samples are needed from each treatment type in order to detect that the restored sites have a higher density of redds than unresotred sites, reporting the estimated effect size and power for each sample size. 


# Libraries ----
library(effsize) ## calculating effect size
library(dplyr) ## summarize and pipe operator for results in bootstrap function 


# Data formatting ----
# Note: Data format will probably differ than the LAR dataset so this code has been removed. 
# The goal is to have a data frame that includes escapement year, site id, treatment (Treatment, Control), site area, total number of surveys for the season, total number of redds for the season, and total area surveyed for the season (site area * number of surveys). The response variable is redd density, calculated by dividing the total number of redds by the total area surveyed. See example data below.
example_df <- data.frame(esc_year = c(2022, 2022, 2022, 2022, 
                                      2023, 2023, 2023, 2023, 
                                      2024, 2024, 2024, 2024),
                         siteid = rep(c("site1", "site2", "site3", "site4"), 3),
                         location = rep(c("Upstream", "Downstream"), 6),
                         treatment = rep(c("Treatment", "Treatment", "Control", "Control"), 3),
                         sub_area = rep(c(10, 12), 6),
                         total_surveys = c(rep(5,12)),
                         redd_count = c(38, 45, 5, 3, 42, 50, 4, 6, 40, 48, 7, 4))
example_df$total_area = example_df$sub_area * example_df$total_surveys
example_df$density = example_df$redd_count/example_df$total_area ## Ensure there are no NAs for density (0/0 in R results in NaN).


# Power Analysis ----
# Below is the bootstrap function. Input the control and treatment datasets, the number of bootstrap iterations, and a list of sample sizes per treatment that you would like to test. Tests 1-3 show how data can be subset to look at different questions and hypothesis
# Test 1: If you want to conduct the power analysis by year, subset the data for the year of interest then create the control and treatment data frames.
# Test 2: If you want to conduct the power analysis by location, subset the data to include the location of interest (ex: upstream only or downstream only) then create the control and treatment data frames.
# Test 3: If you want to conduct the power analysis to look at the effect of seasonality and sampling regimes. The data needs to be filtered to only have the number of redds from the weeks of interest (surveys 2-4 instead of surveys 1-5) and the total_surveys must be updated to reflect the removed surveys (total_surveys = 3 instead of total_survyes = 5)


## Function Inputs ----
control = example_df[example_df$treatment == "Control",] # control data
treatment = example_df[example_df$treatment == "Treatment",] # treatment data
n_sites = seq(from = 2, to = 6, by = 1) # number of samples for each treatment for each power analysis (n_sites will vary based on your data and how many sites you have to pull from. ) 
boot_itr = 1000 # Number of bootstrap iterations


## Function ----
boot_func <- function(control, treatment, n_sites, boot_itr){
  
  total_rows <- length(n_sites)*boot_itr # pre-allocate results table
  # create results table
  boot_res <- data.frame(n = numeric(total_rows),         
                         effect_size = numeric(total_rows),
                         pvalue = numeric(total_rows))
  counter <- 1 # counter for storing results
  
  for(n_site in n_sites){ # repeat bootstrap for each n value
    for(i in 1:boot_itr){ 
      
      # resample data
      con <- control[sample(nrow(control), size = n_site, replace = TRUE), ] # resample control data with replacement
      treat <- treatment[sample(nrow(treatment), size = n_site, replace = TRUE), ] # resample treatment data with replacement
      
      # Effect size
      eff_size <- as.numeric(cohen.d(treat$density, con$density)$estimate)
      eff_size[is.infinite(eff_size)] <- NA # change Inf to Na for ease when calculating mean effect size
      
      # non parametric test: tryCatch() added due to NA and INF values with small sample sizes and 0 densities. Warning about ties is OK.
      MW <- tryCatch(
        wilcox.test(treat$density, con$density, alternative = "greater", paired = FALSE), # alternative = "greater" tests for redd density treatment > redd density control.
        error = function(e) list(p.value = NA))
      p_value <- MW$p.value
      
      # Save results
      res <- c(n_site, eff_size, p_value)
      boot_res[counter,] <- res
      counter <- counter +1
      
    } #/bootstrap
  } #/sample size
  
  # summary of results for each sample size.
  summary <- boot_res %>% group_by(n) %>% summarize(mean_eff_size = mean(effect_size, na.rm = T),
                                                    mean_power = mean(pvalue < 0.05, na.rm=T)) #Accept Alternative if p-value is less than 0.05.
  return(summary)
  
} #/ Function


## Results ----
set.seed(2026) # set before calling the function for reproducibility
ex1 <- boot_func(control, treatment, n_sites, boot_itr) ## Warning about ties sometimes happens this is due to 0 densities being resampled multiple times. 





 
       
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
           
     