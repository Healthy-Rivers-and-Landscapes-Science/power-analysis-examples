# ----------------------------------------------------------------------------
## HTribPop1 power analysis
## Estimate adult carcass tissue sample sizes for detecting floodplain rearing
## in natural origin CCV Chinook salmon. 
# ----------------------------------------------------------------------------



# ------------------------------------------------------------
## Functions used for power analysis & Results Table
# ------------------------------------------------------------


calculate_power_result <- function(approach, year, p1, p0, pS, alpha = 0.05, power = 0.80)
{
  ## keep the value slightly below 1 so the power function can solve for n
  p1_power <- ifelse(p1 == 1, 0.99, p1)

  power_result <- pwrss.z.prop(
    p = p1_power,
    p0 = p0,
    alpha = alpha,
    alternative = "not equal",
    n = NULL,
    power = power
  )

  result <- tibble::tibble(
    Approach = approach,
    Year = year,
    p1 = p1_power,
    Effect_Size = abs(p1_power - p0),
    pS = pS,
    n_required = ceiling(power_result$n),
    natal_origin_rate_adjusted_n = ceiling(ceiling(power_result$n) / pS)
  )

  return(result)
}


# ------------------------------------------------------------
## Now setup inputs for power analysis
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(knitr)
  library(pwrss)
})

## this defines the null expectation of equal off-channel and channel habitat use
p0 <- 0.50

## this defines the combined off-channel proportion from Sacramento adult datasets
p1 <- 0.83

## this defines the combined Sacramento true natal origin assignment rate
pS <- 0.49

## this defines the number of otoliths analyzed and true Sacramento origin adults 
oto_all_2016 <- 170
oto_sac_2016 <- 60
oto_all_2017 <- 193
oto_sac_2017 <- 115
oto_all_2021 <- 61
oto_sac_2021 <- 32

## this defines the number of eye lenses analyzed and the number of off-channel users
lens_all_2016 <- 54
lens_off_2016 <- 38
lens_all_2017 <- 102
lens_off_2017 <- 89
lens_all_2021 <- 19
lens_off_2021 <- 19

## create input summary table 
input_table <- tibble::tribble(
  ~outmigration_year, ~otolith_analyzed, ~true_origin, ~true_origin_pct, ~lens_analyzed, ~off_channel, ~off_channel_pct, ~channel_pct,
  "2016", oto_all_2016, oto_sac_2016, oto_sac_2016 / oto_all_2016, lens_all_2016, lens_off_2016, lens_off_2016 / lens_all_2016, 1 - lens_off_2016 / lens_all_2016,
  "2017", oto_all_2017, oto_sac_2017, oto_sac_2017 / oto_all_2017, lens_all_2017, lens_off_2017, lens_off_2017 / lens_all_2017, 1 - lens_off_2017 / lens_all_2017,
  "2021", oto_all_2021, oto_sac_2021, oto_sac_2021 / oto_all_2021, lens_all_2021, lens_off_2021, lens_off_2021 / lens_all_2021, 1 - lens_off_2021 / lens_all_2021,
  "Total",
  oto_all_2016 + oto_all_2017 + oto_all_2021,
  oto_sac_2016 + oto_sac_2017 + oto_sac_2021,
  (oto_sac_2016 + oto_sac_2017 + oto_sac_2021) / (oto_all_2016 + oto_all_2017 + oto_all_2021),
  lens_all_2016 + lens_all_2017 + lens_all_2021,
  lens_off_2016 + lens_off_2017 + lens_off_2021,
  (lens_off_2016 + lens_off_2017 + lens_off_2021) / (lens_all_2016 + lens_all_2017 + lens_all_2021),
  1 - (lens_off_2016 + lens_off_2017 + lens_off_2021) / (lens_all_2016 + lens_all_2017 + lens_all_2021)
)

# ------------------------------------------------------------
## Run combined effect size power analysis
# ------------------------------------------------------------

## this approach uses the combined Sacramento adult off-channel proportion across
## outmigration year 2016, 2017 and 2021
combined_result <- calculate_power_result(
  approach = "Combined",
  year = "Total",
  p1 = p1,
  p0 = p0,
  pS = pS
)

# ------------------------------------------------------------
## Run range effect size power analysis 
# ------------------------------------------------------------

## this approach uses year-specific Sacramento adult off-channel proportions
p1_2016 <- lens_off_2016 / lens_all_2016
pS_2016 <- oto_sac_2016 / oto_all_2016

p1_2017 <- lens_off_2017 / lens_all_2017
pS_2017 <- oto_sac_2017 / oto_all_2017

p1_2021 <- lens_off_2021 / lens_all_2021
pS_2021 <- oto_sac_2021 / oto_all_2021

result_2016 <- calculate_power_result(
  approach = "Range",
  year = "2016",
  p1 = p1_2016,
  p0 = p0,
  pS = pS_2016
)

result_2017 <- calculate_power_result(
  approach = "Range",
  year = "2017",
  p1 = p1_2017,
  p0 = p0,
  pS = pS_2017
)

result_2021 <- calculate_power_result(
  approach = "Range",
  year = "2021",
  p1 = p1_2021,
  p0 = p0,
  pS = pS_2021
)

# ------------------------------------------------------------
## Bind and display results
# ------------------------------------------------------------

results_table <- bind_rows(result_2016, result_2017, result_2021, combined_result)

cat("Table 1. Results of Otolith and Eye Lens Analyses of Sacramento River Returning Adults with Adipose Fin Present from Outmigration Year 2016, 2017 and 2021.\n\n")
print(knitr::kable(
  input_table,
  digits = 2,
  col.names = c(
    "Outmigration year(s)",
    "# otolith analyzed",
    "# true origin",
    "% true origin",
    "# lens analyzed",
    "# off-channel",
    "% off-channel usage",
    "% channel usage"
  )
))

cat("\n\nTable 2. Sample Size Estimates From Year Combined and Year Specific One Proportion Power Analyses.\n\n")
print(knitr::kable(
  results_table,
  digits = 2,
  col.names = c(
    "Approach",
    "Year",
    "p1",
    "Effect Size",
    "Natal Origin Rate",
    "Natal Origin n",
    "Natal Origin Rate Adjusted n"
  )
))
