# ============================================================
# TOST Equivalence Testing in R: Annotated Example
# ============================================================

# Reference: https://aaroncaldwell.us/TOSTERpkg/articles/IntroTOSTt.html#power-analysis-for-tost

# ============================================================

# Install package if needed
# install.packages("TOSTER")

library(TOSTER)
library(tidyverse)

# ============================================================

# ============================================================

# ============================================================
# Power analysis (two-sample design) with data
# ============================================================

# code for TW3
# inputs using data
# run get_data_wtih_zeros.R to produce fin_dat
# library

# delta
# true difference
# difference between the mean restored and reference CPUE after construction
fin_dat %>%
  group_by(Location, test) %>%
  summarise(mean(CPUE)
  )

#Ryer - Tule Red = -1332
410-1742

#Browns - Winter = -600
370-970

#Webb - Decker = 311
692-381

# sigma
# exoected standard deviation
#standard deviate of all sites
sd(fin_dat$CPUE) #1272.856

# desired power = 0.08
# alpha = 0.05


power_result <- power_t_TOST(
  delta = -600, # True difference in mean between restored and reference post
  sd = 1272.856, # Full data set SD restored and reference
  eqb = 1272.856     , # the margin that you would consider restored and reference post are equivalent = +/- xx CPUE (e.g., within one SD)
  power = 0.80,
  alpha = 0.05,
  #low_eqbound = low_eqbound_d,
  #high_eqbound = high_eqbound_d,
  type = "two.sample"
)


print(power_result)
# Kiea Results: For a power of 0.90 alpha of 0.05 and the given bounds, we would need a sample size of 113.96 = 114 (always round up)




# ============================================================
# Interpretation:
#
# Traditional NHST:
#   "Can I detect a difference?"
#
# TOST:
#   "Can I rule out differences larger than my
#    predefined equivalence margin?"
#
# A non-significant t-test does NOT demonstrate equivalence.
# A significant TOST provides evidence that the true
# difference lies within the equivalence bounds.
# ============================================================
