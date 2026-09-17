# ============================================================
# TOST Equivalence Testing in R: Annotated Example
# ============================================================

# Reference: https://aaroncaldwell.us/TOSTERpkg/articles/IntroTOSTt.html#power-analysis-for-tost

#
# Example assumptions:
#   Equivalence margin = +/- 0.2 mg/L
#   Standard deviation = 0.3 mg/L
#   Desired power = 90%
#   Alpha = 0.05
#   Expected true bias = 0.05 mg/L
#
# ============================================================

# Install package if needed
# install.packages("TOSTER")

library(TOSTER)
library(tidyverse)

# ============================================================
# STEP 1: Define study assumptions
# ============================================================

delta <- 0.2      # Equivalence margin
sigma <- 0.3      # Expected standard deviation
true_diff <- 0.05 # Expected true difference


delta <- 40      # Equivalence margin
sigma <- 50      # Expected standard deviation
true_diff <- 35 # Expected true difference


# ============================================================
# STEP 2: Convert to standardized effect sizes
# ============================================================

low_eqbound_d  <- -delta / sigma
high_eqbound_d <-  delta / sigma
expected_d     <- true_diff / sigma

cat("Lower equivalence bound (d):", low_eqbound_d, "\n")
cat("Upper equivalence bound (d):", high_eqbound_d, "\n")
cat("Expected effect size (d):", expected_d, "\n")

?power_t_TOST

# ============================================================
# STEP 3: Power analysis (two-sample design)
# ============================================================

power_result <- power_t_TOST(
  delta = true_diff,
  sd = sigma,
  eqb = expected_d,
  power = 0.90,
  alpha = 0.05,
  low_eqbound = low_eqbound_d,
  high_eqbound = high_eqbound_d,
  type = "two.sample"
)

power_result <- power_t_TOST(
  delta = -600, # True difference in mean between restored and reference post
  sd = 1272.856, # Full data set SD restored and reference post
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
# STEP 4: get data
# ============================================================

# test "Ryer Island", "Tule Red", 2019/10/15
ref <- subset(eury_dat_rest, Location == "Ryer Island" & Date>= as.Date("2019/10/15"))
current_method <- ref$Count/ref$effort

#I don't know what current method is or new method.
summary(ref$CPUE)
summary(current_method)
#no zeros

#pascale says this might be it
mightbeit = read_csv("pwr_dat.csv")
summary(mightbeit)
#well, this seems odd
ref <- subset(mightbeit, Location == "Ryer Island" & Date>= as.Date("2019/10/15"))
hist(log(ref$CPUE))

current_method <- ref$Count/ref$effort


##############################
head(ref)
mean(current_method, na.rm=TRUE)
sd(current_method, na.rm = TRUE)

mean(new_method, na.rm=TRUE)
sd(new_method, na.rm=TRUE)
mean(current_method, na.rm=TRUE)-mean(new_method, na.rm=TRUE)

# -35.06795, -1.614559, -0.9234827
##############################

restored <- subset(eury_dat_rest, Location == "Tule Red" & Date>= as.Date("2019/10/15"))
restored2 = filter(mightbeit, Location == "Tule Red" & Date>= as.Date("2019/10/15"))
new_method <- restored$Count/restored$effort

# test "Browns Island", "Winter Island", 2019/09/25
ref <- subset(eury_dat_rest, Location == "Browns Island" & Date>= as.Date("2019/09/25"))
current_method <- ref$Count/ref$effort

restored <- subset(eury_dat_rest, Location == "Winter Island" & Date>= as.Date("2019/09/25"))
new_method <- restored$Count/restored$effort

# test "Webb Tract Islands and Berms", "Decker Island", 2018/10/11
ref <- subset(eury_dat_rest, Location == "Webb Tract Islands and Berms" & Date>= as.Date("2018/10/11"))
current_method <- ref$Count/ref$effort

restored <- subset(eury_dat_rest, Location == "Decker Island" & Date>= as.Date("2018/10/11"))
new_method <- restored$Count/restored$effort

# ============================================================
# STEP 5: Run TOST for independent samples
# ============================================================

tost_result <- tsum_TOST(
  m1 = mean(current_method),
  m2 = mean(new_method),
  sd1 = sd(current_method),
  sd2 = sd(new_method),
  n1 = n,
  n2 = n,
  low_eqbound = -0.2,
  high_eqbound = 0.2,
  alpha = 0.05
)

print(tost_result)
describe(tost_result)
# Results: Reject the null, so the test confirms the difference is small and the methods are equivalent.

# ============================================================
# STEP 6: Paired-sample power analysis
# ============================================================
#
# Often preferred for method-comparison studies because
# each sample is analyzed by both methods.
#

paired_power <- powerTOSTpaired(
  alpha = 0.05,
  statistical_power = 0.90,
  low_eqbound_dz = low_eqbound_d,
  high_eqbound_dz = high_eqbound_d
)

print(paired_power)

# You would need 25 pairs to get a power of 0.90 with equivalence bounds of -0.66 and 0.66.

# ============================================================
# STEP 7: Paired-sample TOST
# ============================================================

paired_tost <- TOSTpaired.raw(
  n = length(current_method),
  m1 = mean(current_method),
  m2 = mean(new_method),
  sd1 = sd(current_method),
  sd2 = sd(new_method),
  low_eqbound = -0.2,
  high_eqbound = 0.2,
  r12 = cor(current_method, new_method), ## Can only use cor() if the index in current and new method are matched, which they should be if we are pairing them.
  alpha = 0.05
)

# Results: Reject the null, the data is equivalent.


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
