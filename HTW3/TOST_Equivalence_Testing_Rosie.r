#FRP data organization and TOST test from Rosie


library(EDIutils)
library(tidyverse)

# Set your access/API key in the R session environment
Sys.setenv(EDI_API_KEY = "JmNEJU-QJlBRfemWBXRUZ8AvKbc")

# Call the method to read entity names and IDs for a package
temp <- read_data_entity_names(packageId = "edi.269.6")
# zoop
temp_dat <- read_data_entity(packageId = "edi.269.6", entityId = temp$entityId[3])
data <- readr::read_csv(file = temp_dat)

# spring only
dat_season = data %>% mutate(Month = month(Date),
                             Season  = case_when(Month %in% c(3,4,5) ~ "Spring",
                                                 Month %in% c(6,7,8) ~ "Summer",
                                                 Month %in% c(9,10,11) ~ "Fall",
                                                 Month %in% c(12,1,2) ~ "Winter"))

# subset to just spring

dat_spring <- subset(dat_season, Season == "Spring")

#add in zeros
dat_spring_wzeros = pivot_wider(dat_spring,
                                id_cols = c(SampleID_frp, Location, Date),
                                names_from = CommonName, values_from = CPUE, values_fn = sum,
                                values_fill = 0) %>%
  pivot_longer(cols = c(`Pseudodiaptomus nauplii`:last_col()), names_to = "CommonName", values_to = "CPUE")

#filter to just eurytemora juveniles and adults,
eury_spring_wzeros = filter(dat_spring_wzeros, CommonName %in% c("Eurytemora affinis", "Eurytemora copepodid")) %>%
  group_by(SampleID_frp, Location, Date) %>%
  summarize(CPUE = sum(CPUE), logCPUE = log(CPUE+1))


hist(eury_spring_wzeros$CPUE)

hist(eury_spring_wzeros$logCPUE)

ggplot(eury_spring_wzeros, aes(x= Location, y = logCPUE)) + geom_boxplot()

#now for the TOST test ###########################
#filter to just two sites, calculate mean and sd
TuleRed = filter(eury_spring_wzeros, Location == "Tule Red" & Date>= as.Date("2019/10/15"))
Ryer = filter(eury_spring_wzeros, Location == "Ryer Island" & Date>= as.Date("2019/10/15"))


#log-transformed data
tost_result2 <- tsum_TOST(
  m1 = mean(TuleRed$logCPUE),
  m2 = mean(Ryer$logCPUE),
  sd1 = sd(TuleRed$logCPUE),
  sd2 = sd(Ryer$logCPUE),
  n1 = 20,
  n2 = 27,
eqb = 1,
  alpha = 0.05
)

print(tost_result2)
describe(tost_result2)


tostpower <- power_t_TOST(
  alpha = 0.05,
  power = 0.80,
  eqb =1,
  sd = 1.49
)
tostpower
