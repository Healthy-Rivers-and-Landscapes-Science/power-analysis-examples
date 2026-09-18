#Load the library
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
dat_spring <- subset(dat_season, Season == "Spring")

# subset to sites of interest
dat_loc = filter(dat_spring, Location %in% c("Browns Island","Decker Island","Ryer Island","Tule Red","Webb Tract Islands and Berms","Winter Island"))

# Eurytemora only
eury_dat <- subset(dat_loc, CommonName == c("Eurytemora affinis", "Eurytemora copepodid", "Eurytemora nauplii"))

test = dat_spring %>%
  filter(Date>= as.Date("2019/10/15")) %>%
  select(SampleID_frp, Location) %>%
  distinct() %>%
  group_by(Location) %>%
  summarize(N = n())

dat_spring_wzeros = pivot_wider(dat_spring,
                                id_cols = c(SampleID_frp, Location, Date),
                                names_from = CommonName, values_from = CPUE, values_fn = sum,
                                values_fill = 0) %>%
  pivot_longer(cols = c(`Pseudodiaptomus nauplii`:last_col()), names_to = "CommonName", values_to = "CPUE")

eury_spring_wzeros = filter(dat_spring_wzeros, CommonName %in% c("Eurytemora affinis", "Eurytemora copepodid")) %>%
  group_by(SampleID_frp, Location, Date) %>%
  summarize(CPUE = sum(CPUE))




# zero data needs to be added
all_samples <- unique(dat_loc[,c(4,6)])
eury_samples <- unique(eury_dat[,c(4,6)])

all_samples$ID <- paste(all_samples$Location, all_samples$Date, sep = "")
eury_samples$ID <- paste(eury_samples$Location, eury_samples$Date, sep = "") # 18 less

all_samples$CPUE = 0

check <- merge(all_samples, eury_samples, by = "ID", all = TRUE) # there are zeros that need to be added

check_filtered <- check[is.na(check$Location.y), ] # keep NAs from eury data

check_filtered <- check_filtered[,c(2:4)]

colnames(check_filtered) <- c("Location", "Date", "CPUE")

# final data
zeros <- setdiff(names(eury_dat), names(check_filtered))
check_filtered[zeros] <- NA

fin_dat <- rbind(eury_dat, check_filtered)

write.csv(fin_dat, "pwr_dat.csv")

ggplot(dat_loc, aes(x = Location, y = CPUE)) +
  geom_boxplot() +
  facet_grid( ~factor(SiteType, levels = c("Post-restoration", "Reference", "Pre-restoration"))) +
  ylim(0, 10)+
  scale_y_log10()
