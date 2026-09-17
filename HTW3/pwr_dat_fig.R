# run get_data_wtih_zeros.R to produce fin_dat
# library
library(ggplot2)
library(ggsignif)
library(patchwork)
library(tidyverse)

# add construction date
# Winter Island, 2019/09/25
# Tule Red, 2019/10/15
# Decker Island, 2018/10/11
fin_dat$consutruction_date <- ifelse(fin_dat$Location == "Winter Island", as.Date("2019/09/25"),
                                     ifelse(fin_dat$Location == "Browns Island", as.Date("2019/09/25"),
                                            ifelse(fin_dat$Location == "Tule Red", as.Date("2019/10/15"),
                                                   ifelse(fin_dat$Location == "Ryer Island", as.Date("2019/10/15"),
                                                          ifelse(fin_dat$Location == "Decker Island", as.Date("2018/10/11"),
                                                                 ifelse(fin_dat$Location == "Webb Tract Islands and Berms", as.Date("2018/10/11"),
                                                                        NA))))))

fin_dat$test <- ifelse(fin_dat$Date <= fin_dat$consutruction_date, "before",
                      ifelse (fin_dat$Date > fin_dat$consutruction_date, "after",
                              NA))

# check if it worked correctly
fin_dat$consutruction_date <- as.Date(fin_dat$consutruction_date)
unique(fin_dat[,c(4,30)])

# summary
fin_dat %>%
  group_by(Location, test) %>%
  summarise(
    min_date = min(Date),
    max_date = max(Date),
    sample_size = n(),
    mean(CPUE),
    sd(CPUE)
  )

sd(fin_dat$CPUE) # 1272.856

# plot
RI_TR <- subset(fin_dat, Location == c("Ryer Island", "Tule Red"))
BI_WI <- subset(fin_dat, Location == c("Browns Island", "Winter Island"))
WI_DI <- subset(fin_dat, Location == c("Webb Tract Islands and Berms", "Decker Island"))


p1 <-  ggplot(RI_TR, aes(x = Location, y = CPUE, fill = Location)) +
  geom_boxplot() +
  facet_grid( ~factor(test, levels = c("before", "after"))) +
  scale_y_log10()+
  scale_fill_manual(values = c("Tule Red" = "#555F61",
                               "Ryer Island" = "#F2F3F4")) +
  #geom_signif(comparisons = list(c("Ryer Island", "Tule Red")),
  #            map_signif_level=TRUE) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  labs( y = "log(CPUE)")

p2 <- ggplot(BI_WI, aes(x = Location, y = CPUE, fill = Location)) +
  geom_boxplot() +
  facet_grid( ~factor(test, levels = c("before", "after"))) +
  scale_y_log10()+
  scale_fill_manual(values = c("Winter Island" = "#555F61",
                               "Browns Island" = "#F2F3F4")) +
  #geom_signif(comparisons = list(c("Browns Island", "Winter Island")),
  #                            map_signif_level=TRUE)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank())+
  labs( y = "log(CPUE)")

p3 <- ggplot(WI_DI, aes(x = fct_relevel(Location, "Webb Tract Islands and Berms", "Decker Island"), y = CPUE, fill = Location)) +
  geom_boxplot() +
  facet_grid( ~factor(test, levels = c("before", "after"))) +
  scale_y_log10()+
  scale_fill_manual(values = c("Decker Island" = "#555F61",
                               "Webb Tract Islands and Berms" = "#F2F3F4")) +
  #geom_signif(comparisons = list(c("Decker Island", "Webb Tract Islands and Berms")),
  #                            map_signif_level=TRUE)+
  theme_bw(base_size = 18) +
  theme(legend.position = "none")+
  theme(axis.title.x = element_blank()) +
  labs( y = "log(CPUE)")

p1 / p2 / p3
