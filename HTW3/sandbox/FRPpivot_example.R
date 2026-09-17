#demo for pascale on how to add zeros

library(tidyverse)


Sites = read_csv("https://pasta.lternet.edu/package/data/eml/edi/269/6/ce64ec720105ebc3d887e511965e1095") #sampling_data
Zoops = read_csv("https://pasta.lternet.edu/package/data/eml/edi/269/6/53e5b46098e9cba7accfb575ec493a14") # data

#join sites with zoop data

ZoopsSites = left_join(Zoops, Sites, by = c("VisitNo", "Date"))

glimpse(ZoopsSites)

#Something is up with the "Location", a few of them don't match

weirdsites = filter(ZoopsSites, Location.x != Location.y)
#oh, one table is missing the "s" in "Berms". I thought I told them to fix that. Whatever.

ZoopsSites = mutate(ZoopsSites, Location = Location.x) %>%
  select(-Location.x, -Location.y)

#now to add the zeros.
# I use "pivot_wider" with value_fill = 0, then "pivot_longer".
#some people prefer "complete_cases", but I have trouble getting that to work right.

ZoopswZeros = ZoopsSites %>%
  arrange(CommonName) %>%
  pivot_wider( id_cols = c(VisitNo:subsample,effort, Temp:Location),
                          names_from = CommonName, #value we want for the column names
               values_from = Count, #value we want in the cells - this is the number counted, not adjusted for subsampling or effort yet.
               values_fill = 0) %>% #fill any missing values with zeros

  #now put it back into longformat
  pivot_longer(cols = c(Acanthocyclops:`Worm UNID`), names_to = "CommonName", values_to = "Count")

nrow(ZoopsSites)
nrow(ZoopswZeros)

#test out whether we have zeros for jus one taxon
pseudo = filter(ZoopswZeros, CommonName == "Pseudodiaptomus forbesi")

summary(pseudo)

