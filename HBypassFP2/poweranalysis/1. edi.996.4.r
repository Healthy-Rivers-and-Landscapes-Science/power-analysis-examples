  # Package ID: edi.996.4 Cataloging System:https://pasta.edirepository.org.
  # Data set title: Fish Food on Floodplain Farm Fields, California Central Valley, Seasons 2019 and 2021.
  # Data set creator:  Jacob Montgomery - California Trout
  # Contact:  Jacob Montgomery -  California Trout  - jacob@caltrout.org
  # Stylesheet v2.16 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu
  # Uncomment the following lines to have R clear previous work, or set a working directory
  # rm(list=ls())

setwd("C:/Users/...")
rm(list=ls())


options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/996/4/93a42c53f51fd826f2d3914d5cbc913a"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "date",
                 "location",
                 "pit",
                 "length_mm",
                 "weight_g",
                 "comment"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$date dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp1date<-as.Date(dt1$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$date != "",]) == length(tmp1date[!is.na(tmp1date)])){dt1$date <- tmp1date } else {print("Date conversion failed for dt1$date. Please inspect the data and do the date conversion yourself.")}

if (class(dt1$location)!="factor") dt1$location<- as.factor(dt1$location)
if (class(dt1$pit)!="factor") dt1$pit<- as.factor(dt1$pit)
if (class(dt1$length_mm)=="factor") dt1$length_mm <-as.numeric(levels(dt1$length_mm))[as.integer(dt1$length_mm) ]
if (class(dt1$length_mm)=="character") dt1$length_mm <-as.numeric(dt1$length_mm)
if (class(dt1$weight_g)=="factor") dt1$weight_g <-as.numeric(levels(dt1$weight_g))[as.integer(dt1$weight_g) ]
if (class(dt1$weight_g)=="character") dt1$weight_g <-as.numeric(dt1$weight_g)
if (class(dt1$comment)!="factor") dt1$comment<- as.factor(dt1$comment)

# Convert Missing Values to NA for non-dates

dt1$location <- as.factor(ifelse((trimws(as.character(dt1$location))==trimws("NA")),NA,as.character(dt1$location)))
dt1$pit <- as.factor(ifelse((trimws(as.character(dt1$pit))==trimws("NA")),NA,as.character(dt1$pit)))
dt1$length_mm <- ifelse((trimws(as.character(dt1$length_mm))==trimws("NA")),NA,dt1$length_mm)
suppressWarnings(dt1$length_mm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$length_mm))==as.character(as.numeric("NA"))),NA,dt1$length_mm))
dt1$weight_g <- ifelse((trimws(as.character(dt1$weight_g))==trimws("NA")),NA,dt1$weight_g)
suppressWarnings(dt1$weight_g <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$weight_g))==as.character(as.numeric("NA"))),NA,dt1$weight_g))
dt1$comment <- as.factor(ifelse((trimws(as.character(dt1$comment))==trimws("NA")),NA,as.character(dt1$comment)))


# Here is the structure of the input data frame:
print("dt1) Structure")
str(dt1)
attach(dt1)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

print(" ")
print("Summary of date")
print(summary(date))
print(" ")
print("Summary of location")
print(summary(location))
print(" ")
print("Summary of pit")
print(summary(pit))
print(" ")
print("Summary of length_mm")
print(summary(length_mm))
print(" ")
print("Summary of weight_g")
print(summary(weight_g))
print(" ")
print("Summary of comment")
print(summary(comment))
# Get more details on character variables


print(" ")
print("Summary of location")
print(summary(as.factor(dt1$location)))

print(" ")
print("Summary of pit")
print(summary(as.factor(dt1$pit)))

print(" ")
print("Summary of comment")
print(summary(as.factor(dt1$comment)))
detach(dt1)



inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/996/4/51359c8a7192198eff8ef4aa0a46eeb3"
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "location",
                 "date",
                 "time",
                 "do_mg_l",
                 "temp_f"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$location)!="factor") dt2$location<- as.factor(dt2$location)
# attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt2[dt2$date != "",]) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}

if (class(dt2$do_mg_l)=="factor") dt2$do_mg_l <-as.numeric(levels(dt2$do_mg_l))[as.integer(dt2$do_mg_l) ]
if (class(dt2$do_mg_l)=="character") dt2$do_mg_l <-as.numeric(dt2$do_mg_l)
if (class(dt2$temp_f)=="factor") dt2$temp_f <-as.numeric(levels(dt2$temp_f))[as.integer(dt2$temp_f) ]
if (class(dt2$temp_f)=="character") dt2$temp_f <-as.numeric(dt2$temp_f)

# Convert Missing Values to NA for non-dates

dt2$location <- as.factor(ifelse((trimws(as.character(dt2$location))==trimws("NA")),NA,as.character(dt2$location)))
dt2$do_mg_l <- ifelse((trimws(as.character(dt2$do_mg_l))==trimws("NA")),NA,dt2$do_mg_l)
suppressWarnings(dt2$do_mg_l <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$do_mg_l))==as.character(as.numeric("NA"))),NA,dt2$do_mg_l))
dt2$temp_f <- ifelse((trimws(as.character(dt2$temp_f))==trimws("NA")),NA,dt2$temp_f)
suppressWarnings(dt2$temp_f <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$temp_f))==as.character(as.numeric("NA"))),NA,dt2$temp_f))


# Here is the structure of the input data frame:
print("dt2) Structure")
str(dt2)
attach(dt2)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

print(" ")
print("Summary of location")
print(summary(location))
print(" ")
print("Summary of date")
print(summary(date))
print(" ")
print("Summary of time")
print(summary(time))
print(" ")
print("Summary of do_mg_l")
print(summary(do_mg_l))
print(" ")
print("Summary of temp_f")
print(summary(temp_f))
# Get more details on character variables


print(" ")
print("Summary of location")
print(summary(as.factor(dt2$location)))
detach(dt2)



inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/996/4/9db194385e5e7bfbbe95fcbcd9911c3c"
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "id",
                 "pseudodiptomus_adult",
                 "psedodiptomus_copepidite",
                 "acanthocyclops_adult",
                 "acanthocyclops_copepididte",
                 "cyclopoid_sp",
                 "cyclopoid_nauplii",
                 "calanoid_naulpii",
                 "harpaticoid",
                 "daphnia_pulex",
                 "daphnia_laevis",
                 "daphnia_magna",
                 "daphnia_mendotea",
                 "ceriodaphnia_sp",
                 "simocephalus_sp",
                 "bosmina_sp",
                 "sididae",
                 "chydorus_sphaericus",
                 "eurycercus",
                 "alona",
                 "chydoridae",
                 "diaphanosoma",
                 "scapholeberis",
                 "cladocera_embryo",
                 "eucypris",
                 "ilyocypris",
                 "cypridopsis",
                 "gammarus",
                 "rotifer",
                 "polychaete",
                 "acari",
                 "chironomid_larvae",
                 "oliogochaete",
                 "gastropod",
                 "tardigrade",
                 "nematode",
                 "hyalella",
                 "hydra",
                 "terrestrial_insect",
                 "collembola",
                 "baetidae",
                 "diptera",
                 "hydroptilidae",
                 "coleoptera",
                 "ephemerellidae",
                 "odonata",
                 "bivalve",
                 "trichoptera",
                 "corixidae",
                 "fish_larvae",
                 "streblocerus",
                 "amphipod",
                 "date",
                 "location",
                 "time",
                 "temp_c",
                 "ec_s_cm",
                 "spc_s_cm",
                 "tds_mg_l",
                 "salinity_psu",
                 "do_sat",
                 "do_mg_l",
                 "ph",
                 "turbidity_ntu",
                 "chl_g_l",
                 "bga_g_l",
                 "start_rotation",
                 "end_rotation",
                 "zoop_score_1_10",
                 "notes",
                 "volume_sampled",
                 "ilyocryptus",
                 "moina"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$id)!="factor") dt3$id<- as.factor(dt3$id)
if (class(dt3$pseudodiptomus_adult)=="factor") dt3$pseudodiptomus_adult <-as.numeric(levels(dt3$pseudodiptomus_adult))[as.integer(dt3$pseudodiptomus_adult) ]
if (class(dt3$pseudodiptomus_adult)=="character") dt3$pseudodiptomus_adult <-as.numeric(dt3$pseudodiptomus_adult)
if (class(dt3$psedodiptomus_copepidite)=="factor") dt3$psedodiptomus_copepidite <-as.numeric(levels(dt3$psedodiptomus_copepidite))[as.integer(dt3$psedodiptomus_copepidite) ]
if (class(dt3$psedodiptomus_copepidite)=="character") dt3$psedodiptomus_copepidite <-as.numeric(dt3$psedodiptomus_copepidite)
if (class(dt3$acanthocyclops_adult)=="factor") dt3$acanthocyclops_adult <-as.numeric(levels(dt3$acanthocyclops_adult))[as.integer(dt3$acanthocyclops_adult) ]
if (class(dt3$acanthocyclops_adult)=="character") dt3$acanthocyclops_adult <-as.numeric(dt3$acanthocyclops_adult)
if (class(dt3$acanthocyclops_copepididte)=="factor") dt3$acanthocyclops_copepididte <-as.numeric(levels(dt3$acanthocyclops_copepididte))[as.integer(dt3$acanthocyclops_copepididte) ]
if (class(dt3$acanthocyclops_copepididte)=="character") dt3$acanthocyclops_copepididte <-as.numeric(dt3$acanthocyclops_copepididte)
if (class(dt3$cyclopoid_sp)=="factor") dt3$cyclopoid_sp <-as.numeric(levels(dt3$cyclopoid_sp))[as.integer(dt3$cyclopoid_sp) ]
if (class(dt3$cyclopoid_sp)=="character") dt3$cyclopoid_sp <-as.numeric(dt3$cyclopoid_sp)
if (class(dt3$cyclopoid_nauplii)=="factor") dt3$cyclopoid_nauplii <-as.numeric(levels(dt3$cyclopoid_nauplii))[as.integer(dt3$cyclopoid_nauplii) ]
if (class(dt3$cyclopoid_nauplii)=="character") dt3$cyclopoid_nauplii <-as.numeric(dt3$cyclopoid_nauplii)
if (class(dt3$calanoid_naulpii)=="factor") dt3$calanoid_naulpii <-as.numeric(levels(dt3$calanoid_naulpii))[as.integer(dt3$calanoid_naulpii) ]
if (class(dt3$calanoid_naulpii)=="character") dt3$calanoid_naulpii <-as.numeric(dt3$calanoid_naulpii)
if (class(dt3$harpaticoid)=="factor") dt3$harpaticoid <-as.numeric(levels(dt3$harpaticoid))[as.integer(dt3$harpaticoid) ]
if (class(dt3$harpaticoid)=="character") dt3$harpaticoid <-as.numeric(dt3$harpaticoid)
if (class(dt3$daphnia_pulex)=="factor") dt3$daphnia_pulex <-as.numeric(levels(dt3$daphnia_pulex))[as.integer(dt3$daphnia_pulex) ]
if (class(dt3$daphnia_pulex)=="character") dt3$daphnia_pulex <-as.numeric(dt3$daphnia_pulex)
if (class(dt3$daphnia_laevis)=="factor") dt3$daphnia_laevis <-as.numeric(levels(dt3$daphnia_laevis))[as.integer(dt3$daphnia_laevis) ]
if (class(dt3$daphnia_laevis)=="character") dt3$daphnia_laevis <-as.numeric(dt3$daphnia_laevis)
if (class(dt3$daphnia_magna)=="factor") dt3$daphnia_magna <-as.numeric(levels(dt3$daphnia_magna))[as.integer(dt3$daphnia_magna) ]
if (class(dt3$daphnia_magna)=="character") dt3$daphnia_magna <-as.numeric(dt3$daphnia_magna)
if (class(dt3$daphnia_mendotea)=="factor") dt3$daphnia_mendotea <-as.numeric(levels(dt3$daphnia_mendotea))[as.integer(dt3$daphnia_mendotea) ]
if (class(dt3$daphnia_mendotea)=="character") dt3$daphnia_mendotea <-as.numeric(dt3$daphnia_mendotea)
if (class(dt3$ceriodaphnia_sp)=="factor") dt3$ceriodaphnia_sp <-as.numeric(levels(dt3$ceriodaphnia_sp))[as.integer(dt3$ceriodaphnia_sp) ]
if (class(dt3$ceriodaphnia_sp)=="character") dt3$ceriodaphnia_sp <-as.numeric(dt3$ceriodaphnia_sp)
if (class(dt3$simocephalus_sp)=="factor") dt3$simocephalus_sp <-as.numeric(levels(dt3$simocephalus_sp))[as.integer(dt3$simocephalus_sp) ]
if (class(dt3$simocephalus_sp)=="character") dt3$simocephalus_sp <-as.numeric(dt3$simocephalus_sp)
if (class(dt3$bosmina_sp)=="factor") dt3$bosmina_sp <-as.numeric(levels(dt3$bosmina_sp))[as.integer(dt3$bosmina_sp) ]
if (class(dt3$bosmina_sp)=="character") dt3$bosmina_sp <-as.numeric(dt3$bosmina_sp)
if (class(dt3$sididae)=="factor") dt3$sididae <-as.numeric(levels(dt3$sididae))[as.integer(dt3$sididae) ]
if (class(dt3$sididae)=="character") dt3$sididae <-as.numeric(dt3$sididae)
if (class(dt3$chydorus_sphaericus)=="factor") dt3$chydorus_sphaericus <-as.numeric(levels(dt3$chydorus_sphaericus))[as.integer(dt3$chydorus_sphaericus) ]
if (class(dt3$chydorus_sphaericus)=="character") dt3$chydorus_sphaericus <-as.numeric(dt3$chydorus_sphaericus)
if (class(dt3$eurycercus)=="factor") dt3$eurycercus <-as.numeric(levels(dt3$eurycercus))[as.integer(dt3$eurycercus) ]
if (class(dt3$eurycercus)=="character") dt3$eurycercus <-as.numeric(dt3$eurycercus)
if (class(dt3$alona)=="factor") dt3$alona <-as.numeric(levels(dt3$alona))[as.integer(dt3$alona) ]
if (class(dt3$alona)=="character") dt3$alona <-as.numeric(dt3$alona)
if (class(dt3$chydoridae)=="factor") dt3$chydoridae <-as.numeric(levels(dt3$chydoridae))[as.integer(dt3$chydoridae) ]
if (class(dt3$chydoridae)=="character") dt3$chydoridae <-as.numeric(dt3$chydoridae)
if (class(dt3$diaphanosoma)=="factor") dt3$diaphanosoma <-as.numeric(levels(dt3$diaphanosoma))[as.integer(dt3$diaphanosoma) ]
if (class(dt3$diaphanosoma)=="character") dt3$diaphanosoma <-as.numeric(dt3$diaphanosoma)
if (class(dt3$scapholeberis)=="factor") dt3$scapholeberis <-as.numeric(levels(dt3$scapholeberis))[as.integer(dt3$scapholeberis) ]
if (class(dt3$scapholeberis)=="character") dt3$scapholeberis <-as.numeric(dt3$scapholeberis)
if (class(dt3$cladocera_embryo)=="factor") dt3$cladocera_embryo <-as.numeric(levels(dt3$cladocera_embryo))[as.integer(dt3$cladocera_embryo) ]
if (class(dt3$cladocera_embryo)=="character") dt3$cladocera_embryo <-as.numeric(dt3$cladocera_embryo)
if (class(dt3$eucypris)=="factor") dt3$eucypris <-as.numeric(levels(dt3$eucypris))[as.integer(dt3$eucypris) ]
if (class(dt3$eucypris)=="character") dt3$eucypris <-as.numeric(dt3$eucypris)
if (class(dt3$ilyocypris)=="factor") dt3$ilyocypris <-as.numeric(levels(dt3$ilyocypris))[as.integer(dt3$ilyocypris) ]
if (class(dt3$ilyocypris)=="character") dt3$ilyocypris <-as.numeric(dt3$ilyocypris)
if (class(dt3$cypridopsis)=="factor") dt3$cypridopsis <-as.numeric(levels(dt3$cypridopsis))[as.integer(dt3$cypridopsis) ]
if (class(dt3$cypridopsis)=="character") dt3$cypridopsis <-as.numeric(dt3$cypridopsis)
if (class(dt3$gammarus)=="factor") dt3$gammarus <-as.numeric(levels(dt3$gammarus))[as.integer(dt3$gammarus) ]
if (class(dt3$gammarus)=="character") dt3$gammarus <-as.numeric(dt3$gammarus)
if (class(dt3$rotifer)=="factor") dt3$rotifer <-as.numeric(levels(dt3$rotifer))[as.integer(dt3$rotifer) ]
if (class(dt3$rotifer)=="character") dt3$rotifer <-as.numeric(dt3$rotifer)
if (class(dt3$polychaete)=="factor") dt3$polychaete <-as.numeric(levels(dt3$polychaete))[as.integer(dt3$polychaete) ]
if (class(dt3$polychaete)=="character") dt3$polychaete <-as.numeric(dt3$polychaete)
if (class(dt3$acari)=="factor") dt3$acari <-as.numeric(levels(dt3$acari))[as.integer(dt3$acari) ]
if (class(dt3$acari)=="character") dt3$acari <-as.numeric(dt3$acari)
if (class(dt3$chironomid_larvae)=="factor") dt3$chironomid_larvae <-as.numeric(levels(dt3$chironomid_larvae))[as.integer(dt3$chironomid_larvae) ]
if (class(dt3$chironomid_larvae)=="character") dt3$chironomid_larvae <-as.numeric(dt3$chironomid_larvae)
if (class(dt3$oliogochaete)=="factor") dt3$oliogochaete <-as.numeric(levels(dt3$oliogochaete))[as.integer(dt3$oliogochaete) ]
if (class(dt3$oliogochaete)=="character") dt3$oliogochaete <-as.numeric(dt3$oliogochaete)
if (class(dt3$gastropod)=="factor") dt3$gastropod <-as.numeric(levels(dt3$gastropod))[as.integer(dt3$gastropod) ]
if (class(dt3$gastropod)=="character") dt3$gastropod <-as.numeric(dt3$gastropod)
if (class(dt3$tardigrade)=="factor") dt3$tardigrade <-as.numeric(levels(dt3$tardigrade))[as.integer(dt3$tardigrade) ]
if (class(dt3$tardigrade)=="character") dt3$tardigrade <-as.numeric(dt3$tardigrade)
if (class(dt3$nematode)=="factor") dt3$nematode <-as.numeric(levels(dt3$nematode))[as.integer(dt3$nematode) ]
if (class(dt3$nematode)=="character") dt3$nematode <-as.numeric(dt3$nematode)
if (class(dt3$hyalella)=="factor") dt3$hyalella <-as.numeric(levels(dt3$hyalella))[as.integer(dt3$hyalella) ]
if (class(dt3$hyalella)=="character") dt3$hyalella <-as.numeric(dt3$hyalella)
if (class(dt3$hydra)=="factor") dt3$hydra <-as.numeric(levels(dt3$hydra))[as.integer(dt3$hydra) ]
if (class(dt3$hydra)=="character") dt3$hydra <-as.numeric(dt3$hydra)
if (class(dt3$terrestrial_insect)=="factor") dt3$terrestrial_insect <-as.numeric(levels(dt3$terrestrial_insect))[as.integer(dt3$terrestrial_insect) ]
if (class(dt3$terrestrial_insect)=="character") dt3$terrestrial_insect <-as.numeric(dt3$terrestrial_insect)
if (class(dt3$collembola)=="factor") dt3$collembola <-as.numeric(levels(dt3$collembola))[as.integer(dt3$collembola) ]
if (class(dt3$collembola)=="character") dt3$collembola <-as.numeric(dt3$collembola)
if (class(dt3$baetidae)=="factor") dt3$baetidae <-as.numeric(levels(dt3$baetidae))[as.integer(dt3$baetidae) ]
if (class(dt3$baetidae)=="character") dt3$baetidae <-as.numeric(dt3$baetidae)
if (class(dt3$diptera)=="factor") dt3$diptera <-as.numeric(levels(dt3$diptera))[as.integer(dt3$diptera) ]
if (class(dt3$diptera)=="character") dt3$diptera <-as.numeric(dt3$diptera)
if (class(dt3$hydroptilidae)=="factor") dt3$hydroptilidae <-as.numeric(levels(dt3$hydroptilidae))[as.integer(dt3$hydroptilidae) ]
if (class(dt3$hydroptilidae)=="character") dt3$hydroptilidae <-as.numeric(dt3$hydroptilidae)
if (class(dt3$coleoptera)=="factor") dt3$coleoptera <-as.numeric(levels(dt3$coleoptera))[as.integer(dt3$coleoptera) ]
if (class(dt3$coleoptera)=="character") dt3$coleoptera <-as.numeric(dt3$coleoptera)
if (class(dt3$ephemerellidae)=="factor") dt3$ephemerellidae <-as.numeric(levels(dt3$ephemerellidae))[as.integer(dt3$ephemerellidae) ]
if (class(dt3$ephemerellidae)=="character") dt3$ephemerellidae <-as.numeric(dt3$ephemerellidae)
if (class(dt3$odonata)=="factor") dt3$odonata <-as.numeric(levels(dt3$odonata))[as.integer(dt3$odonata) ]
if (class(dt3$odonata)=="character") dt3$odonata <-as.numeric(dt3$odonata)
if (class(dt3$bivalve)=="factor") dt3$bivalve <-as.numeric(levels(dt3$bivalve))[as.integer(dt3$bivalve) ]
if (class(dt3$bivalve)=="character") dt3$bivalve <-as.numeric(dt3$bivalve)
if (class(dt3$trichoptera)=="factor") dt3$trichoptera <-as.numeric(levels(dt3$trichoptera))[as.integer(dt3$trichoptera) ]
if (class(dt3$trichoptera)=="character") dt3$trichoptera <-as.numeric(dt3$trichoptera)
if (class(dt3$corixidae)=="factor") dt3$corixidae <-as.numeric(levels(dt3$corixidae))[as.integer(dt3$corixidae) ]
if (class(dt3$corixidae)=="character") dt3$corixidae <-as.numeric(dt3$corixidae)
if (class(dt3$fish_larvae)=="factor") dt3$fish_larvae <-as.numeric(levels(dt3$fish_larvae))[as.integer(dt3$fish_larvae) ]
if (class(dt3$fish_larvae)=="character") dt3$fish_larvae <-as.numeric(dt3$fish_larvae)
if (class(dt3$streblocerus)=="factor") dt3$streblocerus <-as.numeric(levels(dt3$streblocerus))[as.integer(dt3$streblocerus) ]
if (class(dt3$streblocerus)=="character") dt3$streblocerus <-as.numeric(dt3$streblocerus)
if (class(dt3$amphipod)=="factor") dt3$amphipod <-as.numeric(levels(dt3$amphipod))[as.integer(dt3$amphipod) ]
if (class(dt3$amphipod)=="character") dt3$amphipod <-as.numeric(dt3$amphipod)
# attempting to convert dt3$date dateTime string to R date structure (date or POSIXct)
tmpDateFormat<-"%Y-%m-%d"
tmp3date<-as.Date(dt3$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt3[dt3$date != "",]) == length(tmp3date[!is.na(tmp3date)])){dt3$date <- tmp3date } else {print("Date conversion failed for dt3$date. Please inspect the data and do the date conversion yourself.")}

if (class(dt3$location)!="factor") dt3$location<- as.factor(dt3$location)
if (class(dt3$temp_c)=="factor") dt3$temp_c <-as.numeric(levels(dt3$temp_c))[as.integer(dt3$temp_c) ]
if (class(dt3$temp_c)=="character") dt3$temp_c <-as.numeric(dt3$temp_c)
if (class(dt3$ec_s_cm)=="factor") dt3$ec_s_cm <-as.numeric(levels(dt3$ec_s_cm))[as.integer(dt3$ec_s_cm) ]
if (class(dt3$ec_s_cm)=="character") dt3$ec_s_cm <-as.numeric(dt3$ec_s_cm)
if (class(dt3$spc_s_cm)=="factor") dt3$spc_s_cm <-as.numeric(levels(dt3$spc_s_cm))[as.integer(dt3$spc_s_cm) ]
if (class(dt3$spc_s_cm)=="character") dt3$spc_s_cm <-as.numeric(dt3$spc_s_cm)
if (class(dt3$tds_mg_l)=="factor") dt3$tds_mg_l <-as.numeric(levels(dt3$tds_mg_l))[as.integer(dt3$tds_mg_l) ]
if (class(dt3$tds_mg_l)=="character") dt3$tds_mg_l <-as.numeric(dt3$tds_mg_l)
if (class(dt3$salinity_psu)=="factor") dt3$salinity_psu <-as.numeric(levels(dt3$salinity_psu))[as.integer(dt3$salinity_psu) ]
if (class(dt3$salinity_psu)=="character") dt3$salinity_psu <-as.numeric(dt3$salinity_psu)
if (class(dt3$do_sat)=="factor") dt3$do_sat <-as.numeric(levels(dt3$do_sat))[as.integer(dt3$do_sat) ]
if (class(dt3$do_sat)=="character") dt3$do_sat <-as.numeric(dt3$do_sat)
if (class(dt3$do_mg_l)=="factor") dt3$do_mg_l <-as.numeric(levels(dt3$do_mg_l))[as.integer(dt3$do_mg_l) ]
if (class(dt3$do_mg_l)=="character") dt3$do_mg_l <-as.numeric(dt3$do_mg_l)
if (class(dt3$ph)=="factor") dt3$ph <-as.numeric(levels(dt3$ph))[as.integer(dt3$ph) ]
if (class(dt3$ph)=="character") dt3$ph <-as.numeric(dt3$ph)
if (class(dt3$turbidity_ntu)=="factor") dt3$turbidity_ntu <-as.numeric(levels(dt3$turbidity_ntu))[as.integer(dt3$turbidity_ntu) ]
if (class(dt3$turbidity_ntu)=="character") dt3$turbidity_ntu <-as.numeric(dt3$turbidity_ntu)
if (class(dt3$chl_g_l)=="factor") dt3$chl_g_l <-as.numeric(levels(dt3$chl_g_l))[as.integer(dt3$chl_g_l) ]
if (class(dt3$chl_g_l)=="character") dt3$chl_g_l <-as.numeric(dt3$chl_g_l)
if (class(dt3$bga_g_l)=="factor") dt3$bga_g_l <-as.numeric(levels(dt3$bga_g_l))[as.integer(dt3$bga_g_l) ]
if (class(dt3$bga_g_l)=="character") dt3$bga_g_l <-as.numeric(dt3$bga_g_l)
if (class(dt3$start_rotation)=="factor") dt3$start_rotation <-as.numeric(levels(dt3$start_rotation))[as.integer(dt3$start_rotation) ]
if (class(dt3$start_rotation)=="character") dt3$start_rotation <-as.numeric(dt3$start_rotation)
if (class(dt3$end_rotation)=="factor") dt3$end_rotation <-as.numeric(levels(dt3$end_rotation))[as.integer(dt3$end_rotation) ]
if (class(dt3$end_rotation)=="character") dt3$end_rotation <-as.numeric(dt3$end_rotation)
if (class(dt3$zoop_score_1_10)=="factor") dt3$zoop_score_1_10 <-as.numeric(levels(dt3$zoop_score_1_10))[as.integer(dt3$zoop_score_1_10) ]
if (class(dt3$zoop_score_1_10)=="character") dt3$zoop_score_1_10 <-as.numeric(dt3$zoop_score_1_10)
if (class(dt3$notes)!="factor") dt3$notes<- as.factor(dt3$notes)
if (class(dt3$volume_sampled)=="factor") dt3$volume_sampled <-as.numeric(levels(dt3$volume_sampled))[as.integer(dt3$volume_sampled) ]
if (class(dt3$volume_sampled)=="character") dt3$volume_sampled <-as.numeric(dt3$volume_sampled)
if (class(dt3$ilyocryptus)=="factor") dt3$ilyocryptus <-as.numeric(levels(dt3$ilyocryptus))[as.integer(dt3$ilyocryptus) ]
if (class(dt3$ilyocryptus)=="character") dt3$ilyocryptus <-as.numeric(dt3$ilyocryptus)
if (class(dt3$moina)=="factor") dt3$moina <-as.numeric(levels(dt3$moina))[as.integer(dt3$moina) ]
if (class(dt3$moina)=="character") dt3$moina <-as.numeric(dt3$moina)

# Convert Missing Values to NA for non-dates

dt3$id <- as.factor(ifelse((trimws(as.character(dt3$id))==trimws("NA")),NA,as.character(dt3$id)))
dt3$pseudodiptomus_adult <- ifelse((trimws(as.character(dt3$pseudodiptomus_adult))==trimws("NA")),NA,dt3$pseudodiptomus_adult)
suppressWarnings(dt3$pseudodiptomus_adult <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$pseudodiptomus_adult))==as.character(as.numeric("NA"))),NA,dt3$pseudodiptomus_adult))
dt3$psedodiptomus_copepidite <- ifelse((trimws(as.character(dt3$psedodiptomus_copepidite))==trimws("NA")),NA,dt3$psedodiptomus_copepidite)
suppressWarnings(dt3$psedodiptomus_copepidite <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$psedodiptomus_copepidite))==as.character(as.numeric("NA"))),NA,dt3$psedodiptomus_copepidite))
dt3$acanthocyclops_adult <- ifelse((trimws(as.character(dt3$acanthocyclops_adult))==trimws("NA")),NA,dt3$acanthocyclops_adult)
suppressWarnings(dt3$acanthocyclops_adult <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$acanthocyclops_adult))==as.character(as.numeric("NA"))),NA,dt3$acanthocyclops_adult))
dt3$acanthocyclops_copepididte <- ifelse((trimws(as.character(dt3$acanthocyclops_copepididte))==trimws("NA")),NA,dt3$acanthocyclops_copepididte)
suppressWarnings(dt3$acanthocyclops_copepididte <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$acanthocyclops_copepididte))==as.character(as.numeric("NA"))),NA,dt3$acanthocyclops_copepididte))
dt3$cyclopoid_sp <- ifelse((trimws(as.character(dt3$cyclopoid_sp))==trimws("NA")),NA,dt3$cyclopoid_sp)
suppressWarnings(dt3$cyclopoid_sp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cyclopoid_sp))==as.character(as.numeric("NA"))),NA,dt3$cyclopoid_sp))
dt3$cyclopoid_nauplii <- ifelse((trimws(as.character(dt3$cyclopoid_nauplii))==trimws("NA")),NA,dt3$cyclopoid_nauplii)
suppressWarnings(dt3$cyclopoid_nauplii <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cyclopoid_nauplii))==as.character(as.numeric("NA"))),NA,dt3$cyclopoid_nauplii))
dt3$calanoid_naulpii <- ifelse((trimws(as.character(dt3$calanoid_naulpii))==trimws("NA")),NA,dt3$calanoid_naulpii)
suppressWarnings(dt3$calanoid_naulpii <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$calanoid_naulpii))==as.character(as.numeric("NA"))),NA,dt3$calanoid_naulpii))
dt3$harpaticoid <- ifelse((trimws(as.character(dt3$harpaticoid))==trimws("NA")),NA,dt3$harpaticoid)
suppressWarnings(dt3$harpaticoid <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$harpaticoid))==as.character(as.numeric("NA"))),NA,dt3$harpaticoid))
dt3$daphnia_pulex <- ifelse((trimws(as.character(dt3$daphnia_pulex))==trimws("NA")),NA,dt3$daphnia_pulex)
suppressWarnings(dt3$daphnia_pulex <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$daphnia_pulex))==as.character(as.numeric("NA"))),NA,dt3$daphnia_pulex))
dt3$daphnia_laevis <- ifelse((trimws(as.character(dt3$daphnia_laevis))==trimws("NA")),NA,dt3$daphnia_laevis)
suppressWarnings(dt3$daphnia_laevis <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$daphnia_laevis))==as.character(as.numeric("NA"))),NA,dt3$daphnia_laevis))
dt3$daphnia_magna <- ifelse((trimws(as.character(dt3$daphnia_magna))==trimws("NA")),NA,dt3$daphnia_magna)
suppressWarnings(dt3$daphnia_magna <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$daphnia_magna))==as.character(as.numeric("NA"))),NA,dt3$daphnia_magna))
dt3$daphnia_mendotea <- ifelse((trimws(as.character(dt3$daphnia_mendotea))==trimws("NA")),NA,dt3$daphnia_mendotea)
suppressWarnings(dt3$daphnia_mendotea <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$daphnia_mendotea))==as.character(as.numeric("NA"))),NA,dt3$daphnia_mendotea))
dt3$ceriodaphnia_sp <- ifelse((trimws(as.character(dt3$ceriodaphnia_sp))==trimws("NA")),NA,dt3$ceriodaphnia_sp)
suppressWarnings(dt3$ceriodaphnia_sp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ceriodaphnia_sp))==as.character(as.numeric("NA"))),NA,dt3$ceriodaphnia_sp))
dt3$simocephalus_sp <- ifelse((trimws(as.character(dt3$simocephalus_sp))==trimws("NA")),NA,dt3$simocephalus_sp)
suppressWarnings(dt3$simocephalus_sp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$simocephalus_sp))==as.character(as.numeric("NA"))),NA,dt3$simocephalus_sp))
dt3$sididae <- ifelse((trimws(as.character(dt3$sididae))==trimws("NA")),NA,dt3$sididae)
suppressWarnings(dt3$sididae <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$sididae))==as.character(as.numeric("NA"))),NA,dt3$sididae))
dt3$chydorus_sphaericus <- ifelse((trimws(as.character(dt3$chydorus_sphaericus))==trimws("NA")),NA,dt3$chydorus_sphaericus)
suppressWarnings(dt3$chydorus_sphaericus <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$chydorus_sphaericus))==as.character(as.numeric("NA"))),NA,dt3$chydorus_sphaericus))
dt3$eurycercus <- ifelse((trimws(as.character(dt3$eurycercus))==trimws("NA")),NA,dt3$eurycercus)
suppressWarnings(dt3$eurycercus <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$eurycercus))==as.character(as.numeric("NA"))),NA,dt3$eurycercus))
dt3$alona <- ifelse((trimws(as.character(dt3$alona))==trimws("NA")),NA,dt3$alona)
suppressWarnings(dt3$alona <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$alona))==as.character(as.numeric("NA"))),NA,dt3$alona))
dt3$chydoridae <- ifelse((trimws(as.character(dt3$chydoridae))==trimws("NA")),NA,dt3$chydoridae)
suppressWarnings(dt3$chydoridae <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$chydoridae))==as.character(as.numeric("NA"))),NA,dt3$chydoridae))
dt3$diaphanosoma <- ifelse((trimws(as.character(dt3$diaphanosoma))==trimws("NA")),NA,dt3$diaphanosoma)
suppressWarnings(dt3$diaphanosoma <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$diaphanosoma))==as.character(as.numeric("NA"))),NA,dt3$diaphanosoma))
dt3$scapholeberis <- ifelse((trimws(as.character(dt3$scapholeberis))==trimws("NA")),NA,dt3$scapholeberis)
suppressWarnings(dt3$scapholeberis <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$scapholeberis))==as.character(as.numeric("NA"))),NA,dt3$scapholeberis))
dt3$cladocera_embryo <- ifelse((trimws(as.character(dt3$cladocera_embryo))==trimws("NA")),NA,dt3$cladocera_embryo)
suppressWarnings(dt3$cladocera_embryo <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cladocera_embryo))==as.character(as.numeric("NA"))),NA,dt3$cladocera_embryo))
dt3$eucypris <- ifelse((trimws(as.character(dt3$eucypris))==trimws("NA")),NA,dt3$eucypris)
suppressWarnings(dt3$eucypris <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$eucypris))==as.character(as.numeric("NA"))),NA,dt3$eucypris))
dt3$ilyocypris <- ifelse((trimws(as.character(dt3$ilyocypris))==trimws("NA")),NA,dt3$ilyocypris)
suppressWarnings(dt3$ilyocypris <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ilyocypris))==as.character(as.numeric("NA"))),NA,dt3$ilyocypris))
dt3$cypridopsis <- ifelse((trimws(as.character(dt3$cypridopsis))==trimws("NA")),NA,dt3$cypridopsis)
suppressWarnings(dt3$cypridopsis <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$cypridopsis))==as.character(as.numeric("NA"))),NA,dt3$cypridopsis))
dt3$gammarus <- ifelse((trimws(as.character(dt3$gammarus))==trimws("NA")),NA,dt3$gammarus)
suppressWarnings(dt3$gammarus <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$gammarus))==as.character(as.numeric("NA"))),NA,dt3$gammarus))
dt3$rotifer <- ifelse((trimws(as.character(dt3$rotifer))==trimws("NA")),NA,dt3$rotifer)
suppressWarnings(dt3$rotifer <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$rotifer))==as.character(as.numeric("NA"))),NA,dt3$rotifer))
dt3$polychaete <- ifelse((trimws(as.character(dt3$polychaete))==trimws("NA")),NA,dt3$polychaete)
suppressWarnings(dt3$polychaete <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$polychaete))==as.character(as.numeric("NA"))),NA,dt3$polychaete))
dt3$acari <- ifelse((trimws(as.character(dt3$acari))==trimws("NA")),NA,dt3$acari)
suppressWarnings(dt3$acari <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$acari))==as.character(as.numeric("NA"))),NA,dt3$acari))
dt3$chironomid_larvae <- ifelse((trimws(as.character(dt3$chironomid_larvae))==trimws("NA")),NA,dt3$chironomid_larvae)
suppressWarnings(dt3$chironomid_larvae <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$chironomid_larvae))==as.character(as.numeric("NA"))),NA,dt3$chironomid_larvae))
dt3$oliogochaete <- ifelse((trimws(as.character(dt3$oliogochaete))==trimws("NA")),NA,dt3$oliogochaete)
suppressWarnings(dt3$oliogochaete <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$oliogochaete))==as.character(as.numeric("NA"))),NA,dt3$oliogochaete))
dt3$gastropod <- ifelse((trimws(as.character(dt3$gastropod))==trimws("NA")),NA,dt3$gastropod)
suppressWarnings(dt3$gastropod <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$gastropod))==as.character(as.numeric("NA"))),NA,dt3$gastropod))
dt3$tardigrade <- ifelse((trimws(as.character(dt3$tardigrade))==trimws("NA")),NA,dt3$tardigrade)
suppressWarnings(dt3$tardigrade <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$tardigrade))==as.character(as.numeric("NA"))),NA,dt3$tardigrade))
dt3$nematode <- ifelse((trimws(as.character(dt3$nematode))==trimws("NA")),NA,dt3$nematode)
suppressWarnings(dt3$nematode <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$nematode))==as.character(as.numeric("NA"))),NA,dt3$nematode))
dt3$hyalella <- ifelse((trimws(as.character(dt3$hyalella))==trimws("NA")),NA,dt3$hyalella)
suppressWarnings(dt3$hyalella <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$hyalella))==as.character(as.numeric("NA"))),NA,dt3$hyalella))
dt3$hydra <- ifelse((trimws(as.character(dt3$hydra))==trimws("NA")),NA,dt3$hydra)
suppressWarnings(dt3$hydra <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$hydra))==as.character(as.numeric("NA"))),NA,dt3$hydra))
dt3$terrestrial_insect <- ifelse((trimws(as.character(dt3$terrestrial_insect))==trimws("NA")),NA,dt3$terrestrial_insect)
suppressWarnings(dt3$terrestrial_insect <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$terrestrial_insect))==as.character(as.numeric("NA"))),NA,dt3$terrestrial_insect))
dt3$collembola <- ifelse((trimws(as.character(dt3$collembola))==trimws("NA")),NA,dt3$collembola)
suppressWarnings(dt3$collembola <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$collembola))==as.character(as.numeric("NA"))),NA,dt3$collembola))
dt3$baetidae <- ifelse((trimws(as.character(dt3$baetidae))==trimws("NA")),NA,dt3$baetidae)
suppressWarnings(dt3$baetidae <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$baetidae))==as.character(as.numeric("NA"))),NA,dt3$baetidae))
dt3$diptera <- ifelse((trimws(as.character(dt3$diptera))==trimws("NA")),NA,dt3$diptera)
suppressWarnings(dt3$diptera <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$diptera))==as.character(as.numeric("NA"))),NA,dt3$diptera))
dt3$hydroptilidae <- ifelse((trimws(as.character(dt3$hydroptilidae))==trimws("NA")),NA,dt3$hydroptilidae)
suppressWarnings(dt3$hydroptilidae <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$hydroptilidae))==as.character(as.numeric("NA"))),NA,dt3$hydroptilidae))
dt3$coleoptera <- ifelse((trimws(as.character(dt3$coleoptera))==trimws("NA")),NA,dt3$coleoptera)
suppressWarnings(dt3$coleoptera <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$coleoptera))==as.character(as.numeric("NA"))),NA,dt3$coleoptera))
dt3$odonata <- ifelse((trimws(as.character(dt3$odonata))==trimws("NA")),NA,dt3$odonata)
suppressWarnings(dt3$odonata <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$odonata))==as.character(as.numeric("NA"))),NA,dt3$odonata))
dt3$bivalve <- ifelse((trimws(as.character(dt3$bivalve))==trimws("NA")),NA,dt3$bivalve)
suppressWarnings(dt3$bivalve <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$bivalve))==as.character(as.numeric("NA"))),NA,dt3$bivalve))
dt3$trichoptera <- ifelse((trimws(as.character(dt3$trichoptera))==trimws("NA")),NA,dt3$trichoptera)
suppressWarnings(dt3$trichoptera <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$trichoptera))==as.character(as.numeric("NA"))),NA,dt3$trichoptera))
dt3$corixidae <- ifelse((trimws(as.character(dt3$corixidae))==trimws("NA")),NA,dt3$corixidae)
suppressWarnings(dt3$corixidae <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$corixidae))==as.character(as.numeric("NA"))),NA,dt3$corixidae))
dt3$fish_larvae <- ifelse((trimws(as.character(dt3$fish_larvae))==trimws("NA")),NA,dt3$fish_larvae)
suppressWarnings(dt3$fish_larvae <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$fish_larvae))==as.character(as.numeric("NA"))),NA,dt3$fish_larvae))
dt3$streblocerus <- ifelse((trimws(as.character(dt3$streblocerus))==trimws("NA")),NA,dt3$streblocerus)
suppressWarnings(dt3$streblocerus <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$streblocerus))==as.character(as.numeric("NA"))),NA,dt3$streblocerus))
dt3$amphipod <- ifelse((trimws(as.character(dt3$amphipod))==trimws("NA")),NA,dt3$amphipod)
suppressWarnings(dt3$amphipod <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$amphipod))==as.character(as.numeric("NA"))),NA,dt3$amphipod))
dt3$location <- as.factor(ifelse((trimws(as.character(dt3$location))==trimws("NA")),NA,as.character(dt3$location)))
dt3$temp_c <- ifelse((trimws(as.character(dt3$temp_c))==trimws("NA")),NA,dt3$temp_c)
suppressWarnings(dt3$temp_c <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$temp_c))==as.character(as.numeric("NA"))),NA,dt3$temp_c))
dt3$ec_s_cm <- ifelse((trimws(as.character(dt3$ec_s_cm))==trimws("NA")),NA,dt3$ec_s_cm)
suppressWarnings(dt3$ec_s_cm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ec_s_cm))==as.character(as.numeric("NA"))),NA,dt3$ec_s_cm))
dt3$spc_s_cm <- ifelse((trimws(as.character(dt3$spc_s_cm))==trimws("NA")),NA,dt3$spc_s_cm)
suppressWarnings(dt3$spc_s_cm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$spc_s_cm))==as.character(as.numeric("NA"))),NA,dt3$spc_s_cm))
dt3$tds_mg_l <- ifelse((trimws(as.character(dt3$tds_mg_l))==trimws("NA")),NA,dt3$tds_mg_l)
suppressWarnings(dt3$tds_mg_l <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$tds_mg_l))==as.character(as.numeric("NA"))),NA,dt3$tds_mg_l))
dt3$salinity_psu <- ifelse((trimws(as.character(dt3$salinity_psu))==trimws("NA")),NA,dt3$salinity_psu)
suppressWarnings(dt3$salinity_psu <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$salinity_psu))==as.character(as.numeric("NA"))),NA,dt3$salinity_psu))
dt3$do_sat <- ifelse((trimws(as.character(dt3$do_sat))==trimws("NA")),NA,dt3$do_sat)
suppressWarnings(dt3$do_sat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$do_sat))==as.character(as.numeric("NA"))),NA,dt3$do_sat))
dt3$do_mg_l <- ifelse((trimws(as.character(dt3$do_mg_l))==trimws("NA")),NA,dt3$do_mg_l)
suppressWarnings(dt3$do_mg_l <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$do_mg_l))==as.character(as.numeric("NA"))),NA,dt3$do_mg_l))
dt3$ph <- ifelse((trimws(as.character(dt3$ph))==trimws("NA")),NA,dt3$ph)
suppressWarnings(dt3$ph <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ph))==as.character(as.numeric("NA"))),NA,dt3$ph))
dt3$turbidity_ntu <- ifelse((trimws(as.character(dt3$turbidity_ntu))==trimws("NA")),NA,dt3$turbidity_ntu)
suppressWarnings(dt3$turbidity_ntu <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$turbidity_ntu))==as.character(as.numeric("NA"))),NA,dt3$turbidity_ntu))
dt3$chl_g_l <- ifelse((trimws(as.character(dt3$chl_g_l))==trimws("NA")),NA,dt3$chl_g_l)
suppressWarnings(dt3$chl_g_l <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$chl_g_l))==as.character(as.numeric("NA"))),NA,dt3$chl_g_l))
dt3$bga_g_l <- ifelse((trimws(as.character(dt3$bga_g_l))==trimws("NA")),NA,dt3$bga_g_l)
suppressWarnings(dt3$bga_g_l <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$bga_g_l))==as.character(as.numeric("NA"))),NA,dt3$bga_g_l))
dt3$start_rotation <- ifelse((trimws(as.character(dt3$start_rotation))==trimws("NA")),NA,dt3$start_rotation)
suppressWarnings(dt3$start_rotation <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$start_rotation))==as.character(as.numeric("NA"))),NA,dt3$start_rotation))
dt3$end_rotation <- ifelse((trimws(as.character(dt3$end_rotation))==trimws("NA")),NA,dt3$end_rotation)
suppressWarnings(dt3$end_rotation <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$end_rotation))==as.character(as.numeric("NA"))),NA,dt3$end_rotation))
dt3$zoop_score_1_10 <- ifelse((trimws(as.character(dt3$zoop_score_1_10))==trimws("NA")),NA,dt3$zoop_score_1_10)
suppressWarnings(dt3$zoop_score_1_10 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$zoop_score_1_10))==as.character(as.numeric("NA"))),NA,dt3$zoop_score_1_10))
dt3$notes <- as.factor(ifelse((trimws(as.character(dt3$notes))==trimws("NA")),NA,as.character(dt3$notes)))
dt3$volume_sampled <- ifelse((trimws(as.character(dt3$volume_sampled))==trimws("NA")),NA,dt3$volume_sampled)
suppressWarnings(dt3$volume_sampled <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$volume_sampled))==as.character(as.numeric("NA"))),NA,dt3$volume_sampled))
dt3$ilyocryptus <- ifelse((trimws(as.character(dt3$ilyocryptus))==trimws("NA")),NA,dt3$ilyocryptus)
suppressWarnings(dt3$ilyocryptus <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$ilyocryptus))==as.character(as.numeric("NA"))),NA,dt3$ilyocryptus))
dt3$moina <- ifelse((trimws(as.character(dt3$moina))==trimws("NA")),NA,dt3$moina)
suppressWarnings(dt3$moina <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$moina))==as.character(as.numeric("NA"))),NA,dt3$moina))


# Here is the structure of the input data frame:
print("dt3) Structure")
str(dt3)
attach(dt3)

# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

print(" ")
print("Summary of id")
print(summary(id))
print(" ")
print("Summary of pseudodiptomus_adult")
print(summary(pseudodiptomus_adult))
print(" ")
print("Summary of psedodiptomus_copepidite")
print(summary(psedodiptomus_copepidite))
print(" ")
print("Summary of acanthocyclops_adult")
print(summary(acanthocyclops_adult))
print(" ")
print("Summary of acanthocyclops_copepididte")
print(summary(acanthocyclops_copepididte))
print(" ")
print("Summary of cyclopoid_sp")
print(summary(cyclopoid_sp))
print(" ")
print("Summary of cyclopoid_nauplii")
print(summary(cyclopoid_nauplii))
print(" ")
print("Summary of calanoid_naulpii")
print(summary(calanoid_naulpii))
print(" ")
print("Summary of harpaticoid")
print(summary(harpaticoid))
print(" ")
print("Summary of daphnia_pulex")
print(summary(daphnia_pulex))
print(" ")
print("Summary of daphnia_laevis")
print(summary(daphnia_laevis))
print(" ")
print("Summary of daphnia_magna")
print(summary(daphnia_magna))
print(" ")
print("Summary of daphnia_mendotea")
print(summary(daphnia_mendotea))
print(" ")
print("Summary of ceriodaphnia_sp")
print(summary(ceriodaphnia_sp))
print(" ")
print("Summary of simocephalus_sp")
print(summary(simocephalus_sp))
print(" ")
print("Summary of bosmina_sp")
print(summary(bosmina_sp))
print(" ")
print("Summary of sididae")
print(summary(sididae))
print(" ")
print("Summary of chydorus_sphaericus")
print(summary(chydorus_sphaericus))
print(" ")
print("Summary of eurycercus")
print(summary(eurycercus))
print(" ")
print("Summary of alona")
print(summary(alona))
print(" ")
print("Summary of chydoridae")
print(summary(chydoridae))
print(" ")
print("Summary of diaphanosoma")
print(summary(diaphanosoma))
print(" ")
print("Summary of scapholeberis")
print(summary(scapholeberis))
print(" ")
print("Summary of cladocera_embryo")
print(summary(cladocera_embryo))
print(" ")
print("Summary of eucypris")
print(summary(eucypris))
print(" ")
print("Summary of ilyocypris")
print(summary(ilyocypris))
print(" ")
print("Summary of cypridopsis")
print(summary(cypridopsis))
print(" ")
print("Summary of gammarus")
print(summary(gammarus))
print(" ")
print("Summary of rotifer")
print(summary(rotifer))
print(" ")
print("Summary of polychaete")
print(summary(polychaete))
print(" ")
print("Summary of acari")
print(summary(acari))
print(" ")
print("Summary of chironomid_larvae")
print(summary(chironomid_larvae))
print(" ")
print("Summary of oliogochaete")
print(summary(oliogochaete))
print(" ")
print("Summary of gastropod")
print(summary(gastropod))
print(" ")
print("Summary of tardigrade")
print(summary(tardigrade))
print(" ")
print("Summary of nematode")
print(summary(nematode))
print(" ")
print("Summary of hyalella")
print(summary(hyalella))
print(" ")
print("Summary of hydra")
print(summary(hydra))
print(" ")
print("Summary of terrestrial_insect")
print(summary(terrestrial_insect))
print(" ")
print("Summary of collembola")
print(summary(collembola))
print(" ")
print("Summary of baetidae")
print(summary(baetidae))
print(" ")
print("Summary of diptera")
print(summary(diptera))
print(" ")
print("Summary of hydroptilidae")
print(summary(hydroptilidae))
print(" ")
print("Summary of coleoptera")
print(summary(coleoptera))
print(" ")
print("Summary of ephemerellidae")
print(summary(ephemerellidae))
print(" ")
print("Summary of odonata")
print(summary(odonata))
print(" ")
print("Summary of bivalve")
print(summary(bivalve))
print(" ")
print("Summary of trichoptera")
print(summary(trichoptera))
print(" ")
print("Summary of corixidae")
print(summary(corixidae))
print(" ")
print("Summary of fish_larvae")
print(summary(fish_larvae))
print(" ")
print("Summary of streblocerus")
print(summary(streblocerus))
print(" ")
print("Summary of amphipod")
print(summary(amphipod))
print(" ")
print("Summary of date")
print(summary(date))
print(" ")
print("Summary of location")
print(summary(location))
print(" ")
print("Summary of time")
print(summary(time))
print(" ")
print("Summary of temp_c")
print(summary(temp_c))
print(" ")
print("Summary of ec_s_cm")
print(summary(ec_s_cm))
print(" ")
print("Summary of spc_s_cm")
print(summary(spc_s_cm))
print(" ")
print("Summary of tds_mg_l")
print(summary(tds_mg_l))
print(" ")
print("Summary of salinity_psu")
print(summary(salinity_psu))
print(" ")
print("Summary of do_sat")
print(summary(do_sat))
print(" ")
print("Summary of do_mg_l")
print(summary(do_mg_l))
print(" ")
print("Summary of ph")
print(summary(ph))
print(" ")
print("Summary of turbidity_ntu")
print(summary(turbidity_ntu))
print(" ")
print("Summary of chl_g_l")
print(summary(chl_g_l))
print(" ")
print("Summary of bga_g_l")
print(summary(bga_g_l))
print(" ")
print("Summary of start_rotation")
print(summary(start_rotation))
print(" ")
print("Summary of end_rotation")
print(summary(end_rotation))
print(" ")
print("Summary of zoop_score_1_10")
print(summary(zoop_score_1_10))
print(" ")
print("Summary of notes")
print(summary(notes))
print(" ")
print("Summary of volume_sampled")
print(summary(volume_sampled))
print(" ")
print("Summary of ilyocryptus")
print(summary(ilyocryptus))
print(" ")
print("Summary of moina")
print(summary(moina))
# Get more details on character variables


print(" ")
print("Summary of id")
print(summary(as.factor(dt3$id)))

print(" ")
print("Summary of location")
print(summary(as.factor(dt3$location)))

print(" ")
print("Summary of notes")
print(summary(as.factor(dt3$notes)))
detach(dt3)



inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/996/4/a5d8d657bc560da0750232bf140701ba"
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F
               ,skip=1
               ,sep=","
               ,quot='"'
               , col.names=c(
                 "location",
                 "latitude_utm",
                 "longitude_utm",
                 "habitat_type",
                 "purpose",
                 "habitat_type_2"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$location)!="factor") dt4$location<- as.factor(dt4$location)
if (class(dt4$latitude_utm)=="factor") dt4$latitude_utm <-as.numeric(levels(dt4$latitude_utm))[as.integer(dt4$latitude_utm) ]
if (class(dt4$latitude_utm)=="character") dt4$latitude_utm <-as.numeric(dt4$latitude_utm)
if (class(dt4$longitude_utm)=="factor") dt4$longitude_utm <-as.numeric(levels(dt4$longitude_utm))[as.integer(dt4$longitude_utm) ]
if (class(dt4$longitude_utm)=="character") dt4$longitude_utm <-as.numeric(dt4$longitude_utm)
if (class(dt4$habitat_type)!="factor") dt4$habitat_type<- as.factor(dt4$habitat_type)
if (class(dt4$purpose)!="factor") dt4$purpose<- as.factor(dt4$purpose)
if (class(dt4$habitat_type_2)!="factor") dt4$habitat_type_2<- as.factor(dt4$habitat_type_2)

# Convert Missing Values to NA for non-dates

dt4$location <- as.factor(ifelse((trimws(as.character(dt4$location))==trimws("NA")),NA,as.character(dt4$location)))
dt4$latitude_utm <- ifelse((trimws(as.character(dt4$latitude_utm))==trimws("NA")),NA,dt4$latitude_utm)
suppressWarnings(dt4$latitude_utm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$latitude_utm))==as.character(as.numeric("NA"))),NA,dt4$latitude_utm))
dt4$longitude_utm <- ifelse((trimws(as.character(dt4$longitude_utm))==trimws("NA")),NA,dt4$longitude_utm)
suppressWarnings(dt4$longitude_utm <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$longitude_utm))==as.character(as.numeric("NA"))),NA,dt4$longitude_utm))
dt4$habitat_type <- as.factor(ifelse((trimws(as.character(dt4$habitat_type))==trimws("NA")),NA,as.character(dt4$habitat_type)))
dt4$purpose <- as.factor(ifelse((trimws(as.character(dt4$purpose))==trimws("NA")),NA,as.character(dt4$purpose)))
dt4$habitat_type_2 <- as.factor(ifelse((trimws(as.character(dt4$habitat_type_2))==trimws("NA")),NA,as.character(dt4$habitat_type_2)))


# Here is the structure of the input data frame:
print("dt4) Structure")
str(dt4)
attach(dt4)
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.

print(" ")
print("Summary of location")
print(summary(location))
print(" ")
print("Summary of latitude_utm")
print(summary(latitude_utm))
print(" ")
print("Summary of longitude_utm")
print(summary(longitude_utm))
print(" ")
print("Summary of habitat_type")
print(summary(habitat_type))
print(" ")
print("Summary of purpose")
print(summary(purpose))
print(" ")
print("Summary of habitat_type_2")
print(summary(habitat_type_2))
# Get more details on character variables


print(" ")
print("Summary of location")
print(summary(as.factor(dt4$location)))

print(" ")
print("Summary of habitat_type")
print(summary(as.factor(dt4$habitat_type)))

print(" ")
print("Summary of purpose")
print(summary(as.factor(dt4$purpose)))

print(" ")
print("Summary of habitat_type_2")
print(summary(as.factor(dt4$habitat_type_2)))
detach(dt4)



