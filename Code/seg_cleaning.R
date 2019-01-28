#project-wide packages and working directory

setwd(Data)

#Read in a lookup from LSOA to LA and RUC

lookup <- read.csv("SEG_lookup.csv")

# Section one: creating three data frames for 1) ethnicity data from 2001 and 2011 census, 2) NS-SEC data from 2011 census, 3) ethnicity data from CRDC modelled ethnicity and ONS population estimates

# 1) ethnicity data from 2001 and 2011 census, merged with rural-urban classification and LA codes

lsoa_cen <- read.csv("SEG_eth.csv")
names(lsoa_cen)[1] <- "LSOA11CD"
ETH <- left_join(lsoa_cen, lookup, by = "LSOA11CD")

# 2) NS-SEC data from the 2011 census, merged with rural-urban classification and LA codes

lsoa_cns <- read.csv("SEG_seg.csv")
names(lsoa_cns)[1] <- "LSOA11CD"
SEG <- left_join(lsoa_cns, lookup, by = "LSOA11CD")

# 3) Age categories from the 2011 census

lsoa_cas <- read.csv("SEG_age.csv")
AGE <- left_join(lsoa_cas, lookup, by = "LSOA11CD")

# 4) Qualifications from the 2011 census

lsoa_cqs <- read.csv("SEG_quals.csv")
QUAL<- left_join(lsoa_cqs, lookup, by = "LSOA11CD")

# 5) Country of birth from the 2011 census

lsoa_ccs <- read.csv("SEG_cob.csv")
COB <- left_join(lsoa_ccs, lookup, by = "LSOA11CD")

setwd(Code)

