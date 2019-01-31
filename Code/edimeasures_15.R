###Uses three-year APS to calculate training participation
###By various demographic categories

setwd(Data)

#specifying the variables we need and opening the Stata file, then opening up lookup tables

myvars <- c("ILODEFR", "AAGE", "PWTA17C", "SEX", "ETHEWEUL", "GOVTOF", "FUTUR13", "ED13WK", "DISEA")
aps <- read.dta13("aps_3yr_jan15dec17_eul.dta", select.cols = myvars, convert.factors = F)

#keeping London

aps0 <- aps[ which(aps$GOVTOF==8 & (aps$ILODEFR==1|aps$ILODEFR==2)), ]

#Creating table by ethnicity

age <- read.xlsx("lookups.xls", 4, startRow=1)
eth <- read.xlsx("lookups.xls", 6, startRow=1)

aps2 <- left_join(aps0, age, by = c("AAGE"))
aps3 <- left_join(aps2, eth, by = c("ETHEWEUL"))

#make a new variable for those in training =1

aps3$train <- 0
aps3$train[aps3$ED13WK==1] <- 1
aps3$train[aps3$FUTUR13==1] <- 1

#Totals by ethnicity

Teth <- 
  aps3 %>% group_by(train, ETH) %>% 
  summarise(FREQ = sum(PWTA17C, na.rm = TRUE))

#Totals by age

Tage <- 
  aps3 %>% group_by(train, AGE) %>% 
  summarise(FREQ = sum(PWTA17C, na.rm = TRUE))

#Totals by disability

Tdis <- 
  aps3 %>% group_by(train, DISEA) %>% 
  summarise(FREQ = sum(PWTA17C, na.rm = TRUE))

#Totals by gender

Tgen <- 
  aps3 %>% group_by(train, SEX) %>% 
  summarise(FREQ = sum(PWTA17C, na.rm = TRUE))