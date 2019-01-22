## Part four of the code produces employment and unemployment rates, using Annual Population Survey
## TO ADD

setwd(Data)

#specifying the variables we need and opening the Stata file, then opening up lookup tables

myvars <- c("ILODEFR", "AAGE", "PWTA17C", "SEX", "ETHEWEUL", "GOVTOF")
aps <- read.dta13("aps_3yr_j14d16_eul.dta", select.cols = myvars, convert.factors = F)

age <- read.xlsx("lookups.xls", 4, startRow=1)
eth <- read.xlsx("lookups.xls", 5, startRow=1)

#selecting only cases where the respondent lives in London, then merging the lookups in

aps0 <- aps[ which(aps$GOVTOF==8), ]
aps2 <- left_join(aps0, age, by = c("AAGE"))
aps3 <- left_join(aps2, eth, by = c("ETHEWEUL"))
aps2 <- aps3[ which(aps3$AGE==AGE1|aps3$AGE==AGE2), ]

#creating totals

totals <- 
  aps2 %>% group_by(ILODEFR, SEX, ETH) %>% 
  summarise(FREQ = sum(PWTA17C, na.rm = TRUE))

#creating group totals

ototal <- 
  aps2 %>% group_by(SEX, ETH) %>% 
  summarise(DEMOG = sum(PWTA17C, na.rm = TRUE))

#merging

total <- left_join(totals, ototal, by = c("SEX", "ETH"))

#using spread to get it to ILODEFR

table0 <- spread(total, ILODEFR, FREQ)

names(table0)[4] <- "EMP"
names(table0)[5] <- "UNEMP"

#calculating employment and unemployment rates

table0$Employment_rate <- table0$EMP/table0$DEMOG
table0$Unemployment_rate <- table0$UNEMP/(table0$EMP + table0$UNEMP)

# saving output

myvars <- c("SEX", "ETH", "Employment_rate", "Unemployment_rate", "DEMOG")
table <- table0[myvars]

setwd(Output)

write.csv(table, file="employment.csv", row.names=FALSE)

setwd(Code)
