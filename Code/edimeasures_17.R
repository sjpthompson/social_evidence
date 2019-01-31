###Uses four years of the HBAI to calculate poverty by ethnicity

setwd(Data)

#specifying the variables we need and opening the Stata file, then opening up lookup tables

myvars <- c("ETHGRPHH", "GVTREGN", "LOW60AHC", "GS_NEWPP")
h17 <- read.dta13("hbai1617_g4.dta", select.cols = myvars, convert.factors = F)
h16 <- read.dta13("hbai1516_g4.dta", select.cols = myvars, convert.factors = F)
h15 <- read.dta13("hbai1415_g4.dta", select.cols = myvars, convert.factors = F)
h14 <- read.dta13("hbai1314_g4.dta", select.cols = myvars, convert.factors = F)

#Add years

h17$year <- "2017"
h16$year <- "2016"
h15$year <- "2015"
h14$year <- "2014"

#Row Bind

h1 <- rbind(h17, h16, h15, h14)

#Keeping London results

h2 <- h1[ which(h1$GVTREGN==8), ]

#summarise by ethnicity, year and low-income status

h3 <- 
  h2 %>% group_by(year, ETHGRPHH, LOW60AHC) %>% 
  summarise(FREQ = sum(GS_NEWPP, na.rm = TRUE))

#spreading the low income status

h4 <- spread(h3, LOW60AHC, FREQ)

setwd(Output)

write.csv(h4, file="poverty.csv", row.names=FALSE)

setwd(Code)