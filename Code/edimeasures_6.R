setwd(Data)

install.packages("readstata13")
install.packages("tidyverse")
install.packages("foreign")
install.packages("survey")
install.packages("descr")
install.packages("Hmisc")
library(descr)
library(plyr)
library(dplyr)
library(tidyr)
library(foreign)
library(survey)
library(readstata13)
library(Hmisc)

h17 <- read.dta13("interviewfs16_eul.dta", convert.factors = FALSE, generate.factors = FALSE,
                 encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                 missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                 add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                 strlexport = FALSE, strlpath = ".")

g17 <- read.dta13("generalfs16_eul.dta", convert.factors = FALSE, generate.factors = FALSE,
                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                  missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                  strlexport = FALSE, strlpath = ".")


h16 <- read.dta13("h16.dta", convert.factors = FALSE, generate.factors = FALSE,
                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                  missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                  strlexport = FALSE, strlpath = ".")

h15 <- read.dta13("h15.dta", convert.factors = FALSE, generate.factors = FALSE,
                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                  missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                  strlexport = FALSE, strlpath = ".")

h14 <- read.dta13("h14.dta", convert.factors = FALSE, generate.factors = FALSE,
                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                  missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                  strlexport = FALSE, strlpath = ".")

#Testing the following variables EHCOST/ENTINCHH
#ES_HCOST - SPI'ed housing costs
#ESAHCHH - SPI'ed after housing costs income
#ESNINCHH - SPI'ed household net income
#G_NEWHH - household grossing factor
#GS_NEWHH - SPI'ed grossing factor

h17$share <- h17$EHCOST/h17$ENTINCHH

#keep only the variables needed

myvars <- c("LOW60AHC", "S_OE_BHC", "S_OE_AHC", "GS_NEWPP", "G_NEWAD", "G_NEWCH", "G_NEWPN", "GVTREGN", "ETHGRPHH", "TENURE", "OQINAHC")

h171 <- h17[myvars]
h161 <- h16[myvars]
h151 <- h15[myvars]
h141 <- h14[myvars]

#add a year variable

h171$year <- 2017
h161$year <- 2016
h151$year <- 2015
h141$year <- 2014

#add rows

h1 <- rbind(h171, h161)
h2 <- rbind(h151, h1)
h3 <- rbind(h141, h2)

#subset to London

h4 <- h3[ which(h3$GVTREGN==8), ]

#add share variable

h4$IncOnHouse <- h4$S_OE_BHC-h4$S_OE_AHC
h4$IncOnHousePC <- h4$IncOnHouse/h4$S_OE_BHC

h4$IncFlag <- 0
h4$IncFlag[h4$share>=(1/3)] <- 1
h4$IncFlag[h4$S_OE_BHC==0 & h4$IncOnHouse>0] <- 1

#stats on share

housing <- crosstab(h4$year,h4$IncFlag, prop.r=T, plot=F,
                   weight=h4$GS_NEWPP)

housing_mtrx <- descr:::CreateNewTab(housing)

poverty <- crosstab(h4$year,h4$LOW60AHC, prop.r=T, plot=F,
                    weight=h4$GS_NEWPP)

poverty_mtrx <- descr:::CreateNewTab(poverty)


