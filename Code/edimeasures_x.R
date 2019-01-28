setwd("C:/Users/Spencer/Documents/R/EHS")

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

h13 <- read.dta13("h13.dta", convert.factors = FALSE, generate.factors = FALSE,
                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                  missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                  strlexport = FALSE, strlpath = ".")

g16 <- read.dta13("g16.dta", convert.factors = FALSE, generate.factors = FALSE,
                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                  missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                  strlexport = FALSE, strlpath = ".")

g15 <- read.dta13("g15.dta", convert.factors = FALSE, generate.factors = FALSE,
                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                  missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                  strlexport = FALSE, strlpath = ".")

g14 <- read.dta13("g14.dta", convert.factors = FALSE, generate.factors = FALSE,
                  encoding = "UTF-8", fromEncoding = NULL, convert.underscore = FALSE,
                  missing.type = TRUE, convert.dates = TRUE, replace.strl = TRUE,
                  add.rownames = FALSE, nonint.factors = FALSE, select.rows = NULL, 
                  strlexport = FALSE, strlpath = ".")

g13 <- read.dta13("g13.dta", convert.factors = FALSE, generate.factors = FALSE,
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

#keep only the variables needed

myvars1 <- c("ethhrp2x", "agehrp6x", "bedstdx", "serialanon", "hhtype6")

h161 <- h16[myvars1]
h151 <- h15[myvars1]

myvars1 <- c("ethhrp2x", "agehrp6x", "bedstdx", "aacode", "hhtype6")

h141 <- h14[myvars1]
names(h141)[4] <- "serialanon"

myvars1 <- c("ethhrp2x", "agehrp6x", "bedstdx", "aacode", "hhtype6")

h131 <- h13[myvars1]
names(h131)[4] <- "serialanon"

myvars2 <- c("serialanon", "gorEHS", "aagfh16", "tenure4x")

g161 <- g16[myvars2]

myvars2 <- c("serialanon", "gorEHS", "aagfh15", "tenure4x")

g151 <- g15[myvars2]
names(g151)[1] <- "serialanon"
names(g151)[3] <- "aagfh16"

myvars2 <- c("aacode", "gorEHS", "aagfh14", "tenure4x")

g141 <- g14[myvars2]
names(g141)[1] <- "serialanon"
names(g141)[3] <- "aagfh16"

myvars2 <- c("aacode", "GorEHS", "aagfh13", "tenure4x")

g131 <- g13[myvars2]
names(g131)[1] <- "serialanon"
names(g131)[2] <- "gorEHS"
names(g131)[3] <- "aagfh16"

#merging general and interview files

h162 <- left_join(h161, g161, by = c("serialanon"))
h152 <- left_join(h151, g151, by = c("serialanon"))
h142 <- left_join(h141, g141, by = c("serialanon"))
h132 <- left_join(h131, g131, by = c("serialanon"))                  

#add a year variable

h162$year <- 2016
h152$year <- 2015
h142$year <- 2014
h132$year <- 2013

#add rows

h1 <- rbind(h162, h152)
h2 <- rbind(h142, h1)
h3 <- rbind(h132, h2)

#subset to London

h4 <- h3[ which(h3$gorEHS==8), ]


#stats on bedroom standard

years <- crosstab(h4$year,h4$bedstdx, prop.r=T, plot=F,
                   weight=h4$aagfh16)

ethnicity <- crosstab(h4$ethhrp2x,h4$bedstdx, prop.r=T, plot=F,
                    weight=h4$aagfh16)

htype <- crosstab(h4$hhtype6,h4$bedstdx, prop.r=T, plot=F,
                  weight=h4$aagfh16)

tenure <- crosstab(h4$tenure4x,h4$bedstdx, prop.r=T, plot=F,
                   weight=h4$aagfh16)

age <- crosstab(h4$agehrp6x,h4$bedstdx, prop.r=T, plot=F,
                   weight=h4$aagfh16)

years_mtrx <- descr:::CreateNewTab(years)
ethnicity_mtrx <- descr:::CreateNewTab(ethnicity)
htype_mtrx <- descr:::CreateNewTab(htype)
tenure_mtrx <- descr:::CreateNewTab(tenure)
age_mtrx <- descr:::CreateNewTab(age)

yearsu <- xtabs(~year+bedstdx, data=h4)
ethnicityu <- xtabs(~ethhrp2x+bedstdx, data=h4)
htypeu <- xtabs(~hhtype6+bedstdx, data=h4)
tenureu <- xtabs(~tenure4x+bedstdx, data=h4)
ageu <- xtabs(~agehrp6x+bedstdx, data=h4)

ftable(yearsu)
ftable(ethnicityu) 
ftable(htypeu) 
ftable(tenureu) 
ftable(ageu)


