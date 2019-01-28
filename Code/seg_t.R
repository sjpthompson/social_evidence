# This code calculates segregation indices for ethnicity and social class in London and other urban areas using LSOA-level data

#project-wide packages and working directory

install.packages("tidyverse")
library(dplyr)
library(tidyr)

setwd("//homedata/home$/SPThompson/R data")

#load in the census data

mydata <- read.table("lsoa_ceC.csv", header=TRUE, sep=",")

mydata$threshold <- 0
mydata$threshold[mydata$wbr<(1/3)] <- 1
mydata$threshold[mydata$wbr<(1/3) & (mydata$wao>(1/3)|mydata$aao>(1/3)|mydata$abd>(1/3)|mydata$apk>(1/3)|mydata$ain>(1/3)|mydata$acn>(1/3)|mydata$baf>(1/3)|mydata$bca>(1/3)|mydata$oxx>(1/3))] <- 2

mydata$count <- 1

thresholdsL <-
  mydata%>% group_by(year, london, threshold) %>%
  summarise(count = sum(count, na.rm = TRUE))

thresholdsLA <-
  mydata%>% group_by(year, london, threshold, LA) %>%
  summarise(count = sum(count, na.rm = TRUE))

write.csv(thresholdsL, file="threshL.csv", row.names=FALSE)
write.csv(thresholdsLA, file="threshLA.csv", row.names=FALSE)

#load in the CRDC data

mydata <- read.table("lsoa_cdC.csv", header=TRUE, sep=",")

#get the London flag

mydata <- na.omit(mydata_na)

mydata$wao <- mydata$wot+mydata$wir

#create London flag

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#Do the thresholds

mydata$threshold <- 0
mydata$threshold[mydata$wbr<(1/3)] <- 1
mydata$threshold[mydata$wbr<(1/3) & (mydata$wao>(1/3)|mydata$aao>(1/3)|mydata$abd>(1/3)|mydata$apk>(1/3)|mydata$ain>(1/3)|mydata$acn>(1/3)|mydata$baf>(1/3)|mydata$bca>(1/3)|mydata$oxx>(1/3))] <- 2

mydata$count <- 1

thresholdsL <-
  mydata%>% group_by(year, london, threshold) %>%
  summarise(count = sum(count, na.rm = TRUE))

thresholdsLA <-
  mydata%>% group_by(year, london, threshold, LA) %>%
  summarise(count = sum(count, na.rm = TRUE))

write.csv(thresholdsL, file="threshCL.csv", row.names=FALSE)
write.csv(thresholdsLA, file="threshCLA.csv", row.names=FALSE)