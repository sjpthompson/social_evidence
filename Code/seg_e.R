# This code calculates segregation indices for ethnicity and social class in London and other urban areas using LSOA-level data

#project-wide packages and working directory

install.packages("tidyverse")
library(dplyr)
library(tidyr)

setwd("C:/Users/Spencer/Documents/R/segregation")

#load in the csv file

mydata <- read.table("lsoa_ceC.csv", header=TRUE, sep=",")

# create LSOA-level entropy index by multiplying the ethnic group proportions by their logs # (combining white Irish and white other into an 'other white' category

for(vrb in c("wbr", "wao", "abd", "acn", "ain", "apk", "aao", "baf", "bca", "oxx")) {
    mydata[[paste0("ent_", vrb)]] <- mydata[[vrb]] * log(mydata[[vrb]])
    mydata[[paste0("num_", vrb)]] <- mydata[[vrb]] * mydata$all_ages
}

mydata[is.na(mydata)] <- 0

# sum the individual parameters to create LSOA-level entropy index

mydata$ent <- -(mydata$ent_wbr+mydata$ent_wao+mydata$ent_abd+mydata$ent_acn+mydata$ent_ain+
                  mydata$ent_apk+mydata$ent_aao+mydata$ent_baf+mydata$ent_bca+mydata$ent_oxx)

## JH ==> example of dplyr/tidyverse

totals <- 
    mydata %>% group_by(year, london) %>% 
    summarise(totalpop = sum(all_ages, na.rm = TRUE), # group_by() %>% summarise() rather than aggregate()
              totalwbr = sum(num_wbr, na.rm = TRUE),
              totalwao = sum(num_wao, na.rm = TRUE),
              totalabd = sum(num_abd, na.rm = TRUE),
              totalacn = sum(num_acn, na.rm = TRUE),
              totalain = sum(num_ain, na.rm = TRUE),
              totalapk = sum(num_apk, na.rm = TRUE),
              totalaao = sum(num_aao, na.rm = TRUE),
              totalbaf = sum(num_baf, na.rm = TRUE),
              totalbca = sum(num_bca, na.rm = TRUE),
              totaloxx = sum(num_oxx, na.rm = TRUE) ) 

totalpop10 <- left_join(mydata, totals, by = c("year","london")) # left_join rather than merge()

totalsb <- 
  mydata %>% group_by(year, LA) %>% 
  summarise(totalpopb = sum(all_ages, na.rm = TRUE), # group_by() %>% summarise() rather than aggregate()
            totalwbrb = sum(num_wbr, na.rm = TRUE),
            totalwaob = sum(num_wao, na.rm = TRUE),
            totalabdb = sum(num_abd, na.rm = TRUE),
            totalacnb = sum(num_acn, na.rm = TRUE),
            totalainb = sum(num_ain, na.rm = TRUE),
            totalapkb = sum(num_apk, na.rm = TRUE),
            totalaaob = sum(num_aao, na.rm = TRUE),
            totalbafb = sum(num_baf, na.rm = TRUE),
            totalbcab = sum(num_bca, na.rm = TRUE),
            london = mean(london, na.rm = TRUE),
            totaloxxb = sum(num_oxx, na.rm = TRUE))

totalpop11 <- left_join(totalpop10, totalsb, by = c("year","LA")) # left_join rather than merge()

# london-wide proportions for each ethnic group

for(vrb in c("totalwbr", "totalwao", "totalabd", "totalacn", "totalain", "totalapk", "totalaao", "totalbaf", "totalbca", "totaloxx")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpop
}

# borough-level proportions for each ethnic group

for(vrb in c("totalwbrb", "totalwaob", "totalabdb", "totalacnb", "totalainb", "totalapkb", "totalaaob", "totalbafb", "totalbcab", "totaloxxb")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopb
}

#create London-level entropy index by multiplying the ethnic group proportions by their logs 

for(vrb in c("Tp_totalwbr", "Tp_totalwao", "Tp_totalabd", "Tp_totalacn", "Tp_totalain", "Tp_totalapk", "Tp_totalaao", "Tp_totalbaf", "Tp_totalbca", "Tp_totaloxx")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#create borough-level entropy indices by multiplying the ethnic group proportions by their logs 

for(vrb in c("Tp_totalwbrb", "Tp_totalwaob", "Tp_totalabdb", "Tp_totalacnb", "Tp_totalainb", "Tp_totalapkb", "Tp_totalaaob", "Tp_totalbafb", "Tp_totalbcab", "Tp_totaloxxb")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#sum these to make the London-level entropy index and borough level entropy indices

totalpop11$entT <- -(totalpop11$ent_Tp_totalwbr+totalpop11$ent_Tp_totalwao+totalpop11$ent_Tp_totalabd+totalpop11$ent_Tp_totalacn+totalpop11$ent_Tp_totalain+totalpop11$ent_Tp_totalapk+totalpop11$ent_Tp_totalaao+totalpop11$ent_Tp_totalbaf+totalpop11$ent_Tp_totalbca+totalpop11$ent_Tp_totaloxx)  

totalpop11$entb <- -(totalpop11$ent_Tp_totalwbrb+totalpop11$ent_Tp_totalwaob+totalpop11$ent_Tp_totalabdb+totalpop11$ent_Tp_totalacnb+totalpop11$ent_Tp_totalainb+totalpop11$ent_Tp_totalapkb+totalpop11$ent_Tp_totalaaob+totalpop11$ent_Tp_totalbafb+totalpop11$ent_Tp_totalbcab+totalpop11$ent_Tp_totaloxxb)  

#weight each LSOA entropy index by share of London population

totalpop11$entw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpop)

#weight each LSOA entropy index by share of borough population

totalpop11$entbw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopb)

#weight each Borough entropy index by share of London population

totalpop11$entbb <- totalpop11$entb*(totalpop11$totalpopb/totalpop11$totalpop)

#sum the weighted LSOA entropy indices for each year

totalweightl <- aggregate(totalpop11$entw, by=list(totalpop11$year, totalpop11$london), FUN=sum, na.rm=TRUE)
names(totalweightl) <- c("year", "london", "totalweightl")
popl<-merge(totalpop11,totalweightl,by=c("year", "london"))

#sum the weighted LSOA entropy indices for each borough and year

totalweightb <- aggregate(totalpop11$entbw, by=list(totalpop11$year, totalpop11$LA), FUN=sum, na.rm=TRUE)
names(totalweightb) <- c("year", "LA", "totalweightb")
popff<-merge(popl,totalweightb,by=c("year", "LA"))

#sum the borough entropy index for each year

totalweightb1 <-
  totalpop11%>% group_by(year, LA) %>%
  summarise(totalweightb1 = mean(entbb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE))

totalweightbb <-
  totalweightb1%>% group_by(year, london) %>%
  summarise(totalweightbb = sum(totalweightb1, na.rm = TRUE))

popf<-merge(popff,totalweightbb,by=c("year", "london"))

#create the final entropy index comparable across time

popf$entc <- (popf$entT-popf$totalweightl)/popf$entT

popf$entb <- (popf$entb-popf$totalweightb)/popf$entb

popf$entbb <- (popf$entT-popf$totalweightbb)/popf$entT

#average for each year gives us the London-wide entropy index by year

entropy <-
  popf%>% group_by(year, london) %>%
  summarise(entc = mean(entc, na.rm = TRUE))

entropyb <-
  popf%>% group_by(year, LA) %>%
  summarise(entb = mean(entb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE),
            pop = sum(all_ages, na.rm=TRUE))

entropybb <-
  popf%>% group_by(year, london) %>%
  summarise(entbb = mean(entbb, na.rm = TRUE))

#saving

write.csv(entropy, file="ent.csv", row.names=FALSE)
write.csv(entropyb, file="entb.csv", row.names=FALSE)
write.csv(entropybb, file="entbb.csv", row.names=FALSE)

##doing CDRC version

mydata_na <- read.table("lsoa_cdC.csv", header=TRUE, sep=",")

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

# create LSOA-level entropy index by multiplying the ethnic group proportions by their logs # (combining white Irish and white other into an 'other white' category

for(vrb in c("wbr", "wao", "abd", "acn", "ain", "apk", "aao", "baf", "bca", "oxx")) {
  mydata[[paste0("ent_", vrb)]] <- mydata[[vrb]] * log(mydata[[vrb]])
  mydata[[paste0("num_", vrb)]] <- mydata[[vrb]] * mydata$all_ages
}

mydata[is.na(mydata)] <- 0

#then run the rest of the code
#saving

#saving

write.csv(entropy, file="entCD.csv", row.names=FALSE)
write.csv(entropyb, file="entbCD.csv", row.names=FALSE)
write.csv(entropybb, file="entbbCD.csv", row.names=FALSE)

#Now doing NS-sEC version

mydata <- read.csv(file="lsoa_cnC.csv", header=TRUE, sep=",")

#run the code linking rural urban classification

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#two summations - to London level and to borough level

totals <- 
  mydata %>% group_by(year, london) %>% 
  summarise(totalpopl = sum(all_ages, na.rm = TRUE),
            total1l = sum(One, na.rm = TRUE),
            total2l = sum(Two, na.rm = TRUE),
            total3l = sum(Three, na.rm = TRUE),
            total15l = sum(Unemp, na.rm = TRUE),
            total16l = sum(Students, na.rm = TRUE))

totalsb <- 
  mydata %>% group_by(year, LA) %>% 
  summarise(totalpopb = sum(all_ages, na.rm = TRUE),
            total1b = sum(One, na.rm = TRUE),
            total2b = sum(Two, na.rm = TRUE),
            total3b = sum(Three, na.rm = TRUE),
            total15b = sum(Unemp, na.rm = TRUE),
            total16b = sum(Students, na.rm = TRUE)) 

mydataLU <- left_join(mydata, totals, by = c("year","london"))
totalpop11 <- left_join(mydataLU, totalsb, by = c("year", "LA"))

#first need to make proportions

for(vrb in c("One", "Two", "Three", "Unemp", "Students")) {
  totalpop11[[paste0("p_", vrb)]] <- totalpop11[[vrb]]/totalpop11$all_ages
}

#then LSOA-level logs

for(vrb in c("p_One", "p_Two", "p_Three", "p_Unemp", "p_Students")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]]*log(totalpop11[[vrb]])
}

totalpop11[is.na(totalpop11)] <- 0

#Sum the LSOA-level logs to get entropy

totalpop11$ent <- -(totalpop11$ent_p_One+totalpop11$ent_p_Two+totalpop11$ent_p_Three+totalpop11$ent_p_Unemp+totalpop11$ent_p_Students)

# london-wide proportions for each NS-SEC group

for(vrb in c("total1l", "total2l", "total3l", "total15l", "total16l")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopl
}

# borough-level proportions for each NS-SEC group

for(vrb in c("total1b", "total2b", "total3b", "total15b", "total16b")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopb
}

#create London-level entropy index by multiplying the NS-SEC group proportions by their logs 

for(vrb in c("Tp_total1l", "Tp_total2l", "Tp_total3l", "Tp_total15l", "Tp_total16l")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#create borough-level entropy indices by multiplying the NS-SEC group proportions by their logs 

for(vrb in c("Tp_total1b", "Tp_total2b", "Tp_total3b", "Tp_total15b", "Tp_total16b")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#sum these to make the London-level entropy index and borough level entropy indices

totalpop11$entT <- -(totalpop11$ent_Tp_total1l+totalpop11$ent_Tp_total2l+totalpop11$ent_Tp_total3l+totalpop11$ent_Tp_total15l+totalpop11$ent_Tp_total16l)  

totalpop11$entb <- -(totalpop11$ent_Tp_total1b+totalpop11$ent_Tp_total2b+totalpop11$ent_Tp_total3b+totalpop11$ent_Tp_total15b+totalpop11$ent_Tp_total16b)  

#weight each LSOA entropy index by share of London population

totalpop11$entw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopl)

#weight each LSOA entropy index by share of borough population

totalpop11$entbw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopb)

#weight each Borough entropy index by share of London population

totalpop11$entbb <- totalpop11$entb*(totalpop11$totalpopb/totalpop11$totalpopl)

#sum the weighted LSOA entropy indices for each year

totalweightl <- aggregate(totalpop11$entw, by=list(totalpop11$year, totalpop11$london), FUN=sum, na.rm=TRUE)
names(totalweightl) <- c("year", "london", "totalweightl")
popl<-merge(totalpop11,totalweightl,by=c("year", "london"))

#sum the weighted LSOA entropy indices for each borough and year

totalweightb <- aggregate(totalpop11$entbw, by=list(totalpop11$year, totalpop11$LA), FUN=sum, na.rm=TRUE)
names(totalweightb) <- c("year", "LA", "totalweightb")
popff<-merge(popl,totalweightb,by=c("year", "LA"))

#sum the borough entropy index for each year

totalweightb1 <-
  totalpop11%>% group_by(year, LA) %>%
  summarise(totalweightb1 = mean(entbb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE))

totalweightbb <-
  totalweightb1%>% group_by(year, london) %>%
  summarise(totalweightbb = sum(totalweightb1, na.rm = TRUE))

popf<-merge(popff,totalweightbb,by=c("year", "london"))

#create the final entropy index comparable across time

popf$entc <- (popf$entT-popf$totalweightl)/popf$entT

popf$entb <- (popf$entb-popf$totalweightb)/popf$entb

popf$entbb <- (popf$entT-popf$totalweightbb)/popf$entT

#average for each year gives us the London-wide entropy index by year

entropy <-
  popf%>% group_by(year, london) %>%
  summarise(entc = mean(entc, na.rm = TRUE))

entropyb <-
  popf%>% group_by(year, LA) %>%
  summarise(entb = mean(entb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE),
            pop = sum(all_ages, na.rm=TRUE))

entropybb <-
  popf%>% group_by(year, london) %>%
  summarise(entbb = mean(entbb, na.rm = TRUE))

#saving

write.csv(entropy, file="entN.csv", row.names=FALSE)
write.csv(entropyb, file="entbN.csv", row.names=FALSE)
write.csv(entropybb, file="entbbN.csv", row.names=FALSE)

#Now doing AGE version

mydata <- read.csv(file="lsoa_ca.csv", header=TRUE, sep=",")

#run the code linking rural urban classification

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#two summations - to London level and to borough level

totals <- 
  mydata %>% group_by(london) %>% 
  summarise(totalpopl = sum(all_ages, na.rm = TRUE),
            totalAl = sum(A, na.rm = TRUE),
            totalBl = sum(B, na.rm = TRUE),
            totalCl = sum(C, na.rm = TRUE),
            totalDl = sum(D, na.rm = TRUE),
            totalEl = sum(E, na.rm = TRUE),
            totalFl = sum(F, na.rm = TRUE),
            totalGl = sum(G, na.rm = TRUE),
            totalHl = sum(H, na.rm = TRUE),
            totalIl = sum(I, na.rm = TRUE),
            totalJl = sum(J, na.rm = TRUE))

totalsb <- 
  mydata %>% group_by(LA) %>% 
  summarise(totalpopb = sum(all_ages, na.rm = TRUE),
            totalAb = sum(A, na.rm = TRUE),
            totalBb = sum(B, na.rm = TRUE),
            totalCb = sum(C, na.rm = TRUE),
            totalDb = sum(D, na.rm = TRUE),
            totalEb = sum(E, na.rm = TRUE),
            totalFb = sum(F, na.rm = TRUE),
            totalGb = sum(G, na.rm = TRUE),
            totalHb = sum(H, na.rm = TRUE),
            totalIb = sum(I, na.rm = TRUE),
            totalJb = sum(J, na.rm = TRUE)) 

mydataLU <- left_join(mydata, totals, by = c("london"))
totalpop11 <- left_join(mydataLU, totalsb, by = c("LA"))

#first need to make proportions

for(vrb in c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")) {
  totalpop11[[paste0("p_", vrb)]] <- totalpop11[[vrb]]/totalpop11$all_ages
}

#then LSOA-level logs

for(vrb in c("p_A", "p_B", "p_C", "p_D", "p_E", "p_F", "p_G", "p_H", "p_I", "p_J")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]]*log(totalpop11[[vrb]])
}

totalpop11[is.na(totalpop11)] <- 0

#Sum the LSOA-level logs to get entropy

totalpop11$ent <- -(totalpop11$ent_p_A+totalpop11$ent_p_B+totalpop11$ent_p_C+totalpop11$ent_p_D+totalpop11$ent_p_E+totalpop11$ent_p_F+totalpop11$ent_p_G+totalpop11$ent_p_H+totalpop11$ent_p_I+totalpop11$ent_p_J)

# london-wide proportions for each age group

for(vrb in c("totalAl", "totalBl", "totalCl", "totalDl", "totalEl", "totalFl", "totalGl", "totalHl", "totalIl", "totalJl")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopl
}

# borough-level proportions for each NS-SEC group

for(vrb in c("totalAb", "totalBb", "totalCb", "totalDb", "totalEb", "totalFb", "totalGb", "totalHb", "totalIb", "totalJb")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopb
}

#create London-level entropy index by multiplying the NS-SEC group proportions by their logs 

for(vrb in c("Tp_totalAl", "Tp_totalBl", "Tp_totalCl", "Tp_totalDl", "Tp_totalEl", "Tp_totalFl", "Tp_totalGl", "Tp_totalHl", "Tp_totalIl", "Tp_totalJl")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#create borough-level entropy indices by multiplying the NS-SEC group proportions by their logs 

for(vrb in c("Tp_totalAb", "Tp_totalBb", "Tp_totalCb", "Tp_totalDb", "Tp_totalEb", "Tp_totalFb", "Tp_totalGb", "Tp_totalHb", "Tp_totalIb", "Tp_totalJb")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#sum these to make the London-level entropy index and borough level entropy indices

totalpop11$entT <- -(totalpop11$ent_Tp_totalAl+totalpop11$ent_Tp_totalBl+totalpop11$ent_Tp_totalCl+totalpop11$ent_Tp_totalDl+totalpop11$ent_Tp_totalEl+totalpop11$ent_Tp_totalFl+totalpop11$ent_Tp_totalGl+totalpop11$ent_Tp_totalHl+totalpop11$ent_Tp_totalIl+totalpop11$ent_Tp_totalJl)  

totalpop11$entb <- -(totalpop11$ent_Tp_totalAb+totalpop11$ent_Tp_totalBb+totalpop11$ent_Tp_totalCb+totalpop11$ent_Tp_totalDb+totalpop11$ent_Tp_totalEb+totalpop11$ent_Tp_totalFb+totalpop11$ent_Tp_totalGb+totalpop11$ent_Tp_totalHb+totalpop11$ent_Tp_totalIb+totalpop11$ent_Tp_totalJb)  

#weight each LSOA entropy index by share of London population

totalpop11$entw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopl)

#weight each LSOA entropy index by share of borough population

totalpop11$entbw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopb)

#weight each Borough entropy index by share of London population

totalpop11$entbb <- totalpop11$entb*(totalpop11$totalpopb/totalpop11$totalpopl)

#sum the weighted LSOA entropy indices for each year

totalweightl <- aggregate(totalpop11$entw, by=list(totalpop11$london), FUN=sum, na.rm=TRUE)
names(totalweightl) <- c("london", "totalweightl")
popl<-merge(totalpop11,totalweightl,by=c("london"))

#sum the weighted LSOA entropy indices for each borough and year

totalweightb <- aggregate(totalpop11$entbw, by=list(totalpop11$LA), FUN=sum, na.rm=TRUE)
names(totalweightb) <- c("LA", "totalweightb")
popff<-merge(popl,totalweightb,by=c("LA"))

#sum the borough entropy index for each year

totalweightb1 <-
  totalpop11%>% group_by(LA) %>%
  summarise(totalweightb1 = mean(entbb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE))

totalweightbb <-
  totalweightb1%>% group_by(london) %>%
  summarise(totalweightbb = sum(totalweightb1, na.rm = TRUE))

popf<-merge(popff,totalweightbb,by=c("london"))

#create the final entropy index comparable across time

popf$entc <- (popf$entT-popf$totalweightl)/popf$entT

popf$entb <- (popf$entb-popf$totalweightb)/popf$entb

popf$entbb <- (popf$entT-popf$totalweightbb)/popf$entT

#average for each year gives us the London-wide entropy index by year

entropy <-
  popf%>% group_by(london) %>%
  summarise(entc = mean(entc, na.rm = TRUE))

entropyb <-
  popf%>% group_by(LA) %>%
  summarise(entb = mean(entb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE),
            pop = sum(all_ages, na.rm=TRUE))

entropybb <-
  popf%>% group_by(london) %>%
  summarise(entbb = mean(entbb, na.rm = TRUE))

#saving

write.csv(entropy, file="entA.csv", row.names=FALSE)
write.csv(entropyb, file="entbA.csv", row.names=FALSE)
write.csv(entropybb, file="entbbA.csv", row.names=FALSE)

#Now doing QUALS version

mydata <- read.csv(file="lsoa_cq.csv", header=TRUE, sep=",")

#run the code linking rural urban classification

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#two summations - to London level and to borough level

totals <- 
  mydata %>% group_by(london) %>% 
  summarise(totalpopl = sum(all_ages, na.rm = TRUE),
            totalAl = sum(A, na.rm = TRUE),
            totalBl = sum(B, na.rm = TRUE),
            totalCl = sum(C, na.rm = TRUE),
            totalDl = sum(D, na.rm = TRUE),
            totalEl = sum(E, na.rm = TRUE),
            totalFl = sum(F, na.rm = TRUE),
            totalGl = sum(G, na.rm = TRUE))

totalsb <- 
  mydata %>% group_by(LA) %>% 
  summarise(totalpopb = sum(all_ages, na.rm = TRUE),
            totalAb = sum(A, na.rm = TRUE),
            totalBb = sum(B, na.rm = TRUE),
            totalCb = sum(C, na.rm = TRUE),
            totalDb = sum(D, na.rm = TRUE),
            totalEb = sum(E, na.rm = TRUE),
            totalFb = sum(F, na.rm = TRUE),
            totalGb = sum(G, na.rm = TRUE)) 

mydataLU <- left_join(mydata, totals, by = c("london"))
totalpop11 <- left_join(mydataLU, totalsb, by = c("LA"))

#first need to make proportions

for(vrb in c("A", "B", "C", "D", "E", "F", "G")) {
  totalpop11[[paste0("p_", vrb)]] <- totalpop11[[vrb]]/totalpop11$all_ages
}

#then LSOA-level logs

for(vrb in c("p_A", "p_B", "p_C", "p_D", "p_E", "p_F", "p_G")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]]*log(totalpop11[[vrb]])
}

totalpop11[is.na(totalpop11)] <- 0

#Sum the LSOA-level logs to get entropy

totalpop11$ent <- -(totalpop11$ent_p_A+totalpop11$ent_p_B+totalpop11$ent_p_C+totalpop11$ent_p_D+totalpop11$ent_p_E+totalpop11$ent_p_F+totalpop11$ent_p_G)

# london-wide proportions for each age group

for(vrb in c("totalAl", "totalBl", "totalCl", "totalDl", "totalEl", "totalFl", "totalGl")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopl
}

# borough-level proportions for each NS-SEC group

for(vrb in c("totalAb", "totalBb", "totalCb", "totalDb", "totalEb", "totalFb", "totalGb")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopb
}

#create London-level entropy index by multiplying the NS-SEC group proportions by their logs 

for(vrb in c("Tp_totalAl", "Tp_totalBl", "Tp_totalCl", "Tp_totalDl", "Tp_totalEl", "Tp_totalFl", "Tp_totalGl")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#create borough-level entropy indices by multiplying the NS-SEC group proportions by their logs 

for(vrb in c("Tp_totalAb", "Tp_totalBb", "Tp_totalCb", "Tp_totalDb", "Tp_totalEb", "Tp_totalFb", "Tp_totalGb")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#sum these to make the London-level entropy index and borough level entropy indices

totalpop11$entT <- -(totalpop11$ent_Tp_totalAl+totalpop11$ent_Tp_totalBl+totalpop11$ent_Tp_totalCl+totalpop11$ent_Tp_totalDl+totalpop11$ent_Tp_totalEl+totalpop11$ent_Tp_totalFl+totalpop11$ent_Tp_totalGl)  

totalpop11$entb <- -(totalpop11$ent_Tp_totalAb+totalpop11$ent_Tp_totalBb+totalpop11$ent_Tp_totalCb+totalpop11$ent_Tp_totalDb+totalpop11$ent_Tp_totalEb+totalpop11$ent_Tp_totalFb+totalpop11$ent_Tp_totalGb)  

#weight each LSOA entropy index by share of London population

totalpop11$entw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopl)

#weight each LSOA entropy index by share of borough population

totalpop11$entbw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopb)

#weight each Borough entropy index by share of London population

totalpop11$entbb <- totalpop11$entb*(totalpop11$totalpopb/totalpop11$totalpopl)

#sum the weighted LSOA entropy indices for each year

totalweightl <- aggregate(totalpop11$entw, by=list(totalpop11$london), FUN=sum, na.rm=TRUE)
names(totalweightl) <- c("london", "totalweightl")
popl<-merge(totalpop11,totalweightl,by=c("london"))

#sum the weighted LSOA entropy indices for each borough and year

totalweightb <- aggregate(totalpop11$entbw, by=list(totalpop11$LA), FUN=sum, na.rm=TRUE)
names(totalweightb) <- c("LA", "totalweightb")
popff<-merge(popl,totalweightb,by=c("LA"))

#sum the borough entropy index for each year

totalweightb1 <-
  totalpop11%>% group_by(LA) %>%
  summarise(totalweightb1 = mean(entbb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE))

totalweightbb <-
  totalweightb1%>% group_by(london) %>%
  summarise(totalweightbb = sum(totalweightb1, na.rm = TRUE))

popf<-merge(popff,totalweightbb,by=c("london"))

#create the final entropy index comparable across time

popf$entc <- (popf$entT-popf$totalweightl)/popf$entT

popf$entb <- (popf$entb-popf$totalweightb)/popf$entb

popf$entbb <- (popf$entT-popf$totalweightbb)/popf$entT

#average for each year gives us the London-wide entropy index by year

entropy <-
  popf%>% group_by(london) %>%
  summarise(entc = mean(entc, na.rm = TRUE))

entropyb <-
  popf%>% group_by(LA) %>%
  summarise(entb = mean(entb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE),
            pop = sum(all_ages, na.rm=TRUE))

entropybb <-
  popf%>% group_by(london) %>%
  summarise(entbb = mean(entbb, na.rm = TRUE))

#saving

write.csv(entropy, file="entQ.csv", row.names=FALSE)
write.csv(entropyb, file="entbQ.csv", row.names=FALSE)
write.csv(entropybb, file="entbbQ.csv", row.names=FALSE)

#Now doing COUNTRY OF BIRTH version

mydata <- read.csv(file="lsoa_cc.csv", header=TRUE, sep=",")

#run the code linking rural urban classification

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#two summations - to London level and to borough level

totals <- 
  mydata %>% group_by(london) %>% 
  summarise(totalpopl = sum(all_ages, na.rm = TRUE),
            totalAl = sum(A, na.rm = TRUE),
            totalBl = sum(B, na.rm = TRUE),
            totalCl = sum(C, na.rm = TRUE),
            totalDl = sum(D, na.rm = TRUE))

totalsb <- 
  mydata %>% group_by(LA) %>% 
  summarise(totalpopb = sum(all_ages, na.rm = TRUE),
            totalAb = sum(A, na.rm = TRUE),
            totalBb = sum(B, na.rm = TRUE),
            totalCb = sum(C, na.rm = TRUE),
            totalDb = sum(D, na.rm = TRUE)) 

mydataLU <- left_join(mydata, totals, by = c("london"))
totalpop11 <- left_join(mydataLU, totalsb, by = c("LA"))

#first need to make proportions

for(vrb in c("A", "B", "C", "D")) {
  totalpop11[[paste0("p_", vrb)]] <- totalpop11[[vrb]]/totalpop11$all_ages
}

#then LSOA-level logs

for(vrb in c("p_A", "p_B", "p_C", "p_D")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]]*log(totalpop11[[vrb]])
}

totalpop11[is.na(totalpop11)] <- 0

#Sum the LSOA-level logs to get entropy

totalpop11$ent <- -(totalpop11$ent_p_A+totalpop11$ent_p_B+totalpop11$ent_p_C+totalpop11$ent_p_D)

# london-wide proportions for each age group

for(vrb in c("totalAl", "totalBl", "totalCl", "totalDl")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopl
}

# borough-level proportions for each NS-SEC group

for(vrb in c("totalAb", "totalBb", "totalCb", "totalDb")) {
  totalpop11[[paste0("Tp_", vrb)]] <- totalpop11[[vrb]] / totalpop11$totalpopb
}

#create London-level entropy index by multiplying the NS-SEC group proportions by their logs 

for(vrb in c("Tp_totalAl", "Tp_totalBl", "Tp_totalCl", "Tp_totalDl")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#create borough-level entropy indices by multiplying the NS-SEC group proportions by their logs 

for(vrb in c("Tp_totalAb", "Tp_totalBb", "Tp_totalCb", "Tp_totalDb")) {
  totalpop11[[paste0("ent_", vrb)]] <- totalpop11[[vrb]] * log(totalpop11[[vrb]])
}

#sum these to make the London-level entropy index and borough level entropy indices

totalpop11$entT <- -(totalpop11$ent_Tp_totalAl+totalpop11$ent_Tp_totalBl+totalpop11$ent_Tp_totalCl+totalpop11$ent_Tp_totalDl)  

totalpop11$entb <- -(totalpop11$ent_Tp_totalAb+totalpop11$ent_Tp_totalBb+totalpop11$ent_Tp_totalCb+totalpop11$ent_Tp_totalDb)  

#weight each LSOA entropy index by share of London population

totalpop11$entw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopl)

#weight each LSOA entropy index by share of borough population

totalpop11$entbw <- totalpop11$ent*(totalpop11$all_ages/totalpop11$totalpopb)

#weight each Borough entropy index by share of London population

totalpop11$entbb <- totalpop11$entb*(totalpop11$totalpopb/totalpop11$totalpopl)

#sum the weighted LSOA entropy indices for each year

totalweightl <- aggregate(totalpop11$entw, by=list(totalpop11$london), FUN=sum, na.rm=TRUE)
names(totalweightl) <- c("london", "totalweightl")
popl<-merge(totalpop11,totalweightl,by=c("london"))

#sum the weighted LSOA entropy indices for each borough and year

totalweightb <- aggregate(totalpop11$entbw, by=list(totalpop11$LA), FUN=sum, na.rm=TRUE)
names(totalweightb) <- c("LA", "totalweightb")
popff<-merge(popl,totalweightb,by=c("LA"))

#sum the borough entropy index for each year

totalweightb1 <-
  totalpop11%>% group_by(LA) %>%
  summarise(totalweightb1 = mean(entbb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE))

totalweightbb <-
  totalweightb1%>% group_by(london) %>%
  summarise(totalweightbb = sum(totalweightb1, na.rm = TRUE))

popf<-merge(popff,totalweightbb,by=c("london"))

#create the final entropy index comparable across time

popf$entc <- (popf$entT-popf$totalweightl)/popf$entT

popf$entb <- (popf$entb-popf$totalweightb)/popf$entb

popf$entbb <- (popf$entT-popf$totalweightbb)/popf$entT

#average for each year gives us the London-wide entropy index by year

entropy <-
  popf%>% group_by(london) %>%
  summarise(entc = mean(entc, na.rm = TRUE))

entropyb <-
  popf%>% group_by(LA) %>%
  summarise(entb = mean(entb, na.rm = TRUE),
            london = mean(london, na.rm = TRUE),
            pop = sum(all_ages, na.rm=TRUE))

entropybb <-
  popf%>% group_by(london) %>%
  summarise(entbb = mean(entbb, na.rm = TRUE))

#saving

write.csv(entropy, file="entC.csv", row.names=FALSE)
write.csv(entropyb, file="entbC.csv", row.names=FALSE)
write.csv(entropybb, file="entbbC.csv", row.names=FALSE)

#change in Simpson diversity index

mydata_na <- read.table("lsoa_cdC.csv", header=TRUE, sep=",")

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

# create LSOA-level entropy index by multiplying the ethnic group proportions by their logs # (combining white Irish and white other into an 'other white' category

mydata$simpson <- 1/((mydata$wbr)^2+(mydata$wao)^2+(mydata$abd)^2+(mydata$acn)^2+(mydata$ain)^2+(mydata$apk)^2+(mydata$aao)^2+(mydata$baf)^2+(mydata$acn)^2+(mydata$bca)^2)
newdata <- mydata[which(mydata$london==1 & mydata$year==2011), ]
newdata2 <- mydata[which(mydata$london==1 & mydata$year==2016), ]
newdata3 <- mydata[which(mydata$london==1 & mydata$year==2002), ]


write.csv(newdata, file="simpson2.csv", row.names=FALSE)
write.csv(newdata2, file="simpson3.csv", row.names=FALSE)
write.csv(newdata3, file="simpson01.csv", row.names=FALSE)

#do the same for the two census years

#load in the csv file

mydata <- read.table("lsoa_ceC.csv", header=TRUE, sep=",")

# create LSOA-level entropy index by multiplying the ethnic group proportions by their logs # (combining white Irish and white other into an 'other white' category

mydata$simpson <- 1/((mydata$wbr)^2+(mydata$wao)^2+(mydata$abd)^2+(mydata$acn)^2+(mydata$ain)^2+(mydata$apk)^2+(mydata$aao)^2+(mydata$baf)^2+(mydata$acn)^2+(mydata$bca)^2)
newdata <- mydata[which(mydata$london==1 & mydata$year==2001), ]
newdata2 <- mydata[which(mydata$london==1 & mydata$year==2011), ]

mydata[is.na(mydata)] <- 0

write.csv(newdata, file="simpson4.csv", row.names=FALSE)
write.csv(newdata2, file="simpson5.csv", row.names=FALSE)
