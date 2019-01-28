#This code calculates the index of dissimilarity for ethnicity, social class, age, qualifications and  in London and other urban areas using LSOA-level data

setwd(Data)

####1) Ethnicity

#First we need London and LA totals for each ethnicity group
#get total numbers of each group

for(vrb in c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")) {
  lsoa_ce[[paste0("num_", vrb)]] <- mydata[[vrb]] * mydata$all_ages
}

#Gather the different ethnicity categories

#two summations - one at London/other urban level, one at LA level

totalsLU <- 
  mydata %>% group_by(year, london) %>% 
  summarise(totalpopl = sum(all_ages, na.rm = TRUE), # group_by() %>% summarise() rather than aggregate()
            totalwbrl = sum(num_wbr, na.rm = TRUE),
            totalwaol = sum(num_wao, na.rm = TRUE),
            totalabdl = sum(num_abd, na.rm = TRUE),
            totalacnl = sum(num_acn, na.rm = TRUE),
            totalainl = sum(num_ain, na.rm = TRUE),
            totalapkl = sum(num_apk, na.rm = TRUE),
            totalaaol = sum(num_aao, na.rm = TRUE),
            totalbafl = sum(num_baf, na.rm = TRUE),
            totalbcal = sum(num_bca, na.rm = TRUE),
            totaloxxl = sum(num_oxx, na.rm = TRUE) )

totalsLA <- 
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
            totaloxxb = sum(num_oxx, na.rm = TRUE) ) 

mydataLU <- left_join(mydata, totalsLU, by = c("year","london"))
mydataLULA <- left_join(mydataLU, totalsLA, by = c("year", "LA"))

# calculate LSOA-level dissimilarity, first relative to London then relative to LA

# relative to London

mydataLULA$dl_wbr <- 0.5*abs(mydataLULA$num_wbr/mydataLULA$totalwbrl - (mydataLULA$all_ages-mydataLULA$num_wbr)/(mydataLULA$totalpopl-mydataLULA$totalwbrl))
mydataLULA$dl_wao <- 0.5*abs(mydataLULA$num_wao/mydataLULA$totalwaol - (mydataLULA$all_ages-mydataLULA$num_wao)/(mydataLULA$totalpopl-mydataLULA$totalwaol))
mydataLULA$dl_abd <- 0.5*abs(mydataLULA$num_abd/mydataLULA$totalabdl - (mydataLULA$all_ages-mydataLULA$num_abd)/(mydataLULA$totalpopl-mydataLULA$totalabdl))
mydataLULA$dl_acn <- 0.5*abs(mydataLULA$num_acn/mydataLULA$totalacnl - (mydataLULA$all_ages-mydataLULA$num_acn)/(mydataLULA$totalpopl-mydataLULA$totalacnl))
mydataLULA$dl_ain <- 0.5*abs(mydataLULA$num_ain/mydataLULA$totalainl - (mydataLULA$all_ages-mydataLULA$num_ain)/(mydataLULA$totalpopl-mydataLULA$totalainl))
mydataLULA$dl_apk <- 0.5*abs(mydataLULA$num_apk/mydataLULA$totalapkl - (mydataLULA$all_ages-mydataLULA$num_apk)/(mydataLULA$totalpopl-mydataLULA$totalapkl))
mydataLULA$dl_aao <- 0.5*abs(mydataLULA$num_aao/mydataLULA$totalaaol - (mydataLULA$all_ages-mydataLULA$num_aao)/(mydataLULA$totalpopl-mydataLULA$totalaaol))
mydataLULA$dl_baf <- 0.5*abs(mydataLULA$num_baf/mydataLULA$totalbafl - (mydataLULA$all_ages-mydataLULA$num_baf)/(mydataLULA$totalpopl-mydataLULA$totalbafl))
mydataLULA$dl_bca <- 0.5*abs(mydataLULA$num_bca/mydataLULA$totalbcal - (mydataLULA$all_ages-mydataLULA$num_bca)/(mydataLULA$totalpopl-mydataLULA$totalbcal))
mydataLULA$dl_oxx <- 0.5*abs(mydataLULA$num_oxx/mydataLULA$totaloxxl - (mydataLULA$all_ages-mydataLULA$num_oxx)/(mydataLULA$totalpopl-mydataLULA$totaloxxl))

#relative to LA

mydataLULA$db_wbr <- 0.5*abs(mydataLULA$num_wbr/mydataLULA$totalwbrb - (mydataLULA$all_ages-mydataLULA$num_wbr)/(mydataLULA$totalpopb-mydataLULA$totalwbrb))
mydataLULA$db_wao <- 0.5*abs(mydataLULA$num_wao/mydataLULA$totalwaob - (mydataLULA$all_ages-mydataLULA$num_wao)/(mydataLULA$totalpopb-mydataLULA$totalwaob))
mydataLULA$db_abd <- 0.5*abs(mydataLULA$num_abd/mydataLULA$totalabdb - (mydataLULA$all_ages-mydataLULA$num_abd)/(mydataLULA$totalpopb-mydataLULA$totalabdb))
mydataLULA$db_acn <- 0.5*abs(mydataLULA$num_acn/mydataLULA$totalacnb - (mydataLULA$all_ages-mydataLULA$num_acn)/(mydataLULA$totalpopb-mydataLULA$totalacnb))
mydataLULA$db_ain <- 0.5*abs(mydataLULA$num_ain/mydataLULA$totalainb - (mydataLULA$all_ages-mydataLULA$num_ain)/(mydataLULA$totalpopb-mydataLULA$totalainb))
mydataLULA$db_apk <- 0.5*abs(mydataLULA$num_apk/mydataLULA$totalapkb - (mydataLULA$all_ages-mydataLULA$num_apk)/(mydataLULA$totalpopb-mydataLULA$totalapkb))
mydataLULA$db_aao <- 0.5*abs(mydataLULA$num_aao/mydataLULA$totalaaob - (mydataLULA$all_ages-mydataLULA$num_aao)/(mydataLULA$totalpopb-mydataLULA$totalaaob))
mydataLULA$db_baf <- 0.5*abs(mydataLULA$num_baf/mydataLULA$totalbafb - (mydataLULA$all_ages-mydataLULA$num_baf)/(mydataLULA$totalpopb-mydataLULA$totalbafb))
mydataLULA$db_bca <- 0.5*abs(mydataLULA$num_bca/mydataLULA$totalbcab - (mydataLULA$all_ages-mydataLULA$num_bca)/(mydataLULA$totalpopb-mydataLULA$totalbcab))
mydataLULA$db_oxx <- 0.5*abs(mydataLULA$num_oxx/mydataLULA$totaloxxb - (mydataLULA$all_ages-mydataLULA$num_oxx)/(mydataLULA$totalpopb-mydataLULA$totaloxxb))

#summarise and print csv files

DISLU <- 
  mydataLULA %>% group_by(year, london) %>% 
  summarise(dls_wbr = sum(dl_wbr, na.rm = TRUE),
            dls_wao = sum(dl_wao, na.rm = TRUE),
            dls_abd = sum(dl_abd, na.rm = TRUE),
            dls_acn = sum(dl_acn, na.rm = TRUE),
            dls_ain = sum(dl_ain, na.rm = TRUE),
            dls_apk = sum(dl_apk, na.rm = TRUE),
            dls_aao = sum(dl_aao, na.rm = TRUE),
            dls_baf = sum(dl_baf, na.rm = TRUE),
            dls_bca = sum(dl_bca, na.rm = TRUE),
            dls_oxx = sum(dl_oxx, na.rm = TRUE) )

DISLA <- 
  mydataLULA %>% group_by(year, LA) %>% 
  summarise(dbs_wbr = sum(db_wbr, na.rm = TRUE),
            dbs_wao = sum(db_wao, na.rm = TRUE),
            dbs_abd = sum(db_abd, na.rm = TRUE),
            dbs_acn = sum(db_acn, na.rm = TRUE),
            dbs_ain = sum(db_ain, na.rm = TRUE),
            dbs_apk = sum(db_apk, na.rm = TRUE),
            dbs_aao = sum(db_aao, na.rm = TRUE),
            dbs_baf = sum(db_baf, na.rm = TRUE),
            dbs_bca = sum(db_bca, na.rm = TRUE),
            dbs_oxx = sum(db_oxx, na.rm = TRUE),
            london = mean(london, na.rm = TRUE) )

write.csv(DISLU, file="DISLUC.csv", row.names=FALSE)
write.csv(DISLA, file="DISLA.csv", row.names=FALSE)

# Different code for running the CDRC data

#for loading the data and removing NA's

mydata_na <- read.csv(file="lsoa_cdC.csv", header=TRUE, sep=",")
mydata <- na.omit(mydata_na)

#add together white irish (wir) to white other (wot)

mydata$wao <- mydata$wot+mydata$wir

# create London, with a value of 1 for London, 2 for other urban and 3 for all other LA's

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#then run the rest of the above code

#then saving

write.csv(DISLU, file="DISLOCDC.csv", row.names=FALSE)
write.csv(DISLA, file="DISLACDC.csv", row.names=FALSE)

#different code for running NS-SEC analysis

mydata <- read.csv(file="lsoa_cnC.csv", header=TRUE, sep=",")

#run the code linking rural urban classification

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#two summations - one at London/other urban level, one at LA level

totalsLU <- 
  mydata %>% group_by(year, london) %>% 
  summarise(totalpopl = sum(all_ages, na.rm = TRUE),
            total1l = sum(One, na.rm = TRUE),
            total2l = sum(Two, na.rm = TRUE),
            total3l = sum(Three, na.rm = TRUE),
            total15l = sum(Unemp, na.rm = TRUE),
            total16l = sum(Students, na.rm = TRUE))

totalsLA <- 
  mydata %>% group_by(year, LA) %>% 
  summarise(totalpopb = sum(all_ages, na.rm = TRUE),
            total1b = sum(One, na.rm = TRUE),
            total2b = sum(Two, na.rm = TRUE),
            total3b = sum(Three, na.rm = TRUE),
            total15b = sum(Unemp, na.rm = TRUE),
            total16b = sum(Students, na.rm = TRUE)) 

mydataLU <- left_join(mydata, totalsLU, by = c("year","london"))
mydataLULA <- left_join(mydataLU, totalsLA, by = c("year", "LA"))

# calculate LSOA-level dissimilarity, first relative to London then relative to LA

# relative to London

mydataLULA$dl_1 <- 0.5*abs(mydataLULA$One/mydataLULA$total1l - (mydataLULA$all_ages-mydataLULA$One)/(mydataLULA$totalpopl-mydataLULA$total1l))
mydataLULA$dl_2 <- 0.5*abs(mydataLULA$Two/mydataLULA$total2l - (mydataLULA$all_ages-mydataLULA$Two)/(mydataLULA$totalpopl-mydataLULA$total2l))
mydataLULA$dl_3 <- 0.5*abs(mydataLULA$Three/mydataLULA$total3l - (mydataLULA$all_ages-mydataLULA$Three)/(mydataLULA$totalpopl-mydataLULA$total3l))
mydataLULA$dl_15 <- 0.5*abs(mydataLULA$Unemp/mydataLULA$total15l - (mydataLULA$all_ages-mydataLULA$Unemp)/(mydataLULA$totalpopl-mydataLULA$total15l))
mydataLULA$dl_16 <- 0.5*abs(mydataLULA$Students/mydataLULA$total16l - (mydataLULA$all_ages-mydataLULA$Students)/(mydataLULA$totalpopl-mydataLULA$total16l))

#relative to LA

mydataLULA$db_1 <- 0.5*abs(mydataLULA$One/mydataLULA$total1b - (mydataLULA$all_ages-mydataLULA$One)/(mydataLULA$totalpopb-mydataLULA$total1b))
mydataLULA$db_2 <- 0.5*abs(mydataLULA$Two/mydataLULA$total2b - (mydataLULA$all_ages-mydataLULA$Two)/(mydataLULA$totalpopb-mydataLULA$total2b))
mydataLULA$db_3 <- 0.5*abs(mydataLULA$Three/mydataLULA$total3b - (mydataLULA$all_ages-mydataLULA$Three)/(mydataLULA$totalpopb-mydataLULA$total3b))
mydataLULA$db_15 <- 0.5*abs(mydataLULA$Unemp/mydataLULA$total15b - (mydataLULA$all_ages-mydataLULA$Unemp)/(mydataLULA$totalpopb-mydataLULA$total15b))
mydataLULA$db_16 <- 0.5*abs(mydataLULA$Students/mydataLULA$total16b - (mydataLULA$all_ages-mydataLULA$Students)/(mydataLULA$totalpopb-mydataLULA$total16b))

#summarise and print csv files

DISLU <- 
  mydataLULA %>% group_by(year, london) %>% 
  summarise(dls_1 = sum(dl_1, na.rm = TRUE),
            dls_2 = sum(dl_2, na.rm = TRUE),
            dls_3 = sum(dl_3, na.rm = TRUE),
            dls_15 = sum(dl_15, na.rm = TRUE),
            dls_16 = sum(dl_16, na.rm = TRUE))

DISLA <- 
  mydataLULA %>% group_by(year, LA) %>% 
  summarise(dbs_1 = sum(db_1, na.rm = TRUE),
            dbs_2 = sum(db_2, na.rm = TRUE),
            dbs_3 = sum(db_3, na.rm = TRUE),
            dbs_15 = sum(db_15, na.rm = TRUE),
            dbs_16 = sum(db_16, na.rm = TRUE),
            london = mean(london, na.rm = TRUE))

write.csv(DISLU, file="DISNSL.csv", row.names=FALSE)
write.csv(DISLA, file="DISNSLA.csv", row.names=FALSE)

#different code for running AGE analysis

mydata <- read.csv(file="lsoa_ca.csv", header=TRUE, sep=",")

#run the code linking rural urban classification

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#two summations - one at London/other urban level, one at LA level

totalsLU <- 
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

totalsLA <- 
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

mydataLU <- left_join(mydata, totalsLU, by = c("london"))
mydataLULA <- left_join(mydataLU, totalsLA, by = c("LA"))

# calculate LSOA-level dissimilarity, first relative to London then relative to LA

# relative to London

mydataLULA$dl_A <- 0.5*abs(mydataLULA$A/mydataLULA$totalAl - (mydataLULA$all_ages-mydataLULA$A)/(mydataLULA$totalpopl-mydataLULA$totalAl))
mydataLULA$dl_B <- 0.5*abs(mydataLULA$B/mydataLULA$totalBl - (mydataLULA$all_ages-mydataLULA$B)/(mydataLULA$totalpopl-mydataLULA$totalBl))
mydataLULA$dl_C <- 0.5*abs(mydataLULA$C/mydataLULA$totalCl - (mydataLULA$all_ages-mydataLULA$C)/(mydataLULA$totalpopl-mydataLULA$totalCl))
mydataLULA$dl_D <- 0.5*abs(mydataLULA$D/mydataLULA$totalDl - (mydataLULA$all_ages-mydataLULA$D)/(mydataLULA$totalpopl-mydataLULA$totalDl))
mydataLULA$dl_E <- 0.5*abs(mydataLULA$E/mydataLULA$totalEl - (mydataLULA$all_ages-mydataLULA$E)/(mydataLULA$totalpopl-mydataLULA$totalEl))
mydataLULA$dl_F <- 0.5*abs(mydataLULA$F/mydataLULA$totalFl - (mydataLULA$all_ages-mydataLULA$F)/(mydataLULA$totalpopl-mydataLULA$totalFl))
mydataLULA$dl_G <- 0.5*abs(mydataLULA$G/mydataLULA$totalGl - (mydataLULA$all_ages-mydataLULA$G)/(mydataLULA$totalpopl-mydataLULA$totalGl))
mydataLULA$dl_H <- 0.5*abs(mydataLULA$H/mydataLULA$totalHl - (mydataLULA$all_ages-mydataLULA$H)/(mydataLULA$totalpopl-mydataLULA$totalHl))
mydataLULA$dl_I <- 0.5*abs(mydataLULA$I/mydataLULA$totalIl - (mydataLULA$all_ages-mydataLULA$I)/(mydataLULA$totalpopl-mydataLULA$totalIl))
mydataLULA$dl_J <- 0.5*abs(mydataLULA$J/mydataLULA$totalJl - (mydataLULA$all_ages-mydataLULA$J)/(mydataLULA$totalpopl-mydataLULA$totalJl))

#relative to LA

mydataLULA$db_A <- 0.5*abs(mydataLULA$A/mydataLULA$totalAb - (mydataLULA$all_ages-mydataLULA$A)/(mydataLULA$totalpopb-mydataLULA$totalAb))
mydataLULA$db_B <- 0.5*abs(mydataLULA$B/mydataLULA$totalBb - (mydataLULA$all_ages-mydataLULA$B)/(mydataLULA$totalpopb-mydataLULA$totalBb))
mydataLULA$db_C <- 0.5*abs(mydataLULA$C/mydataLULA$totalCb - (mydataLULA$all_ages-mydataLULA$C)/(mydataLULA$totalpopb-mydataLULA$totalCb))
mydataLULA$db_D <- 0.5*abs(mydataLULA$D/mydataLULA$totalDb - (mydataLULA$all_ages-mydataLULA$D)/(mydataLULA$totalpopb-mydataLULA$totalDb))
mydataLULA$db_E <- 0.5*abs(mydataLULA$E/mydataLULA$totalEb - (mydataLULA$all_ages-mydataLULA$E)/(mydataLULA$totalpopb-mydataLULA$totalEb))
mydataLULA$db_F <- 0.5*abs(mydataLULA$F/mydataLULA$totalFb - (mydataLULA$all_ages-mydataLULA$F)/(mydataLULA$totalpopb-mydataLULA$totalFb))
mydataLULA$db_G <- 0.5*abs(mydataLULA$G/mydataLULA$totalGb - (mydataLULA$all_ages-mydataLULA$G)/(mydataLULA$totalpopb-mydataLULA$totalGb))
mydataLULA$db_H <- 0.5*abs(mydataLULA$H/mydataLULA$totalHb - (mydataLULA$all_ages-mydataLULA$H)/(mydataLULA$totalpopb-mydataLULA$totalHb))
mydataLULA$db_I <- 0.5*abs(mydataLULA$I/mydataLULA$totalIb - (mydataLULA$all_ages-mydataLULA$I)/(mydataLULA$totalpopb-mydataLULA$totalIb))
mydataLULA$db_J <- 0.5*abs(mydataLULA$J/mydataLULA$totalJb - (mydataLULA$all_ages-mydataLULA$J)/(mydataLULA$totalpopb-mydataLULA$totalJb))

#summarise and print csv files

DISLU <- 
  mydataLULA %>% group_by(london) %>% 
  summarise(dls_A = sum(dl_A, na.rm = TRUE),
            dls_B = sum(dl_B, na.rm = TRUE),
            dls_C = sum(dl_C, na.rm = TRUE),
            dls_D = sum(dl_D, na.rm = TRUE),
            dls_E = sum(dl_E, na.rm = TRUE),
            dls_F = sum(dl_F, na.rm = TRUE),
            dls_G = sum(dl_G, na.rm = TRUE),
            dls_H = sum(dl_H, na.rm = TRUE),
            dls_I = sum(dl_I, na.rm = TRUE),
            dls_J = sum(dl_J, na.rm = TRUE))
            

DISLA <- 
  mydataLULA %>% group_by(LA) %>% 
  summarise(dbs_A = sum(db_A, na.rm = TRUE),
            dbs_B = sum(db_B, na.rm = TRUE),
            dbs_C = sum(db_C, na.rm = TRUE),
            dbs_D = sum(db_D, na.rm = TRUE),
            dbs_E = sum(db_E, na.rm = TRUE),
            dbs_F = sum(db_F, na.rm = TRUE),
            dbs_G = sum(db_G, na.rm = TRUE),
            dbs_H = sum(db_H, na.rm = TRUE),
            dbs_I = sum(db_I, na.rm = TRUE),
            dbs_J = sum(db_J, na.rm = TRUE))

write.csv(DISLU, file="DISAL.csv", row.names=FALSE)
write.csv(DISLA, file="DISALA.csv", row.names=FALSE)

#different code for running QUAL analysis

mydata <- read.csv(file="lsoa_cq.csv", header=TRUE, sep=",")

#run the code linking rural urban classification

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#two summations - one at London/other urban level, one at LA level

totalsLU <- 
  mydata %>% group_by(london) %>% 
  summarise(totalpopl = sum(all_ages, na.rm = TRUE),
            totalAl = sum(A, na.rm = TRUE),
            totalBl = sum(B, na.rm = TRUE),
            totalCl = sum(C, na.rm = TRUE),
            totalDl = sum(D, na.rm = TRUE),
            totalEl = sum(E, na.rm = TRUE),
            totalFl = sum(F, na.rm = TRUE),
            totalGl = sum(G, na.rm = TRUE))

totalsLA <- 
  mydata %>% group_by(LA) %>% 
  summarise(totalpopb = sum(all_ages, na.rm = TRUE),
            totalAb = sum(A, na.rm = TRUE),
            totalBb = sum(B, na.rm = TRUE),
            totalCb = sum(C, na.rm = TRUE),
            totalDb = sum(D, na.rm = TRUE),
            totalEb = sum(E, na.rm = TRUE),
            totalFb = sum(F, na.rm = TRUE),
            totalGb = sum(G, na.rm = TRUE)) 

mydataLU <- left_join(mydata, totalsLU, by = c("london"))
mydataLULA <- left_join(mydataLU, totalsLA, by = c("LA"))

# calculate LSOA-level dissimilarity, first relative to London then relative to LA

# relative to London

mydataLULA$dl_A <- 0.5*abs(mydataLULA$A/mydataLULA$totalAl - (mydataLULA$all_ages-mydataLULA$A)/(mydataLULA$totalpopl-mydataLULA$totalAl))
mydataLULA$dl_B <- 0.5*abs(mydataLULA$B/mydataLULA$totalBl - (mydataLULA$all_ages-mydataLULA$B)/(mydataLULA$totalpopl-mydataLULA$totalBl))
mydataLULA$dl_C <- 0.5*abs(mydataLULA$C/mydataLULA$totalCl - (mydataLULA$all_ages-mydataLULA$C)/(mydataLULA$totalpopl-mydataLULA$totalCl))
mydataLULA$dl_D <- 0.5*abs(mydataLULA$D/mydataLULA$totalDl - (mydataLULA$all_ages-mydataLULA$D)/(mydataLULA$totalpopl-mydataLULA$totalDl))
mydataLULA$dl_E <- 0.5*abs(mydataLULA$E/mydataLULA$totalEl - (mydataLULA$all_ages-mydataLULA$E)/(mydataLULA$totalpopl-mydataLULA$totalEl))
mydataLULA$dl_F <- 0.5*abs(mydataLULA$F/mydataLULA$totalFl - (mydataLULA$all_ages-mydataLULA$F)/(mydataLULA$totalpopl-mydataLULA$totalFl))
mydataLULA$dl_G <- 0.5*abs(mydataLULA$G/mydataLULA$totalGl - (mydataLULA$all_ages-mydataLULA$G)/(mydataLULA$totalpopl-mydataLULA$totalGl))

#relative to LA

mydataLULA$db_A <- 0.5*abs(mydataLULA$A/mydataLULA$totalAb - (mydataLULA$all_ages-mydataLULA$A)/(mydataLULA$totalpopb-mydataLULA$totalAb))
mydataLULA$db_B <- 0.5*abs(mydataLULA$B/mydataLULA$totalBb - (mydataLULA$all_ages-mydataLULA$B)/(mydataLULA$totalpopb-mydataLULA$totalBb))
mydataLULA$db_C <- 0.5*abs(mydataLULA$C/mydataLULA$totalCb - (mydataLULA$all_ages-mydataLULA$C)/(mydataLULA$totalpopb-mydataLULA$totalCb))
mydataLULA$db_D <- 0.5*abs(mydataLULA$D/mydataLULA$totalDb - (mydataLULA$all_ages-mydataLULA$D)/(mydataLULA$totalpopb-mydataLULA$totalDb))
mydataLULA$db_E <- 0.5*abs(mydataLULA$E/mydataLULA$totalEb - (mydataLULA$all_ages-mydataLULA$E)/(mydataLULA$totalpopb-mydataLULA$totalEb))
mydataLULA$db_F <- 0.5*abs(mydataLULA$F/mydataLULA$totalFb - (mydataLULA$all_ages-mydataLULA$F)/(mydataLULA$totalpopb-mydataLULA$totalFb))
mydataLULA$db_G <- 0.5*abs(mydataLULA$G/mydataLULA$totalGb - (mydataLULA$all_ages-mydataLULA$G)/(mydataLULA$totalpopb-mydataLULA$totalGb))

#summarise and print csv files

DISLU <- 
  mydataLULA %>% group_by(london) %>% 
  summarise(dls_A = sum(dl_A, na.rm = TRUE),
            dls_B = sum(dl_B, na.rm = TRUE),
            dls_C = sum(dl_C, na.rm = TRUE),
            dls_D = sum(dl_D, na.rm = TRUE),
            dls_E = sum(dl_E, na.rm = TRUE),
            dls_F = sum(dl_F, na.rm = TRUE),
            dls_G = sum(dl_G, na.rm = TRUE))


DISLA <- 
  mydataLULA %>% group_by(LA) %>% 
  summarise(dbs_A = sum(db_A, na.rm = TRUE),
            dbs_B = sum(db_B, na.rm = TRUE),
            dbs_C = sum(db_C, na.rm = TRUE),
            dbs_D = sum(db_D, na.rm = TRUE),
            dbs_E = sum(db_E, na.rm = TRUE),
            dbs_F = sum(db_F, na.rm = TRUE),
            dbs_G = sum(db_G, na.rm = TRUE))

write.csv(DISLU, file="DISQL.csv", row.names=FALSE)
write.csv(DISLA, file="DISQLA.csv", row.names=FALSE)

#different code for running COB analysis

mydata <- read.csv(file="lsoa_cc.csv", header=TRUE, sep=",")

#run the code linking rural urban classification

london <- read.csv(file="london.csv", header=TRUE, sep=",")
names(london)[1] <-"londont"
mydata <- left_join(mydata, london, by = "LA")

mydata$london <- NA
mydata$london[mydata$londont==1] <- 1
mydata$london[mydata$londont==0 & mydata$RUC11CD>4] <- 2
mydata$london[mydata$londont==0 & mydata$RUC11CD<5] <- 3

#two summations - one at London/other urban level, one at LA level

totalsLU <- 
  mydata %>% group_by(london) %>% 
  summarise(totalpopl = sum(all_ages, na.rm = TRUE),
            totalAl = sum(A, na.rm = TRUE),
            totalBl = sum(B, na.rm = TRUE),
            totalCl = sum(C, na.rm = TRUE),
            totalDl = sum(D, na.rm = TRUE))

totalsLA <- 
  mydata %>% group_by(LA) %>% 
  summarise(totalpopb = sum(all_ages, na.rm = TRUE),
            totalAb = sum(A, na.rm = TRUE),
            totalBb = sum(B, na.rm = TRUE),
            totalCb = sum(C, na.rm = TRUE),
            totalDb = sum(D, na.rm = TRUE)) 

mydataLU <- left_join(mydata, totalsLU, by = c("london"))
mydataLULA <- left_join(mydataLU, totalsLA, by = c("LA"))

# calculate LSOA-level dissimilarity, first relative to London then relative to LA

# relative to London

mydataLULA$dl_A <- 0.5*abs(mydataLULA$A/mydataLULA$totalAl - (mydataLULA$all_ages-mydataLULA$A)/(mydataLULA$totalpopl-mydataLULA$totalAl))
mydataLULA$dl_B <- 0.5*abs(mydataLULA$B/mydataLULA$totalBl - (mydataLULA$all_ages-mydataLULA$B)/(mydataLULA$totalpopl-mydataLULA$totalBl))
mydataLULA$dl_C <- 0.5*abs(mydataLULA$C/mydataLULA$totalCl - (mydataLULA$all_ages-mydataLULA$C)/(mydataLULA$totalpopl-mydataLULA$totalCl))
mydataLULA$dl_D <- 0.5*abs(mydataLULA$D/mydataLULA$totalDl - (mydataLULA$all_ages-mydataLULA$D)/(mydataLULA$totalpopl-mydataLULA$totalDl))

#relative to LA

mydataLULA$db_A <- 0.5*abs(mydataLULA$A/mydataLULA$totalAb - (mydataLULA$all_ages-mydataLULA$A)/(mydataLULA$totalpopb-mydataLULA$totalAb))
mydataLULA$db_B <- 0.5*abs(mydataLULA$B/mydataLULA$totalBb - (mydataLULA$all_ages-mydataLULA$B)/(mydataLULA$totalpopb-mydataLULA$totalBb))
mydataLULA$db_C <- 0.5*abs(mydataLULA$C/mydataLULA$totalCb - (mydataLULA$all_ages-mydataLULA$C)/(mydataLULA$totalpopb-mydataLULA$totalCb))
mydataLULA$db_D <- 0.5*abs(mydataLULA$D/mydataLULA$totalDb - (mydataLULA$all_ages-mydataLULA$D)/(mydataLULA$totalpopb-mydataLULA$totalDb))

#summarise and print csv files

DISLU <- 
  mydataLULA %>% group_by(london) %>% 
  summarise(dls_A = sum(dl_A, na.rm = TRUE),
            dls_B = sum(dl_B, na.rm = TRUE),
            dls_C = sum(dl_C, na.rm = TRUE),
            dls_D = sum(dl_D, na.rm = TRUE))


DISLA <- 
  mydataLULA %>% group_by(LA) %>% 
  summarise(dbs_A = sum(db_A, na.rm = TRUE),
            dbs_B = sum(db_B, na.rm = TRUE),
            dbs_C = sum(db_C, na.rm = TRUE),
            dbs_D = sum(db_D, na.rm = TRUE))

write.csv(DISLU, file="DISCL.csv", row.names=FALSE)
write.csv(DISLA, file="DISCLA.csv", row.names=FALSE)