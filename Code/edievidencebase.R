setwd(Data)

myvars <- c("NTNLTY12", "ILODEFR", "AAGE", "PWTA17C", "SEX", "ETHEWEUL", "GOVTOF", "LIMACT", "HIQUL15D", "DISCURR13", "DISEA", "RELIGE", "STATR", "FTPTW","STUCUR", "NSECMJ10", "NSECM10", "AGE", "LNGLST", "LIMITK", "LIMITA")
aps <- read.dta13("aps_3yr_jan15dec17_eul.dta", select.cols = myvars, convert.factors = F)

#Introduction figures

#Disability

aps$wlimit <- 0
aps$wlimit[aps$AGE<16|aps$LNGLST>2] <- -9
aps$wlimit[aps$LNGLST==1 & (aps$LIMITK==1|aps$LIMITA==1)] <- 1

aps$disabled <- 0
aps$disabled[aps$wlimit==1|aps$DISEA==1] <- 1
aps$disabled[aps$wlimit<0 & aps$DISEA<0] <- -9

aps$age1 <- 0
aps$age1[aps$AAGE>1 & aps$AAGE<5] <- 1
aps$age1[aps$AAGE>4 & aps$AAGE<10] <- 2
aps$age1[aps$AAGE>9 & aps$AAGE<13] <- 3

apsI1 <- aps[ which(aps$GOVTOF==8), ]

TI1 <-
  apsI1 %>% group_by(age1, disabled) %>% 
  summarise(number = sum(PWTA17C))

#Religion

apsI2 <- aps[ which(aps$GOVTOF==8), ]

TI2 <-
  apsI2 %>% group_by(RELIGE) %>% 
  summarise(number = sum(PWTA17C))

#NS-SEC

apsI2 <- aps[ which(aps$GOVTOF==8), ]

TI2 <-
  apsI2 %>% group_by(NSECMJ10) %>% 
  summarise(number = sum(PWTA17C))

TI3 <-
  apsI2 %>% group_by(NSECM10) %>% 
  summarise(number = sum(PWTA17C))

#Appendix tables 1 - religion by age and by ethnicity

aps$wlimit <- 0
aps$wlimit[aps$AGE<16|aps$LNGLST>2] <- -9
aps$wlimit[aps$LNGLST==1 & (aps$LIMITK==1|aps$LIMITA==1)] <- 1

aps$disabled <- 0
aps$disabled[aps$wlimit==1|aps$DISEA==1] <- 1
aps$disabled[aps$wlimit<0 & aps$DISEA<0] <- -9

aps$ageA <- 0
aps$ageA[aps$AAGE==1] <- 1
aps$ageA[aps$AAGE>1 & aps$AAGE<13] <- 2
aps$ageA[aps$AAGE==13] <- 3

aps$white <- -1
aps$white[aps$ETHEWEUL>0 & aps$ETHEWEUL<4] <- 1
aps$white[aps$ETHEWEUL>3 & aps$ETHEWEUL<17] <- 0

aps$british <- -1
aps$british[aps$NTNLTY12==926] <- 1
aps$british[aps$NTNLTY12==372|aps$NTNLTY12==356|aps$NTNLTY12==586|aps$NTNLTY12==616|aps$NTNLTY12==997] <- 0

apsA <- aps[ which(aps$GOVTOF==8), ]

#Religion

TA1 <-
  apsA %>% group_by(RELIGE, ageA, white) %>% 
  summarise(number = sum(PWTA17C))

TA2 <-
  apsA %>% group_by(disabled, ageA, white) %>% 
  summarise(number = sum(PWTA17C))

TA3 <-
  apsA %>% group_by(british, ageA, white) %>% 
  summarise(number = sum(PWTA17C))

TA4 <-
  apsA %>% group_by(NSECM10, ageA, white) %>% 
  summarise(number = sum(PWTA17C))

#Figure 3.1

aps$age1 <- 0
aps$age1[aps$AAGE>1 & aps$AAGE<5] <- 1
aps$age1[aps$AAGE>4 & aps$AAGE<10] <- 2
aps$age1[aps$AAGE>9 & aps$AAGE<13] <- 3

apsT1 <- aps[ which(aps$GOVTOF==8), ]

Tp1 <-
  apsT1 %>% group_by(age1, HIQUL15D) %>% 
  summarise(number = sum(PWTA17C))

#Figure 3.2

aps$degr <- 0
aps$degr[aps$HIQUL15D==1] <- 1


apsT2 <- aps[ which(aps$GOVTOF==8), ]

Tp2 <-
  apsT2 %>% group_by(age1, SEX, degr) %>% 
  summarise(number = sum(PWTA17C))

#Figure 3.3

aps$no <- 0
aps$no[aps$HIQUL15D==6] <- 1
aps$no[aps$HIQUL15D==7] <- -1

apsT3 <- aps[ which(aps$GOVTOF==8), ]

Tp3 <-
  apsT3 %>% group_by(age1, SEX, no) %>% 
  summarise(number = sum(PWTA17C))

#Figure 3.4

aps$qual <- 0
aps$qual[aps$HIQUL15D==1|aps$HIQUL15D==2] <- 1
aps$qual[aps$HIQUL15D==3|aps$HIQUL15D==4] <- 2
aps$qual[aps$HIQUL15D==5] <- 3
aps$qual[aps$HIQUL15D==6] <- 4

aps$eth <- 0
aps$eth[aps$ETHEWEUL==1|aps$ETHEWEUL==2|aps$ETHEWEUL==3] <- 1
aps$eth[aps$ETHEWEUL==4|aps$ETHEWEUL==5|aps$ETHEWEUL==6|aps$ETHEWEUL==7] <- 2
aps$eth[aps$ETHEWEUL==8|aps$ETHEWEUL==9|aps$ETHEWEUL==10|aps$ETHEWEUL==11|aps$ETHEWEUL==12] <- 3
aps$eth[aps$ETHEWEUL==13|aps$ETHEWEUL==14|aps$ETHEWEUL==15] <- 4
aps$eth[aps$ETHEWEUL==16] <- 5

apsT4 <- aps[ which(aps$GOVTOF==8), ]

Tp4 <-
  apsT4 %>% group_by(age1, eth, qual) %>% 
  summarise(number = sum(PWTA17C))

#Figure 3.5

Tp5 <-
  apsT4 %>% group_by(age1, RELIGE, qual) %>% 
  summarise(number = sum(PWTA17C))

#Figure 3.9

aps$age2 <- 0
aps$age2[aps$AAGE>1 & aps$AAGE<5] <- 1
aps$age2[aps$AAGE>4 & aps$AAGE<7] <- 2
aps$age2[aps$AAGE>6 & aps$AAGE<10] <- 3
aps$age2[aps$AAGE>9 & aps$AAGE<13] <- 4
aps$age2[aps$AAGE==13] <- 5

apsT9 <- aps[ which(aps$GOVTOF==8), ]

Tp9 <-
  apsT9 %>% group_by(age2, SEX, ILODEFR) %>% 
  summarise(number = sum(PWTA17C))

#Figures 3.11, 3.12, 3.13

aps$eth2 <- 0
aps$eth2[aps$ETHEWEUL==1|aps$ETHEWEUL==2|aps$ETHEWEUL==3] <- 1
aps$eth2[aps$ETHEWEUL==4|aps$ETHEWEUL==5|aps$ETHEWEUL==6|aps$ETHEWEUL==7] <- 2
aps$eth2[aps$ETHEWEUL==8] <- 3
aps$eth2[aps$ETHEWEUL==9|aps$ETHEWEUL==10] <- 4
aps$eth2[aps$ETHEWEUL==11|aps$ETHEWEUL==12] <- 5
aps$eth2[aps$ETHEWEUL==13|aps$ETHEWEUL==14|aps$ETHEWEUL==15] <- 6
aps$eth2[aps$ETHEWEUL==16] <- 7

apsT11 <- aps[ which(aps$GOVTOF==8 & aps$STUCUR==2), ]

Tp11 <-
  apsT11 %>% group_by(age1, eth2, SEX, ILODEFR) %>% 
  summarise(number = sum(PWTA17C))

#Figure 3.17

apsT17 <- aps[ which(aps$GOVTOF==8 & aps$ILODEFR==1), ]

Tp17A <-
  apsT17 %>% group_by(age2, STATR, FTPTW) %>% 
  summarise(number = sum(PWTA17C))

Tp17B <-
  apsT17 %>% group_by(eth2, STATR, FTPTW) %>% 
  summarise(number = sum(PWTA17C))

Tp17C <-
  apsT17 %>% group_by(RELIGE, STATR, FTPTW) %>% 
  summarise(number = sum(PWTA17C))

Tp17D <-
  apsT17 %>% group_by(SEX, STATR, FTPTW) %>% 
  summarise(number = sum(PWTA17C))

Tp17E <-
  apsT17 %>% group_by(DISCURR13, STATR, FTPTW) %>% 
  summarise(number = sum(PWTA17C))

#Output

setwd(Output)

write.csv(Tp1, file="Fig31.csv", row.names=FALSE)
write.csv(Tp2, file="Fig32.csv", row.names=FALSE)
write.csv(Tp3, file="Fig33.csv", row.names=FALSE)
write.csv(Tp4, file="Fig34.csv", row.names=FALSE)
write.csv(Tp5, file="Fig35.csv", row.names=FALSE)
write.csv(Tp9, file="Fig39.csv", row.names=FALSE)
write.csv(Tp11, file="Fig311.csv", row.names=FALSE)

write.csv(Tp17A, file="Fig17a.csv", row.names=FALSE)
write.csv(Tp17B, file="Fig17b.csv", row.names=FALSE)
write.csv(Tp17C, file="Fig17c.csv", row.names=FALSE)
write.csv(Tp17D, file="Fig17d.csv", row.names=FALSE)
write.csv(Tp17E, file="Fig17e.csv", row.names=FALSE)