#EDIeb_1a: Housing data

setwd(Data)

#Notes: Uses Census, HBAI and EHS data to report on a variety of EDI measures

#1_1_1: Tenure data from Census

myvars <- c("measure_category", "characteristic", "characteristic_detail", "valueT")

#DC3408EW - Tenure x Gender

tenure_gender <- read.table(file = '1_1_1_sex.tsv', sep = '\t', header = TRUE)

tenure_genderT <-
  tenure_gender %>% group_by(Sex) %>% 
  summarise(number = sum(value))

tenure_gender <- left_join(tenure_gender, tenure_genderT, by="Sex")

tenure_gender$valueT <- tenure_gender$value/tenure_gender$number

tenure_gender$characteristic <- "Gender"

tenure_gender <- rename.variable(tenure_gender, "Sex", "characteristic_detail")
tenure_gender <- rename.variable(tenure_gender, "Tenure", "measure_category")
tenure_gender <- tenure_gender[myvars]

#DC3408EW - Tenure x Age

tenure_age <- read.table(file = '1_1_1_age.tsv', sep = '\t', header = TRUE)

tenure_ageT <-
  tenure_age %>% group_by(Age) %>% 
  summarise(number = sum(value))

tenure_age <- left_join(tenure_age, tenure_ageT, by="Age")

tenure_age$valueT <- tenure_age$value/tenure_age$number

tenure_age$characteristic <- "Age"

tenure_age <- rename.variable(tenure_age, "Age", "characteristic_detail")
tenure_age <- rename.variable(tenure_age, "Tenure", "measure_category")
tenure_age <- tenure_age[myvars]

#DC3408EW - Tenure x Disability

tenure_disability <- read.table(file = '1_1_1_disability.tsv', sep = '\t', header = TRUE)

tenure_disabilityT <-
  tenure_disability %>% group_by(Disability) %>% 
  summarise(number = sum(value))

tenure_disability <- left_join(tenure_disability, tenure_disabilityT, by="Disability")

tenure_disability$valueT <- tenure_disability$value/tenure_disability$number

tenure_disability$characteristic <- "Disability"

tenure_disability <- rename.variable(tenure_disability, "Disability", "characteristic_detail")
tenure_disability <- rename.variable(tenure_disability, "Tenure", "measure_category")
tenure_disability <- tenure_disability[myvars]

#LC4204EW - Tenure x Religion (HRP)

tenure_religion <- read.table(file = '1_1_1_religion.tsv', sep = '\t', header = TRUE)

tenure_religionT <-
  tenure_religion %>% group_by(Religion) %>% 
  summarise(number = sum(value))

tenure_religion <- left_join(tenure_religion, tenure_religionT, by="Religion")

tenure_religion$valueT <- tenure_religion$value/tenure_religion$number

tenure_religion$characteristic <- "Religion"

tenure_religion <- rename.variable(tenure_religion, "Religion", "characteristic_detail")
tenure_religion <- rename.variable(tenure_religion, "Tenure", "measure_category")
tenure_religion <- tenure_religion[myvars]

#DC4201EW - Tenure x Ethnicity (HRP)

tenure_ethnicity <- read.table(file = '1_1_1_ethnicity.tsv', sep = '\t', header = TRUE)

tenure_ethnicityT <-
  tenure_ethnicity %>% group_by(Ethnic.Group) %>% 
  summarise(number = sum(value))

tenure_ethnicity <- left_join(tenure_ethnicity, tenure_ethnicityT, by="Ethnic.Group")

tenure_ethnicity$valueT <- tenure_ethnicity$value/tenure_ethnicity$number

tenure_ethnicity$characteristic <- "Ethnicity"

tenure_ethnicity <- rename.variable(tenure_ethnicity, "Ethnic.Group", "characteristic_detail")
tenure_ethnicity <- rename.variable(tenure_ethnicity, "Tenure", "measure_category")
tenure_ethnicity <- tenure_ethnicity[myvars]

#DC4605EW - Tenure x NS-SEC (HRP)
  
tenure_nssec <- read.table(file = '1_1_1_nssec.tsv', sep = '\t', header = TRUE)

tenure_nssecT <-
  tenure_nssec %>% group_by(NS.SeC) %>% 
  summarise(number = sum(value))

tenure_nssec <- left_join(tenure_nssec, tenure_nssecT, by="NS.SeC")

tenure_nssec$valueT <- tenure_nssec$value/tenure_nssec$number

tenure_nssec$characteristic <- "NSSEC"

tenure_nssec <- rename.variable(tenure_nssec, "NS.SeC", "characteristic_detail")
tenure_nssec <- rename.variable(tenure_nssec, "Tenure", "measure_category")
tenure_nssec <- tenure_nssec[myvars]

#Append files and add common fields

tenure <- rbind(tenure_gender, tenure_age, tenure_disability, tenure_ethnicity, tenure_religion, tenure_nssec)
tenure$code <- "1_1_1"
tenure$measure <- "Tenure"
tenure$source <- "Census 2011"
tenure$units <- "Share of population"
tenure$date <- 2011
tenure$geography <- "London"

#1_1_2: Satisfaction with tenure data is only on special licence EHS - check with Housing team

#1_1_3: Housing affordability data from HBAI

hbai1718 <- spss.get("h1718.sav", lowernames=TRUE, use.value.labels=FALSE, to.data.frame = TRUE)
hbai1617 <- spss.get("hbai1617_g4.sav", lowernames=TRUE, use.value.labels=FALSE, to.data.frame = TRUE)
hbai1516 <- spss.get("hbai1516_g4.sav", lowernames=TRUE, use.value.labels=FALSE, to.data.frame = TRUE)

myvars <- c("s.oe.bhc", "s.oe.ahc",  "gs.newpp", "obhcdec", "ptentyp2", "ethgrphh", "gvtregn")

hbai1718 <- hbai1718[myvars]
hbai1617 <- hbai1617[myvars]
hbai1516 <- hbai1516[myvars]

hbai <- rbind(hbai1516, hbai1617, hbai1718)
hbai <- hbai[ which(hbai$gvtregn==8), ]

hbai$inconhouse <- hbai$s.oe.bhc - hbai$s.oe.ahc
hbai$inconhousePC <- hbai$inconhouse/hbai$s.oe.bhc

hbai$incflag <- "A third of income or less spent on housing"
hbai$incflag[hbai$s.oe.bhc==0 & hbai$inconhouse>0] <- "More than a third of income spent on housing"
hbai$incflag[hbai$inconhousePC>(1/3)] <- "More than a third of income spent on housing"

#ethgrphh

hbai$ethnicity [hbai$ethgrphh==1] <- "White British"
hbai$ethnicity [hbai$ethgrphh>1 & hbai$ethgrphh<5] <- "White (Other)"
hbai$ethnicity [hbai$ethgrphh>4 & hbai$ethgrphh<10] <- "Mixed"
hbai$ethnicity [hbai$ethgrphh==10] <- "Indian"
hbai$ethnicity [hbai$ethgrphh==11|hbai$ethgrphh==12] <- "Pakistani/Bangladeshi"
hbai$ethnicity [hbai$ethgrphh>14 & hbai$ethgrphh<23] <- "Black"
hbai$ethnicity [hbai$ethgrphh>22 & hbai$ethgrphh<25] <- "Other"
hbai$ethnicity [hbai$ethgrphh==3] <- "Other"
hbai$ethnicity [hbai$ethgrphh>12 & hbai$ethgrphh<15] <- "Other"

hbai_eth1 <-
  hbai %>% group_by(ethnicity) %>% 
  summarise(number1 = sum(gs.newpp))

hbai_eth2 <-
  hbai %>% group_by(ethnicity, incflag) %>% 
  summarise(number2 = sum(gs.newpp))

hbai_eth <- left_join(hbai_eth2, hbai_eth1, by="ethnicity")

hbai_eth$value <- hbai_eth$number2/hbai_eth$number1

hbai_eth$characteristic <- "Ethnicity"
hbai_eth <- rename.variable(hbai_eth, "ethnicity", "characteristic_detail")
hbai_eth <- rename.variable(hbai_eth, "incflag", "measure_category")

#tenure type

hbai$tenure[hbai$ptentyp2==1|hbai$ptentyp2==2] <- "Social renters"
hbai$tenure[hbai$ptentyp2==3|hbai$ptentyp2==4] <- "Private renters"
hbai$tenure[hbai$ptentyp2==5|hbai$ptentyp2==6] <- "Owner-occupiers"

hbai_ten1 <-
  hbai %>% group_by(tenure) %>% 
  summarise(number1 = sum(gs.newpp))

hbai_ten2 <-
  hbai %>% group_by(tenure, incflag) %>% 
  summarise(number2 = sum(gs.newpp))

hbai_ten <- left_join(hbai_ten2, hbai_ten1, by="tenure")

hbai_ten$value <- hbai_ten$number2/hbai_ten$number1

hbai_ten$characteristic <- "Tenure"
hbai_ten <- rename.variable(hbai_ten, "tenure", "characteristic_detail")
hbai_ten <- rename.variable(hbai_ten, "incflag", "measure_category")

#income quintile

hbai$income[hbai$obhcdec==1|hbai$obhcdec==2] <- "Bottom income quintile"
hbai$income[hbai$obhcdec==3|hbai$obhcdec==4] <- "2nd income quintile"
hbai$income[hbai$obhcdec==5|hbai$obhcdec==6] <- "3rd income quintile"
hbai$income[hbai$obhcdec==7|hbai$obhcdec==8] <- "4th income quintile"
hbai$income[hbai$obhcdec==9|hbai$obhcdec==10] <- "Top income quintile"

hbai_inc1 <-
  hbai %>% group_by(income) %>% 
  summarise(number1 = sum(gs.newpp))

hbai_inc2 <-
  hbai %>% group_by(income, incflag) %>% 
  summarise(number2 = sum(gs.newpp))

hbai_inc <- left_join(hbai_inc2, hbai_inc1, by="income")

hbai_inc$value <- hbai_inc$number2/hbai_inc$number1

hbai_inc$characteristic <- "Income quintile"
hbai_inc <- rename.variable(hbai_inc, "income", "characteristic_detail")
hbai_inc <- rename.variable(hbai_inc, "incflag", "measure_category")

#Append files and add common fields

hbai_inc <- ungroup(hbai_inc)
hbai_eth <- ungroup(hbai_eth)
hbai_ten <- ungroup(hbai_ten)

affordability <- rbind(hbai_inc, hbai_eth, hbai_ten)
affordability$code <- "1_1_3"
affordability$measure <- "Housing affordability"
affordability$source <- "HBAI"
affordability$units <- "Share of population"
affordability$date <- "2015/16-2017/18"
affordability$geography <- "London"

#1_1_4: Overcrowded houssehold data from EHS



#Append files and add common fields

tenure <- rbind(overcrowded_disability, overcrowded_ethnicity, overcrowded_religion, overcrowded_tenure)
tenure$code <- "1_1_4"
tenure$measure <- "Overcrowded households"
tenure$source <- "Census 2011"
tenure$units <- "Share of population"
tenure$date <- 2011
tenure$geography <- "London"

#1_1_5: Non-decent homes data from EHS physical survey

ehs1g <- spss.get("EHSstock1617general.sav", lowernames=TRUE, use.value.labels=TRUE, to.data.frame = TRUE)
ehs1i <- spss.get("EHSstock1617interview.sav", lowernames=TRUE, use.value.labels=TRUE, to.data.frame = TRUE)
ehs1p <- spss.get("EHSstock1617physical.sav", lowernames=TRUE, use.value.labels=TRUE, to.data.frame = TRUE)

ehs2g <- spss.get("EHSstock1415general.sav", lowernames=TRUE, use.value.labels=TRUE, to.data.frame = TRUE)
ehs2i <- spss.get("EHSstock1415interview.sav", lowernames=TRUE, use.value.labels=TRUE, to.data.frame = TRUE)
ehs2p <- spss.get("EHSstock1415physical.sav", lowernames=TRUE, use.value.labels=TRUE, to.data.frame = TRUE)

ehs1 <- left_join(ehs1g, ehs1i, by="serialanon")  
ehs1 <- left_join(ehs1, ehs1p, by="serialanon")
  
ehs2 <- left_join(ehs2g, ehs2i, by="serialanon")  
ehs2 <- left_join(ehs2, ehs2p, by="serialanon")

ehs1 <- ehs1[ which(ehs1$gorehs=="London"), ]
ehs2 <- ehs2[ which(ehs2$gorehs=="London"), ]

ehs2 <- rename.variable(ehs2, "aagpd1415", "aagpd1617")

#agehrp4x

myvars <- c("aagpd1617", "agehrp4x", "dhomesz")
ehs1_age <- ehs1[myvars]
ehs2_age <- ehs2[myvars]
ehs_age <- rbind(ehs1_age, ehs2_age)

decent_agehrp1 <-
  ehs_age %>% group_by(agehrp4x) %>% 
  summarise(number1 = sum(aagpd1617))

decent_agehrp2 <-
  ehs_age %>% group_by(agehrp4x, dhomesz) %>% 
  summarise(number2 = sum(aagpd1617))

decent_agehrp <- left_join(decent_agehrp2, decent_agehrp1, by="agehrp4x")

decent_agehrp$value <- decent_agehrp$number2/decent_agehrp$number1

decent_agehrp$characteristic <- "Age"
decent_agehrp <- rename.variable(decent_agehrp, "agehrp4x", "characteristic_detail")
decent_agehrp <- rename.variable(decent_agehrp, "dhomesz", "measure_category")

#ethhrp2x

myvars <- c("aagpd1617", "ethhrp2x", "dhomesz")
ehs1_eth <- ehs1[myvars]
ehs2_eth <- ehs2[myvars]
ehs_eth <- rbind(ehs1_eth, ehs2_eth)

decent_ethhrp1 <-
  ehs_eth %>% group_by(ethhrp2x) %>% 
  summarise(number1 = sum(aagpd1617))

decent_ethhrp2 <-
  ehs_eth %>% group_by(ethhrp2x, dhomesz) %>% 
  summarise(number2 = sum(aagpd1617))

decent_ethhrp <- left_join(decent_ethhrp2, decent_ethhrp1, by="ethhrp2x")

decent_ethhrp$value <- decent_ethhrp$number2/decent_ethhrp$number1

decent_ethhrp$characteristic <- "Ethnicity"
decent_ethhrp <- rename.variable(decent_ethhrp, "ethhrp2x", "characteristic_detail")
decent_ethhrp <- rename.variable(decent_ethhrp, "dhomesz", "measure_category")

#hpregdis

myvars <- c("aagpd1617", "hpregdis", "dhomesz")
ehs1_dis <- ehs1[myvars]
ehs2_dis <- ehs2[myvars]
ehs_dis <- rbind(ehs1_dis, ehs2_dis)

decent_dishrp1 <-
  ehs_dis %>% group_by(hpregdis) %>% 
  summarise(number1 = sum(aagpd1617))

decent_dishrp2 <-
  ehs_dis %>% group_by(hpregdis, dhomesz) %>% 
  summarise(number2 = sum(aagpd1617))

decent_dishrp <- left_join(decent_dishrp2, decent_dishrp1, by="hpregdis")

decent_dishrp$value <- decent_dishrp$number2/decent_dishrp$number1

decent_dishrp$characteristic <- "Disability"
decent_dishrp <- rename.variable(decent_dishrp, "hpregdis", "characteristic_detail")
decent_dishrp <- rename.variable(decent_dishrp, "dhomesz", "measure_category")

#hhtype6

#nssech9

#hhvulx

#ahcinceqv5

#tenure4x

#Append files and add common fields

energy <- rbind(energy_dishrp, energy_ethhrp, energy_agehrp)
damp$code <- "1_1_5"
damp$measure <- "Decent homes"
damp$source <- "English Housing Survey"
damp$units <- "Share of dwellings"
damp$date <- "2014-2018"
damp$geography <- "London"

#1_1_6: Energy efficiency data from EHS physical survey

library(plyr)

ehs1$epcband <- revalue(ehs1$epceeb12e, c("A/B"="D or above", "C"="D or above", "D"="D or above", "E"="E or below", "F"="E or below", "G"="E or below"))
ehs2$epcband <- revalue(ehs2$epceeb12e, c("A/B"="D or above", "C"="D or above", "D"="D or above", "E"="E or below", "F"="E or below", "G"="E or below"))

#agehrp4x

detach(package:plyr)

myvars <- c("aagpd1617", "agehrp4x", "epcband")
ehs1_age <- ehs1[myvars]
ehs2_age <- ehs2[myvars]
ehs_age <- rbind(ehs1_age, ehs2_age)

ehs_age <- na.omit(ehs_age)

energy_agehrp1 <-
  ehs_age %>% group_by(agehrp4x) %>% 
  summarise(number1 = sum(aagpd1617))

energy_agehrp2 <-
  ehs_age %>% group_by(agehrp4x, epcband) %>% 
  summarise(number2 = sum(aagpd1617))

energy_agehrp <- left_join(energy_agehrp2, energy_agehrp1, by="agehrp4x")

energy_agehrp$value <- energy_agehrp$number2/energy_agehrp$number1

energy_agehrp$characteristic <- "Age"
energy_agehrp <- rename.variable(energy_agehrp, "agehrp4x", "characteristic_detail")
energy_agehrp <- rename.variable(energy_agehrp, "epcband", "measure_category")

#ethhrp2x

detach(package:plyr)

myvars <- c("aagpd1617", "ethhrp2x", "epcband")
ehs1_eth <- ehs1[myvars]
ehs2_eth <- ehs2[myvars]
ehs_eth <- rbind(ehs1_eth, ehs2_eth)

ehs_eth <- na.omit(ehs_eth)

energy_ethhrp1 <-
  ehs_eth %>% group_by(ethhrp2x) %>% 
  summarise(number1 = sum(aagpd1617))

energy_ethhrp2 <-
  ehs_eth %>% group_by(ethhrp2x, epcband) %>% 
  summarise(number2 = sum(aagpd1617))

energy_ethhrp <- left_join(energy_ethhrp2, energy_ethhrp1, by="ethhrp2x")

energy_ethhrp$value <- energy_ethhrp$number2/energy_ethhrp$number1

energy_ethhrp$characteristic <- "Ethnicity"
energy_ethhrp <- rename.variable(energy_ethhrp, "ethhrp2x", "characteristic_detail")
energy_ethhrp <- rename.variable(energy_ethhrp, "epcband", "measure_category")

#hpregdis

myvars <- c("aagpd1617", "hpregdis", "epcband")
ehs1_dis <- ehs1[myvars]
ehs2_dis <- ehs2[myvars]
ehs_dis <- rbind(ehs1_dis, ehs2_dis)

ehs_dis <- na.omit(ehs_dis)

energy_dishrp1 <-
  ehs_dis %>% group_by(hpregdis) %>% 
  summarise(number1 = sum(aagpd1617))

energy_dishrp2 <-
  ehs_dis %>% group_by(hpregdis, epcband) %>% 
  summarise(number2 = sum(aagpd1617))

energy_dishrp <- left_join(energy_dishrp2, energy_dishrp1, by="hpregdis")

energy_dishrp$value <- energy_dishrp$number2/energy_dishrp$number1

energy_dishrp$characteristic <- "Disability"
energy_dishrp <- rename.variable(energy_dishrp, "hpregdis", "characteristic_detail")
energy_dishrp <- rename.variable(energy_dishrp, "epcband", "measure_category")

#hhtype6

#nssech9

#hhvulx

#ahcinceqv5

#tenure4x

#Append files and add common fields

energy <- rbind(energy_dishrp, energy_ethhrp, energy_agehrp)
damp$code <- "1_1_6"
damp$measure <- "Energy efficiency band"
damp$source <- "English Housing Survey"
damp$units <- "Share of dwellings"
damp$date <- "2014-2018"
damp$geography <- "London"

#1_1_7: Damp data from EHS physical survey

#agehrp4x

detach(package:plyr)

myvars <- c("aagpd1617", "agehrp4x", "dampalf")
ehs1_age <- ehs1[myvars]
ehs2_age <- ehs2[myvars]
ehs_age <- rbind(ehs1_age, ehs2_age)

ehs_age <- na.omit(ehs_age)

damp_agehrp1 <-
  ehs_age %>% group_by(agehrp4x) %>% 
  summarise(number1 = sum(aagpd1617))

damp_agehrp2 <-
  ehs_age %>% group_by(agehrp4x, dampalf) %>% 
  summarise(number2 = sum(aagpd1617))

damp_agehrp <- left_join(damp_agehrp2, damp_agehrp1, by="agehrp4x")

damp_agehrp$value <- damp_agehrp$number2/damp_agehrp$number1

damp_agehrp$characteristic <- "Age"
damp_agehrp <- rename.variable(damp_agehrp, "agehrp4x", "characteristic_detail")
damp_agehrp <- rename.variable(damp_agehrp, "dampalf", "measure_category")

#ethhrp2x

detach(package:plyr)

myvars <- c("aagpd1617", "ethhrp2x", "dampalf")
ehs1_eth <- ehs1[myvars]
ehs2_eth <- ehs2[myvars]
ehs_eth <- rbind(ehs1_eth, ehs2_eth)

ehs_eth <- na.omit(ehs_eth)

damp_ethhrp1 <-
  ehs_eth %>% group_by(ethhrp2x) %>% 
  summarise(number1 = sum(aagpd1617))

damp_ethhrp2 <-
  ehs_eth %>% group_by(ethhrp2x, dampalf) %>% 
  summarise(number2 = sum(aagpd1617))

damp_ethhrp <- left_join(damp_ethhrp2, damp_ethhrp1, by="ethhrp2x")

damp_ethhrp$value <- damp_ethhrp$number2/damp_ethhrp$number1

damp_ethhrp$characteristic <- "Ethnicity"
damp_ethhrp <- rename.variable(damp_ethhrp, "ethhrp2x", "characteristic_detail")
damp_ethhrp <- rename.variable(damp_ethhrp, "dampalf", "measure_category")

#hpregdis

myvars <- c("aagpd1617", "hpregdis", "dampalf")
ehs1_dis <- ehs1[myvars]
ehs2_dis <- ehs2[myvars]
ehs_dis <- rbind(ehs1_dis, ehs2_dis)

ehs_dis <- na.omit(ehs_dis)

damp_dishrp1 <-
  ehs_dis %>% group_by(hpregdis) %>% 
  summarise(number1 = sum(aagpd1617))

damp_dishrp2 <-
  ehs_dis %>% group_by(hpregdis, dampalf) %>% 
  summarise(number2 = sum(aagpd1617))

damp_dishrp <- left_join(damp_dishrp2, damp_dishrp1, by="hpregdis")

damp_dishrp$value <- damp_dishrp$number2/damp_dishrp$number1

damp_dishrp$characteristic <- "Disability"
damp_dishrp <- rename.variable(damp_dishrp, "hpregdis", "characteristic_detail")
damp_dishrp <- rename.variable(damp_dishrp, "dampalf", "measure_category")

#hhtype6

#nssech9

#hhvulx

#ahcinceqv5

#tenure4x

#Append files and add common fields

damp <- rbind(damp_dishrp, damp_ethhrp, damp_agehrp)
damp$code <- "1_1_7"
damp$measure <- "Damp"
damp$source <- "English Housing Survey"
damp$units <- "Share of dwellings"
damp$date <- "2014-2018"
damp$geography <- "London"
