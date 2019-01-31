
setwd(Data)

myvars <- c("soclasm", "phhwt07", "hiquald", "ethcen", "govtor", "hserial")
h98 <- read.dta13("aj98_household_pub.dta", select.cols = myvars, convert.factors = F)

myvars <- c("NSECMMJ", "PHHWT14", "HIQUAL8D", "ETH01", "GOVTOR", "HSERIALP")
h08 <- read.dta13("lfsh_aj08_eul.dta", select.cols = myvars, convert.factors = F)

myvars <- c("NSECMJ10", "PHHWT17", "HIQUL15D", "ETHUKEUL", "GOVTOR", "HSERIALP")
h18 <- read.dta13("lfsh_aj18_eul_phhwt17.dta", select.cols = myvars, convert.factors = F)

h98L <- h98[ which(h98$govtor==13|h98$govtor==14), ]
h08L <- h08[ which(h08$GOVTOR==13|h08$GOVTOR==14), ]
h18L <- h18[ which(h18$GOVTOR==13|h18$GOVTOR==14), ]

#Coding ethnicity into 1 = white and 0 = mixed/other

h98L$white <- 0
h98L$white[h98L$ethcen==0] <- 1
h98L$white[h98L$ethcen==-10] <- NA

h08L$white <- 0
h08L$white[h08L$ETH01==1] <- 1
h08L$white[h08L$ETH01==-10] <- NA

h18L$white <- 0
h18L$white[h18L$ETHUKEUL==1] <- 1
h18L$white[h18L$ETHUKEUL==-8] <- NA

#Coding degree into 1 = degree and 0 = lower-level qualifications

h98L$degre <- 0
h98L$degre[h98L$hiquald==1] <- 1
h98L$degre[h98L$hiquald==-10] <- NA

h08L$degre <- 0
h08L$degre[h08L$HIQUAL8D==1] <- 1
h08L$degre[h08L$HIQUAL8D==-10] <- NA

h18L$degre <- 0
h18L$degre[h18L$HIQUL15D==1] <- 1
h18L$degre[h18L$HIQUL15D==-9|h18L$HIQUL15D==-8] <- NA

#Coding social class into 1 = professional and 0 = other class

h98L$class <- 0
h98L$class[h98L$soclasm==1] <- 1
h98L$class[h98L$soclasm==-10] <- NA

h08L$class <- 0
h08L$class[h08L$NSECMMJ==1|h08L$NSECMMJ==2] <- 1
h08L$class[h08L$NSECMMJ==-10] <- NA

h18L$class <- 0
h18L$class[h18L$NSECMJ10==1|h18L$NSECMJ10==2] <- 1
h18L$class[h18L$NSECMJ10==-9] <- NA

#renaming weights

h98L$HSERIALP <- as.character(h98L$hserial)

names(h98L)[5] <- "weight"
names(h08L)[2] <- "weight"
names(h18L)[6] <- "weight"

#simplify variables

myvars <- c("weight", "white", "degre", "class", "HSERIALP")
f98L <- h98L[myvars]
f08L <- h08L[myvars]
f18L <- h18L[myvars]

#add a year variable

f98L$year <- "1998"
f08L$year <- "2008"
f18L$year <- "2018"

f98L1 <- na.omit(f98L)
f08L1 <- na.omit(f08L)
f18L1 <- na.omit(f18L)

#Bind rows

final0 <- rbind(f98L1, f08L1, f18L1)
final1 <- na.omit(final0)

#Summarise 

totals0 <-
  final1 %>% group_by(year, HSERIALP) %>% 
  summarise(rwhite = sum(white),
            rdegre = sum(degre),
            rclass = sum(class),
            count = n())

myvars <- c("weight", "year", "HSERIALP")
weights0 <- final1[myvars]

weights1 <-
  weights0 %>% group_by(year, HSERIALP) %>%
  summarise(weight = mean(weight))

totals <- left_join(totals0, weights1, by = c("year","HSERIALP"))

totals$mixeth <- 0
totals$mixeth[(totals$rwhite==totals$count)] <- 1
totals$mixeth[(totals$rwhite==0)] <- 2

totals$mixdeg <- 0
totals$mixdeg[(totals$rdegre==totals$count)] <- 1
totals$mixdeg[(totals$rdegre==0)] <- 2

totals$mixcla <- 0
totals$mixcla[(totals$rclass==totals$count)] <- 1
totals$mixcla[(totals$rclass==0)] <- 2

mixeth <-
  totals %>% group_by(year, mixeth) %>% 
  summarise(freq = sum(weight),
            count = n())

mixdeg <-
  totals %>% group_by(year, mixdeg) %>% 
  summarise(freq = sum(weight),
            count = n())

mixcla <-
  totals %>% group_by(year, mixcla) %>% 
  summarise(freq = sum(weight),
            count = n())