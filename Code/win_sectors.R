##
## Part two of the code creates the 'sector integration ratio' - proportion of each demographic group working in a sector, divided by proportion of total age group working in a sector (just for 25-44 year-olds)
##

#Removing total variables (except occupations where we remove the non-totals) and only keeping 25-44 year-olds

merge4 <- merge4e[ which((merge4e$AGE==AGE1|merge4e$AGE==AGE2) & merge4e$ETH!=7 & merge4e$SOC==10), ]

#putting into a long format

mergeG <- gather(merge4, "AE", "C", "F_", "G", "H", "I", "K", "L", "N", "O", "P", "RU", "x58","x61", "x62", "x69", "x70", "x71", "x72", "x73", "x74", "x86","x87", key = "sector", value = "number")

#now using summarise to create a total workforce for each gender/ethnicity combination

mergeGT <- 
  mergeG %>% 
  group_by(gender, ETH) %>% 
  summarise (groupT = sum(number))

#also create a total workforce variable

mergeGT$workforce <- sum(mergeG$number, na.rm = TRUE)

#totals for each gender/ethnicity combination by sector

mergeGM <- 
  mergeG %>% 
  group_by(gender, ETH, sector) %>% 
  summarise (sectorG = sum(number))

#total workforce in each sector

mergeST <-
  mergeG %>%
  group_by(sector) %>%
  summarise (sectorT = sum(number))

#merge the mergeGT totals df with the sector-level mergeGM df

sector0 <- left_join(mergeGM, mergeGT, by=c("gender", "ETH"))
sector <- left_join(sector0, mergeST, by=c("sector"))

#calculate representation ratio

sector$repratio <- (sector$sectorG/sector$groupT)/(sector$sectorT/sector$workforce)

#calculate share of workforce in each sector

myvars <- c("gender", "ETH", "sector", "repratio")
sector0 <- sector[myvars]
sector <- spread(sector0, sector, repratio)

setwd(Output)

write.csv(sector, file="Sectors.csv", row.names=FALSE)

setwd(Code)