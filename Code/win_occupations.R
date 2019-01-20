## Part three of the code creates an 'occupational integration ratio' -
## proportion of each demographic group working in high-skilled occcupations in a given sector, 
## divided by the proportion of all workers in the sector working in high-skilled occupations

#removing total variables and keeping only 25-44s

merge4 <- merge4e[ which(merge4e$AGE!=7 & merge4e$ETH!=7 & merge4e$AGE!=6 & merge4e$AGE!=5 & merge4e$AGE!=4 & merge4e$AGE!=1 & merge4e$SOC!=10), ]

#recode of occupational data into high and low skilled jobs

merge4$occ <- 0
merge4$occ[merge4$SOC==1|merge4$SOC==2] <- 1

#gather the sector data

mergeG <- gather(merge4, "AE", "C", "F_", "G", "H", "I", "K", "L", "N", "O", "P", "RU", "x58","x61", "x62", "x69", "x70", "x71", "x72", "x73", "x74", "x86","x87", key = "sector", value = "number")

#summing by broad (low/high skill) occupational categories - gives one row for every demographic group for each sector and for high/low occupations

mergeoccs <-
  mergeG%>%
  group_by(gender, ETH, sector, occ) %>%
  summarise(groupocc = sum(number))

#creating total number of jobs in each sector for each demographic group

mergeoccTG <- 
  mergeoccs %>% 
  group_by(gender, ETH, sector) %>% 
  summarise (groupT = sum(groupocc))

#creating total number of jobs in each sector

mergeoccTT <-
  mergeoccs %>% 
  group_by(sector) %>% 
  summarise (sectorT = sum(groupocc))

#creating total number of jobs in each sector for the two occupational groups

mergeoccTS <-
  mergeoccs %>% 
  group_by(sector, occ) %>% 
  summarise (occsT = sum(groupocc))

#keeping total number of high skilled jobs in each sector for each demographic group

socG <- mergeoccs[ which(mergeoccs$occ==1), ]

#keeping total number of high skilled jobs in each sector 

socS <- mergeoccTS[ which(mergeoccTS$occ==1), ]

#merge high-skilled jobs in each sector with total jobs in each sector, create proportion

totalS <- left_join(socS, mergeoccTT,by=c("sector"))
totalS$HpropS <- totalS$occsT/totalS$sectorT

#merge high-skilled jobs in each sector with total jobs in each sector for each demographic group
# create proportion

totalG <- left_join(socG, mergeoccTG,by=c("gender", "ETH", "sector"))
totalG$HpropG <- totalG$groupocc/totalG$groupT

#merge the two proportions together, calculate ratio 

occupations0 <- left_join(totalG, totalS, by=c("sector"))
occupations0$repratio <- occupations0$HpropG/occupations0$HpropS

#make a final table with only the data we need

myvars <- c("gender", "ETH", "sector", "repratio")
occupations <- occupations0[myvars]

