## Part three of the code creates an 'occupational integration ratio' -
## proportion of each demographic group working in high-skilled occcupations in a given sector, 
## divided by the proportion of all workers in the sector working in high-skilled occupations

setwd(Data)

#removing total variables and keeping only 25-44s

merge4 <- merge4e[ which(merge4e$AGE!=7 & merge4e$ETH!=7 & merge4e$AGE!=6 & merge4e$AGE!=5 & merge4e$AGE!=4 & merge4e$AGE!=1 & merge4e$SOC!=10), ]

#recode of occupational data into high and low skilled jobs

merge4$occ <- 0
merge4$occ[merge4$SOC==1|merge4$SOC==2] <- 1

#summarise by occs


#gather the sector data

mergeG <- gather(merge4, "AE", "C", "F_", "G", "H", "I", "K", "L", "N", "O", "P", "RU", "x58","x61", "x62", "x69", "x70", "x71", "x72", "x73", "x74", "x86","x87", key = "sector", value = "number")

#reducing the occupational data into our two categories

merge4e <- 
  mergeG %>% 
  group_by(gender, ETH, sector, occ) %>% 
  summarise (groupocc = sum(number))

#creating total number of jobs in each sector for each occupational group

merge5e <- 
  merge4e %>% 
  group_by(gender, ETH, sector) %>% 
  summarise (groupT = sum(groupocc))

#creating total number of jobs in each sector for the two occupational groups

merge4s <-
  mergeG %>% 
  group_by(sector, occ) %>% 
  summarise (occsT = sum(number))

#creating total number of jobs in each sector

merge4t <-
  mergeG %>% 
  group_by(sector) %>% 
  summarise (sectorT = sum(number))

#keeping total number of high skilled jobs in each sector

socS <- merge4s[ which(merge4s$occ==1), ]

#keeping total number of high skilled jobs in each sector for each demographic group

socG <- merge4e[ which(merge4e$occ==1), ]

#merge high-skilled jobs in each sector with total jobs in each sector, create proportion

totalS <- left_join(socS, merge4t,by=c("sector"))
totalS$HpropS <- totalS$occsT/totalS$sectorT

#merge high-skilled jobs in each sector with total jobs in each sector for each demographic group
# create proportion

totalG <- left_join(socG, merge5e,by=c("sector"))
totalG$HpropG <- totalG$groupocc/totalG$groupT



