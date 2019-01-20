##
## Part one of the  code imports and cleans the data, puts it into a long format - categories for age, ethnicity, gender, occupation and industry
##

setwd(Data)

#Read in the relevant worksheets from an ONS Census Table - give counts of men and women by age and ethnicity working in different industries

totalm <- read.xlsx("ct09152011census.xls", 3, sheetName=NULL, rowIndex=NULL,
                    startRow=11, endRow=NULL, colIndex=NULL,
                    as.data.frame=TRUE, header=TRUE, colClasses=NA,
                    keepFormulas=FALSE, encoding="unknown", password=NULL)%>% 
  fill(1,2,3)

totalf <- read.xlsx("ct09152011census.xls", 4, sheetName=NULL, rowIndex=NULL,
                    startRow=11, endRow=NULL, colIndex=NULL,
                    as.data.frame=TRUE, header=TRUE, colClasses=NA,
                    keepFormulas=FALSE, encoding="unknown", password=NULL)%>% 
  fill(1,2,3)

subm <- read.xlsx("ct09152011census.xls", 6, sheetName=NULL, rowIndex=NULL,
                  startRow=11, endRow=NULL, colIndex=NULL,
                  as.data.frame=TRUE, header=TRUE, colClasses=NA,
                  keepFormulas=FALSE, encoding="unknown", password=NULL)%>% 
  fill(1,2,3)

subf <- read.xlsx("ct09152011census.xls", 7, sheetName=NULL, rowIndex=NULL,
                  startRow=11, endRow=NULL, colIndex=NULL,
                  as.data.frame=TRUE, header=TRUE, colClasses=NA,
                  keepFormulas=FALSE, encoding="unknown", password=NULL)%>% 
  fill(1,2,3)

#Harmonising the name of the first column across worksheets

names(totalm)[1] <- "NA"
names(totalf)[1] <- "NA"
names(subm)[1] <- "NA"
names(subf)[1] <- "NA"

#Reading in some lookups for the ethnicity, occupational and age categories

eth <- read.xlsx("lookups.xls", 1, sheetName=NULL, rowIndex=NULL,
                 startRow=1, endRow=NULL, colIndex=NULL,
                 as.data.frame=TRUE, header=TRUE, colClasses=NA,
                 keepFormulas=FALSE, encoding="unknown", password=NULL)

occ <- read.xlsx("lookups.xls", 2, sheetName=NULL, rowIndex=NULL,
                 startRow=1, endRow=NULL, colIndex=NULL,
                 as.data.frame=TRUE, header=TRUE, colClasses=NA,
                 keepFormulas=FALSE, encoding="unknown", password=NULL)

age <- read.xlsx("lookups.xls", 3, sheetName=NULL, rowIndex=NULL,
                 startRow=1, endRow=NULL, colIndex=NULL,
                 as.data.frame=TRUE, header=TRUE, colClasses=NA,
                 keepFormulas=FALSE, encoding="unknown", password=NULL)

#Change the name of the first lookup column in age which is being read in wrong

names(age)[1] <- "NA"

#Creating a new gender variable to specify the gender composition of each sheet

totalm$gender <- 1
totalf$gender <- 2
subm$gender <- 1
subf$gender <- 2

#Adding all the sub-sector cases together into a single data frame

subT <- rbind(subm,subf)

#Adding all the broad sector cases together into a single data frame

totalT <- rbind(totalm,totalf)

#Merging the sub-sector and broad sector tables by age, ethcnitiy, occupation and gender

merge1 <- left_join(totalT, subT, by = c("NA", "NA.", "NA..1", "gender"), copy = FALSE)

#In our new master data table, simplifying variable names

names(merge1)[4:37] <- c("Total","AE","C","F_","G","H","I","J","K","L","M","N","O","P","Q","RU","gender","Total2","x45","x47","x58","x61", "x62", "x69", "x70", "x71", "x72", "x73", "x74", "x86","x87","x90","x92","Oth")

#Adding in new simplified values for age, ethnicity and occupation

merge2 <- left_join(merge1, age, by = c("NA"), copy = FALSE)
merge3 <- left_join(merge2, eth, by = c("NA."), copy = FALSE)
names(merge3)[3] <- "NA..1"
merge4e <- left_join(merge3, occ, by = c("NA..1"), copy = FALSE)