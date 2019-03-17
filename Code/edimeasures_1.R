#EDI measure 1 reads in Profile: NCMP and Child Obesity Profile on the prevalence of obesity from:
#https://fingertips.phe.org.uk/profile/national-child-measurement-programme/data#page/9/gid/8000011/pat/6/par/E12000007/ati/102/are/E09000002
#(Data for County & UA in London region)
#It then extracts headline prevalence of obesity for London by year and by gender, ethnicity and deprivation
#Outputs these in long format

setwd(Data)

#Open the file - this name should not change from year to year

mydata <- read.csv("indicators-CountyUA.data.csv")

#The indicators of interest are 90323 - Year 6: Prevalence of obesity, and 90319 - Reception: Prevalence of obesity
#For inequalities we need 92026 - Reception: Prevalence of obesity (including severe obesity), 5-years data combined and 92033 - Year 6: Prevalence of obesity (including severe obesity), 5-years data combined

myvalues2 <- c("90319", "90323")
myvalues3 <- c("92026", "92033")

#We then want to select the most recent and the second most recent value, so define time periods for those. Roll these forward by one year every year

mytime1 <- c("20170000", "20160000")
mytime2 <- c("20090000", "20130000")

#Select on these

mydata1 <- filter(mydata, Area.Code=="E12000007" & ((Indicator.ID %in% myvalues2 & ((Sex=="Persons" & Time.period.Sortable!="20060000")|(Sex!="Persons" & Time.period.Sortable %in% mytime1)))|(Indicator.ID %in% myvalues3 & Time.period.Sortable %in% mytime2 & Category!="")))

#Add EDI measure flag

mydata1$Measure_code <- "EDI_1"

#Put this in our consistent format - EDI_1, year, geography, variable, value, confidence intervals

EDI_1 <- select(mydata1, Measure_code, Indicator.Name, Area.Name, Sex, Category, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit)

#Rename variables to be consistent

names(EDI_1) <- c("Measure_code", "Measure_name", "Geography", "Characteristics1", "Characteristics2", "Time", "Value", "Lower95CI", "Upper95CI")