#EDI measure 1 reads in Domain: Identification of need:
#https://fingertips.phe.org.uk/profile-group/mental-health/profile/cypmh/data#page/9/gid/1938133090/pat/6/par/E12000007/ati/102/are/E09000002/iid/91871/age/217/sex/4
#(Data for County & UA in London region)
#It then extracts headline prevalence of obesity for London by year and by primary/secondary/school age and by borough
#Outputs these in long format

setwd(Data)

#Open the file - this name should not change from year to year - note this is the same filename as measure 1

mydata <- read.csv("indicators-CountyUA.data.csv")

#The indicators of interest is 91871: School pupils with social, emotional and mental health needs: % of school pupils with social, emotional and mental health needs

mydata1 <- filter(mydata, ((Area.Code=="E12000007"|Parent.Code=="E12000007") & Indicator.ID=="91871"))

mydata1$Measure_code <- "EDI_2"

EDI_2 <- select(mydata1, Measure_code, Indicator.Name, Area.Name, Sex, Category, Time.period, Value, Lower.CI.95.0.limit, Upper.CI.95.0.limit)

names(EDI_2) <- c("Measure_code", "Measure_name", "Geography", "Characteristics1", "Characteristics2", "Time", "Value", "Lower95CI", "Upper95CI")