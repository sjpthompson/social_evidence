#Runs all the code

#directories
Code <- "C:/Users/Spencer/Desktop/Social Evidence Base/social_evidence/Code"
Data <- "C:/Users/Spencer/Desktop/Social Evidence Base/social_evidence/Data"
Charts <- "C:/Users/Spencer/Desktop/Social Evidence Base/social_evidence/Charts"
Output <- "C:/Users/Spencer/Desktop/Social Evidence Base/social_evidence/Output"
Reports <- "C:/Users/Spencer/Desktop/Social Evidence Base/social_evidence/Reports"

setwd(Code)

#Loading packages

source("packages.R")

#EDI Measures

source("edimeasures.R")

#EDI evidence base

source("edievidencebase.R")

#Residential segregation

#School segregation

#Workforce integration

source("win.R")

#Social class

#Social integration - relationships over time

source("rel.R")

#Survey analysis