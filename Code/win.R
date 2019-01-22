#Creates three tables with employment, sector and occupational integration data

## Part one of the  code imports and cleans the data, puts it into a long format -
## categories for age, ethnicity, gender, occupation and industry
## Uses census ad-hoc table ct09152011, available at: 
## https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/adhocs/009241ct09152011census

source("win_cleaning.R")

## Part two of the code creates the 'sector integration ratio' - 
## proportion of each demographic group working in a sector, 
## divided by proportion of total age group working in a sector
## Uses census ad-hoc table ct09152011, available at: 
## https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/adhocs/009241ct09152011census

# Use this to specify an age group - if using one enter into both AGE1 and AGE2

AGE1 <- 1
AGE2 <- 1

source("win_sectors.r")

## Part three of the code creates an 'occupational integration ratio' -
## proportion of each demographic group working in high-skilled occcupations in a given sector, 
## divided by the proportion of all workers in the sector working in high-skilled occupations

# Use this to specify an age group - if using one enter into both AGE1 and AGE2

source("win_occupations.r")

## Part four of the code produces employment and unemployment rates, using A 
## TO ADD

source("win_employment.r")

## Part five of the code prints the three key tables as outputs using the flextable package

