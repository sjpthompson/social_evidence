###Uses taking part survey data to calculate participation in four categories of arts and culture
###By various demographic categories

setwd(Data)

###Specifies the variables needed and opens that Taking Part survey adult file

myvars <- c("artsoverview", "libPSA", "musPSA", "heritage", "socpsa", "rwork", "ethnpsa", "tenharm", "sex1", "dispsa", "educ2", "ageshort", "rimweight", "GOR")
aps <- read.dta13("y2016-17_adult_data_eul.dta", select.cols = myvars, convert.factors = F)

###Selecting only obsevations in London

aps0 <- aps[ which(aps$GOR==7), ]

###Running lots of crosstabs

setwd(Output)

write.csv(aps0, file="arts.csv", row.names=FALSE)

setwd(Code)

x <- c("socpsa", "rwork", "ethnpsa", "tenharm", "sex1", "dispsa", "educ2", "ageshort")
y <- c("artsoverview", "libPSA", "musPSA", "heritage")

for (i in x) {
  for (j in y) {
        cat("\nWeighted proportions for", i, "broken down by", j, "\n")
        print(kable(wtd.table(aps0[[i]], aps0[[j]], aps0$rimweight)/nrow(aps0), 
                digits = 2))
        cat("\n")
  }
}

##To do - recode variables into bigger groups
##Add variable labels
##Print to excel

##Export into excel