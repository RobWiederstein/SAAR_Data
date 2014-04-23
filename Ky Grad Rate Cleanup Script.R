#build table for Kentucky state-wide reported graduation rates
library (ggplot2)
wd <- getwd()

#get grad rate data from KDE
file <- paste (wd, "data sets", "Grad Rate 2003-2007.csv", sep = "/")
gr <- read.csv (file, header = T, sep = ",", as.is = T,
                strip.white = T)

#get afgr data from KDE
file <- paste (wd, "data sets", "AFGR_2012.csv", sep = "/")
afgr <- read.csv (file, header = T, sep = ",", as.is = T,
                  strip.white = T)


# get state totals for 2003-2007
gr <- gr[gr$DISTNAME == "STATE", ]
gr <- gr [, grep ("GRAD_", names (gr))]

#get state totals for 2007 to 2012
afgr <- afgr [afgr$School.Name == "STATE TOTAL" & afgr$Gender == "Total" & 
                afgr$Ethnicity == "Total", c(1, 5, 19)]

#combine in new table for 2003:2012
Year <- 2003:2012
State <- rep ("KY", 10)
Reported <- (c(as.numeric (gr[1, 1:5]), as.numeric (afgr[,3]))) / 100
Method <- c(rep ("NA", 5), rep ("AFGR", 5))
Ky.grad.rate <- (cbind (Year, State, Reported, Method))
Ky.grad.rate <- as.data.frame (Ky.grad.rate, row.names = 1:10, stringsAsFactors = F)
Ky.grad.rate$Year <- as.Date (Ky.grad.rate$Year, "%Y")
Ky.grad.rate$Reported <- as.numeric (Ky.grad.rate$Reported)

#Save as R object to load in later script
file <- paste (wd, "objects", "State.grad.rate.2003.2012.KDE.csv", sep = "/")
write.table (Ky.grad.rate, file = file, sep = ",")

#cleanup
rm (list = ls())
