saar <- read.table ("SAAR.1999.2012.csv", header = TRUE, sep = ",",
                   colClasses = "character")

#import NCES download
wd <- getwd()
ELSI <- paste (wd, "data sets", "ELSI_csv_export_6353280376697842613140.csv",
               sep = "/")

elsi <- read.csv(file = ELSI, 
                 sep = ",", 
                 skip = 6,
                 nrow = 178,
                 header = TRUE,
                 strip.white = TRUE,
                 colClasses = "character")

#build common column for merge
index <- saar$DISTRICT
rm (saar)
reconcile <- elsi$Agency.Name

reconcile [1:77] <- index [1:77]
reconcile [79:112] <- index [78:111]
reconcile [114:144] <- index [112:142]
reconcile [146] <- index [143]
reconcile [147] <- index [144]
reconcile [149:178] <- index [145:174]
elsi$Agency.Name <- reconcile
names(elsi)[1] <- "DISTRICT"

#Merge to index
index <- as.data.frame (index)
names(index)[1] <- "DISTRICT"
elsi <- merge (index, elsi, by = "DISTRICT")

#Write new data file
elsi.new <- paste(wd, "data sets", "elsi_new.csv", sep = "/")
write.table (elsi, file = elsi.new, sep = ",")

#cleanup
rm (list = ls())

