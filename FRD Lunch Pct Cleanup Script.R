library (ggplot2)
wd <- getwd()

#get grad rate data from KDE
file <- paste (wd, "data sets", "LEARNING_ENVIRONMENT_STUDENTS-TEACHERS.csv", sep = "/")
le <- read.csv (file, header = T, sep = ",", as.is = T,
                strip.white = T)

#cut down data frame
le <- le[, c(1, 3, 4, 7, 26:29)]
le <- le [le$SCH_NAME =="---District---" ,]
le <- le[1:174, -c(3, 6, 8)]

#set columns to integer
le$ENROLLMENT_TOTAL <- sub (",", "", le$ENROLLMENT_TOTAL)
le$ENROLLMENT_FREE_LUNCH_CNT <- sub (",", "", le$ENROLLMENT_FREE_LUNCH_CNT)
le$ENROLLMENT_REDUCED_LUNCH_CNT <- sub (",", "", le$ENROLLMENT_REDUCED_LUNCH_CNT)
le [, 3] <- as.integer (le [, 3])
le [, 4] <- as.integer (le [, 4])
le [, 5] <- as.integer (le [, 5])


#clean it up
names (le)[2] <- "DISTRICT"
ENROLLMENT_FRD_CNT <- le$ENROLLMENT_FREE_LUNCH_CNT + le$ENROLLMENT_REDUCED_LUNCH_CNT
FRD_LUNCH_PCT <- ENROLLMENT_FRD_CNT / le$ENROLLMENT_TOTAL
le <- cbind (le, FRD_LUNCH_PCT)


#Save as R object to load in later script
file <- paste (wd, "objects", "FRD.Lunch.Pct.2012.KDE.csv", sep = "/")
write.table (le, file = file, sep = ",")

#cleanup
rm (list = ls())
