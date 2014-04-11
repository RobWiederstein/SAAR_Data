saar <- read.table("SAAR.1999.2012.csv", header = TRUE, sep = ",")

AFGR <- read.csv(file = "AFGR_2012.csv", 
                 sep = ",", 
                 header = TRUE,
                 strip.white = TRUE,
                 colClasses = "character")
attach (AFGR)

#Diploma Recipients from KDE (DR.**) by year
DR.08<- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2008'), c (1, 5, 10)]
DR.09<- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2009'), c (1, 5, 10)]
DR.10<- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2010'), c (1, 5, 10)]
DR.11<- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2011'), c (1, 5, 10)]
DR.12<- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2012'), c (1, 5, 10)]   
rm (AFGR)
detach (AFGR)

#create a common column for merge with SAAR data
b <- DR.08$District.Name
b[84]  <- "Jefferson County"
b[91]  <- "LaRue County"
b[110] <- "McCracken County"
b[111] <- "McCreary County"
b[112] <- "McLean County"
b[142] <- "Raceland Independent"
b[160] <- "Walton Verona Independent"

#assign common column to DR.** for merge
DR.08[,2] <- b
DR.09[,2] <- b
DR.10[,2] <- b
DR.11[,2] <- b
DR.12[,2] <- b

#assign common column name "DISTRICT"
names(DR.08)[2] <- "DISTRICT"
names(DR.09)[2] <- "DISTRICT"
names(DR.10)[2] <- "DISTRICT"
names(DR.11)[2] <- "DISTRICT"
names(DR.12)[2] <- "DISTRICT"

#merge
afgr.08.12 <- merge(DR.08, DR.09, by = "DISTRICT")
afgr.08.12 <- merge(afgr.08.12, DR.10, by = "DISTRICT")
afgr.08.12 <- merge(afgr.08.12, DR.11, by = "DISTRICT")
afgr.08.12 <- merge(afgr.08.12, DR.12, by = "DISTRICT")
afgr.08.12 <- afgr.08.12[, c(1,3,5,7,9,11)]
names(afgr.08.12)[2:6] <- paste ("Grad.w.Diploma.in.4.years", 2008:2012, "KDE", sep = ".")

#save for later combination
write.table(afgr.08.12, file = "afgr.08.12.csv", sep = ",")

#clear workspace
rm (list = ls())





