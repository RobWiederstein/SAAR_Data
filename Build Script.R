#load all cleaned up objects for a build
#library
library (ggplot2)

#load objects
wd <- getwd()

saar <- read.csv (paste (wd, "objects", "SAAR.1999.2012.csv", sep = "/"), 
                  sep = ",", header = T, as.is = TRUE)
afgr <- read.csv (paste (wd, "objects", "afgr.08.12.csv", sep = "/"),
                  sep = ",", header = T, as.is = TRUE)
elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)
cp <- read.csv (paste (wd, "objects", "tot.cp.2001.2012.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)
gr <- read.csv (paste (wd, "objects", "gr.2003.2007.csv", sep = "/" ),
                sep = ",", header = T, as.is = TRUE)
frd <- read.csv (paste (wd, "objects", "FRD.Lunch.Pct.2012.KDE.csv", sep = "/" ),
                 sep = ",", header = T, as.is = TRUE)

#really small schools with 8 grade enrollment, but no 12th grade
#probably why they are missing on the afgr data
attach (saar)
exclude.schools <- saar [DISTRICT == "Anchorage Independent" | 
                        DISTRICT == "East Bernstadt Independent" |
                        DISTRICT == "Science Hill Independent"  |
                        DISTRICT == "Southgate Independent" |
                        DISTRICT == "West Point Independent",]
detach (saar)

#fix new index so lengths of data frames are equal
index <- as.data.frame (afgr$DISTRICT, stringsAsFactors = FALSE)
names (index) <- "DISTRICT"
elsi <- merge (index, elsi)
saar <- merge (index, saar)
gr <- gr[, -1]
names (gr)[1] <- "DISTRICT"
gr[161,1] <- "Walton Verona Independent" #eliminate hyphen
gr <- merge (index, gr)
frd$DISTRICT <- sub ("Raceland-Worthington Independent", "Raceland Independent", frd$DISTRICT)
frd$DISTRICT <- sub ("Walton-Verona Independent", "Walton Verona Independent", frd$DISTRICT)
frd <- merge (index, frd)


#2012
District <- index
Year <- rep (2012, 169)
Tot.Enrollment.saar <- saar$TOTAL.2012.KDE
Tot.Enrollment.elsi <- rep (NA, 169)
FRD.Lunch.Pct <- frd$FRD_LUNCH_PCT                   #taken from KDE school report card
FRD.Decile <- cut_number(FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2012.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2012.KDE /
                    saar$GR8E.2007.KDE
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2012.KDE /
                    saar$GR9E.2008.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2012.KDE / 
                         saar$GR8E.2007.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2012.KDE /
                         saar$GR9E.2008

tot.2012 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                   Gr.12.Gr.9.ratio.KDE)

rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
     Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
     Gr.12.Gr.9.ratio.KDE)

cor(tot.2012[sapply(tot.2012, is.numeric)])  #awesome

#2011
District <- index
Year <- rep (2011, 169)
Tot.Enrollment.saar <- saar$TOTAL.2011.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2011.12
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2011.12) / 
                  as.integer (elsi$Total.Students..UG.PK.12...District..2011.12)
FRD.Decile <- cut_number (FRD.Lunch.Pct, n= 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2011.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / 
                    saar$GR8E.2006.KDE
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / 
                    saar$GR9E.2007.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2011.KDE / 
                         saar$GR8E.2006.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2011.KDE /
                         saar$GR9E.2007.KDE

tot.2011 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                   Gr.12.Gr.9.ratio.KDE)

rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
Gr.12.Gr.9.ratio.KDE)

cor(tot.2011[sapply(tot.2011, is.numeric)])  #awesome

#2010
District <- index
Year <- rep (2010, 169)
Tot.Enrollment.saar <- saar$TOTAL.2010.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2010.11
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2010.11) / 
                  as.integer (elsi$Total.Students..UG.PK.12...District..2010.11)
FRD.Decile <- cut_number (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2010.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / 
                    saar$GR8E.2005.KDE
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / 
                    saar$GR9E.2006.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2010.KDE / 
                         saar$GR8E.2005.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2010.KDE /
                         saar$GR9E.2006.KDE

tot.2010 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                    FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                    Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                    Gr.12.Gr.9.ratio.KDE)
                   
rm    (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)
     
cor(tot.2010[sapply(tot.2010, is.numeric)])  #awesome

#2009
District <- index
Year <-rep (2009, 169)
Tot.Enrollment.saar <- saar$TOTAL.2009.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2009.10
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2009.10) / 
     as.integer (elsi$Total.Students..UG.PK.12...District..2009.10)
FRD.Decile <- cut_number (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2009.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2009.KDE / 
     saar$GR8E.2004.KDE
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2009.KDE / 
     saar$GR9E.2005.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2009.KDE / saar$GR8E.2004.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2009.KDE / saar$GR9E.2005.KDE

tot.2009 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                    FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                    Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                    Gr.12.Gr.9.ratio.KDE)

rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
     Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
     Gr.12.Gr.9.ratio.KDE)

cor(tot.2009[sapply(tot.2009, is.numeric)])  #awesome

#2008
District <- index
Year <-rep (2008, 169)
Tot.Enrollment.saar <- saar$TOTAL.2008.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2008.09
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2008.09) / 
     as.integer (elsi$Total.Students..UG.PK.12...District..2008.09)
FRD.Decile <- cut_number (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2008.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2008.KDE / 
     saar$GR8E.2003.KDE
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2008.KDE / 
     saar$GR9E.2004.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2008.KDE / saar$GR8E.2003.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2008.KDE / saar$GR9E.2004.KDE

tot.2008 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                   Gr.12.Gr.9.ratio.KDE)


rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
     Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
     Gr.12.Gr.9.ratio.KDE)


#2007
District <- index
Year <-rep (2007, 169)
Tot.Enrollment.saar <- saar$TOTAL.2007.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2007.08
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2007.08) / 
     as.integer (elsi$Total.Students..UG.PK.12...District..2007.08)
FRD.Decile <- cut_number (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2007.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- as.integer (gr$GRADS_4YR_07) / saar$GR8E.2002.KDE 
Gr.9.Cohort.KDE <- as.integer (gr$GRADS_4YR_07) / saar$GR9E.2003.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2007.KDE / saar$GR8E.2002.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2007.KDE / saar$GR9E.2003.KDE

tot.2007 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                    FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                    Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                    Gr.12.Gr.9.ratio.KDE)
                   
                   
rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)
     
     
cor(tot.2007[sapply(tot.2007, is.numeric)])  #awesome

#2006
District <- index
Year <-rep (2006, 169)
Tot.Enrollment.saar <- saar$TOTAL.2006.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2006.07
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2006.07) / 
     as.integer (elsi$Total.Students..UG.PK.12...District..2006.07)
FRD.Decile <- cut_number (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2006.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- as.integer (gr$GRADS_4YR_06) / saar$GR8E.2001.KDE 
Gr.9.Cohort.KDE <- as.integer (gr$GRADS_4YR_06) / saar$GR9E.2002.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2006.KDE / saar$GR8E.2001.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2006.KDE / saar$GR9E.2002.KDE

tot.2006 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                   Gr.12.Gr.9.ratio.KDE)


rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)


cor(tot.2006[sapply(tot.2006, is.numeric)])  #awesome

#2005
District <- index
Year <-rep (2005, 169)
Tot.Enrollment.saar <- saar$TOTAL.2005.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2005.06
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2005.06) / 
     as.integer (elsi$Total.Students..UG.PK.12...District..2005.06)
FRD.Decile <- cut_number (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2005.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- as.integer (gr$GRADS_4YR_05) / saar$GR8E.2000.KDE 
Gr.9.Cohort.KDE <- as.integer (gr$GRADS_4YR_05) / saar$GR9E.2001.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2005.KDE / saar$GR8E.2000.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2005.KDE / saar$GR9E.2001.KDE

tot.2005 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                   Gr.12.Gr.9.ratio.KDE)


rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)


cor(tot.2005[sapply(tot.2005, is.numeric)])  #awesome

#2004
District <- index
Year <-rep (2004, 169)
Tot.Enrollment.saar <- saar$TOTAL.2004.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2004.05
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2005.06) / 
     as.integer (elsi$Total.Students..UG.PK.12...District..2005.06) ##wrong years!! Data imputation?
FRD.Decile <- cut_number (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2004.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- as.integer (gr$GRADS_4YR_04) / saar$GR8E.1999.KDE 
Gr.9.Cohort.KDE <- as.integer (gr$GRADS_4YR_04) / saar$GR9E.2000.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2004.KDE / saar$GR8E.1999.KDE
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2004.KDE / saar$GR9E.2000.KDE

tot.2004 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                   Gr.12.Gr.9.ratio.KDE)


rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)


cor(tot.2004[sapply(tot.2004, is.numeric)])  #awesome

#2003
District <- index
Year <-rep (2003, 169)
Tot.Enrollment.saar <- saar$TOTAL.2003.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2003.04
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2005.06) / 
     as.integer (elsi$Total.Students..UG.PK.12...District..2005.06) ##wrong years!! Data imputation?
FRD.Decile <- cut_number (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Child.Poverty.Pct <- cp$cp.2003.AEC
Poverty.Decile <- cut_number(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
Gr.8.Cohort.KDE <- rep (NA, 169) #no saar$GR8E.1998 
Gr.9.Cohort.KDE <- as.integer (gr$GRADS_4YR_03) / saar$GR9E.1999.KDE
Gr.12.Gr.8.ratio.KDE <- rep (NA, 169) #no saar$GR8E.1998
Gr.12.Gr.9.ratio.KDE <- saar$GR12E.2003.KDE / saar$GR9E.1999.KDE

tot.2003 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile,
                   Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                   Gr.12.Gr.9.ratio.KDE)


rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)


cor(tot.2003[sapply(tot.2003, is.numeric)])  #awesome

cum.2003.2012 <- rbind (tot.2012, tot.2011, tot.2010, tot.2009, tot.2008, tot.2007,
                        tot.2006, tot.2005, tot.2004, tot.2003)


#Save as R object to load in later script
cumulative <- paste (wd, "objects", "cum.2003.2012.csv", sep = "/")
write.table (cum.2003.2012, file = cumulative, sep = ",")

#cleanup
rm (list = ls())

