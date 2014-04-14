#library
library (ggplot2)

#load objects
wd <- getwd()

saar <- read.csv (paste (wd, "objects", "SAAR.1999.2012.csv", sep = "/"), 
                  sep = ",", header = T)
afgr <- read.csv (paste (wd, "objects", "afgr.08.12.csv", sep = "/"),
                  sep = ",", header = T)
elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T)
cp <- read.csv (paste (wd, "objects", "tot.cp.2001.2012.csv", sep = "/" ),
                  sep = ",", header = T)

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
index <- as.data.frame (afgr$DISTRICT)
names (index) <- "DISTRICT"
elsi <- merge (index, elsi)
saar <- merge (index, saar)

#2012
District <- index
Year <- rep (2012, 169)
Tot.Enrollment.saar <- saar$TOTAL.2012.KDE
Tot.Enrollment.elsi <- rep (NA, 169)
FRD.Lunch.Pct <- rep (NA, 169)
FRD.Decile <- rep (NA, 169)
Child.Poverty.Pct <- cp$cp.2012.AEC
Poverty.Decile <- cut_interval(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- paste ("D", 1:10, sep = "")
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2012.KDE /
                    saar$GR8E.2007.KDE
Gr.8.Cohort.NCES <- afgr$Grad.w.Diploma.in.4.years.2012.KDE /
                    elsi$Grade.8.Students..Public.School..2007.08
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2012.KDE /
                    saar$GR9E.2008.KDE
Gr.9.Cohort.NCES <- afgr$Grad.w.Diploma.in.4.years.2012.KDE /
                    elsi$Grade.8.Students..Public.School..2008.09

Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2012.KDE / 
                         saar$GR8E.2007.KDE
Gr.12.Gr.8.ratio.NCES <- rep (NA, 169)

tot.2012 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.8.Cohort.NCES, Gr.9.Cohort.KDE,
                   Gr.9.Cohort.NCES, Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.8.ratio.NCES)

rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
     Gr.8.Cohort.KDE, Gr.8.Cohort.NCES, Gr.9.Cohort.KDE,
     Gr.9.Cohort.NCES, Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.8.ratio.NCES)

#2011
District <- index
Year <- rep (2011, 169)
Tot.Enrollment.saar <- saar$TOTAL.2011.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2011.12
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2011.12) / 
                  as.integer (elsi$Total.Students..UG.PK.12...District..2011.12)
FRD.Decile <- cut_interval (FRD.Lunch.Pct, n= 10)
levels (FRD.Decile) <- paste ("D", 1:10, sep = "")
Child.Poverty.Pct <- cp$cp.2011.AEC
Poverty.Decile <- cut_interval(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- paste ("D", 1:10, sep = "")
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / 
                    saar$GR8E.2006.KDE
Gr.8.Cohort.NCES <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / 
                    elsi$Grade.8.Students..Public.School..2006.07
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / 
                    saar$GR9E.2007.KDE
Gr.9.Cohort.NCES <-  rep (NA, 169) #messed up!
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2011.KDE / 
                         saar$GR8E.2006.KDE
Gr.12.Gr.8.ratio.NCES <- as.integer (elsi$Grade.12.Students..Public.School..2011.12) / 
                         as.integer (elsi$Grade.8.Students..Public.School..2006.07)
tot.2011 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.8.Cohort.NCES, Gr.9.Cohort.KDE,
                   Gr.9.Cohort.NCES, Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.8.ratio.NCES
                   )
rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile,
     Gr.8.Cohort.KDE, Gr.8.Cohort.NCES, Gr.9.Cohort.KDE,
     Gr.9.Cohort.NCES, Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.8.ratio.NCES)

cor(tot.2011[sapply(tot.2011, is.numeric)])  #awesome

#2010
District <- index
Year <- rep (2010, 169)
Tot.Enrollment.saar <- saar$TOTAL.2010.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2010.11
FRD.Lunch.Pct <-  as.integer (elsi$Total.Free.and.Reduced.Lunch.Students..Public.School..2010.11) / 
                  as.integer (elsi$Total.Students..UG.PK.12...District..2010.11)
FRD.Decile <- cut_interval (FRD.Lunch.Pct, n = 10)
levels (FRD.Decile) <- paste ("D", 1:10, sep = "")
Child.Poverty.Pct <- cp$cp.2010.AEC
Poverty.Decile <- cut_interval(Child.Poverty.Pct, n = 10)
levels (Poverty.Decile) <- paste ("D", 1:10, sep = "")
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / 
                    saar$GR8E.2005.KDE
Gr.8.Cohort.NCES <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / 
                    elsi$Grade.8.Students..Public.School..2005.06
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2010.KDE / 
                    saar$GR9E.2006.KDE
Gr.9.Cohort.NCES <- rep (NA, 169)

Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2010.KDE / 
                         saar$GR8E.2005.KDE
Gr.12.Gr.8.ratio.NCES <- as.integer (elsi$Grade.12.Students..Public.School..2010.11) / 
                         as.integer (elsi$Grade.8.Students..Public.School..2006.07)

tot.2010 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.8.Cohort.NCES, Gr.9.Cohort.KDE,
                   Gr.9.Cohort.NCES, Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.8.ratio.NCES)

rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile,
     Gr.8.Cohort.KDE, Gr.8.Cohort.NCES, Gr.9.Cohort.KDE,
     Gr.9.Cohort.NCES, Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.8.ratio.NCES)

cor(tot.2010[sapply(tot.2010, is.numeric)])  #awesome
