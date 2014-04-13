wd <- getwd()

saar <- read.csv (paste (wd, "objects", "SAAR.1999.2012.csv", sep = "/"), 
                  sep = ",", header = T)
afgr <- read.csv (paste (wd, "objects", "afgr.08.12.csv", sep = "/"),
                  sep = ",", header = T)
elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T)

cp<- read.csv (paste (wd, "objects", "tot.cp.2001.2012.csv", sep = "/" ),
                  sep = ",", header = T)

#really small schools with 8 grade enrollment, but no 12th grade
#probably why they are missing on the afgr data
exclude.schools <- saar [DISTRICT == "Anchorage Independent" | 
                        DISTRICT == "East Bernstadt Independent" |
                        DISTRICT == "Science Hill Independent"  |
                        DISTRICT == "Southgate Independent" |
                        DISTRICT == "West Point Independent",]

#fix new index so lengths of data frames are equal
index <- as.data.frame (afgr$DISTRICT)
names (index) <- "DISTRICT"
elsi <- merge (index, elsi)
saar <- merge (index, saar)

District <- index
Year <- rep (2011, 169)
Tot.Enrollment.saar <- saar$TOTAL.2011.KDE
Tot.Enrollment.elsi <- elsi$Total.Students..UG.PK.12...District..2011.12
Gr.8.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / saar$GR8E.2006.KDE
Gr.8.Cohort.NCES <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / elsi$Grade.8.Students..Public.School..2006.07
Gr.9.Cohort.KDE <- afgr$Grad.w.Diploma.in.4.years.2011.KDE / saar$GR9E.2007.KDE
Gr.12.Gr.8.ratio.KDE <- saar$GR12E.2011.KDE / saar$GR8E.2006.KDE
Gr.12.Gr.8.ratio.NCES <- as.integer (elsi$Grade.12.Students..Public.School..2011.12) / 
                         as.integer (elsi$Grade.8.Students..Public.School..2006.07)
tot.2011 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   Gr.8.Cohort.KDE, Gr.8.Cohort.NCES, Gr.9.Cohort.KDE,
                   Gr.12.Gr.8.ratio.KDE, Gr.12.Gr.8.ratio.NCES
                   )

FRD.Lunch.Pct <-  as.numeric (tot$Total.Free.and.Reduced.Lunch.Students..Public.School..2011.12) / 
                              as.numeric(tot$Total.Students..UG.PK.12...District..2011.12)
