saar <- read.table("SAAR.1999.2012.csv", header = TRUE, sep = ",",
                   colClasses = "character")

NCES <- read.csv(file = "ELSI_csv_export_6353176361250618987912.csv", 
                   skip = 6, sep = ",", nrow = 178, 
                   strip.white = TRUE, header = TRUE,
                   colClasses = "character")

afgr <- read.table("afgr.08.12.csv", header = TRUE, sep = ",",
                   colClasses = "character")


#load library ggplot2 for cut_number function
library("ggplot2", lib.loc="C:/Users/Cathy/R/win-library/3.0")

#match NCES column, school district, for merge with SAAR
    a <- as.character (saar[,1])
    b <- NCES [,1]
    b[1:77] <- a[1:77]
    b[79:112] <- a[78:111]
    b[114:144] <- a[112:142]
    b[146] <- a[143]
    b[147] <- b[144]
    b[149:178] <- a[145:174]
    NCES[,1] <- b
    names(NCES)[1] <- "DISTRICT"
    rm(a , b)

q <- merge(saar, NCES, by= "DISTRICT")
q <- merge(q, afgr, by= "DISTRICT")

Year <- rep(2010, 169)
District <- q[,1]
Tot.Students <- as.numeric(q[,121])
FRL.Pct <- round (as.numeric(q[,182])/as.numeric(q[,121]), digits = 4)
FRL.Decile <- cut_number(FRL.Pct, 10)
E.Grade.Cohort.KDE <- round(as.integer(q[,197])/as.integer(q[,21]), digits = 4)
N.Grade.Cohort.KDE <- round (as.integer(q[,197]) / as.integer(q[,24]), digits = 4)
E.Grade.Cohort.CCD <- round (as.integer(q[,197]) / as.integer (q [, 150]), digits = 4)
E.to.Twelth.Grade.KDE <- round (as.integer (q[, 20]) / as.integer (q[,37]), digits = 4)
N.to.Twelth.Grade.KDE <- round (as.integer (q[,24]) / as.integer (q[, 37]), digits = 4)

q2010 <- cbind (District, Year, Tot.Students, FRL.Pct, FRL.Decile, E.Grade.Cohort.KDE, 
                N.Grade.Cohort.KDE, E.Grade.Cohort.CCD, E.to.Twelth.Grade.KDE,
                N.to.Twelth.Grade.KDE)

q2010 < as.data.frame(q2010, stringsAsFactors = FALSE)


