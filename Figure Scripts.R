#Make figures for analysis
#libraries
library ("ggplot2")

#Figures
wd <- getwd()
file <- paste (wd, "objects", "cum.2003.2012.csv", sep = "/")
cum <- read.csv (file, sep = ",", header = T, as.is = T)
file <- paste (wd, "objects", "State.grad.rate.2003.2012.KDE.csv", sep = "/")
ky <- read.csv (file, sep = ",", header = T, as.is = T)

#set years to "Date" format
cum$Year <- as.Date (as.character (cum$Year), "%Y")
ky$Year <- as.Date (ky$Year)

#set up 1st and 10th deciles for comparisons
FRD.D01 <- subset (cum, cum$FRD.Decile == "D01")
FRD.D10 <- subset (cum, cum$FRD.Decile =="D10")
CP.D01 <- subset (cum, cum$Poverty.Decile=="D01")
CP.D10 <- subset (cum, cum$Poverty.Decile=="D10")
Extreme <- rbind(FRD.D01, FRD.D10)

#compare Grade 8
Gr.8.Cohort.by.year <- tapply (cum$Gr.8.Cohort.KDE, INDEX = cum$Year, FUN = mean)
Gr.8.Cohort.CP.D01 <-  tapply (CP.D01$Gr.8.Cohort.KDE, INDEX = CP.D01$Year, FUN = mean)
Gr.8.Cohort.CP.D10 <-  tapply (CP.D10$Gr.8.Cohort.KDE, INDEX = CP.D10$Year, FUN = mean)
Gr.8.Cohort.FRD.D01 <- tapply (FRD.D01$Gr.8.Cohort.KDE, INDEX = FRD.D01$Year, FUN = mean)
Gr.8.Cohort.FRD.D10 <- tapply (FRD.D10$Gr.8.Cohort.KDE, INDEX = FRD.D10$Year, FUN = mean)

#compare Grade 9
Gr.9.Cohort.by.year <- tapply (cum$Gr.9.Cohort.KDE, INDEX = cum$Year, FUN = mean)
Gr.9.Cohort.CP.D01 <- tapply (CP.D01$Gr.9.Cohort.KDE, INDEX = CP.D01$Year, FUN = mean)
Gr.9.Cohort.CP.D10 <- tapply (CP.D10$Gr.9.Cohort.KDE, INDEX = CP.D10$Year, FUN = mean)
Gr.9.Cohort.FRD.D01 <- tapply (FRD.D01$Gr.9.Cohort.KDE, INDEX = FRD.D01$Year, FUN = mean)
Gr.9.Cohort.FRD.D10 <- tapply (FRD.D10$Gr.9.Cohort.KDE, INDEX = FRD.D10$Year, FUN = mean)

table <- rbind (Gr.8.Cohort.by.year, Gr.8.Cohort.CP.D01, Gr.8.Cohort.CP.D10, 
                Gr.8.Cohort.FRD.D01, Gr.8.Cohort.FRD.D10, Gr.9.Cohort.by.year, 
                Gr.9.Cohort.CP.D01, Gr.9.Cohort.CP.D10, Gr.9.Cohort.FRD.D01, 
                Gr.9.Cohort.FRD.D10)
table <- as.data.frame(t(table))
Year <- row.names (table)
table <- cbind (Year, table)
table$Year <- as.Date (as.character (table$Year), "%Y")

#Save as R object to load in later script
file <- paste (wd, "objects", "summary.csv", sep = "/")
write.csv (table, file = file)

#boxplot 8th Grade by FRD lunch decile
qplot (FRD.Decile, Gr.8.Cohort.KDE, data = cum, geom = "boxplot")
qplot (FRD.Decile, Gr.9.Cohort.KDE, data = cum, geom = "boxplot")

#boxplot 8th Grade by CP decile
qplot (Poverty.Decile, Gr.8.Cohort.KDE, data = cum, geom = "boxplot")

qplot (Poverty.Decile, Gr.9.Cohort.KDE, data = cum, geom = "boxplot")

#boxplot 8th Grade Cohort by Year--Steady Progress!
qplot (Year, Gr.8.Cohort.KDE, data = cum, geom = "boxplot")

#boxplot 9th Grade Cohort by Year--Steady Progress!
qplot (Year, Gr.9.Cohort.KDE, data = cum, geom = "boxplot")

qplot (Year, Gr.8.Cohort.by.year, data = table, geom = "line", ylim = c(.60, 1), main = "Eighth Grade Cohort")

p <- ggplot (cum, aes(FRD.Lunch.Pct, Gr.8.Cohort.KDE, colour = FRD.Decile))
p <- p + layer (geom = "point")

p1 <- ggplot (table, aes (x = Year, y= Gr.8.Cohort.by.year))
table$Year <- as.Date (as.character (table$Year), "%Y")
p1 <- p1 + layer (geom = "line", colour = "red")
p1 <- p1 + ylim (c(.6, 1))
p1 <- p1 + xlab("") + ylab("Pct.") + ggtitle("Eighth Grade Cohort")
p1 <- p1 + geom_line (aes(Year, Gr.8.Cohort.FRD.D10))
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.FRD.D01))

p2 <- ggplot (table, aes (x = Year, y= Gr.9.Cohort.by.year))
p2 <- p2 + layer (geom = "line", colour = "red")
p2 <- p2 + ylim (c(.6, 1))
p2 <- p2 + xlab("") + ylab("Pct.") + ggtitle("Ninth Grade Cohort")
p2 <- p2 + geom_line (aes(Year, Gr.9.Cohort.FRD.D10))
p2 <- p2 + geom_line (aes (Year, Gr.9.Cohort.FRD.D01))

p3 <- ggplot (table, aes (x = Year, y= Gr.9.Cohort.by.year))
p3 <- p3 + layer (geom = "line", colour = "red")
p3 <- p3 + ylim (c(.6, 1))
p3 <- p3 + xlab("") + ylab("Pct.") + ggtitle("Eighth vs.Ninth \n Grade Cohort")
p3 <- p3 + geom_line (aes(Year, Gr.8.Cohort.by.year ))

p4 <- ggplot (table, aes (x = Year, y= Gr.8.Cohort.by.year))
table$Year <- as.Date (as.character (table$Year), "%Y")
p4 <- p4 + layer (geom = "line", colour = "red")
p4 <- p4 + ylim (c(.6, 1))
p4 <- p4 + xlab("") + ylab("Pct.") + ggtitle("Eighth Grade Cohort vs. State Reported")
p4 <- p4 + geom_line (aes(ky$Year, ky$Reported))

