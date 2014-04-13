#build child poverty indicator
wd <- getwd()

elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T)

afgr <- read.csv (paste (wd, "objects", "afgr.08.12.csv", sep = "/"),
                  sep = ",", header = T)

cp <- read.csv (paste (wd, "data sets", 
                       "Child poverty rate.csv",
                       sep = "/"), sep = ",", header = T)

#change data frame to put years in columns
attach (cp)
cp.2012 <- cp [Location != "Kentucky" & TimeFrame == "2012", c(1, 4)]
cp.2011 <- cp [Location != "Kentucky" & TimeFrame == "2011", c(1, 4)]
cp.2010 <- cp [Location != "Kentucky" & TimeFrame == "2010", c(1, 4)]
cp.2009 <- cp [Location != "Kentucky" & TimeFrame == "2009", c(1, 4)]
cp.2008 <- cp [Location != "Kentucky" & TimeFrame == "2008", c(1, 4)]
cp.2007 <- cp [Location != "Kentucky" & TimeFrame == "2007", c(1, 4)]
cp.2006 <- cp [Location != "Kentucky" & TimeFrame == "2006", c(1, 4)]
cp.2005 <- cp [Location != "Kentucky" & TimeFrame == "2005", c(1, 4)]
cp.2004 <- cp [Location != "Kentucky" & TimeFrame == "2004", c(1, 4)]
cp.2003 <- cp [Location != "Kentucky" & TimeFrame == "2003", c(1, 4)]
cp.2002 <- cp [Location != "Kentucky" & TimeFrame == "2002", c(1, 4)]
cp.2001 <- cp [Location != "Kentucky" & TimeFrame == "2001", c(1, 4)]

tot.cp.2001.2012 <- cbind (cp.2001, cp.2002, cp.2003, cp.2004, cp.2005, 
                           cp.2006, cp.2007, cp.2008, cp.2009, cp.2010, 
                           cp.2011, cp.2012)

tot.cp.2001.2012 <- tot.cp.2001.2012[, c(1, seq (from = 2, to = 24, by = 2))]
names (tot.cp.2001.2012) <- c("County", paste ("cp", 2001:2012, "AEC", 
                              sep = "."))
tot.cp.2001.2012[,1] <- toupper(tot.cp.2001.2012$County)
tot.cp.2001.2012[,2:13] <- tot.cp.2001.2012[,2:13]/100
detach (cp)

#assign county poverty rates to school districts
#120 counties to 174 school districts
index <- elsi[, c(1,3)]
names (index)[2] <- "County"
index [,2] <- sub (" COUNTY", "", index [,2])
temp <- merge (tot.cp.2001.2012, index)
temp <- temp [, c(14,1:13)]
tot.cp.2001.2012 <- temp

#merge with afgr to reduce from 174 school districts
#to 169
index <- as.data.frame (afgr [,1])
names (index)[1] <- "DISTRICT"
tot.cp.2001.2012 <- merge (index, tot.cp.2001.2012)

#Save as R object to load in later script
cp.new <- paste (wd, "objects", "tot.cp.2001.2012.csv", sep = "/")
write.table (tot.cp.2001.2012, file = cp.new, sep = ",")

#cleanup
rm (list = ls())
