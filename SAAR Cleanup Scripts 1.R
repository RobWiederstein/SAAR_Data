#set colClasses
set.colClass.1<-c(rep("character", 2), rep("integer", 13) )
wd <- getwd()
#read KDE data
SAAR1999<-read.csv(paste (wd ,"/data sets/SAAR1999.csv", sep = ""), skip=6, nrow=176,
                   header=TRUE, fill=TRUE, blank.lines.skip=TRUE,
                   strip.white=TRUE, colClasses=c("character", rep("integer", 13)))

SAAR2000<-read.csv(paste (wd ,"/data sets/SAAR2000.csv", sep = ""), skip=6, nrow=176, 
                   header=TRUE, fill=TRUE, blank.lines.skip=TRUE,
                   strip.white=TRUE, colClasses=c("character", rep("integer", 13)))

SAAR2001<-read.csv(paste (wd ,"/data sets/SAAR2001.csv", sep = ""), skip=6, nrow=176, 
                   header=TRUE, fill=TRUE, blank.lines.skip=TRUE,
                   strip.white=TRUE, colClasses=c("character", rep("integer", 13)))

SAAR2002<-read.csv(paste (wd ,"/data sets/SAAR2002.csv", sep = ""), skip=6, nrow=176, 
                   header=TRUE, fill=TRUE, blank.lines.skip=TRUE,
                   strip.white=TRUE, colClasses=c("character", rep("integer", 13)))

SAAR2003<-read.csv(paste (wd ,"/data sets/SAAR2003.csv", sep = ""), skip=6, nrow=176, 
                   header=TRUE, fill=TRUE, blank.lines.skip=TRUE,
                   strip.white=TRUE, colClasses=c("character", rep("integer", 13)))

SAAR2004<-read.csv(paste (wd ,"/data sets/SAAR2004.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                     colClasses=c("numeric", "character", rep("integer", 13)), 
                     strip.white=TRUE, nrow=351)

SAAR2005<-read.csv(paste (wd ,"/data sets/SAAR2005.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", rep("integer", 13)), 
                   strip.white=TRUE, nrow=176)

SAAR2006<-read.csv(paste (wd ,"/data sets/SAAR2006.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", "character", rep("integer", 13)), 
                   strip.white=TRUE)

SAAR2007<-read.csv(paste (wd ,"/data sets/SAAR2007.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", "character", rep("integer", 13)), 
                   strip.white=TRUE)

SAAR2008<-read.csv(paste (wd ,"/data sets/SAAR2008.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", "character", rep("integer", 15)), 
                   strip.white=TRUE)

SAAR2009<-read.csv(paste (wd ,"/data sets/SAAR2009.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", "character", rep("integer", 15)), 
                   strip.white=TRUE)

SAAR2010<-read.csv(paste (wd ,"/data sets/SAAR2010.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", "character", rep("integer", 15)), 
                   strip.white=TRUE)

SAAR2011<-read.csv(paste (wd ,"/data sets/SAAR2011.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", "character", rep("integer", 15)), 
                   strip.white=TRUE)

SAAR2012<-read.csv(paste (wd ,"/data sets/SAAR2012.csv", sep = ""), header=TRUE, sep=",", 
                   blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", "character", rep("integer", 15)), 
                   strip.white=TRUE)

#load libraries
library("gdata", lib.loc="C:/Users/Cathy/R/win-library/3.0")

#build index column for merge using 1999 as base year
district.index<-SAAR1999[,1]                                      #school districts
district.index<-sub("^[0-9][0-9][0-9].", "" , district.index)     #remove number in front
district.index<-sub("Independent", "Independen", district.index) #add "t" to independent
district.index<-sub("Independen", "Independ", district.index)
district.index<-sub("Independ", "Independent", district.index)
district.index<-as.data.frame(district.index)
names(district.index)<-c("DISTRICT")

#clean 1999
temp<-sub("^[0-9][0-9][0-9].", "" , SAAR1999[,1])
temp<-sub("Independent", "Independen", temp)
temp<-sub("Independen", "Independ", temp)
temp<-sub("Independ", "Independent",temp)
SAAR1999[,1]<-temp
setdiff(district.index[,1], SAAR1999[,1])
setdiff(SAAR1999[,1],district.index[,1])

#clean 2000
temp<-sub("^[0-9][0-9][0-9].", "" , SAAR2000$DISTRICT)
temp<-sub("Independent", "Independen", temp)
temp<-sub("Independen", "Independ", temp)
temp<-sub("Independ", "Independent", temp)
temp<-sub("Erlanger-Elsmere Independent.", "Erlanger-Elsmere Independent", temp)
temp<-sub("Walton-Verona Independent", "Walton Verona Independent", temp)
SAAR2000$DISTRICT<-temp
setdiff(district.index$DISTRICT, SAAR2000$DISTRICT)
setdiff(SAAR2000$DISTRICT, district.index$DISTRICT)

#clean 2001
temp<-sub("^[0-9][0-9][0-9].", "" , SAAR2001$DISTRICT)
temp<-sub("Independent", "Independen", temp)
temp<-sub("Independen", "Independ", temp)
temp<-sub("Independ", "Independent", temp)
temp<-sub("Walton-Verona Independent", "Walton Verona Independent", temp)
SAAR2001$DISTRICT<-temp
setdiff(district.index$DISTRICT, SAAR2001$DISTRICT)
setdiff(SAAR2001$DISTRICT, district.index$DISTRICT)

#clean 2002
temp<-sub("^[0-9][0-9][0-9].", "" , SAAR2002$DISTRICT)
temp<-sub("Independent", "Independen", temp)
temp<-sub("Independen", "Independ", temp)
temp<-sub("Independ", "Independent", temp)
temp<-sub("Walton-Verona Independent", "Walton Verona Independent", temp)
SAAR2002$DISTRICT<-temp
setdiff(district.index$DISTRICT, SAAR2002$DISTRICT)
setdiff(SAAR2002$DISTRICT, SAAR.1999.2012$DISTRICT)

#clean 2003
temp<-sub("^[0-9][0-9][0-9].", "" , SAAR2003$DISTRICT)
temp<-sub("Independent", "Independen", temp)
temp<-sub("Independen", "Independ", temp)
temp<-sub("Independ", "Independent", temp)
temp<-sub("Walton-Verona Independent", "Walton Verona Independent", temp)
SAAR2003$DISTRICT<-temp
setdiff(district.index$DISTRICT, SAAR2003$DISTRICT)
setdiff(SAAR2003$DISTRICT, district.index$DISTRICT)

#clean 2004--extra variable--school number
SAAR2004<-SAAR2004[!is.na(SAAR2004$X11),]
temp<-SAAR2004$District
temp<-sub("Walton-Verona Independent", "Walton Verona Independent", temp)
SAAR2004[,2]<-temp
SAAR2004<-SAAR2004[,2:15]
setdiff(district.index$DISTRICT, SAAR2004$District)
setdiff(SAAR2004$District, district.index$DISTRICT)
#capitalize column "DISTRICT"
a<-names(SAAR2004)
a[1]<-"DISTRICT"
names(SAAR2004)<-a
rm(a)

#clean 2005
temp<-sub("^[0-9][0-9][0-9].", "" , SAAR2005$DISTRICT)
temp<-sub("Walton-Verona Independent", "Walton Verona Independent", temp)
SAAR2005$DISTRICT<-temp
setdiff(district.index$DISTRICT, SAAR2005$DISTRICT)
setdiff(SAAR2005$DISTRICT, district.index$DISTRICT)

#clean 2006
SAAR2006<-SAAR2006[!is.na(SAAR2006$X11),]
SAAR2006<-SAAR2006[nchar(SAAR2006[,2])==0,]
SAAR2006[165,1]<-"Walton Verona Independent"
SAAR2006<-SAAR2006[,c(1, 3:15)]
SAAR2006<-SAAR2006[1:175,]
a<-names(SAAR2006)
a[1]<-"DISTRICT"
names(SAAR2006)<-a
setdiff(district.index$DISTRICT, SAAR2006$DISTRICT) #Harrodsburg Independent missing
setdiff(SAAR2006$DISTRICT, district.index$DISTRICT)


#clean 2007
SAAR2007<-SAAR2007[,1:15]
temp<-subset(SAAR2007, (grepl("TOTALS", SAAR2007[,1])))  #extract districts by totals
temp[,1]<-sub("TOTALS for ", "",temp[,1])
temp<-temp[,c(1, 3:15)]
temp[,1]<-trim(temp[,1])
SAAR2007<-temp
SAAR2007[164,1]<-"Walton Verona Independent"
SAAR2007[144,1]<-"Raceland Independent"
setdiff(district.index$DISTRICT, SAAR2007$DISTRICT) #Providence Ind./Harrodsburg Ind. missing, Raceland combined with Worthington?
setdiff(SAAR2007$DISTRICT, district.index$DISTRICT)
setdiff(district.index$DISTRICT, SAAR2006$DISTRICT) #Harrodsburg Independent missing

#clean 2008
temp<-subset(SAAR2008, (grepl("TOTALS", SAAR2008[,1])))  #extract districts by totals
temp[,1]<-sub("TOTALS for ", "",temp[,1])
temp<-temp[,c(1, 3:17)]
temp[,1]<-trim(temp[,1])
SAAR2008<-temp
SAAR2008[144,1]<-"Raceland Independent"
SAAR2008[164,1]<-"Walton Verona Independent"
setdiff(district.index$DISTRICT, SAAR2008$DISTRICT) #Providence & Harrodsburg still missing
setdiff(SAAR2008$DISTRICT, district.index$DISTRICT)



#clean 2009
temp<-subset(SAAR2009, (grepl("TOTALS", SAAR2009[,1])))
temp[,1]<-sub("TOTALS for ", "",temp[,1])
temp[,1]<-trim(temp[,1])
SAAR2009<-temp
SAAR2009[86,1]<-"Jefferson County"
SAAR2009[144,1]<-"Raceland Independent"
SAAR2009[164,1]<-"Walton Verona Independent"
SAAR2009<-SAAR2009[,c(1, 3:17)]
setdiff(district.index$DISTRICT, SAAR2009$DISTRICT) #Providence & Harrodsburg still missing
setdiff(SAAR2009$DISTRICT, district.index$DISTRICT)


#clean 2010
temp<-subset(SAAR2010, (grepl("TOTALS", SAAR2010[,1])))
temp[,1]<-sub("TOTALS for ", "",temp[,1])
temp[,1]<-trim(temp[,1])
temp<-temp[,c(1, 4:17)]
SAAR2010<-temp
SAAR2010[86,1]<-"Jefferson County"
SAAR2010[144,1]<-"Raceland Independent"
SAAR2010[164,1]<-"Walton Verona Independent"
setdiff(district.index$DISTRICT, SAAR2010$DISTRICT) #Providence & Harrodsburg still missing
setdiff(SAAR2010$DISTRICT, district.index$DISTRICT)

#clean 2011
temp<-subset(SAAR2011, (grepl("TOTALS", SAAR2011[,1])))
temp[,1]<-sub("TOTALS for ", "",temp[,1])
temp[,1]<-trim(temp[,1])
temp<-temp[,c(1, 3:17)]
SAAR2011<-temp
SAAR2011[86,1]<-"Jefferson County"
SAAR2011[144,1]<-"Raceland Independent"
SAAR2011[164,1]<-"Walton Verona Independent"
setdiff(district.index$DISTRICT, SAAR2011$DISTRICT) #Providence & Harrodsburg still missing
setdiff(SAAR2011$DISTRICT, district.index$DISTRICT)

#clean 2012
temp<-subset(SAAR2012, (grepl("TOTALS", SAAR2012[,1])))
temp[,1]<-sub("TOTALS for ", "",temp[,1])
temp[,1]<-trim(temp[,1])
temp<-temp[,c(1, 3:17)]
SAAR2012<-temp
SAAR2012[144,1]<-"Raceland Independent"
SAAR2012[164,1]<-"Walton Verona Independent"
setdiff(district.index$DISTRICT, SAAR2012$DISTRICT) #Providence & Harrodsburg still missing
setdiff(SAAR2012$DISTRICT, district.index$DISTRICT)

#merge 1999
   
SAAR.1999.2012<-merge(district.index, SAAR1999[, c(grep("DISTRICT", names(SAAR1999)),
                                                   grep("X8", names(SAAR1999)), 
                                                   grep("X9", names(SAAR1999)), 
                                                   grep("X12", names(SAAR1999)),
                                                   grep ("TOTAL", names (SAAR1999)))], 
                                                       by="DISTRICT")
class(SAAR.1999.2012[,1])
SAAR.1999.2012[,1]<-as.character(SAAR.1999.2012[,1])
#merge 2000
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2000[,c(grep("DISTRICT", names(SAAR2000)),
                                                  grep("X8", names(SAAR2000)), 
                                                  grep("X9", names(SAAR2000)), 
                                                  grep("X12", names(SAAR2000)),
                                                  grep("TOTAL", names (SAAR2000)))], 
                                                       by="DISTRICT")
#merge 2001
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2001[,c(grep("DISTRICT", names(SAAR2001)),
                                                  grep("X8", names(SAAR2001)), 
                                                  grep("X9", names(SAAR2001)), 
                                                  grep("X12", names(SAAR2001)),
                                                  grep("TOTAL", names (SAAR2001)))], 
                                                       by="DISTRICT")
#merge 2002
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2002[, c(grep("DISTRICT", names(SAAR2002)),
                                                   grep("X8", names(SAAR2002)), 
                                                   grep("X9", names(SAAR2002)), 
                                                   grep("X12", names(SAAR2002)),
                                                   grep("TOTAL", names(SAAR2002)))], 
                                                       by="DISTRICT")
#merge 2003
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2003[, c(grep("DISTRICT", names(SAAR2003)),
                                                   grep("X8", names(SAAR2003)), 
                                                   grep("X9", names(SAAR2003)), 
                                                   grep("X12", names(SAAR2003)),
                                                   grep("TOTAL", names(SAAR2003)))], 
                                                       by="DISTRICT")
#merge 2004
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2004[, c(grep("DISTRICT", names(SAAR2004)),
                                                   grep("X8", names(SAAR2004)), 
                                                   grep("X9", names(SAAR2004)), 
                                                   grep("X12", names(SAAR2004)),
                                                   grep("TOTAL", names(SAAR2004)))], 
                                                       by="DISTRICT")
#merge 2005
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2005[, c(grep("DISTRICT", names(SAAR2005)),
                                                   grep("X8", names(SAAR2005)), 
                                                   grep("X9", names(SAAR2005)), 
                                                   grep("X12", names(SAAR2005)),
                                                   grep("TOTAL", names(SAAR2005)))], 
                                                       by="DISTRICT")
#merge 2006--Drop Harrodsburg
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2006[, c(grep("DISTRICT", names(SAAR2006)),
                                                   grep("X8", names(SAAR2006)), 
                                                   grep("X9", names(SAAR2006)), 
                                                   grep("X12", names(SAAR2006)),
                                                   grep("TOTAL", names(SAAR2006)))], 
                                                       by="DISTRICT")
#merge 2007--Drop Providence
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2007[, c(grep("DISTRICT", names(SAAR2007)),
                                                   grep("X8", names(SAAR2007)), 
                                                   grep("X9", names(SAAR2007)), 
                                                   grep("X12", names(SAAR2007)),
                                                   grep("TOTAL", names(SAAR2007)))], 
                                                       by="DISTRICT")
#merge 2008
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2008[, c(grep("DISTRICT", names(SAAR2008)),
                                                   grep("X8", names(SAAR2008)), 
                                                   grep("X9", names(SAAR2008)), 
                                                   grep("X12", names(SAAR2008)),
                                                   grep("TOTAL", names (SAAR2008)))], 
                                                       by="DISTRICT")
#merge 2009
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2009[, c(grep("DISTRICT", names(SAAR2009)),
                                                   grep("X8", names(SAAR2009)), 
                                                   grep("X9", names(SAAR2009)), 
                                                   grep("X12", names(SAAR2009)),
                                                   grep("TOTAL", names (SAAR2009)))], 
                                                       by="DISTRICT")
#merge 2010
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2010[, c(grep("DISTRICT", names(SAAR2010)),
                                                   grep("X8", names(SAAR2010)), 
                                                   grep("X9", names(SAAR2010)), 
                                                   grep("X12", names(SAAR2010)),
                                                   grep("TOTAL", names(SAAR2010)))], 
                                                       by="DISTRICT")
#merge 2011
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2011[, c(grep("DISTRICT", names(SAAR2011)),
                                                   grep("X8", names(SAAR2011)), 
                                                   grep("X9", names(SAAR2011)), 
                                                   grep("X12", names(SAAR2011)),
                                                   grep("TOTAL", names(SAAR2011)))], 
                                                       by="DISTRICT")
#merge 2012
SAAR.1999.2012<-merge(SAAR.1999.2012, SAAR2012[, c(grep("DISTRICT", names(SAAR2012)),
                                                   grep("X8", names(SAAR2012)), 
                                                   grep("X9", names(SAAR2012)), 
                                                   grep("X12", names(SAAR2012)),
                                                   grep("TOTAL", names(SAAR2012)))], 
                                                       by="DISTRICT")

#name columns
a<-c("GR8E", "GR9E", "GR12E", "TOTAL")
names(SAAR.1999.2012) <- c("DISTRICT", paste(a, rep(1999:2012, each=4), "KDE", sep="."))
rm(a)


#Save R object
write.table(SAAR.1999.2012, file = "SAAR.1999.2012.csv", sep = ",")

# remove SAAR spreadsheets
rm(list = ls())



