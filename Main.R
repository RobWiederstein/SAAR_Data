Clean.SAAR   <- function (){
library (gdata)
#SAAR Cleanup Scripts 1.R
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

SAAR2013<-read.csv(paste (wd ,"/data sets/SAAR2013.csv", sep = ""), header=TRUE, sep=",", 
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
setdiff(SAAR2002$DISTRICT, district.index$DISTRICT)

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

#clean 2013
temp <- subset(SAAR2013, (grepl("TOTALS", SAAR2013[,1])))
temp[,1] <- sub("TOTALS for ", "",temp[,1])
temp[,1] <- trim(temp[,1])
temp <- temp[,c(1, 3:17)]
SAAR2013 <- temp
SAAR2013[143,1] <- "Raceland Independent"
SAAR2013[163,1] <- "Walton Verona Independent"
setdiff(district.index$DISTRICT, SAAR2013$DISTRICT) #Providence, Harrodsburg & Monticello
                                                    #still missing
setdiff(SAAR2013$DISTRICT, district.index$DISTRICT)


#merge 1999

SAAR.1999.2013<-merge(district.index, SAAR1999[, c(grep("DISTRICT", names(SAAR1999)),
                                                   grep("X8", names(SAAR1999)), 
                                                   grep("X9", names(SAAR1999)), 
                                                   grep("X12", names(SAAR1999)),
                                                   grep ("TOTAL", names (SAAR1999)))], 
                      by="DISTRICT")
class(SAAR.1999.2013[,1])
SAAR.1999.2013[,1]<-as.character(SAAR.1999.2013[,1])
#merge 2000
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2000[,c(grep("DISTRICT", names(SAAR2000)),
                                                  grep("X8", names(SAAR2000)), 
                                                  grep("X9", names(SAAR2000)), 
                                                  grep("X12", names(SAAR2000)),
                                                  grep("TOTAL", names (SAAR2000)))], 
                      by="DISTRICT")
#merge 2001
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2001[,c(grep("DISTRICT", names(SAAR2001)),
                                                  grep("X8", names(SAAR2001)), 
                                                  grep("X9", names(SAAR2001)), 
                                                  grep("X12", names(SAAR2001)),
                                                  grep("TOTAL", names (SAAR2001)))], 
                      by="DISTRICT")
#merge 2002
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2002[, c(grep("DISTRICT", names(SAAR2002)),
                                                   grep("X8", names(SAAR2002)), 
                                                   grep("X9", names(SAAR2002)), 
                                                   grep("X12", names(SAAR2002)),
                                                   grep("TOTAL", names(SAAR2002)))], 
                      by="DISTRICT")
#merge 2003
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2003[, c(grep("DISTRICT", names(SAAR2003)),
                                                   grep("X8", names(SAAR2003)), 
                                                   grep("X9", names(SAAR2003)), 
                                                   grep("X12", names(SAAR2003)),
                                                   grep("TOTAL", names(SAAR2003)))], 
                      by="DISTRICT")
#merge 2004
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2004[, c(grep("DISTRICT", names(SAAR2004)),
                                                   grep("X8", names(SAAR2004)), 
                                                   grep("X9", names(SAAR2004)), 
                                                   grep("X12", names(SAAR2004)),
                                                   grep("TOTAL", names(SAAR2004)))], 
                      by="DISTRICT")
#merge 2005
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2005[, c(grep("DISTRICT", names(SAAR2005)),
                                                   grep("X8", names(SAAR2005)), 
                                                   grep("X9", names(SAAR2005)), 
                                                   grep("X12", names(SAAR2005)),
                                                   grep("TOTAL", names(SAAR2005)))], 
                      by="DISTRICT")
#merge 2006--Drop Harrodsburg
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2006[, c(grep("DISTRICT", names(SAAR2006)),
                                                   grep("X8", names(SAAR2006)), 
                                                   grep("X9", names(SAAR2006)), 
                                                   grep("X12", names(SAAR2006)),
                                                   grep("TOTAL", names(SAAR2006)))], 
                      by="DISTRICT")
#merge 2007--Drop Providence
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2007[, c(grep("DISTRICT", names(SAAR2007)),
                                                   grep("X8", names(SAAR2007)), 
                                                   grep("X9", names(SAAR2007)), 
                                                   grep("X12", names(SAAR2007)),
                                                   grep("TOTAL", names(SAAR2007)))], 
                      by="DISTRICT")
#merge 2008
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2008[, c(grep("DISTRICT", names(SAAR2008)),
                                                   grep("X8", names(SAAR2008)), 
                                                   grep("X9", names(SAAR2008)), 
                                                   grep("X12", names(SAAR2008)),
                                                   grep("TOTAL", names (SAAR2008)))], 
                      by="DISTRICT")
#merge 2009
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2009[, c(grep("DISTRICT", names(SAAR2009)),
                                                   grep("X8", names(SAAR2009)), 
                                                   grep("X9", names(SAAR2009)), 
                                                   grep("X12", names(SAAR2009)),
                                                   grep("TOTAL", names (SAAR2009)))], 
                      by="DISTRICT")
#merge 2010
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2010[, c(grep("DISTRICT", names(SAAR2010)),
                                                   grep("X8", names(SAAR2010)), 
                                                   grep("X9", names(SAAR2010)), 
                                                   grep("X12", names(SAAR2010)),
                                                   grep("TOTAL", names(SAAR2010)))], 
                      by="DISTRICT")
#merge 2011
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2011[, c(grep("DISTRICT", names(SAAR2011)),
                                                   grep("X8", names(SAAR2011)), 
                                                   grep("X9", names(SAAR2011)), 
                                                   grep("X12", names(SAAR2011)),
                                                   grep("TOTAL", names(SAAR2011)))], 
                      by="DISTRICT")
#merge 2012
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2012[, c(grep("DISTRICT", names(SAAR2012)),
                                                   grep("X8", names(SAAR2012)), 
                                                   grep("X9", names(SAAR2012)), 
                                                   grep("X12", names(SAAR2012)),
                                                   grep("TOTAL", names(SAAR2012)))], 
                      by="DISTRICT")
#merge 2013
SAAR.1999.2013<-merge(SAAR.1999.2013, SAAR2013[, c(grep("DISTRICT", names(SAAR2013)),
                                                   grep("X8", names(SAAR2013)), 
                                                   grep("X9", names(SAAR2013)), 
                                                   grep("X12", names(SAAR2013)),
                                                   grep("TOTAL", names(SAAR2013)))], 
                      by="DISTRICT")
#name columns
a<-c("GR8E", "GR9E", "GR12E", "TOTAL")
names(SAAR.1999.2013) <- c("DISTRICT", paste(a, rep(1999:2013, each=4), "KDE", sep="."))
rm(a)


#Save as R object to load in later script
saar.new <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
write.table (SAAR.1999.2013, file = saar.new, sep = ",")
}
Clean.AFGR   <- function (){
#AFGR data is for 2008-2012.
#David Curd with KDE advised that AFGR data is lagged one year.
#2012 data would be for 2010-11 School Year
#KDE's naming convention is to refer to the second year of the 2010-11 school year.

wd <- getwd()
file <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
saar <- read.table (file, header = TRUE, sep = ",")

AFGR <- read.csv(file = paste (wd, "data sets", "AFGR_2012.csv", sep = "/"), 
                 sep = ",", 
                 header = TRUE,
                 strip.white = TRUE,
                 colClasses = "character")
attach (AFGR)

#Diploma Recipients from KDE (DR.**) by year.  Because 1 yr lag, 2012 is for 2010-2011 school year.
#Code uses first year of school year, contrary to KDE naming conventions
DR.06 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2008'), c (1, 5, 10)]
DR.07 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2009'), c (1, 5, 10)]
DR.08 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2010'), c (1, 5, 10)]
DR.09 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2011'), c (1, 5, 10)]
DR.10 <- AFGR[ which(Gender=='Total' & Ethnicity=='Total' & School.Name == 'DISTRICT TOTAL' & School.Year == '2012'), c (1, 5, 10)]   
rm (AFGR)
detach (AFGR)

#create a common column for merge with SAAR data
b <- DR.06$District.Name
b[84]  <- "Jefferson County"
b[91]  <- "LaRue County"
b[110] <- "McCracken County"
b[111] <- "McCreary County"
b[112] <- "McLean County"
b[142] <- "Raceland Independent"
b[160] <- "Walton Verona Independent"

#assign common column to DR.** for merge
DR.06[,2] <- b
DR.07[,2] <- b
DR.08[,2] <- b
DR.09[,2] <- b
DR.10[,2] <- b

#assign common column name "DISTRICT"
names(DR.06)[2] <- "DISTRICT"
names(DR.07)[2] <- "DISTRICT"
names(DR.08)[2] <- "DISTRICT"
names(DR.09)[2] <- "DISTRICT"
names(DR.10)[2] <- "DISTRICT"

#merge
afgr.06.10 <- merge(DR.06, DR.07, by = "DISTRICT")
afgr.06.10 <- merge(afgr.06.10, DR.08, by = "DISTRICT")
afgr.06.10 <- merge(afgr.06.10, DR.09, by = "DISTRICT")
afgr.06.10 <- merge(afgr.06.10, DR.10, by = "DISTRICT")
afgr.06.10 <- afgr.06.10 [, c(1,3,5,7,9,11)]
names(afgr.06.10)[2:6] <- paste ("Grad.w.Diploma.in.4.years", 2006:2010, "KDE", sep = ".")

#Save as R object to load in later script
afgr.new <- paste (wd, "objects", "afgr.06.10.csv", sep = "/")
write.table (afgr.06.10, file = afgr.new, sep = ",")
}
Clean.GRAD   <- function (){
  #Grad Rate 2003-2007 was sent via email from Kentucky Department of Education
  #No longer available on website. Disaggregated by school district
  
  wd <- getwd()
  file <- paste (wd, "data sets", "Grad Rate 2003-2007.csv", sep = "/")
  gr <- read.csv (file = file, sep = ",", header = TRUE,
                  strip.white = TRUE,
                  colClasses = "character")
  
  #pull out columns for 4 year graduates
  attach (gr)
  gr <- gr [, c(4, 5, grep ("GRADS_4YR", names (gr)))]
  gr <- gr [SCHNAME == "---DISTRICT TOTAL---", ]
  gr <- gr [, -2]
  detach (gr)
  
  names(gr)[2:6] <- paste ("Grad.w.Diploma.in.4.years", 2002:2006, "KDE", sep = ".")
  
  #Save as R object to load in later script
  gr.new <- paste (wd, "objects", "gr.2002.2006.csv", sep = "/")
  write.csv(gr, file = gr.new)
}

Clean.KYGR   <- function (){
  #build table for Kentucky state-wide reported graduation rates
  library (ggplot2)
  wd <- getwd()
  
  #get grad rate data from KDE
  file <- paste (wd, "data sets", "Grad Rate 2003-2007.csv", sep = "/")
  gr <- read.csv (file, header = T, sep = ",", as.is = T,
                  strip.white = T)
  
  #get afgr data from KDE
  file <- paste (wd, "data sets", "AFGR_2012.csv", sep = "/")
  afgr <- read.csv (file, header = T, sep = ",", as.is = T,
                    strip.white = T)
  
  
  # get state totals for 2003-2007
  gr <- gr[gr$DISTNAME == "STATE", ]
  gr <- gr [, grep ("GRAD_", names (gr))]
  
  #get state totals for 2007 to 2012
  afgr <- afgr [afgr$School.Name == "STATE TOTAL" & afgr$Gender == "Total" & 
                  afgr$Ethnicity == "Total", c(1, 5, 19)]
  
  #combine in new table for 2003:2012
  Year <- 2003:2012
  State <- rep ("KY", 10)
  Reported <- (c(as.numeric (gr[1, 1:5]), as.numeric (afgr[,3]))) / 100
  Method <- c(rep ("NA", 5), rep ("AFGR", 5))
  Ky.grad.rate <- (cbind (Year, State, Reported, Method))
  Ky.grad.rate <- as.data.frame (Ky.grad.rate, row.names = 1:10, stringsAsFactors = F)
  Ky.grad.rate$Year <- as.Date (Ky.grad.rate$Year, "%Y")
  Ky.grad.rate$Reported <- as.numeric (Ky.grad.rate$Reported)
  
  #Save as R object to load in later script
  file <- paste (wd, "objects", "State.grad.rate.2003.2012.KDE.csv", sep = "/")
  write.table (Ky.grad.rate, file = file, sep = ",")
}

Clean.ELSI   <- function (){
  #ELSI Cleanup Script
  wd <- getwd()
  file <- paste (wd, "objects", "SAAR.1999.2013.csv", sep = "/")
  saar <- read.table (file, header = TRUE, sep = ",",
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
  
  #Save as R object to load in later script
  elsi.new <- paste(wd, "objects", "elsi_new.csv", sep = "/")
  write.table (elsi, file = elsi.new, sep = ",")
}
Clean.FRDL   <- function (){
#need the free and reduced lunch for 2012.  KDE website for school report card

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
}
Clean.SDCP   <- function (){
#http://www.census.gov/did/www/saipe/data/schools/index.html
#Poverty rates by school district b/c FRL crude
#get saar and subset to index
wd <-getwd()
file  <- paste (wd, "objects", "SAAR.1999.2012.csv", sep = "/")
saar <- read.csv (file, as.is = T)
index <- as.data.frame (saar[,1])
names (index) <- "DISTRICT"

#import and clean 2003
wd <- getwd()
file <- paste (wd, "data sets", "sd03_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd03 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd03 <- sd03 [, 3:6]
names (sd03) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd03 [,4] / sd03 [,3], 4)
sd03 <- cbind (sd03, PCT.CH.POV.5.17)
names (sd03)[-1] <- paste (names(sd03)[-1], "2003", sep = ".")
sd03$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd03$DISTRICT)

#match index columns up for merge
#change first letter to cap
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

a <-  tolower(sd03$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd03$DISTRICT <- a

sd03$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd03$DISTRICT[96] <- "LaRue County"
sd03$DISTRICT[115] <- "McCracken County"
sd03$DISTRICT[116] <- "McCreary County"
sd03$DISTRICT[117] <- "McLean County"


#school district child poverty rate
sdcp <- merge (index, sd03)

#import and clean 2004
wd <- getwd()
file <- paste (wd, "data sets", "sd04_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd04 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd04 <- sd04 [, 3:6]
names (sd04) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd04 [,4] / sd04 [,3], 4)
sd04 <- cbind (sd04, PCT.CH.POV.5.17)
names (sd04)[-1] <- paste (names(sd04)[-1], "2004", sep = ".")

sd04$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd04$DISTRICT)
a <-  tolower(sd04$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd04$DISTRICT <- a

sd04$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd04$DISTRICT[96] <- "LaRue County"
sd04$DISTRICT[115] <- "McCracken County"
sd04$DISTRICT[116] <- "McCreary County"
sd04$DISTRICT[117] <- "McLean County"

sdcp <- merge (sdcp, sd04)

#inport and clean 2005
wd <- getwd()
file <- paste (wd, "data sets", "sd05_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd05 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd05 <- sd05 [, 3:6]
names (sd05) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd05 [,4] / sd05 [,3], 4)
sd05 <- cbind (sd05, PCT.CH.POV.5.17)
names (sd05)[-1] <- paste (names(sd05)[-1], "2005", sep = ".")

sd05$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd05$DISTRICT)
a <-  tolower(sd05$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd05$DISTRICT <- a

sd05$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd05$DISTRICT[96] <- "LaRue County"
sd05$DISTRICT[115] <- "McCracken County"
sd05$DISTRICT[116] <- "McCreary County"
sd05$DISTRICT[117] <- "McLean County"

sdcp <- merge (sdcp, sd05)

#import and clean 2006
wd <- getwd()
file <- paste (wd, "data sets", "sd06_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd06 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd06 <- sd06 [, 3:6]
names (sd06) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd06 [,4] / sd06 [,3], 4)
sd06 <- cbind (sd06, PCT.CH.POV.5.17)
names (sd06)[-1] <- paste (names(sd06)[-1], "2006", sep = ".")

sd06$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd06$DISTRICT)
a <-  tolower(sd06$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd06$DISTRICT <- a

sd06$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd06$DISTRICT[96] <- "LaRue County"
sd06$DISTRICT[115] <- "McCracken County"
sd06$DISTRICT[116] <- "McCreary County"
sd06$DISTRICT[117] <- "McLean County"

sdcp <- merge (sdcp, sd06)

#import and clean 2007
wd <- getwd()
file <- paste (wd, "data sets", "sd07_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd07 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd07 <- sd07 [, 3:6]
names (sd07) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd07 [,4] / sd07 [,3], 4)
sd07 <- cbind (sd07, PCT.CH.POV.5.17)
names (sd07)[-1] <- paste (names(sd07)[-1], "2007", sep = ".")

sd07$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd07$DISTRICT)
a <-  tolower(sd07$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd07$DISTRICT <- a

sd07$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd07$DISTRICT[96] <- "LaRue County"
sd07$DISTRICT[115] <- "McCracken County"
sd07$DISTRICT[116] <- "McCreary County"
sd07$DISTRICT[117] <- "McLean County"

#Laurel & Meade County missing
Laurel.County <- c("Laurel County", rep (NA, 4))
Meade.County <- c("Meade County", rep (NA, 4))
sd07 <- rbind (sd07, Laurel.County, Meade.County)

sdcp <- merge (sdcp, sd07)

#import and clean 2008
wd <- getwd()
file <- paste (wd, "data sets", "sd08_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd08 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd08 <- sd08 [, 3:6]
names (sd08) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd08 [,4] / sd08 [,3], 4)
sd08 <- cbind (sd08, PCT.CH.POV.5.17)
names (sd08)[-1] <- paste (names(sd08)[-1], "2008", sep = ".")

sd08$DISTRICT <- gsub (" SCHOOL DISTRICT", "", sd08$DISTRICT)
a <-  tolower(sd08$DISTRICT)
a <- sapply(a, simpleCap)
a <- as.character (a)
sd08$DISTRICT <- a

sd08$DISTRICT[54] <- "Erlanger-Elsmere Independent"
sd08$DISTRICT[96] <- "LaRue County"
sd08$DISTRICT[115] <- "McCracken County"
sd08$DISTRICT[116] <- "McCreary County"
sd08$DISTRICT[117] <- "McLean County"

#Laurel & Meade County missing
Laurel.County <- c("Laurel County", rep (NA, 4))
Meade.County <- c("Meade County", rep (NA, 4))
sd08 <- rbind (sd08, Laurel.County, Meade.County)

sdcp <- merge (sdcp, sd08)

#import and clean 2009
wd <- getwd()
file <- paste (wd, "data sets", "sd09_KY.txt",  sep = "/")
widths <- c(2, 6, 65, 10, 10, 9, 21)
sd09 <- read.fwf(file = file, widths = widths, as.is = T, strip.white = T)
sd09 <- sd09 [, 3:6]
names (sd09) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (sd09 [,4] / sd09 [,3], 4)
sd09 <- cbind (sd09, PCT.CH.POV.5.17)
names (sd09)[-1] <- paste (names(sd09)[-1], "2009", sep = ".")

sd09$DISTRICT <- gsub (" School District", "", sd09$DISTRICT)

#Laurel & Meade County are back

#name changes and hyphens
sd09$DISTRICT[95] <- "LaRue County"
sd09$DISTRICT[146] <- "Raceland Independent"
sd09$DISTRICT[166] <- "Walton Verona Independent"

#merge
sdcp <- merge (sdcp, sd09)

#import and clean 2010
wd <- getwd()
file <- paste (wd, "data sets", "sd10_KY.txt",  sep = "/")
widths <- c(2, 6, 72, 10, 10, 8, 21)
sd10 <- read.fwf(file = file, widths = widths, colClasses = "character", strip.white = T)
sd10 <- sd10 [, 3:6]
names (sd10) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (as.integer (sd10 [,4]) / as.integer (sd10 [,3]), 4)
sd10 <- cbind (sd10, PCT.CH.POV.5.17)
names (sd10)[-1] <- paste (names(sd10)[-1], "2010", sep = ".")
sd10$DISTRICT <- gsub (" School District", "", sd10$DISTRICT)

#name changes and hyphens
sd10$DISTRICT[95] <- "LaRue County"
sd10$DISTRICT[146] <- "Raceland Independent"
sd10$DISTRICT[166] <- "Walton Verona Independent"

sdcp <- merge (sdcp, sd10)

#import and clean 2011
wd <- getwd()
file <- paste (wd, "data sets", "sd11_KY.txt",  sep = "/")
widths <- c(2, 6, 72, 10, 10, 8, 21)
sd11 <- read.fwf(file = file, widths = widths, colClasses = "character", strip.white = T)
sd11 <- sd11 [, 3:6]
names (sd11) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (as.integer (sd11 [,4]) / as.integer (sd11 [,3]), 4)
sd11 <- cbind (sd11, PCT.CH.POV.5.17)
names (sd11)[-1] <- paste (names(sd11)[-1], "2011", sep = ".")
sd11$DISTRICT <- gsub (" School District", "", sd11$DISTRICT)

#name changes and hyphens
sd11$DISTRICT[95] <- "LaRue County"
sd11$DISTRICT[146] <- "Raceland Independent"
sd11$DISTRICT[166] <- "Walton Verona Independent"

sdcp <- merge (sdcp, sd11)

#import and clean 2012
wd <- getwd()
file <- paste (wd, "data sets", "sd12_KY.txt",  sep = "/")
widths <- c(2, 6, 72, 10, 10, 8, 21)
sd12 <- read.fwf(file = file, widths = widths, colClasses = "character", strip.white = T)
sd12 <- sd12 [, 3:6]
names (sd12) <- c("DISTRICT", "TOT.POP", "AGE.5.17", "EST.POV.5.17")
PCT.CH.POV.5.17 <- round (as.integer (sd12 [,4]) / as.integer (sd12 [,3]), 4)
sd12 <- cbind (sd12, PCT.CH.POV.5.17)
names (sd12)[-1] <- paste (names(sd12)[-1], "2012", sep = ".")
sd12$DISTRICT <- gsub (" School District", "", sd12$DISTRICT)

#name changes and hyphens
sd12$DISTRICT[95] <- "LaRue County"
sd12$DISTRICT[146] <- "Raceland Independent"
sd12$DISTRICT[166] <- "Walton Verona Independent"

sdcp <- merge (sdcp, sd12)

#pull out percentages
sdcp <- sdcp[, c(1, grep ("PCT", names (sdcp)))]

#save in objects
wd <- getwd()
file <- paste (wd, "objects", "Child.Poverty.by.School.District.Census.2003.2012.csv", sep = "/")
write.csv (sdcp, file)
}
Build.CUM    <- function (){
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
cp   <- read.csv (paste (wd, "objects", "tot.cp.2001.2012.csv", sep = "/" ),
                sep = ",", header = T, as.is = TRUE)
gr   <- read.csv (paste (wd, "objects", "gr.2003.2007.csv", sep = "/" ),
                sep = ",", header = T, as.is = TRUE)
frd  <- read.csv (paste (wd, "objects", "FRD.Lunch.Pct.2012.KDE.csv", sep = "/" ),
                 sep = ",", header = T, as.is = TRUE)

sdcp <- read.csv (paste (wd, "objects", "Child.Poverty.by.School.District.Census.2003.2012.csv", sep = "/"),
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
sdcp <- merge (index, sdcp)

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
     saar$GR9E.2008.KDE
tot.2012 <- cbind (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
                   FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
                   Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
                   Gr.12.Gr.9.ratio.KDE)
#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2012
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2012 <- cbind (tot.2012, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
     Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
     Gr.12.Gr.9.ratio.KDE)

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

#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2011
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2011 <- cbind (tot.2011, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
     Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
     Gr.12.Gr.9.ratio.KDE)

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

#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2010
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2010 <- cbind (tot.2010, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm    (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
       FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
       Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
       Gr.12.Gr.9.ratio.KDE)

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
#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2009
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2009 <- cbind (tot.2009, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm  (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
     FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
     Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
     Gr.12.Gr.9.ratio.KDE)

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
#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2008
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2008 <- cbind (tot.2008, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

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
#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2007
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2007 <- cbind (tot.2007, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)

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

#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2006
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2006 <- cbind (tot.2006, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)

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

#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2005
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2005 <- cbind (tot.2005, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)

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
#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2004
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2004 <- cbind (tot.2004, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)


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

#add census data
School.Dist.Child.Poverty.Pct  <- sdcp$PCT.CH.POV.5.17.2003
School.Dist.Child.Poverty.Decile <- cut_number (School.Dist.Child.Poverty.Pct, n = 10)
levels (School.Dist.Child.Poverty.Decile) <- c(paste ("D0", 1:9, sep = ""), "D10")
tot.2003 <- cbind (tot.2003, School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
rm (School.Dist.Child.Poverty.Pct, School.Dist.Child.Poverty.Decile)
#end census data

rm   (District, Year, Tot.Enrollment.saar, Tot.Enrollment.elsi,
      FRD.Lunch.Pct, FRD.Decile, Child.Poverty.Pct, Poverty.Decile, 
      Gr.8.Cohort.KDE, Gr.9.Cohort.KDE, Gr.12.Gr.8.ratio.KDE, 
      Gr.12.Gr.9.ratio.KDE)


cum.2003.2012 <- rbind (tot.2012, tot.2011, tot.2010, tot.2009, tot.2008, tot.2007,
                        tot.2006, tot.2005, tot.2004, tot.2003)


#Save as R object to load in later script
cumulative <- paste (wd, "objects", "cum.2003.2012.csv", sep = "/")
write.table (cum.2003.2012, file = cumulative, sep = ",")
}
Build.NAT    <- function (){
#build national eigth grade cohort per Heckman supplementary material
wd <- getwd()
file <- paste (wd, "data sets", "ELSI_csv_export_6353406681986067459539.csv", sep = "/")
gr <- read.csv (file, header = T, sep = ",", strip.white = T, skip = 6, nrow = 51,
                colClasses = "character")

#set to integer
for (i in 2:ncol (gr)){
  gr[,i] <- as.integer (gr [, i])
}

#eliminate NAs
begin.states <- gr[,1]
apply(gr, 2, function(x) length(which(is.na(x))))
gr <- gr[,1:30]  #25 NAs in column 31
gr <- gr[complete.cases(gr[, 1:30]), ]
remain.states <- gr [,1]
exclude <- setdiff (begin.states, remain.states) #DC, NY, PN, SC, WI excluded

#shorten variable names
names (gr) <- sub("Grade", "Gr", names (gr))
names (gr) <- sub ("Diploma", "Dipl", names (gr))
names (gr) <- sub ("State", "", names (gr))
names (gr) <- sub ("Students", "Stud", names (gr))
names (gr) <- sub ("Recipients", "Reci", names (gr))
names (gr) <- gsub ("..", ".", names (gr), fixed = T)
names (gr) <- gsub ("..", ".", names (gr), fixed = T)
names (gr) <- gsub ("..", ".", names (gr), fixed = T)
names (gr) <- gsub ("..", ".", names (gr), fixed = T)

#total columns
total <- colSums (gr[,2:ncol (gr)], na.rm = T)
total <- as.data.frame (total)
total <- t (total)
total <-as.data.frame (total)



Gr.8.C.National.2009.10 <- total$Dipl.Reci.2009.10 / total$Gr.8.Stud.2004.05
Gr.8.C.National.2008.09 <- total$Dipl.Reci.2008.09 / total$Gr.8.Stud.2003.04
Gr.8.C.National.2007.08 <- total$Dipl.Reci.2007.08 / total$Gr.8.Stud.2002.03
Gr.8.C.National.2006.07 <- total$Dipl.Reci.2006.07 / total$Gr.8.Stud.2001.02
Gr.8.C.National.2005.06 <- total$Dipl.Reci.2005.06 / total$Gr.8.Stud.2000.01
Gr.8.C.National.2004.05 <- total$Dipl.Reci.2004.05 / total$Gr.8.Stud.1999.00
Gr.8.C.National.2003.04 <- total$Dipl.Reci.2003.04 / total$Gr.8.Stud.1998.99
Gr.8.C.National.2002.03 <- total$Dipl.Reci.2002.03 / total$Gr.8.Stud.1997.98


Nat.Grad.Rate <- cbind (Gr.8.C.National.2002.03,   Gr.8.C.National.2003.04,
                        Gr.8.C.National.2004.05,   Gr.8.C.National.2005.06,   Gr.8.C.National.2006.07, Gr.8.C.National.2007.08,
                        Gr.8.C.National.2008.09,   Gr.8.C.National.2009.10) 

Nat.Grad.Rate <- as.data.frame (t(Nat.Grad.Rate))
Year <- (2002:2009)
Nat.Grad.Rate <- cbind (Year, Nat.Grad.Rate)
a <- Nat.Grad.Rate$Year
a <- as.character (a)
a <- as.Date (a, "%Y")
Nat.Grad.Rate$Year <- a
names (Nat.Grad.Rate)[2] <- "Gr.8.Cohort.National"

#write object out
file <- paste (wd, "objects", "Nat.Gr.8.Cohort.2002.2009.csv", sep = "/")
write.csv (Nat.Grad.Rate, file = file)
}
Fig.CCD.KDE  <- function (){
#compare CCD data to KDEdata
# 4 plots
wd <- getwd()
saar <- read.csv (paste (wd, "objects", "SAAR.1999.2012.csv", sep = "/"), 
                  sep = ",", header = T, as.is = TRUE)
elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)

elsi <- elsi [, c(1, grep ("Grade.8", names (elsi)))]
elsi <- elsi [, c(1, 16:2)]
elsi <- elsi [, c(-2, -3)]
saar <- saar[, c(1, grep ("GR8E", names (saar)))]
elsi[2:14] <- sapply (elsi[2:14], as.integer)
names (elsi) <- gsub("Students..Public.School..", "", names(elsi))


Year <- 1999:2012
Grade.8.elsi <- as.array (colSums (elsi[2:14], na.rm = T))
Grade.8.elsi <- c(Grade.8.elsi, NA)
Grade.8.saar <- as.array (colSums (saar [2:15], na.rm = T))
Diff <- Grade.8.saar - Grade.8.elsi
new <- as.data.frame (cbind (Year, Grade.8.saar, Grade.8.elsi, Diff))

#Plot 8th Grade enrollment from CCD to KDE SAAR Report
library (ggplot2)
p1 <- ggplot(new, aes (Year, Grade.8.elsi))
p1 <- p1 + geom_line()
p1 <- p1 + ggtitle ("Kentucky Eighth Grade Enrollments") + ylab ("Students") + xlab ("Year")
p1 <- p1 + ylim (25000, 75000)
p1 <- p1 + scale_x_continuous (breaks = seq(from = 1999, to = 2012, by = 2))
p1 <- p1 + geom_line (aes (Year, Grade.8.saar), colour = "red")
p1 <- p1 + annotate ("text", x = 2011.5, y = 53000, label = "SAAR", colour = "red")
p1 <- p1 + annotate ("text", x = 2011.5, y = 48000, label = "CCD", colour = "black")
p1

#Compare 9th Grade enrollment from CCD to KDE SAAR Report
wd <- getwd()
saar <- read.csv (paste (wd, "objects", "SAAR.1999.2012.csv", sep = "/"), 
                  sep = ",", header = T, as.is = TRUE)
elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)

elsi <- elsi [, c(1, grep ("Grade.9", names (elsi)))]
elsi <- elsi [, c(1, 16:2)]
elsi <- elsi [, c(-2, -3)]
saar <- saar[, c(1, grep ("GR9E", names (saar)))]
elsi[2:14] <- sapply (elsi[2:14], as.integer)
nces <- c(colSums (elsi [2:14], na.rm = T), NA)
kde <- colSums (saar [2:15], na.rm = T)


Year <- c(1999:2012)
Diff <- kde - nces
new <- as.data.frame (cbind (Year, kde, nces, Diff))

#plot 
p1 <- ggplot (new, aes (Year, nces))
p1 <- p1 + geom_line()
p1 <- p1 + geom_line (aes (Year, kde), colour = "red")
p1 <- p1 + ylim (25000, 75000)
p1 <- p1 + scale_x_continuous (breaks = seq(from = 1999, to = 2012, by = 2))
p1 <- p1 + ggtitle ("Kentucky Ninth Grade Enrollments") + ylab ("Students")
p1 <- p1 + annotate ("text", x = 2011.5, y = 57000, label = "SAAR", colour = "red")
p1 <- p1 + annotate ("text", x = 2011, y = 50000, label = "CCD", colour = "black")
p1

#compare diploma recipients
wd <- getwd()
elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)
gr <- read.csv (paste (wd, "objects", "gr.2003.2007.csv", sep = "/" ),
                sep = ",", header = T, as.is = T)
afgr <- read.csv (paste (wd, "objects", "afgr.08.12.csv", sep = "/" ),
                  sep = ",", header = T, as.is = T)


elsi <- elsi[, c(1,grep ("Diploma.Recipients", names (elsi)))]
elsi <- elsi [, c(1, 24:2)]
names (elsi) <- gsub (".Recipients.......District.", "", names (elsi))
elsi <- elsi[, c(1, 19:24)]
elsi[, 2:7] <- sapply (elsi [,2:7], as.integer)
elsi <- colSums (elsi[,2:7], na.rm = T)
elsi[3] <- trunc((elsi[2] + elsi[4])/2, 0)
nces <- c(elsi, rep (NA, 4))


afgr <- colSums (afgr[2:6])
gr <- colSums (gr[3:7], na.rm = T)
kde <- c(gr, afgr)
diff <- kde - nces

Year <- 2003:2012
new <- as.data.frame (cbind (Year, kde, nces, diff))
#new$Year <- as.Date (as.character (new$Year), "%Y")

p1 <- ggplot (new, aes (Year, kde))
p1 <- p1 + geom_line(colour = "red")
p1 <- p1 + ylim (25000, 75000)
p1 <- p1 + scale_x_continuous (limits = c(2001, 2015), breaks = seq (from = 1999, to = 2015, by = 2))
p1 <- p1 + geom_line (aes(Year, nces), colour = "black")
p1 <- p1 + ylab ("Reg. Diplomas") + ggtitle ("Kentucky Diploma Recipients")
p1 <- p1 + annotate ("text", x = 2008, y = 44000, label = "CCD", colour = "black")
p1 <- p1 + annotate ("text", x = 2012, y = 39000, label = "KDE", colour = "red")
p1

# compare saar ratio to common core ratio
wd <- getwd()
elsi <- read.csv (paste (wd, "objects", "elsi_new.csv", sep = "/" ),
                  sep = ",", header = T, as.is = TRUE)
gr <- read.csv (paste (wd, "objects", "gr.2003.2007.csv", sep = "/" ),
                sep = ",", header = T, as.is = T)
afgr <- read.csv (paste (wd, "objects", "afgr.08.12.csv", sep = "/" ),
                  sep = ",", header = T, as.is = T)
saar <- read.csv (paste (wd, "objects", "SAAR.1999.2012.csv", sep = "/"), 
                  sep = ",", header = T, as.is = TRUE)

#kde data
saar <- saar[, c(1, grep ("GR8E", names (saar)))]
gr <- colSums (gr[3:7], na.rm = T)
afgr <- colSums (afgr[2:6])
saar <- saar[, 1:10]
saar <- colSums(saar[2:10])
dip.kde <- c(gr, afgr)
dip.kde <- dip.kde [-1]
gr.kde <- dip.kde / saar

#NCES data
dip.nces <- elsi[, c(1, grep ("Diploma.R", names (elsi)))]
names (dip.nces) <- gsub ("Diploma.Recipients.......District..", "D.R.", names (dip.nces))
dip.nces <- dip.nces[, c(1, 24:2)]
dip.nces <- dip.nces [, 19:24]
dip.nces <- sapply(dip.nces, as.integer)
dip.nces <- colSums (dip.nces, na.rm = T)
dip.nces [3] <- round ((38133 + 39086) / 2, 0) #data imputation for missing data in 2005
dip.nces <- dip.nces [-1]

enr.nces <- elsi [, c(1, grep("Grade.8", names (elsi)))]
names (enr.nces) <- gsub("Grade.8.Students..Public.School..", "G8.", names (enr.nces))
enr.nces <- enr.nces [, c(1, 16:2)]
enr.nces <- enr.nces [, c(-2,-3)]
enr.nces <- enr.nces [, c(2:6)]
enr.nces <- sapply (enr.nces, as.integer)
enr.nces <- colSums (enr.nces, na.rm = T)
gr.nces <- dip.nces / enr.nces
gr.nces <- c(gr.nces, rep (NA, 4))

#build table
Year <- 2004:2012
diff <- gr.nces - gr.kde
grad <- as.data.frame (cbind (Year, gr.kde, gr.nces, diff))

p1 <- ggplot(grad, aes (Year, gr.kde))
p1 <- p1 + geom_line(colour = "red")
p1 <- p1 + ylim (.6, .95)
p1 <- p1 + scale_x_continuous (limits = c(2001, 2015), breaks = seq(from = 2001, to = 2015, by = 2))
p1 <- p1 + geom_line (aes (Year, gr.nces), colour = "black")
p1 <- p1 + xlab("Year") + ylab ("Graduation Rate") + ggtitle ("Kentucky Eighth Grade Cohort")
p1 <- p1 + annotate ("text", x = 2008, y = .85, label = "CCD")
p1 <- p1 + annotate ("text" , x = 2012, y = .78, label = "KDE", colour = "red")
p1 <- p1 + annotate("rect", xmin = 2002, xmax = 2014, ymin = .65, ymax = .9,
                    alpha = .1)
p1 <- p1 + annotate ("text", x = 2013, y = .67, label = "NCLB", size = 5)

p1
}
Chart        <- function (){
library ("ggplot2")

#Figures
wd <- getwd()
file <- paste (wd, "objects", "cum.2003.2012.csv", sep = "/")
cum <- read.csv (file, sep = ",", header = T, as.is = T)
file <- paste (wd, "objects", "State.grad.rate.2003.2012.KDE.csv", sep = "/")
ky <- read.csv (file, sep = ",", header = T, as.is = T)
file <- paste (wd, "objects", "Nat.Gr.8.Cohort.2002.2009.csv", sep = "/")
nat <- read.csv (file, sep = ",", header = T, as.is = T)


#set years to "Date" format
cum$Year <- as.Date (as.character (cum$Year), "%Y")
ky$Year <- as.Date (ky$Year)

#set up 1st and 10th deciles for comparisons
FRD.D01 <- subset (cum, cum$FRD.Decile == "D01")
FRD.D10 <- subset (cum, cum$FRD.Decile =="D10")
CP.D01 <- subset (cum, cum$Poverty.Decile=="D01")
CP.D10 <- subset (cum, cum$Poverty.Decile=="D10")

#set up all deciles for School Poverty
SP.D01 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D01")
SP.D02 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D02")
SP.D03 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D03")
SP.D04 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D04")
SP.D05 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D05")
SP.D06 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D06")
SP.D07 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D07")
SP.D08 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D08")
SP.D09 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D09")
SP.D10 <- subset (cum, cum$School.Dist.Child.Poverty.Decile == "D10")

#set up averages of each decile for school poverty
Gr.8.Cohort.SP.D01 <-  tapply (SP.D01$Gr.8.Cohort.KDE, INDEX = SP.D01$Year, FUN = mean)
Gr.8.Cohort.SP.D02 <-  tapply (SP.D02$Gr.8.Cohort.KDE, INDEX = SP.D02$Year, FUN = mean)
Gr.8.Cohort.SP.D03 <-  tapply (SP.D03$Gr.8.Cohort.KDE, INDEX = SP.D03$Year, FUN = mean)
Gr.8.Cohort.SP.D04 <-  tapply (SP.D04$Gr.8.Cohort.KDE, INDEX = SP.D04$Year, FUN = mean)
Gr.8.Cohort.SP.D05 <-  tapply (SP.D05$Gr.8.Cohort.KDE, INDEX = SP.D05$Year, FUN = mean)
Gr.8.Cohort.SP.D06 <-  tapply (SP.D06$Gr.8.Cohort.KDE, INDEX = SP.D06$Year, FUN = mean)
Gr.8.Cohort.SP.D07 <-  tapply (SP.D07$Gr.8.Cohort.KDE, INDEX = SP.D07$Year, FUN = mean)
Gr.8.Cohort.SP.D08 <-  tapply (SP.D08$Gr.8.Cohort.KDE, INDEX = SP.D08$Year, FUN = mean)
Gr.8.Cohort.SP.D09 <-  tapply (SP.D09$Gr.8.Cohort.KDE, INDEX = SP.D09$Year, FUN = mean)
Gr.8.Cohort.SP.D10 <-  tapply (SP.D10$Gr.8.Cohort.KDE, INDEX = SP.D10$Year, FUN = mean)

#create data frame
Gr.8.Cohort.SP.All <- cbind (Gr.8.Cohort.SP.D01,
                             Gr.8.Cohort.SP.D02,
                             Gr.8.Cohort.SP.D03,
                             Gr.8.Cohort.SP.D04,
                             Gr.8.Cohort.SP.D05,
                             Gr.8.Cohort.SP.D06,
                             Gr.8.Cohort.SP.D07,
                             Gr.8.Cohort.SP.D08,
                             Gr.8.Cohort.SP.D09,
                             Gr.8.Cohort.SP.D10)
Gr.8.Cohort.SP.All <- as.data.frame (Gr.8.Cohort.SP.All)
Year <- as.Date (as.character (2003:2012), "%Y")
Gr.8.Cohort.SP.All <- cbind (Year, Gr.8.Cohort.SP.All)

#plot School Poverty by Deciles of 8 Grade Cohort
p1 <- ggplot (Gr.8.Cohort.SP.All, aes (Year, Gr.8.Cohort.SP.D01))
p1 <- p1 + geom_line ()
p1 <- p1 + ylim (c(.65, .95))
p1 <- p1 + geom_line (aes(Year, Gr.8.Cohort.SP.D10), colour = "green")
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.SP.D02), colour = "pink")
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.SP.D03), colour = "brown")
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.SP.D07), colour = "blue")
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.SP.D08), colour = "red")
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.SP.D09), colour = "yellow")
p1
#compare Grade 8
Gr.8.Cohort.by.year <- tapply (cum$Gr.8.Cohort.KDE, INDEX = cum$Year, FUN = mean)
Gr.8.Cohort.CP.D01 <-  tapply (CP.D01$Gr.8.Cohort.KDE, INDEX = CP.D01$Year, FUN = mean)
Gr.8.Cohort.CP.D10 <-  tapply (CP.D10$Gr.8.Cohort.KDE, INDEX = CP.D10$Year, FUN = mean)
Gr.8.Cohort.FRD.D01 <- tapply (FRD.D01$Gr.8.Cohort.KDE, INDEX = FRD.D01$Year, FUN = mean)
Gr.8.Cohort.FRD.D10 <- tapply (FRD.D10$Gr.8.Cohort.KDE, INDEX = FRD.D10$Year, FUN = mean)
Gr.8.Cohort.SP.D01 <-  tapply (SP.D01$Gr.8.Cohort.KDE, INDEX = SP.D01$Year, FUN = mean)
Gr.8.Cohort.SP.D10 <-  tapply (SP.D10$Gr.8.Cohort.KDE, INDEX = SP.D10$Year, FUN = mean)

#compare Grade 9
Gr.9.Cohort.by.year <- tapply (cum$Gr.9.Cohort.KDE, INDEX = cum$Year, FUN = mean)
Gr.9.Cohort.CP.D01 <- tapply (CP.D01$Gr.9.Cohort.KDE, INDEX = CP.D01$Year, FUN = mean)
Gr.9.Cohort.CP.D10 <- tapply (CP.D10$Gr.9.Cohort.KDE, INDEX = CP.D10$Year, FUN = mean)
Gr.9.Cohort.FRD.D01 <- tapply (FRD.D01$Gr.9.Cohort.KDE, INDEX = FRD.D01$Year, FUN = mean)
Gr.9.Cohort.FRD.D10 <- tapply (FRD.D10$Gr.9.Cohort.KDE, INDEX = FRD.D10$Year, FUN = mean)
Gr.9.Cohort.SP.D01 <-  tapply (SP.D01$Gr.9.Cohort.KDE, INDEX = SP.D01$Year, FUN = mean)
Gr.9.Cohort.SP.D10 <- tapply (SP.D10$Gr.9.Cohort.KDE, INDEX = SP.D10$Year, FUN = mean)

table <- rbind (Gr.8.Cohort.by.year, Gr.8.Cohort.CP.D01, Gr.8.Cohort.CP.D10, 
                Gr.8.Cohort.FRD.D01, Gr.8.Cohort.FRD.D10, Gr.8.Cohort.SP.D01, 
                Gr.8.Cohort.SP.D10, Gr.9.Cohort.by.year, Gr.9.Cohort.CP.D01, Gr.9.Cohort.CP.D10, 
                Gr.9.Cohort.FRD.D01, Gr.9.Cohort.FRD.D10, Gr.9.Cohort.SP.D01, Gr.9.Cohort.SP.D10)
table <- as.data.frame(t(table))
Year <- row.names (table)
table <- cbind (Year, table)
table$Year <- levels (table$Year)
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
p1 <- p1 + ylim (c(.65, .95))
p1 <- p1 + xlab("") + ylab("Pct.") + ggtitle("Kentucky 8th Grade Cohort \n (Free & Reduced Lunch)")
p1 <- p1 + geom_line (aes(Year, Gr.8.Cohort.FRD.D10))
p1 <- p1 + geom_line (aes (Year, Gr.8.Cohort.FRD.D01))
p1 <- p1 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .9, label = "D01", size = 3.5)
p1 <- p1 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .78, label = "Avg", size = 3.5, colour = "red")
p1 <- p1 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .7, label = "D10", size = 3.5)

#plot for school district poverty rate
p1.1 <- ggplot (table, aes (x = Year, y= Gr.8.Cohort.by.year))
table$Year <- as.Date (as.character (table$Year), "%Y")
p1.1 <- p1.1 + layer (geom = "line", colour = "red")
p1.1 <- p1.1 + ylim (c(.65, .95))
p1.1 <- p1.1 + xlab("") + ylab("Pct.") + ggtitle("Kentucky 8th Grade Cohort \n (School Poverty Rate)")
p1.1 <- p1.1 + geom_line (aes (Year, Gr.8.Cohort.SP.D10))
p1.1 <- p1.1 + geom_line (aes (Year, Gr.8.Cohort.SP.D01))
p1.1 <- p1.1 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .9, label = "D01", size = 3.5)
p1.1 <- p1.1 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .78, label = "Avg", size = 3.5, colour = "red")
p1.1 <- p1.1 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .7, label = "D10", size = 3.5)

#put both FRD and School Poverty on Chart
p1.2 <- ggplot (table, aes (x = Year, y= Gr.8.Cohort.by.year))
table$Year <- as.Date (as.character (table$Year), "%Y")
p1.2 <- p1.2 + layer (geom = "line", colour = "red")
p1.2 <- p1.2 + ylim (c(.65, .95))
p1.2 <- p1.2 + xlab("") + ylab("Pct.") + ggtitle("Kentucky 8th Grade Cohort \n (School Poverty Rate)")
p1.2 <- p1.2 + geom_line (aes (Year, Gr.8.Cohort.SP.D10), colour = "blue")
p1.2 <- p1.2 + geom_line (aes (Year, Gr.8.Cohort.FRD.D10))
p1.2 <- p1.2 + geom_line (aes (Year, Gr.8.Cohort.SP.D01), colour = "blue")
p1.2 <- p1.2 + geom_line (aes (Year, Gr.8.Cohort.FRD.D01))
p1.2 <- p1.2 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                         y = .9, label = "D01", size = 3.5)
p1.2 <- p1.2 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                         y = .78, label = "Avg", size = 3.5, colour = "red")
p1.2 <- p1.2 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                         y = .7, label = "D10", size = 3.5)


p2 <- ggplot (table, aes (x = Year, y= Gr.9.Cohort.by.year))
p2 <- p2 + layer (geom = "line", colour = "red")
p2 <- p2 + ylim (c(.55, .95))
p2 <- p2 + xlab("") + ylab("Pct.") + ggtitle("Kentucky \n 9th Grade Cohort")
p2 <- p2 + geom_line (aes(Year, Gr.9.Cohort.FRD.D10))
p2 <- p2 + geom_line (aes (Year, Gr.9.Cohort.FRD.D01))
p2 <- p2 + annotate ("text", x = as.Date (as.character (2003), "%Y"), 
                     y = .83, label = "D01", size = 3.5)
p2 <- p2 + annotate ("text", x = as.Date (as.character (2003), "%Y"), 
                     y = .69, label = "Avg", size = 3.5, colour = "red")
p2 <- p2 + annotate ("text", x = as.Date (as.character (2003), "%Y"), 
                     y = .62, label = "D10", size = 3.5)

#school poverty rate by 9th grade cohort
p2.1 <- ggplot (table, aes (x = Year, y= Gr.9.Cohort.by.year))
p2.1 <- p2.1 + layer (geom = "line", colour = "red")
p2.1 <- p2.1 + ylim (c(.55, .95))
p2.1 <- p2.1 + xlab("") + ylab("Pct.") + ggtitle("Kentucky 9th Grade Cohort \n (School Poverty Rate)")
p2.1 <- p2.1 + geom_line (aes(Year, Gr.9.Cohort.SP.D10))
p2.1 <- p2.1 + geom_line (aes (Year, Gr.9.Cohort.SP.D01))
p2.1 <- p2.1 + annotate ("text", x = as.Date (as.character (2003), "%Y"), 
                     y = .83, label = "D01", size = 3.5)
p2.1 <- p2.1 + annotate ("text", x = as.Date (as.character (2003), "%Y"), 
                     y = .69, label = "Avg", size = 3.5, colour = "red")
p2.1 <- p2.1 + annotate ("text", x = as.Date (as.character (2003), "%Y"), 
                     y = .62, label = "D10", size = 3.5)

p3 <- ggplot (table, aes (x = Year, y= Gr.9.Cohort.by.year))
p3 <- p3 + layer (geom = "line", colour = "red")
p3 <- p3 + ylim (c(.65, .95))
p3 <- p3 + xlab("") + ylab("Pct.") + ggtitle("Kentucky \n 8th vs. 9th Grade Cohort")
p3 <- p3 + geom_line (aes(Year, Gr.8.Cohort.by.year ))
p3 <- p3 + annotate ("text", x = as.Date (as.character (2003), "%Y"), 
                     y = .69, label = "9th", size = 3.5, colour = "red")
p3 <- p3 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .78, label = "8th", size = 3.5)


p4 <- ggplot (table, aes (x = Year, y= Gr.8.Cohort.by.year))
table$Year <- as.Date (as.character (table$Year), "%Y")
p4 <- p4 + layer (geom = "line", colour = "red")
p4 <- p4 + ylim (c(.65, .95))
p4 <- p4 + xlab("") + ylab("Pct.") + ggtitle("Kentucky \n 8th Grade Cohort vs. State Reported")
p4 <- p4 + geom_line (aes(ky$Year, ky$Reported))
p4 <- p4 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .84, label = "Reported", size = 3.5, colour = "black")
p4 <- p4 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .78, label = "8th", size = 3.5, colour = "red")


#add column to table for national eighth grade cohort rate
Gr.8.Cohort.National <- nat[2:8, 3]
Gr.8.Cohort.National <- c(Gr.8.Cohort.National, NA, NA, NA)
table <- cbind (table, Gr.8.Cohort.National)
table$Year <- as.Date (as.character (table$Year), "%Y")


p5 <- ggplot (table, aes (x = Year, y= Gr.8.Cohort.by.year))
p5 <- p5 + layer (geom = "line", colour = "red")
p5 <- p5 + ylim (c(.65, .95))
p5 <- p5 + xlab("") + ylab("Pct.") + ggtitle("National vs. Kentucky \n 8th Grade Cohort")
p5 <- p5 + geom_line (aes(table$Year, table$Gr.8.Cohort.National))
p5 <- p5 + annotate ("text", x = as.Date (as.character (2003), "%Y"), 
                     y = .81, label = "USA", size = 3.5, colour = "black")
p5 <- p5 + annotate ("text", x = as.Date (as.character (2004), "%Y"), 
                     y = .77, label = "KY", size = 3.5, colour = "red")


p6 <- ggplot (cum, aes (x = School.Dist.Child.Poverty.Pct, y = Gr.8.Cohort.KDE))
p6 <- p6 + geom_point ()
p6 <- p6 + geom_point (aes (colour = factor (Year)))
p6
} #Clean this up next!
Fig.MISSING   <- function (){
#check absolute differences between 8th Graders and number of 
#diploma recipients
library (ggplot2)
wd <- getwd()
file <- paste (wd, "objects", "SAAR.1999.2012.csv", sep = "/")
saar <- read.csv (file)
file <- paste (wd, "data sets", "Grad Rate 2003-2007.csv", sep = "/")
gr <- read.csv (file)
file <- paste (wd, "objects", "afgr.08.12.csv", sep = "/")
afgr <- read.csv (file)

Gr8E <- saar [, c(1, grep ("GR8E", names (saar)))]
Gr8E.tot <- colSums (Gr8E[,2:15])
Gr8E.tot <- Gr8E.tot[5: length (Gr8E.tot)]

attach (gr)
gr <- gr [, c(4, 5, grep ("GRADS_4YR", names (gr)))]
gr <- gr [SCHNAME == "---DISTRICT TOTAL---", ]
gr <- gr[, -2]
detach (gr)

Diplomas <- c(colSums (gr[, 2:6], na.rm = T), colSums (afgr[, 2:6 ]))
Diplomas <- as.data.frame (Diplomas)
row.names(Diplomas) <-  (paste ("Grad.w.Diploma.in.4.years", 2003:2012, "KDE", sep = "."))
Year <- (2003:2012)
Diplomas <- cbind (Year, Diplomas, Gr8E.tot)
Diff <- Diplomas$Diplomas - Diplomas$Gr8E.tot
Diplomas <- cbind (Diplomas, Diff)
#plot
p1 <- ggplot (Diplomas, aes(Year, Gr8E.tot))
p1 <- p1 + geom_line ()
p1 <- p1 + xlab("Year") + ylab("Students") + ggtitle ("Difference in 8th Grade Enrollments
                                                      Diploma Recipients")
p1 <- p1 + scale_x_continuous (limits = c(2003, 2012), breaks = seq(from = 2003, to = 2012, by = 2))
p1 <- p1 + ylim(-17500, 60000)
p1 <- p1 + geom_line (aes(Year, Diplomas))
p1 <- p1 + geom_line (aes(Year, Diff), colour = "red")
p1
}