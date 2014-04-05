for( i in 1999:2012){
  a<-(paste("SAAR", i , sep=""))
  b<-(paste(a, ".csv", sep=""))
  a<-read.csv(b, skip=6, nrow=176, header=TRUE, fill=TRUE, blank.lines.skip=TRUE,
              strip.white=TRUE, colClasses=c("character", rep("integer", 13)))
    print(a);print(b)
}
         
SAAR1999<-read.csv("SAAR1999.csv", skip=6, nrow=176, header=TRUE, fill=TRUE, blank.lines.skip=TRUE,
                   strip.white=TRUE, colClasses=c("character", rep("integer", 13)))

SAAR2005<-read.csv("SAAR2005.csv", header=TRUE, sep=",", blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", rep("integer", 13)), 
                   strip.white=TRUE, nrow=176)

SAAR2006<-read.csv("SAAR2006.csv", header=TRUE, sep=",", blank.lines.skip=TRUE, fill=TRUE, skip=6,
                   colClasses=c("character", "character", rep("integer", 13)), 
                   strip.white=TRUE)


#build index column for merge using 1999 as base year
district.index<-SAAR1999[,1]                                      #school districts
district.index<-sub("^[0-9][0-9][0-9].", "" , district.index)     #remove number in front
district.index<-sub("Independent", "Independen", district.index) #add "t" to independent
district.index<-sub("Independen", "Independ", district.index)
district.index<-sub("Independ", "Independent", district.index)
district.index<-as.data.frame(district.index)
names(district.index)<-c("DISTRICT")

#clean 2005
temp<-sub("^[0-9][0-9][0-9].", "" , SAAR2005$DISTRICT)
temp<-sub("Walton-Verona Independent", "Walton Verona Independent", temp)
SAAR2005$DISTRICT<-temp
setdiff(district.index$DISTRICT, SAAR2005$DISTRICT)
setdiff(SAAR2005$DISTRICT, district.index$DISTRICT)

#reduce to variables
YEAR<-rep(2005, 2288)
DISTRICT<-rep(SAAR2005[,1], each=13)
GRADE<-rep(names(SAAR2005)[2:ncol(SAAR2005)], 176)
SCHOOL<-rep("", 2288)
STUDENTS<-SAAR2005[1:176, 2:14]
STUDENTS<-t(STUDENTS)
STUDENTS<-as.character(matrix(STUDENTS, ncol=1))
SAAR2005.rev1<-cbind(YEAR, DISTRICT, SCHOOL, GRADE, STUDENTS)
SAAR2005.rev1<-as.data.frame(SAAR2005.rev1)
rm(YEAR, DISTRICT, GRADE, SCHOOL, STUDENTS, temp)

#clean 2006
    SAAR2006<-SAAR2006[!is.na(SAAR2006$X11),]
#eliminate blank "" fields
    a<-SAAR2006[,2]
    blank<-a==""
    a[blank]<-"Total"
    SAAR2006[,2]<-a
#take numbers off
    temp<-sub("^[0-9][0-9][0-9].", "" , SAAR2006$School)
    temp<-trim(temp)
    SAAR2006[,2]<-temp
#hyphen
    SAAR2006[,1]<-sub("Walton-Verona", "Walton Verona", SAAR2006[,1])
    SAAR2006[,2]<-sub("Walton-Verona", "Walton Verona", SAAR2006[,2])
#reduce to variables
  names(SAAR2006)<-toupper(names(SAAR2006))
  YEAR<-rep(2006, nrow(SAAR2006)*13)
  DISTRICT<-rep(SAAR2006[,1], each=13)
  SCHOOL<-rep(SAAR2006[,2], each=13)
  GRADE<-rep(names(SAAR2006)[3:ncol(SAAR2006)], 1553)
  STUDENTS<-SAAR2006[1:1553, 3:ncol(SAAR2006)]
  STUDENTS<-t(STUDENTS)
  STUDENTS<-as.character(matrix(STUDENTS, ncol=1))
#combine
  SAAR2006.rev1<-as.data.frame(cbind(YEAR, DISTRICT, SCHOOL, GRADE, STUDENTS))
#clean up workspace
  rm(DISTRICT, GRADE, SCHOOL, STUDENTS, YEAR, a, blank, temp)






