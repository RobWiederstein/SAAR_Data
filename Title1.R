Url<-"http://www2.ed.gov/about/overview/budget/titlei/fy09recovery/kentucky.xls"
Date.download<-date()
Title1<-read.csv("Kentucky.Title1.2009.csv", skip=6, header=FALSE, blank.lines.skip=TRUE, as.is=TRUE, sep=",")

Title1<-subset(Title1, Title1[,3]!="") #get rid of empty fields
Title1<-Title1[,2:3]                   #get rid of empty columns & LEA number
Title1<-Title1[1:176,]                 #drop 2 rows off bottom
Title1[,2]<-sub(",", "", Title1[,2])   #drop first comma
Title1[,2]<-sub(",", "", Title1[,2])   #drop 2nd comma
Title1[,2]<-as.integer(Title1[,2])    # change column to numbers

#match character fields
Title1[,1]<-sub(" SCHOOL DISTRICT", "", Title1[,1])
di<-toupper(district.index[,1])
di<-as.data.frame(di, by="col")
setdiff(di[,1], Title1[,1])
setdiff(Title1[,1], di[,1])
names(Title1)<-c("DISTRICT", "FUNDS")
names(di)<-"DISTRICT"
di<-merge(di, Title1, by="DISTRICT")

#match school district to cohorts in SAAR2012
temp<-cbind (SAAR2012[,1], di)
Title1<-temp[, c(1,3)]
names(SAAR2012)<- c("DISTRICT", "2009 TOT. FUNDS")
Title1<-cbind (Title1, SAAR2008[,16])
names(Title1)<-c("DISTRICT", "TOT.FUNDS.2008", "TOT.ENROLLMENT")
Title1.Per.Student<-Title1$TOT.FUNDS.2008/ Title1$TOT.ENROLLMENT #scale by tot. enrollment
Title1<-cbind (Title1, Title1.Per.Student)

#Divide into Deciles by Per Student Title 1 spending
T1.Decile<-cut_number(Title1$Title1.Per.Student, 10)
levels(T1.Decile)<-paste("D", 1:10, sep="")
Title1<-cbind (Title1, T1.Decile)

#Merge Title 1 with Cohorts
Cohort.9G.ex<-merge(Cohort.9G, Title1, by="DISTRICT")

D1<-subset(Cohort.9G.ex, Cohort.9G.ex[,15]=="D1")
D1.Grad<-colMeans(D1[2:11], na.rm=TRUE)

D10<-subset(Cohort.9G.ex, Cohort.9G.ex[,15]=="D10")
D10.Grad<-colMeans(D10[2:11], na.rm=TRUE)

#build dataframe for ggplot
T1.Grad.df<-as.data.frame(cbind(D1.Grad, D10.Grad, Year=2003:2012))


#Plot graduation rate by Title 1 decile
p001<-qplot(T1.Grad.df[,3], T1.Grad.df[,1], data=T1.Grad.df)
SAAR.Cohorts[,10], data=SAAR.Cohorts, 
            geom= c("point", "smooth"),
            xlab="Free/Reduced Lunch",
            ylab="Ratio",
            main="2012 8th/12th Grade Ratio",
            method = "rlm",
            ylim=c(.5, 1.25))






