setwd("~/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow")
load("3TOPPPARTURITION.rdata")

#Roxanne, here is an R table that has the birthdates for TOPP females as I estimated. 
#It should include all the females with a post-molt instrument, but only through 2020.

colnames(TOPPPARTURITION)
head(TOPPPARTURITION)

#There is a column 'firstwith' that means the first day a female was seen with a pup. 
#It counts days starting 1 Dec (subtract 31 and it's a date in January).

TOPPPARTURITION$PupDate=TOPPPARTURITION$firstwith-31 #convert to day of year

#There is a note column that filters out bad observations:
#1) You want records where the note column has 'good...'. 
#2) You can also use those with note='satDead', since you don't care about arrival date. 
#(I excluded satDead in the pre-parturition analysis because arrival was not known precisely; their birth observations were solid.)

TOPPPARTURITION=TOPPPARTURITION[TOPPPARTURITION$note=="good"|TOPPPARTURITION$note=="satDead",]

#3) All the other notes indicate poor observations or no pup at all, so birth date doesn't make sense. 

#There is a column 'gap' which means the number of days a female was missed prior to the birth. You could ignore this. In the pre-parturition analysis, I only used those with gap=1, but gap is never more than 6 days, and that's probably fine for your analysis. In the vast majority, gap<5 so birth date is known within 4 days.

hist(TOPPPARTURITION$gap)

#now, add parturition dates
dat=read.csv('2TrackSummaries 2021_09_30.csv')

colnames(dat); nrow(dat)
colnames(TOPPPARTURITION); nrow(TOPPPARTURITION)

#need to add PupDate from TOPPPARTURITION into data, by TOPPID
dat=merge(dat,TOPPPARTURITION[,c("TOPPID","PupDate")],by="TOPPID",all.x=FALSE,all.y=FALSE) #require a pupping date to have them retained

#check
plot(dat$PupDate,dat$ArrivalDOY,xlab="Parturition Day Of Year",ylab="Arrival Day Of Year")

#change column name of PupDate to ParturitionDOY
colnames(dat)[7]="ParturitionDOY"

write.csv(dat,'2TrackSummaries 2021_09_30.csv',row.names=FALSE)


