## Turnaround in female migration
library(date) #for the 'mdy.date' function

turnpath="C:/Users/roxan/Documents/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow/"
trackfile='1TLL TV3 2021_09_30.csv'

# Attach one or more datafiles, checking first whether the file is already attached. If it is attached,
# it is not reattached, and the search position where attached is returned.
attach_if_needed=function(datafiles)
{
  already=numeric()
  j=1
  for(file in datafiles)
  {
    found=grep(file,search())
    if(length(found)==0)
    {
      attach(file)
      already[j]=2
      if(j>1) for(k in 1:(j-1)) already[k]=already[k]+1
    }
    else already[j]=found
    j=j+1
  }
  return(already)
}


readtrack=function(input=trackfile,dform="%d-%b-%Y %H:%M:%S",path=turnpath,suffix='AN',saveit=TRUE)
{
  x=read.csv(paste0(turnpath,input)) #read in CSV file
  posix=strptime(x$DateUTC,format=dform) #separate UTC date into d-b-y H:M:S
  date=data.frame(year=1900+posix$year,month=posix$mon+1,day=posix$mday,hour=posix$hour,min=posix$min,yday=posix$yday+1) 
  #make dataframe of date
  jul=mdy.date(month=date$month,day=date$day,year=date$year) #make day-of-year
  date$julian=jul-min(jul) #calculate days since start
  y=cbind(x,date) #bind the new date column(s) onto the dataframe
  
  ord=order(y$TOPPID,y$julian,y$hour,y$min) #set order of rows
  migrationTrack=y[ord,] #change order of rows
  
  output=paste0(turnpath,'migrationTrack',suffix,'.rdata')#set output file
  if(saveit) save(migrationTrack,file=output)    
  if(saveit) cat("Attach: ", paste0("attach_if_needed('",output,"')"), "\n") #concatenate and print
  
  return(migrationTrack)
}

findfirst=function(x) return(x[1]) #find first instance
findlast=function(x) return(x[length(x)]) #find last instance

#this provides a summary of the starting and ending dates and locations, then makes a data frame
trackSummary=function(suffix='AN')
{
  attach_if_needed(paste0(turnpath,'migrationTrack',suffix,'.rdata'))
  TOPPID=unique(migrationTrack$TOPPID) 
  startmon=tapply(migrationTrack$month,migrationTrack$TOPPID,findfirst)
  startday=tapply(migrationTrack$day,migrationTrack$TOPPID,findfirst)
  startdist=tapply(migrationTrack$Dist,migrationTrack$TOPPID,findfirst)
  startjul=tapply(migrationTrack$julian,migrationTrack$TOPPID,findfirst)
  startyday=tapply(migrationTrack$yday,migrationTrack$TOPPID,findfirst)
  endmon=tapply(migrationTrack$month,migrationTrack$TOPPID,findlast)
  endday=tapply(migrationTrack$day,migrationTrack$TOPPID,findlast)
  enddist=tapply(migrationTrack$Dist,migrationTrack$TOPPID,findlast)
  endjul=tapply(migrationTrack$julian,migrationTrack$TOPPID,findlast)
  endyday=tapply(migrationTrack$yday,migrationTrack$TOPPID,findlast)
  startlon=tapply(migrationTrack$Lon,migrationTrack$TOPPID,findfirst)
  startlat=tapply(migrationTrack$Lat,migrationTrack$TOPPID,findfirst)
  m=match(TOPPID,names(startmon)) #uses TOPPID, looks up the index of startmon corresponds
  z=data.frame(TOPPID,smon=startmon[m],sday=startday[m],startjul=startjul[m],syday=startyday[m],
               emon=endmon[m],eday=endday[m],endjul=endjul[m],eyday=endyday[m],duration=(endjul-startjul)[m],
               sdist=startdist[m],edist=enddist[m],lat=startlat[m],lon=startlon[m],
               postmolt=startmon[m]%in%(5:7),postbreed=startmon[m]%in%(1:3))
  return(z)
}

is.leap=function(yr,start=1904,end=2096)
{
  leapyears=seq(start,end,by=4)
  matchleap=match(yr,leapyears)
  leap=(!is.na(matchleap))
  
  nonleap=seq(100,3000,by=100)
  matchnonleap=match(yr,nonleap)
  leap[!is.na(matchnonleap)]=FALSE
  
  leapyears=seq(400,3000,by=400)
  matchleap=match(yr,leapyears)
  leap[!is.na(matchleap)]=TRUE
  
  return(leap)
}

readOneTrack=function(a='2007049',suffix='AN',map=NULL,sz=8,debug=0)
{
  attach_if_needed(paste0(turnpath,'migrationTrack',suffix,'.rdata'))
  x=subset(migrationTrack,TOPPID==a) #subset by TOPPID
  if(debug>1) browser()
  
  firstyr=min(x$year) #first year of tracking data
  ylength=ifelse(is.leap(firstyr),366,365) #figure out how many days are in the year
  ydiff=x$year-firstyr
  
  if(length(which(ydiff>1))>0) browser()
  if(debug>0) browser()
  
  second=ydiff>0 #if one animal has data in more than 1 year
  x$yday[second]=x$yday[second]+ylength #wrap year around 365 by adding whole length of year
  
  start=min(x$yday) #first day of trip
  x$exacthr=24*(x$yday-start)+x$hour+x$min/60 #calculates the exact hour
  
  return(x)   
}

distancePerBin=function(x=oneTrack,id='2018022',div=6,graphit=FALSE,debug=0)
{
  if(!is.null(id)) x=readOneTrack(a=id)
  
  x$bin=floor(x$exacthr/div) #exact hour divided by 6 hours, rounded down
  dcol=c('month','day','yday') #column names
  udate=unique(x[,c(dcol,'bin')]) #unique dates
  if(debug>1) browser()
  
  mdist=tapply(x$Dist,x$bin,mean) #calculate mean of distance from colony, for each 6hr bin
  bdist=tapply(x$bin,x$bin,mean) #calculate mean of the bins themselves
  deltad=c(NA,diff(mdist)) #calculate first derivative of distance
  deltat=c(NA,diff(bdist)) #calculate first derivative of bins
  
  result=data.frame(#make dataframe of results
      bin=bdist,#mean bin size (6 hours?)
      mdist, #mean of distance from colony, for each 6 hr bin
      deltad,#first derivative of distance traveled in each 6 hr bin
      deltat, #first derivative of bin size (6 hours?)
      advance=deltad/deltat #distance derivative divided by time derivative  (hereafter, displacement)
      ) 
  m=match(result$bin,udate$bin) #find positions of first within second
  result[,dcol]=udate[m,dcol] #add them to results matrix
  if(debug>0) browser()
  
  if(graphit) #if asked to graph
  {
    par(mfcol=c(2,1)) #2 rows, 1 column of plots
    plot(mdist~bdist,type='l') #plot mean distance from colony ~ mean bin size
    abline(h=0) #add line at distance from colony = 0
    plot(advance~bdist,data=result,pch=16) #plot displacement ~ mean bin size
    abline(h=0) #add line at displacement = 0
  }
  
  return(result[-1,]) #return the results minus the first column (NA because of first derivative)
}

kernelGauss=function(x,y,SD,fnc=dnorm,debug=0)
{
  #check that the lengths of x and y are the same
  if(length(x)!=length(y)) return("Two vectors must have equal length")
  N=length(x)
  kernel=numeric()
  for(i in 1:N) #for each point
  {
    centered=abs(x[i]-x) #center by taking the absolute value of difference between each point and all points
    weight=dnorm(centered,mean=0,sd=SD) #SD will be 6 hours
    fity=y*weight #weighted by how far they are
    kernel[i]=sum(fity)/sum(weight) #calculates kernel
    if(debug>1) browser()
  }
  
  return(data.frame(x,kernel))
}

############################################################################## RSB Analysis

readtrack() # to read the big time-lat-long file and save it as an R object. 
pm=ts=TRIPSUMMARY=trackSummary() #produce track summary
pm$TurnaroundDOY=NA #allocates space for the turnaround dates

#pull out dist
setwd("~/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow")
TLL=read.csv('1TLL TV3 2021_09_30.csv')

for(i in 1:nrow(pm)){
tryID=pm$TOPPID[i]

b6=distancePerBin(div=24,id=tryID,graphit=FALSE,debug=0)
y=kernelGauss(x=b6$yday,
              y=b6$advance,
              SD=6) #smooth y

goingaway=which(y$kernel>=0) #find out which displacement kernels are greater than zero
pm$TurnaroundDOY[i]=b6$yday[max(goingaway)] #find the last positive displacement kernel

TLL_i=TLL[TLL$TOPPID==tryID,] #subset TLL
index=which.min(abs(TLL_i$DOY-pm$TurnaroundDOY[i])) #index of closest in time
pm$TurnaroundDistanceKM[i]=TLL_i[index,"Dist"]
pm$TurnaroundLat[i]=TLL_i[index,"Lat"]
pm$TurnaroundLon[i]=TLL_i[index,"Lon"]

}

#add turnaround dates and distances to sumamry  spreadsheet
dat=read.csv('2TrackSummaries 2021_09_30.csv') #read in

colnames(dat); nrow(dat)
colnames(pm); nrow(pm)

#need to add PupDate from out into data, by TOPPID
dat=merge(dat,pm[,c("TOPPID","TurnaroundDOY","TurnaroundDistanceKM","TurnaroundLat","TurnaroundLon")],
          by="TOPPID",all.x=TRUE,all.y=TRUE) 

#check
plot(dat$ParturitionDOY,dat$turnaround6,xlab="Parturition Day Of Year",ylab="Turnaround Day Of Year")
plot(dat$TurnaroundDistanceKM,dat$turnaround6,xlab="Turnaround Distance",ylab="Turnaround Day Of Year")

#correct for the fact that 5 dates are end of dec (days 353-361)
dat$ArrivalDOY[dat$ArrivalDOY>100]=dat$ArrivalDOY[dat$ArrivalDOY>100]-365

dat$DepartureDSP=(dat$DepartureDOY-365)-dat$ParturitionDOY #calculate departure days since parturition
dat$TurnaroundDSP=(dat$TurnaroundDOY-365)-dat$ParturitionDOY #calculate turnaround days since parturition
dat$ArrivalDSP=dat$ArrivalDOY-dat$ParturitionDOY #calculates arrival days since parturition

write.csv(dat,'2TrackSummaries 2021_09_30.csv',row.names=FALSE)
