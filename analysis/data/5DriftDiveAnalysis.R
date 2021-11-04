#set working directory
setwd("~/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow")

#define dataset as 'dat'
dat=read.csv('4DriftDives4Alex.csv',header = FALSE)
colnames(dat)=c("TOPPID","DOY","DriftRate","DriftRateChangePerDay") #add column headers

#lists unique TOPPID
Seals=unique(dat$TOPPID)

#change working directory to drift rate change figures
setwd("~/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow/Old Not Using/DriftRateChangeFigs")

for(i in 1:length(Seals)){#subset different seals

#Subset a single seal from the overall dataset
dat_i=dat[dat$TOPPID==Seals[i],]

#identify maximum drift rate change
index=which.max(dat_i$DriftRateChangePerDay)
date=dat_i$DOY[index]
value=dat_i$DriftRateChangePerDay[index]

#maybe constrain for middle of trip? 
#maybe not if we can make magnitude clear. 
  
#For seal i, plot driftrate and driftratechange with respect to julian date
png(paste0(Seals[i],"_DriftRateFig.png"),width=5,height=9,units="in",res=300)
    par(mfrow=c(2,1))
    plot(dat_i$DOY,dat_i$DriftRate,type="l",xlab="",ylab="Drift Rate (m/sec)",
         main=Seals[i])
    abline(v=date,col="red")
    plot(dat_i$DOY,dat_i$DriftRateChangePerDay,type="l",xlab="Julian Date",ylab="Drift Rate Change (m/sec*day)")
    abline(v=date,col="red")
dev.off()
#FYI these plots are kind of funky because the post-molt trip often wraps around January 1

#export data
out=cbind(Seals[i],date,value,max(dat_i$DriftRate))
if(i==1){
  outall=out
}else{
  outall=rbind(outall,out)
}

}

setwd("~/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow")

colnames(outall)=c("TOPPID","DateMaxChange","ValueMaxChange","MaxDriftRate")
#write.csv(outall,'4DriftRateAnalysisTEMP.csv',row.names=FALSE)

#manually looked at all the figures, and four seals did not switch to positive buoyancy. change their data to NA: 
outall=data.frame(outall)
outall[outall$TOPPID==2004017,2:4]=rep(NA,3)
outall[outall$TOPPID==2004030,2:4]=rep(NA,3)
outall[outall$TOPPID==2005028,2:4]=rep(NA,3)
outall[outall$TOPPID==2013034,2:4]=rep(NA,3)

#add drift rate change dates to big spreadsheet
setwd("~/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow")
dat=read.csv('2TrackSummaries 2021_09_30.csv') #read in

colnames(dat); nrow(dat)
colnames(outall); nrow(outall)

#need to add PupDate from outall into data, by TOPPID
dat=merge(dat,outall[,c("TOPPID","DateMaxChange")],by="TOPPID",all.x=TRUE,all.y=FALSE) #don't require a drift rate to have them be retained

#check
plot(dat$PupDate,dat$DateMaxChange,xlab="Parturition Day Of Year",ylab="Arrival Day Of Year")

#change column name of PupDate to ParturitionDOY
colnames(dat)[8]="DriftRateSwitchDOY"

write.csv(dat,'2TrackSummaries 2021_09_30.csv',row.names=FALSE)


