
#Bring in packages
library(ggmap)
library(mapdata)
library(cowplot)

#Setup map
ylim=c(32,62) #latitude limits
xlim=c(-190,-121) #longitude limits
w=map_data("worldHires",ylim=ylim,xlim=xlim) #extract world map data

#Theme for Map
theme_white = function(base_size = 12, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size, color = "black", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size, color = "black", lineheight = 0.9),  
      axis.ticks = element_line(color = "black", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "black", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "black", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.position = "none",
      # Specify panel options
      panel.background = element_rect(fill = "white", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "black"),  
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),  
      panel.spacing = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "white", color = "black"),  
      strip.text.x = element_text(size = base_size*0.8, color = "black"),  
      strip.text.y = element_text(size = base_size*0.8, color = "black",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "white", fill = "white"),  
      plot.title = element_text(size = base_size*1.2, color = "black"),  
      plot.margin = unit(rep(1, 4), "lines")
    )
}

############################################# PULL IN DATA

setwd("~/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow")

#pull in TLL data (each row is a satellite tag position)
TLL=read.csv("1TLL TV3 2021_09_30.csv")

#pull in turnaround dates (each row is a seal)
dat=read.csv("2TrackSummaries 2021_09_30.csv")

#find unique seals in turnaround date data
seals=c(2005022,2007048,2004021)
#replace with unique(dat$TOPPID) if you want all seals

#convert Lon-180:180 to Lon360 (for map)
TLL$Lon=ifelse(TLL$Lon > 0, -360 + TLL$Lon, TLL$Lon)
TLL$DOY=ifelse(TLL$DOY < 100, TLL$DOY+365, TLL$DOY) #fix DOY after jan 1

for(i in length(seals)){
  #subset data
  dat_i=dat[dat$TOPPID==seals[i],]
  TLL_i=TLL[TLL$TOPPID==seals[i],]
  
  ##########################PANEL A
  
  #Map with turnaround point
  PanelA=ggplot()+labs(y= "Latitude (°)", x = "Longitude (°)")+
    coord_fixed(1.5,xlim=xlim,ylim=ylim)+
    geom_path(TLL_i,mapping=aes(x=Lon,y=Lat),colour="grey40",size=.4)+
    geom_point(dat_i,mapping=aes(x=TurnaroundLon,y=TurnaroundLat),color="black",size=3.3)+
    geom_point(dat_i,mapping=aes(x=TurnaroundLon,y=TurnaroundLat,color="Turnaround"),size=2.6)+
    geom_polygon(data=w,aes(x=long,y=lat,group=group),fill="grey10")+
    geom_point(dat,mapping=aes(x=-122,y=37.12),color="white",fill="black",pch=22,size=6)+
    labs(colour=" ")+
    scale_colour_manual(values = "goldenrod")+
    theme_white()
  
  ##########################PANEL B
  
  #add all days before pupping by converting TLL_i$Date
  TLL_i$DateBeforePupping=(TLL_i$DOY-365)-(dat_i$ArrivalDOY-dat_i$ArrivalDSP)
  
  #add turnaround days before pupping
  dat_i$TurnaroundDaySincePupping=dat_i$TurnaroundDSP
  
  # Distance to colony ~ date, with vertical line indicating turnaround location.
  par(mar=c(5,4,1,0),oma=c(1,1,1,1),xpd = TRUE, bg = "transparent")
  plot(Dist~DateBeforePupping,data=TLL_i,type="l",lwd=1,
       xlab="Days Before Parturition",ylab="Distance From Breeding Beach (km)")
  points(TurnaroundDistanceKM~TurnaroundDSP,data=dat_i,pch=21,bg="goldenrod",cex=1.6)
  
  ### record the previous plot
  PanelB=recordPlot()  
  
  ##########################PANEL C
  
  #to run this panel, you need to run the first 175 lines of the "6TurnaroundCalculations 2021_11_01.R" code.
  tryID=pm$TOPPID[i]
  
  b6=distancePerBin(div=24,id=tryID,graphit=FALSE,debug=0)
  b6$daysbeforepupping=(b6$yday-365)-dat_i$ParturitionDOY

  y=kernelGauss(x=b6$daysbeforepupping,
                y=b6$advance,
                par=6) #
  
  goingaway=which(y$kernel>=0) #find out which displacement kernels are greater than zero
  pm$turnaround6[i]=b6$daysbeforepupping[max(goingaway)] #find the last positive displacement kernel
  
  plot(advance~daysbeforepupping,dat=b6,pch=21,bg="grey60",col=NA,
       xlab="Days Before Parturition",ylab="Daily Displacement (km)") 
  lines(y$x,y$kernel,col="black",lwd=2) #add 6 hour kernel
  abline(h=0,xpd=FALSE) #add line for displacement = 0
  points(x=pm$turnaround6[i],y=0,col="black",pch=21,bg="goldenrod",cex=1.6,lwd=0.5) #overlay turnaround points
  
  #record the previous plot
  PanelC=recordPlot()
  
  #########################COMBINE PANELS
  plot_grid(PanelA,PanelB,PanelC,nrow=3,rel_heights=c(1,1))
  ggsave(paste0("Turnaround_Supplement_",seals[i],".png"),height=12,width=6)
  
}