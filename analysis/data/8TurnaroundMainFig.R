#add letters to each panel

library(gridGraphics)
library(cowplot)
library(grid)

setwd("~/Publications/In Prep/ROXANNE_Turnaround/Alex Turnaround Workflow")
dat=read.csv('2TrackSummaries 2021_09_30.csv')

logo=magick::image_read("8Seal.png")

#################################################################### PANEL A

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
      plot.margin = unit(c(0, 0, 0, 0), "cm")
      
    )
  
}

library(viridis)

#load in time lat long
TLL=read.csv("1TLL TV3 2021_09_30.csv")
#convert Lon-180:180 to Lon360
TLL$Lon=ifelse(TLL$Lon > 0, -360 + TLL$Lon, TLL$Lon)

#next step, edit this to add points
library(ggmap)
library(mapdata)
library(patchwork)

ylim=c(32,62) #find latitude limits of data to inform map
xlim=c(-190,-121) #find longitude limits of data to inform map

#world map data
w=map_data("worldHires",ylim=ylim,xlim=xlim) #extract map data

#make the plot
z1=ggplot()+labs(y= "Latitude (°)", x = "Longitude (°)")+
  coord_fixed(1.5,xlim=xlim,ylim=ylim)+
  geom_path(TLL,mapping=aes(x=Lon,y=Lat,group=TOPPID),colour="grey40",size=.2)+
  geom_point(dat,mapping=aes(x=TurnaroundLon,y=TurnaroundLat),color="black",size=2.3)+
  geom_point(dat,mapping=aes(x=TurnaroundLon,y=TurnaroundLat,color="Turnaround"),size=1.6)+
  geom_polygon(data=w,aes(x=long,y=lat,group=group),fill="grey10")+
  geom_point(dat,mapping=aes(x=-122,y=37.12),color="white",fill="black",pch=22,size=6)+
  labs(colour=" ")+
  scale_colour_manual(values = "goldenrod")+
  geom_text(aes(x=-191.5,y=62.5,hjust=0,vjust=1,label="A",size=2))+
  theme_white()+
  draw_image(logo,  x = -180, y = 58, scale = 17,interpolate = TRUE)
  
#z1

################################################ PANEL B

col1="#414487FF"
col2="goldenrod"
col3="#22A884FF"

#setup the plot
par(xpd = NA, 
    bg = "transparent",mfrow=c(2,2),oma=c(1,3,1,0),mar=c(3,1,1,1))

dat=dat[order(dat$ArrivalDSP),]

plot(1,1,ylim=c(-250,49),xlim=c(1,nrow(dat)),xaxt="n",
     col="white",ylab="",xlab="")
mtext("Seals",side=1,line=2,cex=.8)
arrows(x0=1:nrow(dat),
       x1=1:nrow(dat),
       y0=dat$DepartureDSP, 
       y1=dat$ArrivalDSP,
       col="grey70",length=0)
points(y=dat$DepartureDSP,x=1:nrow(dat),
       pch=21,bg=col1)
points(y=dat$TurnaroundDSP,x=1:nrow(dat),
       pch=21,bg=col2)
points(y=dat$ArrivalDSP,x=1:nrow(dat),
       pch=21,bg=col3)
axis(side=1,at=1:nrow(dat),labels=FALSE)
mtext("Days Before Parturition",side=2,line=2,cex=.8)
legend("topleft", "B", bty="n",cex=1.2,inset=c(-.05,0))

#calculate density histograms
DepDen=density(dat$DepartureDSP)
TurnDen=density(dat$TurnaroundDSP)
ArrDen=density(dat$ArrivalDSP)

plot(DepDen$y,DepDen$x, xlab= "", ylab="",xlim=c(0,.25),
     col="white",yaxt="n",xaxs="i",ylim=c(-250,49))
mtext("Density",side=1,line=2,cex=.8)
polygon(DepDen$y,DepDen$x,border="black",col=col1)
polygon(TurnDen$y,TurnDen$x,border="black",col=col2)
polygon(ArrDen$y,ArrDen$x,border="black",col=col3)
text(x=.25,y=-210,"Departure",col=col1,pos=2)
text(x=.25,y=-125,"Turnaround",col=col2,pos=2)
text(x=.25,y=-40,"Arrival",col=col3,pos=2)
axis(side=2,labels=FALSE,tick=TRUE)
legend("topleft", "C", bty="n",cex=1.2,inset=c(-.05,0))

####################################################### PANEL C

#Turnaround Distance
plot(dat$TurnaroundDSP~dat$TurnaroundDistanceKM, ylim=c(-250,49),
     xlab="",cex=1.2,pch=21,bg=col2, ylab="")
axis(side=2,labels=FALSE)
mtext(c("Turnaround Date","(Days Before Parturition)"),side=2,line=c(3,2),cex=.8)
mtext("Turnaround Distance (km)",side=1,line=2,cex=.8)
abline(lm(TurnaroundDSP~TurnaroundDistanceKM,data=dat),lty=2,lwd=2)
points(dat$TurnaroundDSP~dat$TurnaroundDistanceKM,cex=1.2,pch=21,bg=col2)
legend("topleft", "D", bty="n",cex=1.2,inset=c(-.05,0))

####################################################### PANEL D

#Drift Rate Change DOY
plot(dat$TurnaroundDSP~dat$DriftRateSwitchDOY, ylim=c(-250,49),
     pch=21,xlab="",ylab="",cex=1.2,yaxt="n", bg=col2)
axis(side=2,labels=FALSE)
mtext("Drift Rate Change Day of Year",side=1,line=2,cex=.8)
legend("topleft", "E", bty="n",cex=1.2,inset=c(-.05,0))

### record the previous plot
p1 = recordPlot()  

### combine all plots together
par(mar=c(1,1,1,1))
plot_grid(z1, p1,nrow=2,rel_heights=c(.8,1),align="hv")
ggsave("8TurnaroundFigComposite_New.png",height=8,width=6)
