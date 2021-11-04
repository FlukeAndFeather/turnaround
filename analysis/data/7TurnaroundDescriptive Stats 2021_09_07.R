setwd("~/Publications/In Prep/ROXANNE_Turnaround/Turnaround MS Workflow")
dat=read.csv('2TrackSummaries 2021_09_30.csv')

#sample sizes
length(unique(dat$Field))
length(unique(dat$TOPPID))

summary(dat$DepartureDOY)
sd(dat$DepartureDOY)

summary(dat$TurnaroundDOY)
sd(dat$TurnaroundDOY)

summary(dat$ArrivalDOY)
sd(dat$ArrivalDOY)

summary(dat$TurnaroundDOY-dat$DepartureDOY)
sd(dat$TurnaroundDOY-dat$DepartureDOY)

summary(dat$ArrivalDOY-dat$TurnaroundDOY)
sd(dat$ArrivalDOY-dat$TurnaroundDOY)

summary(dat$TurnaroundDist)
sd(dat$TurnaroundDist)

summary(dat$ParturitionDOY)
sd(dat$ParturitionDOY)

summary(dat$TurnaroundDSP)
sd(dat$TurnaroundDSP)


#the complex model with random effects
#scale turnarounddist and driftratechangeday
dat$sTurnaroundDist = scale(dat$TurnaroundDistanceKM, center=TRUE, scale=TRUE)
dat$sDriftRateChangeDay = scale(dat$DriftRateSwitchDOY, center=TRUE, scale=TRUE)

library(lme4)
library(lmerTest)
library(MuMIn)
#model 1, Turnaround Date (Days Before Parturition)
m1=lmer(TurnaroundDSP~sTurnaroundDist+sDriftRateChangeDay+(1|FieldID),
        data=dat)
summary(m1)
anova(m1) 
r.squaredGLMM(m1)
coef(summary(m1))[ , "Estimate"]


#model 3, Turnaround day of year
m3=lmer(TurnaroundDOY~sTurnaroundDist+sDriftRateChangeDay+(1|FieldID),
        data=dat)
summary(m3)
anova(m3) 
r.squaredGLMM(m3)
coef(summary(m3))[ , "Estimate"]

#and regress date on distance for regression of Fig1D
m4=lmer(TurnaroundDSP~TurnaroundDistanceKM+(1|FieldID),data=dat)
coef(summary(m4))[ , "Estimate"]

