###############################################
# Margaret Jensen
# Kestrel nest boxes
# poisson models for nest success
# 2/03/21
###############################################

library(dplyr)
library(lme4)
library(ggplot2)
full<-read.csv('~/Desktop/kestrel/AMKE master.csv')

#replace the periods with NAs
for(i in 1:nrow(full)){
  if (full$Success[i]=='.'){
    full$Success[i]<-NA
  }
}
full$Success<-as.numeric(full$Success)

#Poisson analysis -- 250m
r250<-filter(full,Radial.Zone==250)
effects250 <- glmer(formula = Success ~ `Development.P` + `Agriculture.P` + `Natural.P` + (1 | Nest.Box) + (1 | Study.Year),
                      data = r250, family = "poisson")
summary(effects250)

#750m
r725<-filter(full,Radial.Zone==725)
effects725 <- glmer(formula = Success ~ `Development.P` + `Agriculture.P` + `Natural.P` + (1 | Nest.Box) + (1 | Study.Year),
                    data = r725, family = "poisson")
summary(effects725)

#1200m
r1200<-filter(full,Radial.Zone==1200)
effects1200 <- glmer(formula = Success ~ `Development.P` + `Agriculture.P` + `Natural.P` + (1 | Nest.Box) + (1 | Study.Year),
                    data = r1200, family = "poisson")
summary(effects1200)

#just for fun what happens with three categories

full<-na.omit(full,Success)

for(i in 1:nrow(full)){
  if (full$Success[i] == 1 | full$Success[i] == 2){
    full$SuccessCat[i]<-1
  } else if (full$Success[i]==3|full$Success[i]==4){
    full$SuccessCat[i]<-2
  } else if (full$Success[i]>4){
    full$SuccessCat[i]<-3
  }
}
full$SuccessCat<-as.factor(full$SuccessCat)
r250<-filter(full,Radial.Zone==250)
effects250 <- glmer(formula = SuccessCat ~ `Development.P` + `Agriculture.P` + `Natural.P` + (1 | Nest.Box) + (1 | Study.Year),
                    data = r250, family = "binomial")
summary(effects250)

r725<-filter(full,Radial.Zone==725)
effects725 <- glmer(formula = SuccessCat ~ `Development.P` + `Agriculture.P` + `Natural.P` + (1 | Nest.Box) + (1 | Study.Year),
                    data = r725, family = "binomial")
summary(effects725)

r1200<-filter(full,Radial.Zone==1200)
effects1200 <- glmer(formula = SuccessCat ~ `Development.P` + `Agriculture.P` + `Natural.P` + (1 | Nest.Box) + (1 | Study.Year),
                     data = r1200, family = "binomial")
summary(effects1200)

#graphses
ggplot(full,aes(Agriculture.P,Success))+geom_point(position='jitter')
ggplot(full,aes(Development.P,Success))+geom_point(position='jitter')

