#option 1 to call areas
##source ("C:/Users/Hector/Documents/Pughtam-cropozone/Global_Evaluation/Area_correction.R")

#option 2 call SPAMest_[Crop]_1960_2010.nc
library(ncdf4)
library(raster)
library(rworldmap)
library(dplyr)
library(plyr)
library(forecast)
library(zoo)

crop<-c("SPAMest_Wheat_","SPAMest_Maize_","SPAMest_Rice_")
cropirr<-c("SPAMest_Wheatirr_","SPAMest_Maizeirr_","SPAMest_Riceirr_")
ArNew<-list()
ArNewi<-list()
ArNewr<-list()
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/Area")
for (j in 1:length(crop)){
  ArNew[[j]]<-list.files(pattern=crop[j])
  ArNewi[[j]]<-list.files(pattern=cropirr[j])
  ArNew[[j]]<-brick(ArNew[[j]])
  ArNew[[j]]<-ArNew[[j]]
  ArNewi[[j]]<-brick(ArNewi[[j]])*(ArNew[[j]]/ArNew[[j]])
  ArNewi[[j]]<-ArNewi[[j]]
  ArNewr[[j]]<-ArNew[[j]]-ArNewi[[j]]
}

Mai_dist<-read.table("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/Maize_dist.csv",h=T)
MaizCult<-read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAO_MaizBC.csv",h=T)
MaizCult<-subset(MaizCult,FArea>1000)
countries<-as.data.frame(gridCountriesDegreesHalf)
names(countries)<-c("UN","Lon","Lat")
country<-as.data.frame(unique(countries$UN));names(country)="UN"
country<-join(country,Mai_dist);names(country)[2]="cultivar"
country$cultivar<-ifelse(!(country$UN %in% MaizCult$UN),NA,country$cultivar)
nc_wheat<-"C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/winter_and_spring_wheat_areas_phase3.nc4"
WWdist<-stack(nc_wheat,varname="wwh_mask")
SWdist<-stack(nc_wheat,varname="swh_mask")

#Calling the yield output of LPJ-GUESS
Global_data<-list()
setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Chapter2/CRU_sim/")
YIFiles<-list.files(pattern ="^LUH.*\\.out$")
a=0
for (k in YIFiles){
  a=a+1
yield=read.table(k, h=TRUE);names(yield)[3]="year"
yield<-yield[,c(1:14)]
yield<-join(yield,countries)
yield<-join(yield,country)
yield2=yield
yield2=na.locf(yield2,fromLast = TRUE)
yield2$TeCo<-ifelse(yield2$cultivar==2,yield2$TeCG,yield2$TeCS)
yield2$TeCoi<-ifelse(yield2$cultivar==2,yield2$TeCGi,yield2$TeCSi)
yield2<-yield2[,c(1,2,3,18,9,10,14,17,4,5,8)]

vect<-c(1961:2010)
li<-list()
for (i in 1:length(vect)){
  li[[i]]<-subset(yield2,year==vect[i])
  li[[i]]<-li[[i]][c(-3)]
}

crops<-names(li[[i]])[c(-1,-2)]
bb <- extent(-180, 180, -90, 90)
rastcrop<-list()
for(j in 1:8){
crop_r<-list()
for (i in 1:length(vect)){
  crop_r[[i]]<-rasterFromXYZ(li[[i]][,c(1,2,j+2)])
}

names(crop_r)<-paste0("Yi_",vect)
rastcrop[[j]]<-stack(crop_r)
rastcrop[[j]]<-extend(rastcrop[[j]],bb)
}
names(rastcrop)<-paste0(crops)


#multiplied by 10 to convert yield from kg/m2 to Ton/ha production(ha)
#Divided by 0.88, 0.87, 0.91 since FAO production is assumed to have 12% water content
#2*0.446 since the model creates yield based on 0.5 Carbon ratio but it is 0.446 in crops

WWyi<-((rastcrop[[6]]*ArNewr[[1]]+rastcrop[[2]]*ArNewi[[1]])*10/ArNew[[1]])/(2*0.446*0.88)
SWyi<-((rastcrop[[7]]*ArNewr[[1]]+rastcrop[[3]]*ArNewi[[1]])*10/ArNew[[1]])/(2*0.446*0.88)
Wyield<-WWyi*WWdist+SWyi*SWdist
Myield<-((rastcrop[[5]]*ArNewr[[2]]+rastcrop[[1]]*ArNewi[[2]])*10/ArNew[[2]])/(2*0.446*0.88)
Ryield<-((rastcrop[[8]]*ArNewr[[3]]+rastcrop[[4]]*ArNewi[[3]])*10/ArNew[[3]])/(2*0.446*0.88)
WProd<-(Wyield*ArNew[[1]])
MProd<-(Myield*ArNew[[2]])
RProd<-(Ryield*ArNew[[3]])

WGlobP<-as.numeric(as.data.frame(
  aggregate(WProd, fact=c(720,360), fun=sum)))
WGlobA<-as.numeric(as.data.frame(
  aggregate(ArNew[[1]], fact=c(720,360), fun=sum)))
WGlobY<-WGlobP/WGlobA

MGlobP<-as.numeric(as.data.frame(
  aggregate(MProd, fact=c(720,360), fun=sum)))
MGlobA<-as.numeric(as.data.frame(
  aggregate(ArNew[[2]], fact=c(720,360), fun=sum)))
MGlobY<-MGlobP/MGlobA

RGlobP<-as.numeric(as.data.frame(
  aggregate(RProd, fact=c(720,360), fun=sum)))
RGlobA<-as.numeric(as.data.frame(
  aggregate(ArNew[[3]], fact=c(720,360), fun=sum)))
RGlobY<-RGlobP/RGlobA

Gyield<-data.frame("Year"=c(1961:2010),"WYield"=WGlobY, "MYield"=MGlobY, "RYield"=RGlobY)
Global_data[[a]]<-Gyield
Gyield<-subset(Gyield,Year>1961)

X11(width=6,height=10)
par(mfrow=c(3,1),mai=c(0.3,0.7,0.2,0.2))
plot(Gyield$Year,Gyield$WYield,ylim=c(1,3.5),ylab="Yield (T/ha)",main="Wheat",
     xaxt='n',cex.lab=1.3,font.lab=2)
lines(Gyield$Year,Gyield$WYield,col="Blue")
lines(Gyield$Year,predict(lm(Gyield$WYield~Gyield$Year)),col='Red')

plot(Gyield$Year,Gyield$MYield,ylim=c(0,6),ylab="Yield (T/ha)",main="Maize",
     xaxt='n',cex.lab=1.3,font.lab=2)
lines(Gyield$Year,Gyield$MYield,col="Blue")
lines(Gyield$Year,predict(lm(Gyield$MYield~Gyield$Year)),col='Red')

plot(Gyield$Year,Gyield$RYield,ylim=c(1.5,6),ylab="Yield (T/ha)",main="Rice",
     cex.lab=1.3,font.lab=2)
lines(Gyield$Year,Gyield$RYield,col="Blue")
lines(Gyield$Year,predict(lm(Gyield$RYield~Gyield$Year)),col='Red')

dev.copy2pdf(file = paste0("Plots/",k,".pdf"))
}

names(Global_data)<-unlist(strsplit(YIFiles,split=".",fixed=TRUE))[c(TRUE,FALSE)]
