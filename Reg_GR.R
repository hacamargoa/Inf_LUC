library(ncdf4)
library(raster)
library(rworldmap)
library(dplyr)
library(plyr)
library(forecast)
library(zoo)
setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Chapter2/CRU_sim/")
yield=read.table("LUH4yieldAllvar.out", h=TRUE)
#yield=read.table("LUH1yieldAllvar.out", h=TRUE)
yield<-yield[,c(1:14)]
yield<-join(yield,countries)
yield<-join(yield,country)
yield2=yield
yield2=na.locf(yield2,fromLast = TRUE)
yield2$TeCo<-ifelse(yield2$cultivar==2,yield2$TeCG,yield2$TeCS)
yield2$TeCoi<-ifelse(yield2$cultivar==2,yield2$TeCGi,yield2$TeCSi)
yield2<-yield2[,c(1,2,3,18,9,10,14,17,4,5,8)]
colnames(yield2)[3]<-"year"
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
Rast_crops<-list(Wyield,Myield,Ryield)
#REGRESSION
regfunG<-function(x){
  GRY=lm(yield~year,data=x,na.action=na.omit)
  return(data.frame(Slope=GRY$coefficients[2],Int=GRY$coefficients[1]))
  }
yield_reg<-list()
regGRY<-list()
regGRY1<-list()
Cropr<-list()
y_df<-list()
regGRY<-list()
Slo_ras<-list()
Int_ras<-list()
for (i in 1:3){
  temp<-as.data.frame(Rast_crops[[i]],xy=T)
  temp$num<-seq(1,259200)
  temp1<-do.call("rbind",replicate(50,temp[,c(1,2,53)],simplify=F))
  temp1$year<-rep(seq(1,50),each=259200)
  y_df[[i]]<-temp1
  y_df[[i]]$yield<-unlist(temp[,-c(1,2,53)])
  y_df[[i]]<-y_df[[i]][order(y_df[[i]]$num),]
  y_df[[i]]<-na.omit(y_df[[i]])
  regGRY[[i]]<-ddply(y_df[[i]], .(num),regfunG)
  test<-join(y_df[[i]][which(y_df[[i]]$year==1),-5],regGRY[[i]])
  Slo_ras[[i]]<-rasterFromXYZ(test[,-c(3,4,6)]);Slo_ras[[i]]<-extend(Slo_ras[[i]],bb)
  Int_ras[[i]]<-rasterFromXYZ(test[,-c(3,4,5)]);Int_ras[[i]]<-extend(Int_ras[[i]],bb)
}

liml=10
limu=-10
liml_=10
limu_=-10
for (i in 1:3){ 
  temp<-as(Slo_ras[[i]],"SpatialGridDataFrame")
  temp_<-as(Int_ras[[i]],"SpatialGridDataFrame")
  liml<-min(liml,quantile(temp$Slope, 0.005,na.rm =T))
  limu<-max(limu,quantile(temp$Slope,0.995,na.rm =T))
  liml_<-min(liml_,quantile(temp_$Int, 0.005,na.rm =T))
  limu_<-max(limu_,quantile(temp_$Int,0.995,na.rm =T))
}
rang<-(limu-liml)/10
rang_<-(limu_-liml_)/10
leg<- c(seq(liml,limu,by=rang))
leg_<- c(seq(liml_,limu_,by=rang_))
ind<-length(leg[which(leg<=0)])
ind_<-length(leg[which(leg_<=0)])
col_<-c("palegreen3","palegreen","gold",
       "darkorange","orangered","red","red4",
       "orchid4","orchid1")
col<-append(col_,"white",after=ind-1)
col_<-append(col_,"white",after=ind_-1)


X11(width=18,height=13)
par(mfrow=c(3,2),omi=c(0.3,0,0,0),mai=c(0,0,0,0))
for (i in 1:3){
SloDF<-as(Slo_ras[[i]],"SpatialGridDataFrame")
mapSlo<-mapGriddedData(SloDF,catMethod = leg,
                      colourPalette = col,
                      borderCol = "black",
                      oceanCol="azure2",xlim=c(-180,180),ylim=c(-30,90),landCol="gray",
                      addLegend=F)
if (i==3){
do.call(addMapLegend,c(mapSlo, legendLabels="all", 
                      legendWidth=1.1,digits=2,legendShrink=0.9,
                       legendMar=0,labelFontSize=0.9))
}

IntDF<-as(Int_ras[[i]],"SpatialGridDataFrame")
mapLUT<-mapGriddedData(IntDF,catMethod = leg_,
                       colourPalette = col_,
                       borderCol = "black",
                       oceanCol="azure2",xlim=c(-180,180),ylim=c(-30,90),landCol="gray",
                       addLegend=F)
if (i==3){
do.call(addMapLegend,c(mapLUT, legendLabels="all", legendIntervals="page",
                       legendWidth=1.1,digits=2,legendShrink=0.9,
                       legendMar=0,labelFontSize=1))
}
}


Slo_ras<-test1
Int_ras<-test2
LC1_slo<-Slo_ras
LC1_int<-Int_ras
intdiff<-list()
slodiff<-list()

liml=0
limu=0
liml_=0
limu_=0
for (i in 1:3){ 
slodiff[[i]]<-Slo_ras[[i]]-LC1_slo[[i]]
intdiff[[i]]<-Int_ras[[i]]-LC1_int[[i]]
temp<-as(slodiff[[i]],"SpatialGridDataFrame")
temp_<-as(intdiff[[i]],"SpatialGridDataFrame")
  liml<-min(liml,quantile(temp$layer, 0.05,na.rm =T))
  limu<-max(limu,quantile(temp$layer,0.95,na.rm =T))
  liml_<-min(liml_,quantile(temp_$layer, 0.05,na.rm =T))
  limu_<-max(limu_,quantile(temp_$layer,0.95,na.rm =T))
  }

rang<-(limu-liml)/8
rang_<-(limu_-liml_)/8
leg<- c(seq(liml,limu,by=rang))
leg_<- c(seq(liml_,limu_,by=rang_))
ind<-length(leg[which(leg<=0)])
ind_<-length(leg[which(leg_<=0)])
col_<-c("palegreen3","palegreen","gold",
       "darkorange","red","red4",
       "orchid4")
col<-append(col_,"white",after=ind-1)
col_<-append(col_,"white",after=ind_-1)

X11(width=18,height=13)
par(mfrow=c(3,2),omi=c(0.3,0,0,0),mai=c(0,0,0,0))
for (i in 1:3){
  SloDF<-as(slodiff[[i]],"SpatialGridDataFrame")
  mapSlo<-mapGriddedData(SloDF,catMethod = leg,
                         colourPalette = col,
                         borderCol = "black",
                         oceanCol="azure2",xlim=c(-180,180),ylim=c(-30,90),landCol="gray",
                         addLegend=F)
  if (i==3){
  do.call(addMapLegend,c(mapSlo, legendLabels="all", 
                         legendWidth=1.1,digits=2,legendShrink=0.9,
                         legendMar=0,labelFontSize=0.9))
  }
  IntDF<-as(intdiff[[i]],"SpatialGridDataFrame")
  mapLUT<-mapGriddedData(IntDF,catMethod = leg_,
                         colourPalette = col_,
                         borderCol = "black",
                         oceanCol="azure2",xlim=c(-180,180),ylim=c(-30,90),landCol="gray",
                         addLegend=F)
  if (i==3){
  do.call(addMapLegend,c(mapLUT, legendLabels="all",
                         legendWidth=1.1,digits=2,legendShrink=0.9,
                         legendMar=0,labelFontSize=1))
  }
}

