library(raster)
library(rworldmap)

setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Chapter2/CRU_sim/Npool")
Npool<-list.files(pattern="npool*")
Npool_<-list()
LC<-c("LC-1","LC-2","LC-3","LC-4")
a=0
for ( i in Npool){
  a=a+1
  temp<-read.table(i,h=T)
  Npool_[[a]]<-subset(temp,Year==1960)
  Npool_[[a]]$LC<-LC[a]
  Npool_[[a]]<-Npool_[[a]][,c(1,2,5,6,9)]
}

NpoolR<-list()
X11(width=10,height=5)
par(mfrow=c(2,2),omi=c(0.6,0,0,0),mai=c(0,0,0,0))
#layout(matrix(c(1,2,3,4,5,5),ncol=2,byrow=T),heights=c(4,4,1))
##Fixing graph for AGU
myFun<-function(x) {ifelse (x > 0.5,0.5,1)}
temp1<-raster(LUDF)
test<-calc(temp1,myFun)

for (i in 1:4){
  NpoolR[[i]]<-rasterFromXYZ(Npool_[[i]][-c(3,5)])
  NPDF<-as(NpoolR[[i]],"SpatialGridDataFrame")
  mapNpool<-mapGriddedData(NPDF,catMethod = c(seq(0,0.5,by=0.1),0.7,1,1.5,2.3,3.9,5,8),
                           colourPalette = c("white","lightyellow2","yellow","darkgoldenrod2",
                                             "darkgoldenrod","orangered","red","blueviolet",
                                             "orchid4","orchid3", "palevioletred2",
                                             "palevioletred3"),
                           borderCol = "black",
                           oceanCol="azure2",xlim=c(-180,180),ylim=c(-50,90),landCol="gray",
                           addLegend=F)
  if(i==3|i==4){do.call(addMapLegend,c(mapNpool, legendLabels="all",legendIntervals="page",
                                 legendWidth=1.1,digits=2,legendShrink=0.8,
                                 legendMar=0,labelFontSize=0.8))}
} 


#LitterN
NlitR<-list()
X11(width=10,height=5)
par(mfrow=c(2,2),omi=c(0.6,0,0,0),mai=c(0,0,0,0))
#layout(matrix(c(1,2,3,4,5,5),ncol=2,byrow=T),heights=c(4,4,1))
for (i in 1:4){
  NlitR[[i]]<-rasterFromXYZ(Npool_[[i]][-c(4,5)])
  NPDF<-as(NlitR[[i]],"SpatialGridDataFrame")
  mapNpool<-mapGriddedData(NPDF,catMethod = c(seq(0,0.02,by=0.002)),
                           colourPalette = c("white","lightyellow2","yellow",
                                             "darkgoldenrod2","orangered","red","red4",
                                             "orchid3","orchid4", 
                                             "palevioletred3"),
                           borderCol = "black",
                           oceanCol="azure2",xlim=c(-180,180),ylim=c(-50,90),landCol="gray",
                           addLegend=F)
  if(i==3|i==4){do.call(addMapLegend,c(mapNpool, legendLabels="all",legendIntervals="page",
                                       legendWidth=1.1,digits=2,legendShrink=0.8,
                                       legendMar=0,labelFontSize=0.8))}
} 

#LCluh differences with LCnat
X11(width=8,height=2.5)
par(mfrow=c(1,2),omi=c(0.6,0,0,0),mai=c(0,0,0,0))
#layout(matrix(c(1,2,3,4,5,5),ncol=2,byrow=T),heights=c(4,4,1))

DiffN<-(NpoolR[[2]]-NpoolR[[4]])
Difflit<-(NlitR[[2]]-NlitR[[4]])

DiffN_<-as(DiffN,"SpatialGridDataFrame")
mapNpool<-mapGriddedData(DiffN_,catMethod = c(-1,seq(0,1,by=0.2),2),
                         colourPalette = c("white","yellow","darkgoldenrod2",
                                           "orangered","red","red2","red4"),
                         borderCol = "black",
                         oceanCol="azure2",xlim=c(-180,180),ylim=c(-35,90),landCol="gray",
                         addLegend=F)
do.call(addMapLegend,c(mapNpool, legendLabels="all",legendIntervals="page",
                                     legendWidth=0.5,digits=2,legendShrink=0.9,
                                     legendMar=0,labelFontSize=0.8))

Difflit_<-as(Difflit,"SpatialGridDataFrame")
mapNpool<-mapGriddedData(Difflit_,catMethod = c(-0.02,seq(0,0.05,by=0.01),0.09),
                         colourPalette = c("white","yellow","darkgoldenrod2",
                                           "orangered","red","red2","red4"),
                         borderCol = "black",
                         oceanCol="azure2",xlim=c(-180,180),ylim=c(-35,90),landCol="gray",
                         addLegend=F)
do.call(addMapLegend,c(mapNpool, legendLabels="all",legendIntervals="page",
                       legendWidth=0.5,digits=2,legendShrink=0.9,
                       legendMar=0,labelFontSize=0.8))


#Land use
  LU<-read.table("lu_1901_2015_.txt",h=T)
  LU1<-subset(LU,Year<=1960)[-c(4,6)]
  LU2<-subset(LU1,Year==1960)[-3]
  LU1$Crop_time<-ifelse(LU1$CROPLAND==0,0,1)
  LU3<-aggregate(LU1,by=list(paste0(LU1$Lon,LU1$Lat)),FUN=sum)

LU_R<-rasterFromXYZ(LU2)
LUT_R<-rasterFromXYZ
LU_R<-extend(LU_R,bb)
Crops<-(ArNew[[1]][[1]]+ArNew[[2]][[1]]+ArNew[[3]][[1]])/1000
LU_land<-overlay(LU_R,fun=function(x){ifelse(!is.na(x),1,x)})
Crops<-overlay(Crops,fun=function(x){ifelse(is.na(x),0,x)})*LU_land

X11(width=10,height=3)
par(mfrow=c(1,2),omi=c(0.6,0,0,0),mai=c(0,0,0,0))
  LUDF<-as(LU_R,"SpatialGridDataFrame")
  mapLU<-mapGriddedData(LUDF,catMethod = c(seq(0,1,by=0.1)),
                           colourPalette = c("white","palegreen3","palegreen","gold",
                                             "darkorange","orangered","red","red4",
                                             "orchid4","orchid1"),
                           borderCol = "black",
                           oceanCol="azure2",xlim=c(-180,180),ylim=c(-40,90),landCol="gray",
                           addLegend=F)
  do.call(addMapLegend,c(mapLU, legendLabels="all", 
                                       legendWidth=1.1,digits=2,legendShrink=0.7,
                                       legendMar=0,labelFontSize=0.8))
  
  LUTDF<-as(Crops,"SpatialGridDataFrame")
  mapLUT<-mapGriddedData(LUTDF,catMethod = c(seq(0,64,8),100,200,300),
                        colourPalette = c("white","palegreen3","palegreen","gold",
                                          "gold4","darkorange","orangered","red","red4",
                                          "orchid4","orchid1"),
                        borderCol = "black",
                        oceanCol="azure2",xlim=c(-180,180),ylim=c(-40,90),landCol="gray",
                        addLegend=F)
  do.call(addMapLegend,c(mapLUT, legendLabels="all", legendIntervals="page",
                         legendWidth=1.1,digits=2,legendShrink=0.7,
                         legendMar=0,labelFontSize=0.7))
  
  #Nltter and Nsoil in crop higher than 0.3
  
  LU_cropR<-overlay(LU_R,fun=function (x){ifelse(x>=0.5,1,0)})
  Nlitt_crop<-list()
  Mlit_LC<-numeric()
  for (i in 1:4){
    NlitR[[i]]<-extend(NlitR[[i]],bb)
    Nlitt_crop[[i]]<-as.data.frame(LU_cropR*NlitR[[i]],xy=T)
    Mlit_LC[i]<-mean(Nlitt_crop[[i]]$layer[Nlitt_crop[[i]]$layer!=0],na.rm=T)
  }
  
  
  Nsoil_crop<-list()
  Mean_LC<-numeric()
  for (i in 1:4){
    NpoolR[[i]]<-extend(NpoolR[[i]],bb)
    Nsoil_crop[[i]]<-as.data.frame(LU_cropR*NpoolR[[i]],xy=T)
    Mean_LC[i]<-mean(Nsoil_crop[[i]]$layer[Nsoil_crop[[i]]$layer!=0],na.rm=T)
  }
  
  test<-read.table("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Chapter2/CRU_sim/LUH4yieldAllvar.out")
  