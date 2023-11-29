library(raster)
library(ncdf4)
library(rworldmap)
library(dplyr)
library(plyr)
library(forecast)
library(zoo)
#library(ggplot2)
#library(rgdal)
#library(sf)
library(maps)
library(grid)
library(plotrix)


setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/data/env")
Nfert<-read.table("NfertN.dat",h=T)
Nfert1<-subset(Nfert,Year==2000)
Nfert1<-Nfert1[,-c(5:10,12:13)]
Nfert_sdf<-rasterFromXYZ(Nfert1[,-c(3)])
Nfert_sdf<-extend(Nfert_sdf,bb)

tresholds<-c(quantile(Nfert1$TeSW,probs=0.80,na.rm=T),quantile(Nfert1$TeCGi,probs=0.80,na.rm=T),quantile(Nfert1$TrRin,probs=0.80,na.rm=T))
#tresholds<-c(quantile(Nfert1$TeSW[which(Nfert1$TeSW!=0)],probs=0.70,na.rm=T),
             #quantile(Nfert1$TeCGi[which(Nfert1$TeCGi!=0)],probs=0.70,na.rm=T),
             #quantile(Nfert1$TrRin[which(Nfert1$TrRin!=0)],probs=0.70,na.rm=T))
Nfert_low<-list()
Nfert_high<-list()
for (i in 1:3){
func<-function(x){ifelse(x>tresholds[i],1,0)}
Nfert_high[[i]]<-calc(Nfert_sdf[[i]],func)
func2<-function(x){ifelse(x<=tresholds[i]&x!=0,1,0)}
Nfert_low[[i]]<-calc(Nfert_sdf[[i]],func2)
}

#fertilization maps transform from kg/m2 to kg/ha
X11(width=8,height=8)
par(mfrow=c(4,2),omi=c(0.2,0,0,0),mai=c(0,0,0,0))
layout(matrix(c(1,2,3,4,5,6,7,6),ncol=2,byrow=T),heights=c(4,4,4,1))
for (i in 1:3){
NfertR[[i]]<-Nfert_low[[i]]+Nfert_high[[i]]+Nfert_high[[i]]
NDF<-as(NfertR[[i]],"SpatialGridDataFrame")
mapMyield<-mapGriddedData(NDF,catMethod = c(0,0.9,1.1,2.1),
                          colourPalette = c("gray","orchid","royalblue"),
                          borderCol = "black",oceanCol="azure2",xlim=c(-180,180),ylim=c(-90,90),landCol="gray",addLegend=FALSE)

if(i==3){par(mai=c(0.465,0,0.0,0))}
Nfert_sdf[[i]]<-reclassify(Nfert_sdf[[i]], cbind(-1, 0, NA), right=T)
NDF2<-as(Nfert_sdf[[i]]*10000,"SpatialGridDataFrame")
mapMyield<-mapGriddedData(NDF2,catMethod = c(seq(0,300,30)),colourPalette = "heat",
                          borderCol = "black",oceanCol="azure2",
                          xlim=c(-180,180),ylim=c(-90,90),landCol="gray",
                          addLegend=F)

}
do.call(addMapLegend,c(mapMyield, legendLabels="all", legendWidth=1.1,digits=1,legendShrink=0.9,legendMar=3.5))
par(mai=c(0,0,1,0))
plot.new()
legend(x="center", legend=c("Low N","High N"), fill=c("orchid","royalblue"), ncol=2, 
       cex=1.3,bty="n")

legend.col(col=colr,lev)
legend.col(col = colr, lev = myDat$z)
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


#Fert vs differnce between LC1 and LC2  at gridded level
#Calling the yield output of LPJ-GUESS
Global_data_H<-list()
Global_data_L<-list()
setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Chapter2/CRU_sim/")
YIFiles2<-list.files(pattern ="^LUH.*\\Allvar.out$")
X11(width=20,height=17)
par(mfrow=c(4,3),mai=c(0.15,0.45,0.2,0.1))
layout(matrix(c(1:13,13,13),ncol=3,byrow=T),heights=c(4,4,4,4,1))
LCnames<-c(expression(bold(LC[crop])),expression(bold(LC[nat])),expression(bold(LC[natpas])),expression(bold(LC[LUH])))
a=0
for (k in YIFiles2){
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
  for(j in 1:length(crops)){
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
  
  Wprod_high<-WProd*Nfert_high[[1]];WArea_high<-ArNew[[1]]*Nfert_high[[1]]
  Wprod_low<-WProd*Nfert_low[[1]];WArea_low<-ArNew[[1]]*Nfert_low[[1]]
  
  Mprod_high<-MProd*Nfert_high[[2]];MArea_high<-ArNew[[2]]*Nfert_high[[2]]
  Mprod_low<-MProd*Nfert_low[[2]];MArea_low<-ArNew[[2]]*Nfert_low[[2]]
  
  Rprod_high<-RProd*Nfert_high[[3]];RArea_high<-ArNew[[3]]*Nfert_high[[3]]
  Rprod_low<-RProd*Nfert_low[[3]];RArea_low<-ArNew[[3]]*Nfert_low[[3]]
  
  
  WGlobP_H<-as.numeric(as.data.frame(aggregate(Wprod_high, fact=c(720,360), fun=sum)))
  WGlobP_L<-as.numeric(as.data.frame(aggregate(Wprod_low, fact=c(720,360), fun=sum)))
  
  WGlobA_H<-as.numeric(as.data.frame(aggregate(WArea_high, fact=c(720,360), fun=sum)))
  WGlobA_L<-as.numeric(as.data.frame(aggregate(WArea_low, fact=c(720,360), fun=sum)))
  
  WGlobY_H<-WGlobP_H/WGlobA_H
  WGlobY_L<-WGlobP_L/WGlobA_L
  
  MGlobP_H<-as.numeric(as.data.frame(aggregate(Mprod_high, fact=c(720,360), fun=sum)))
  MGlobP_L<-as.numeric(as.data.frame(aggregate(Mprod_low, fact=c(720,360), fun=sum)))
  
  MGlobA_H<-as.numeric(as.data.frame(aggregate(MArea_high, fact=c(720,360), fun=sum)))
  MGlobA_L<-as.numeric(as.data.frame(aggregate(MArea_low, fact=c(720,360), fun=sum)))
  
  MGlobY_H<-MGlobP_H/MGlobA_H
  MGlobY_L<-MGlobP_L/MGlobA_L
  
  RGlobP_H<-as.numeric(as.data.frame(aggregate(Rprod_high, fact=c(720,360), fun=sum)))
  RGlobP_L<-as.numeric(as.data.frame(aggregate(Rprod_low, fact=c(720,360), fun=sum)))
  
  RGlobA_H<-as.numeric(as.data.frame(aggregate(RArea_high, fact=c(720,360), fun=sum)))
  RGlobA_L<-as.numeric(as.data.frame(aggregate(RArea_low, fact=c(720,360), fun=sum)))
  
  RGlobY_H<-RGlobP_H/RGlobA_H
  RGlobY_L<-RGlobP_L/RGlobA_L
  
  
  Gyield_H<-data.frame("Year"=c(1961:2010),"WYield"=WGlobY_H, "MYield"=MGlobY_H, "RYield"=RGlobY_H)
  Gyield_L<-data.frame("Year"=c(1961:2010),"WYield"=WGlobY_L, "MYield"=MGlobY_L, "RYield"=RGlobY_L)
  Global_data_H[[a]]<-Gyield_H
  Global_data_L[[a]]<-Gyield_L

  Gyield_H_<-subset(Gyield_H,Year>1965)
  Gyield_L_<-subset(Gyield_L,Year>1965)
  
  plot(Gyield_H_$Year,Gyield_H_$WYield,ylim=c(1,4.5),ylab="",
       main=ifelse(a==1,"Wheat",""),
       cex.lab=1.3,font.lab=2)
  title(ylab=LCnames[a], line=1.8, cex.lab=1.35,font.lab=2)
  #title(ylab=expression(bold(LC[crop])), line=1.8, cex.lab=1.3,font.lab=2)
  points(Gyield_L_$Year,Gyield_L_$WYield)
  lines(Gyield_H_$Year,Gyield_H_$WYield,col="Blue")
  lines(Gyield_L_$Year,Gyield_L_$WYield,col="Red")
  lines(Gyield_H_$Year,predict(lm(Gyield_H_$WYield~Gyield_H_$Year)),col='Black')
  lines(Gyield_L_$Year,predict(lm(Gyield_L_$WYield~Gyield_L_$Year)),col='Black')
  
  plot(Gyield_H_$Year,Gyield_H_$MYield,ylim=c(1,9),ylab="",
       main=ifelse(a==1,"Maize",""),
       cex.lab=1.3,font.lab=2)
  points(Gyield_L_$Year,Gyield_L_$MYield)
  lines(Gyield_H_$Year,Gyield_H_$MYield,col="Blue")
  lines(Gyield_L_$Year,Gyield_L_$MYield,col="Red")
  lines(Gyield_H_$Year,predict(lm(Gyield_H_$MYield~Gyield_H_$Year)),col='Black')
  lines(Gyield_L_$Year,predict(lm(Gyield_L_$MYield~Gyield_L_$Year)),col='Black')
  
  plot(Gyield_H_$Year,Gyield_H_$RYield,ylim=c(2,6),ylab="",
       main=ifelse(a==1,"Rice",""),
        cex.lab=1.3,font.lab=2)
  points(Gyield_L_$Year,Gyield_L_$RYield)
  lines(Gyield_H_$Year,Gyield_H_$RYield,col="Blue")
  lines(Gyield_L_$Year,Gyield_L_$RYield,col="Red")
  lines(Gyield_H_$Year,predict(lm(Gyield_H_$RYield~Gyield_H_$Year)),col='Black')
  lines(Gyield_L_$Year,predict(lm(Gyield_L_$RYield~Gyield_L_$Year)),col='Black')

 # dev.copy2pdf(file = paste0("Plots_Nfert/",k,".pdf"))
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=c("High N","Low N"), col=c("Blue","Red"), ncol=2, 
       lty=c(1,1),cex=1.3,lwd=2,bty="n")

#Regressions

Treats_N<-data.frame("LUC"=rep(LUC,each=100),"Dr"=rep(rep(c("Allcons","Allvar"),each=50),4))

Chap_2N<-c(Global_data_H[c(1,2,8,9,15,16,22,23)],Global_data_L[c(1,2,8,9,15,16,22,23)])
YielsN<-bind_rows(Chap_2N)
Treats_N<-rbind(Treats_N,Treats_N)
Treats_N$Fert<-rep(c("High","Low"),each=400)
YielsN<-cbind(Treats_N,YielsN)
YielsN$year<-YielsN$Year-1960
Dumm_N<-list()
for (i in 1:3){
  a=0
  for (j in c("Allcons","Allvar")){
    a=a+1
    temp1<-YielsN[which(YielsN$Dr==j),]
    b=0
      for (k in LUC){
        b=b+1
      temp<-temp1[which(temp1$LUC==k),]
  Dumm_N[[paste0(i,j,k)]]<-lm(temp[,i+4]~Fert+year+year*Fert,data=temp)
    }
  }
}

Treat<-data.frame("Crop"=rep(c("Wheat","Maize","Rice"),each=8),"Dr"=rep(rep(c("Allcons","Allvar"),each=4),3),"LUC"=rep(LUC,6))
for (i in 1:24){
  Treat$Int_diff[i]<-Dumm_N[[i]]$coefficients[2]
  Treat$p_val[i]<-summary(Dumm_N[[i]])$coefficients[2,4]
}
Treat2<-data.frame(Treat[c(1:8),c(1:4)],"Maize"=Treat[c(9:16),4],"Rice"=Treat[c(17:24),4])
Treat2<-Treat2[c(1:4),]
colnames(Treat2)[4]<-"Wheat"

#Paired t test

YieldsN_<-subset(YielsN,Year<=1970)
Dumm2_N<-list()
for (i in 1:3){
  a=0
  for (j in c("Allcons","Allvar")){
    a=a+1
    temp1<-YieldsN_[which(YieldsN_$Dr==j),]
    b=0
    for (k in LUC){
      b=b+1
      temp<-temp1[which(temp1$LUC==k),]
      Dumm2_N[[paste0(i,j,k)]]<-t.test(temp[c(1:10),i+4],
                                       temp[c(11:20),i+4],paired=T,alternative="two.sided")
    }
  }
}

DB<-Treat[c(1:8),c(2,3)]
for (i in 1:8){
  DB$Wheat[i]<-Dumm2_N[[i]]$p.value
  DB$Maize[i]<-Dumm2_N[[i+8]]$p.value
  DB$Rice[i]<-Dumm2_N[[i+16]]$p.value
}

#barplot Make it manual to get the stars
Inter<-data.frame(matrix(nrow=1,ncol=1))
a=c("*"," "," ","*")
b=c("*","*","*","*")
a1=c("   *"," "," ","   *")
b1=c("   *","   *","   *","   *")   
X11(width=8,height=15)
par(mfrow=c(3,1),mai=c(0.2,0.65,0.2,0.05))
layout(matrix(c(1,2,3,4),ncol=1,byrow=T),heights=c(4,4,4,1))
for (j in 1:3){
  for (i in 1:4){
    Inter[,i] <- Treat2[which(Treat2$LUC==unique(Treat2$LUC)[i]),j+3]
  }
  rownames(Inter)<-""
  colnames(Inter)<-c(unique(Treat2$LUC))
  Inter<-abs(Inter)
  
  test<-barplot(t(Inter), beside=TRUE, ylab="", xlab="",
                cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
                ylim=c(0,range(Inter)[2]+abs(range(Inter)[1]*0.20)))
  text(test,y=as.numeric(Inter[1,]+abs(range(Inter)[1]*0.10)),
       label=a,cex = 2)
  text(test,y=as.numeric(Inter[1,]+abs(range(Inter)[1]*0.10)-0.01), #0.0025#0.09 #0.01
       label=b1,cex = 2,col="Red")
  box(bty="l")
  title(xlab="",ylab=ifelse(j==2,expression(paste(bold("Intercept diff"))),""), line=3.4, cex.lab=1.5)
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=c(as.expression(bquote('LC'['crop'])),
                            as.expression(bquote('LC'['nat'])),
                            as.expression(bquote('LC'['natpas'])),
                            as.expression(bquote('LC'['LUH']))),
       fill=col__[c(1:4)], ncol=4, 
       bty="n",cex=1.3)

