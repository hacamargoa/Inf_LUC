LPJ_yield<-Global_data[[2]]

FGlobal<- read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAOGl.csv", h=T)
FGlobal<-subset(FGlobal,year<2011)
x11(width=5,height=10)
par(mfrow=c(3,1),mai=c(0.3,0.7,0.2,0.2))
plot(LPJ_yield$Year,LPJ_yield$WYield,type="l", col="red",ylim=c(1,6),lwd=3)
LPline<-lm(WYield~Year,data=LPJ_yield)
abline(LPline)
lines(FGlobal$year,FGlobal$FW_Yield,col="blue",ylim=c(0,4),lwd=3)
FAline<-lm(FW_Yield~year,data=FGlobal)
abline(FAline)
#cor.test(LPline$residuals,FAline$residuals)

plot(LPJ_yield$Year,LPJ_yield$MYield,type="l", col="red",ylim=c(1,6),lwd=3)
LPline<-lm(MYield~Year,data=LPJ_yield)
abline(LPline)
lines(FGlobal$year,FGlobal$FM_Yield,col="blue",lwd=3)
FAline<-lm(FM_Yield~year,data=FGlobal)
abline(FAline)
#cor.test(LPline$residuals,FAline$residuals)

plot(LPJ_yield$Year,LPJ_yield$RYield,type="l", col="red",ylim=c(1,6),lwd=3)
LPline<-lm(RYield~Year,data=LPJ_yield)
abline(LPline)
lines(FGlobal$year,FGlobal$FR_Yield,col="blue",lwd=3)
FAline<-lm(FR_Yield~year,data=FGlobal)
abline(FAline)
#cor.test(LPline$residuals,FAline$residuals)

LPJ_yield<-Global_data_A[[2]]
plot(LPJ_yield$Year,LPJ_yield$WYield,type="l", col="red",ylim=c(0,4),lwd=3)
lines(FGlobal$year,FGlobal$FW_Yield,col="blue",ylim=c(0,4),lwd=3)
cor.test(LPJ_yield$WYield,FGlobal$FW_Yield)

plot(LPJ_yield$Year,LPJ_yield$MYield,type="l", col="red",ylim=c(1,6),lwd=3)
lines(FGlobal$year,FGlobal$FM_Yield,col="blue",lwd=3)
cor.test(LPJ_yield$MYield,FGlobal$FM_Yield)

plot(LPJ_yield$Year,LPJ_yield$RYield,type="l", col="red",ylim=c(1,6),lwd=3)
lines(FGlobal$year,FGlobal$FR_Yield,col="blue",lwd=3)
cor.test(LPJ_yield$RYield,FGlobal$FR_Yield)


as.numeric(as.data.frame(
  aggregate(ArNew[[3]], fact=c(720,360), fun=sum)))

plot(FGlobal$FR_Area,as.numeric(as.data.frame(
  aggregate(ArNew[[3]], fact=c(720,360), fun=sum))))
abline(1,1)

plot(FGlobal$FM_Area,as.numeric(as.data.frame(
  aggregate(ArNew[[2]], fact=c(720,360), fun=sum))))
abline(1,1)

plot(FGlobal$FW_Area,as.numeric(as.data.frame(
  aggregate(ArNew[[1]], fact=c(720,360), fun=sum))))
abline(1,1)
test<-data.frame("year"=seq(1961:2010))
test$Area<-as.numeric(as.data.frame(aggregate(ArNew[[1]], fact=c(720,360), fun=sum)))
test$FArea<-FGlobal$FW_Area


GProd<-data.frame("Year"=c(1961:2010),"WYield"=WGlobP, "MYield"=MGlobP, "RYield"=RGlobP)
plot(GProd$Year,GProd$WYield,type="l",lwd=3,ylim=c(200000000,900000000))
lines(FGlobal$year,FGlobal$FW_Prod,col="blue",lwd=3)

plot(GProd$Year,GProd$MYield,type="l",lwd=3,ylim=c(200000000,900000000))
lines(FGlobal$year,FGlobal$FM_Prod,col="blue",lwd=3)

plot(GProd$Year,GProd$RYield,type="l",lwd=3,ylim=c(200000000,900000000))
lines(FGlobal$year,FGlobal$FR_Prod,col="blue",lwd=3)

plot(GProd$Year,GProd$WYield/FGlobal$FW_Area,type="l",lwd=3,ylim=c(1,6))
lines(FGlobal$year,FGlobal$FW_Yield,col="blue",lwd=3)

plot(GProd$Year,GProd$MYield/FGlobal$FM_Area,type="l",lwd=3,ylim=c(1,6))
lines(FGlobal$year,FGlobal$FM_Yield,col="blue",lwd=3)

plot(GProd$Year,GProd$RYield/FGlobal$FR_Area,type="l",lwd=3,ylim=c(1,6))
lines(FGlobal$year,FGlobal$FR_Yield,col="blue",lwd=3)

