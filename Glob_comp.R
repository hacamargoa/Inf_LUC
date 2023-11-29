LPJ_yield<-Global_data[[2]][-1,]
LPJ_yield2<-Global_data[[23]][-1,]

FGlobal<- read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAOGl.csv", h=T)
FGlobal<-subset(FGlobal,year<2011)[-1,]
x11(width=15,height=20)
par(mfrow=c(3,1),mai=c(0.2,0.65,0.2,0.05))
layout(matrix(c(1,2,3,4),ncol=1,byrow=T),heights=c(4,4,4,1))
plot(LPJ_yield$Year,LPJ_yield$WYield,type="l", col="red",ylim=c(1,4),lwd=3,
     ylab=quote(bold("Yield (T/ha)")),cex.lab=1.4,cex.axis=1.3)
LPline<-lm(WYield~Year,data=LPJ_yield)
abline(LPline,col="red")
lines(LPJ_yield2$Year,LPJ_yield2$WYield,col="blue",ylim=c(0,4),lwd=3)
LPline<-lm(WYield~Year,data=LPJ_yield2)
abline(LPline,col="blue")
lines(FGlobal$year,FGlobal$FW_Yield,ylim=c(0,4),lwd=3)
FAline<-lm(FW_Yield~year,data=FGlobal)
abline(FAline)
lines(Gyield$Year,Gyield$WYield,col="orange",lwd=3)
Grossline<-lm(WYield~Year,data=Gyield)
abline(Grossline,col="orange")
# lines(GyieldA$Year,GyieldA$WYield,col="green4",lwd=3)
# Grossline<-lm(WYield~Year,data=GyieldA)
# abline(Grossline,col="green4")

plot(LPJ_yield$Year,LPJ_yield$MYield,type="l", col="red",ylim=c(1,6),lwd=3,
     ylab=quote(bold("Yield (T/ha)")),cex.lab=1.4,cex.axis=1.3)
LPline<-lm(MYield~Year,data=LPJ_yield)
abline(LPline,col="red")
lines(LPJ_yield2$Year,LPJ_yield2$MYield,col="blue",ylim=c(0,4),lwd=3)
LPline<-lm(MYield~Year,data=LPJ_yield2)
abline(LPline,col="blue")
lines(FGlobal$year,FGlobal$FM_Yield,lwd=3)
FAline<-lm(FM_Yield~year,data=FGlobal)
abline(FAline)
lines(GyieldC$Year,GyieldC$MYield,col="orange",lwd=3)
Grossline<-lm(MYield~Year,data=GyieldC)
abline(Grossline,col="orange")
# lines(GyieldA$Year,GyieldA$MYield,col="green4",lwd=3)
# Grossline<-lm(MYield~Year,data=GyieldA)
# abline(Grossline,col="green4")

plot(LPJ_yield$Year,LPJ_yield$RYield,type="l", col="red",ylim=c(1,6),lwd=3,
     ylab=quote(bold("Yield (T/ha)")),cex.lab=1.4,cex.axis=1.3)
LPline<-lm(RYield~Year,data=LPJ_yield)
abline(LPline,col="red")
lines(LPJ_yield2$Year,LPJ_yield2$RYield,col="blue",ylim=c(0,4),lwd=3)
LPline<-lm(RYield~Year,data=LPJ_yield2)
abline(LPline,col="blue")
lines(FGlobal$year,FGlobal$FR_Yield,lwd=3)
FAline<-lm(FR_Yield~year,data=FGlobal)
abline(FAline)
lines(GyieldC$Year,GyieldC$RYield,col="orange",lwd=3)
Grossline<-lm(RYield~Year,data=GyieldC)
abline(Grossline,col="orange")
lines(GyieldA$Year,GyieldA$RYield,col="green4",lwd=3)
Grossline<-lm(RYield~Year,data=GyieldA)
abline(Grossline,col="green4")

par(mai=c(0,0,0,0))
plot.new()
legend(x="right", legend=c(expression(LC[crop]),expression(LC[LUH]),"FAO",expression(LC[LUHGross]),"AgMERRA"), col=c("red","blue","black","orange","green4"), ncol=5, 
       lty=c(1,1),cex=1,lwd=2,bty="n")

#correlation
cor.test(LPline$residuals,FAline$residuals)

#line comparisons
test<-FGlobal[,c(1,4,7,10)]
colnames(test)<-names(LPJ_yield)
GlobY_df<-rbind(LPJ_yield,test)
GlobY_df$Source<-rep(c("L","F"),each=50)
GlobY_df$year<-GlobY_df$Year-1960
Dumm_Glob<-list()
for (i in 1:3){
  Dumm_Glob[[i]]<-lm(GlobY_df[,i+1]~Source*year,data=GlobY_df)
  }
summary(Dumm_Glob[[3]])
