library(car)
library(foreign)
library(agricolae)
library(plyr)
library(dplyr)


#Calculating slopes
Clim<-c("CRU","Ag")
LUC<-paste0(c("LUH"),c(1:4))
Driver<-c("Allcons","Allvar","CO2var","Fervar","Precvar","Radvar","Tempvar")
Comb<-paste0(rep(Clim,each=28),rep(rep(LUC,each=7),2),rep(Driver,8))
Treats<-data.frame("Clim"=rep(Clim,each=28),"LUC"=rep(rep(LUC,each=7),2),"Dr"=rep(Driver,8))


Chap_2<-c(Global_data,Global_data_A)
Chap_2<-lapply(Chap_2,function(x) {x[-c(1:5),]})
names(Chap_2)<-Comb

for (i in 1:length(Chap_2)){
  temp<-Treats[rep(i,dim(Chap_2[[i]])[1]),]
  Chap_2[[i]]<-cbind(temp,Chap_2[[i]])
}
Yiels<-bind_rows(Chap_2)
colnames(Yiels)[4]<-"year"
Yields_CRU<-subset(Yiels,Clim=="CRU")
Yields_CRU$Year<-Yields_CRU$year-1960
Yields_Ag<-subset(Yiels,Clim=="Ag")
Yields_Ag$Year<-Yields_Ag$year-1980

Ancov_Ag<-list()
Ancov_CRU<-list()
for (i in 1:3){
Ancov_Ag[[i]]<-lm(Yields_Ag[,i+4]~Year*Dr*LUC,data=Yields_Ag)
Ancov_CRU[[i]]<-lm(Yields_CRU[,i+4]~Year*Dr*LUC,data=Yields_CRU)
Yields_CRU[,i+8]<-ifelse(Yields_CRU$LUC==paste0("LUH",i+1),1,0)
Yields_Ag[,i+8]<-ifelse(Yields_Ag$LUC==paste0("LUH",i+1),1,0)
}
colnames(Yields_CRU)[c(9:11)]<-c("DL1","DL2","DL3")
colnames(Yields_Ag)[c(9:11)]<-c("DL1","DL2","DL3")
Anova(Ancov_CRU[[3]])
cbind(anova(Ancov_CRU[[1]])[5],anova(Ancov_Ag[[1]])[5],
      anova(Ancov_CRU[[2]])[5],anova(Ancov_Ag[[2]])[5],
      anova(Ancov_CRU[[3]])[5],anova(Ancov_Ag[[3]])[5])

Dumm_Ag<-list()
Dumm_CRU<-list()
for (i in 1:3){
Dumm_Ag[[i]]<-lm(Yields_Ag[,i+4]~DL1+DL2+DL3+Year+Year*DL1+Year*DL2+Year*DL3,data=Yields_Ag)
Dumm_CRU[[i]]<-lm(Yields_CRU[,i+4]~DL1+DL2+DL3+Year+Year*DL1+Year*DL2+Year*DL3,data=Yields_CRU)
}
cbind(anova(Dumm_CRU[[1]])[5],anova(Dumm_Ag[[1]])[5],
      anova(Dumm_CRU[[2]])[5],anova(Dumm_Ag[[2]])[5],
      anova(Dumm_CRU[[3]])[5],anova(Dumm_Ag[[3]])[5])

Driver_<-c("Allvar","Allcons","CO2var","Fervar","Precvar","Radvar","Tempvar")
for (i in 1:6){ 
Yields_CRU[,i+11]<-ifelse(Yields_CRU$Dr==Driver_[i+1],1,0)
Yields_Ag[,i+11]<-ifelse(Yields_Ag$Dr==Driver_[i+1],1,0)
}
colnames(Yields_CRU)[c(12:17)]<-c("DD1","DD2","DD3","DD4","DD5","DD6")
colnames(Yields_Ag)[c(12:17)]<-c("DD1","DD2","DD3","DD4","DD5","DD6")

Dumm2_Ag<-list()
Dumm2_CRU<-list()
for (i in 1:3){
  Dumm2_Ag[[i]]<-lm(Yields_Ag[,i+4]~DD1+DD2+DD3+DD4+DD5+DD6+Year+Year*DD1+
                     Year*DD2+Year*DD3+Year*DD4+Year*DD5+Year*DD6,data=Yields_Ag)
  Dumm2_CRU[[i]]<-lm(Yields_CRU[,i+4]~DD1+DD2+DD3+DD4+DD5+DD6+Year+Year*DD1+
                      Year*DD2+Year*DD3+Year*DD4+Year*DD5+Year*DD6,data=Yields_CRU)
}

cbind(anova(Dumm2_CRU[[1]])[5],anova(Dumm2_Ag[[1]])[5],
      anova(Dumm2_CRU[[2]])[5],anova(Dumm2_Ag[[2]])[5],
      anova(Dumm2_CRU[[3]])[5],anova(Dumm2_Ag[[3]])[5])

#Plots

X11(width=12,height=10)
par(mfrow=c(3,2),mai=c(0.3,0.57,0.18,0.08))
col_<-c("Red","Blue","forestgreen")
for (i in 1:3){
  plot(Yields_Ag[,i+4]~Yields_Ag$year,type="n",ylab=substitute(paste(bold("Yield (T/ha)"))),xlab="",
       main=ifelse(i==1,"AgMERRA",""),cex.lab=1.4,cex.axis=1.3,cex.main=1.5)
  coeff<-Dumm_Ag[[i]]$coefficients[1]+Dumm_Ag[[i]]$coefficients[5]*(-1980)
  abline(coeff,Dumm_Ag[[i]]$coefficients[5],lwd=2)
  for (j in 1:3){
    coeff_<-Dumm_Ag[[i]]$coefficients[1]+Dumm_Ag[[i]]$coefficients[j+1]+(Dumm_Ag[[i]]$coefficients[5]+Dumm_Ag[[i]]$coefficients[j+5])*(-1980)
  abline(coeff_,Dumm_Ag[[i]]$coefficients[5]+Dumm_Ag[[i]]$coefficients[j+5],col=col_[j],lwd=2) 
  }
  plot(Yields_CRU[,i+4]~Yields_CRU$year,type="n",ylab="",xlab=ifelse(i==3,"Year",""),
       main=ifelse(i==1,"CRU",""),cex.lab=1.4,cex.axis=1.3,cex.main=1.5)
  coeff<-Dumm_CRU[[i]]$coefficients[1]+Dumm_CRU[[i]]$coefficients[5]*(-1960)
  abline(coeff,Dumm_CRU[[i]]$coefficients[5],lwd=2)
  for (j in 1:3){
    coeff_<-Dumm_CRU[[i]]$coefficients[1]+Dumm_CRU[[i]]$coefficients[j+1]+(Dumm_CRU[[i]]$coefficients[5]+Dumm_CRU[[i]]$coefficients[j+5])*(-1960)
  abline(coeff_,Dumm_CRU[[i]]$coefficients[5]+Dumm_CRU[[i]]$coefficients[j+5],col=col_[j],lwd=2)
  }
  }

X11(width=12,height=10)
par(mfrow=c(3,2),mai=c(0.3,0.57,0.18,0.08))
col_<-c("Red","Blue","forestgreen","darkorange3","gold3","gray54")
for (i in 1:3){
  plot(Yields_Ag[,i+4]~Yields_Ag$year,type="n",ylab=substitute(paste(bold("Yield (T/ha)"))),xlab="",
       main=ifelse(i==1,"AgMERRA",""),cex.lab=1.4,cex.axis=1.3,cex.main=1.5)
  coeff<-Dumm2_Ag[[i]]$coefficients[1]+Dumm2_Ag[[i]]$coefficients[8]*(-1980)
  abline(coeff,Dumm2_Ag[[i]]$coefficients[8],lwd=2)
  for (j in 1:6){
    coeff_<-Dumm2_Ag[[i]]$coefficients[1]+Dumm2_Ag[[i]]$coefficients[j+1]+(Dumm2_Ag[[i]]$coefficients[8]+Dumm2_Ag[[i]]$coefficients[j+8])*(-1980)
    abline(coeff_,Dumm2_Ag[[i]]$coefficients[8]+Dumm2_Ag[[i]]$coefficients[j+8],col=col_[j],lwd=2) 
  }
  
  plot(Yields_CRU[,i+4]~Yields_CRU$year,type="n",ylab=substitute(paste(bold("Yield (T/ha)"))),xlab="",
       main=ifelse(i==1,"CRU",""),cex.lab=1.4,cex.axis=1.3,cex.main=1.5)
  coeff<-Dumm2_CRU[[i]]$coefficients[1]+Dumm2_CRU[[i]]$coefficients[8]*(-1960)
  abline(coeff,Dumm2_CRU[[i]]$coefficients[8],lwd=2)
  for (j in 1:6){
    coeff_<-Dumm2_CRU[[i]]$coefficients[1]+Dumm2_CRU[[i]]$coefficients[j+1]+(Dumm2_CRU[[i]]$coefficients[8]+Dumm2_CRU[[i]]$coefficients[j+8])*(-1960)
    abline(coeff_,Dumm2_CRU[[i]]$coefficients[8]+Dumm2_CRU[[i]]$coefficients[j+8],col=col_[j],lwd=2,xpd=F) 
  }
}



slopes<-cbind(Treats,matrix(ncol = 15, nrow = 56))
colnames(slopes)[c(4:18)]<-c("slopeW","slopeM","slopeR","IntW","IntM","IntR","PvSlW","PvSlM",
                    "PvSlR","PvInW","PvInM","PvInR","RsqW","RsqM","RsqR")

Chap_2detr<-list()
for (j in 1:length(Chap_2)){
  temp1<-data.frame(Year=Chap_2[[j]]$Year)
  for (i in 1:3){
    temp<-lm(Chap_2[[j]][,i+4]~Chap_2[[j]]$Year)
    slopes[j,i+3]<-temp$coefficients[2]
    slopes[j,i+6]<-temp$coefficients[1]
    slopes[j,i+9]<-summary(temp)$coefficients[,4][2]
    slopes[j,i+12]<-summary(temp)$coefficients[,4][1]
    slopes[j,i+15]<-summary(temp)$r.square
    temp1[,i+1]<-temp$residuals
  }
  colnames(temp1)<-c("Year","Wres","Mres","Rres")
  Chap_2detr[[j]]<-cbind(Chap_2[[j]][,c(1:3)],temp1)
}

Yiels_detr<-bind_rows(Chap_2detr)
Yields_CRU_detr<-subset(Yiels_detr,Clim=="CRU")
Yields_Ag_detr<-subset(Yiels_detr,Clim=="Ag")

Yields_Ag_detr$LUC<-factor(Yields_Ag_detr$LUC)
Yields_Ag_detr$Dr<-factor(Yields_Ag_detr$Dr)

hist(Yields_Ag_detr$Rres)
shapiro.test(Yields_Ag_detr$Wres)

leveneTest(Wres~interaction(Dr,LUC),data=Yields_Ag_detr)
leveneTest(Wres~Dr,data=Yields_Ag_detr)
leveneTest(Wres~LUC,data=Yields_Ag_detr)





names(Chap_2detr)<-Comb
slopes$RefW<-slopes$IntW+1980*slopes$slopeW
slopes$RefM<-slopes$IntM+1980*slopes$slopeM
slopes$RefR<-slopes$IntR+1980*slopes$slopeR




Var<-cbind(Treats,matrix(ncol = 6, nrow = 56))
colnames(Var)[c(4:9)]<-c("varW","varM","varR","RanW","RanM","RanR")

#Calculating detrended variance and range
for (j in 1:length(Chap_2detr)){
  for (i in 1:3){
    Var[j,i+3]<-var(Chap_2detr[[j]][,i+4])
    Var[j,i+6]<-range(Chap_2detr[[j]][,i+4])[2]-range(Chap_2detr[[j]][,i+4])[1]
  }
}

slopW<-data.frame(matrix(nrow=4,ncol=1))
VarW<-data.frame(matrix(nrow=4,ncol=1))
slopeCRU<-slopes[c(1:28),c(1:6)]
VarCRU<-Var[c(1:28),c(1:6)]
col__<-c("coral1","dodgerblue",col_[c(3:6)],"cyan")


#barplot
#CRU
X11(width=15,height=15)
par(mfrow=c(3,2),mai=c(0.2,0.65,0.05,0.05))
layout(matrix(c(1,2,3,4,5,6,7,7),ncol=2,byrow=T),heights=c(4,4,4,1))
for (j in 1:3){
for (i in 1:7){
slopW[,i] <- slopeCRU[which(slopeCRU$Dr==unique(slopeCRU$Dr)[i]),j+3]
VarW[,i] <- VarCRU[which(VarCRU$Dr==unique(VarCRU$Dr)[i]),j+3]
}
rownames(slopW)<-unique(slopeCRU$LUC)
colnames(slopW)<-c(unique(slopeCRU$Dr))
rownames(VarW)<-unique(VarCRU$LUC)
colnames(VarW)<-c(unique(VarCRU$Dr))

barplot(t(slopW), beside=TRUE, ylab="", 
        cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
        ylim=c(range(slopW)[1]-0.005,range(slopW)[2]+0.005))
box(bty="l")
title(ylab="Slope (T/ha*year)", line=3.4, cex.lab=1.5)
barplot(t(VarW), beside=TRUE, ylab="", 
        cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
        ylim=c(0,range(VarW)[2]+0.005))
title(ylab="Variance", line=3.7, cex.lab=1.5)
box(bty="l")
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=colnames(VarW), fill=col__, ncol=7, 
       bty="n",cex=1.3)

slopW<-data.frame(matrix(nrow=4,ncol=1))
VarW<-data.frame(matrix(nrow=4,ncol=1))
slopeAg<-slopes[c(29:56),c(1:6)]
VarAg<-Var[c(29:56),c(1:6)]

#AgMERRA
X11(width=15,height=15)
par(mfrow=c(3,2),mai=c(0.2,0.65,0.05,0.05))
layout(matrix(c(1,2,3,4,5,6,7,7),ncol=2,byrow=T),heights=c(4,4,4,1))
for (j in 1:3){
  for (i in 1:7){
    slopW[,i] <- slopeAg[which(slopeAg$Dr==unique(slopeAg$Dr)[i]),j+3]
    VarW[,i] <- VarAg[which(VarAg$Dr==unique(VarAg$Dr)[i]),j+3]
  }
  rownames(slopW)<-unique(slopeAg$LUC)
  colnames(slopW)<-c(unique(slopeAg$Dr))
  rownames(VarW)<-unique(VarAg$LUC)
  colnames(VarW)<-c(unique(VarAg$Dr))
  
  
  barplot(t(slopW), beside=TRUE, ylab="", 
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(range(slopW)[1]-0.005,range(slopW)[2]+0.005))
  box(bty="l")
  title(ylab="Slope (T/ha*year)", line=3.7, cex.lab=1.5)
  barplot(t(VarW), beside=TRUE, ylab="", 
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(0,range(VarW)[2]+0.005))
  title(ylab="Variance", line=3.7, cex.lab=1.5)
  box(bty="l")
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=colnames(VarW), fill=col__, ncol=7, 
       bty="n",cex=1.3)

IntC<-data.frame(matrix(nrow=4,ncol=1))
IntA<-data.frame(matrix(nrow=4,ncol=1))
IntCRU<-slopes[c(1:28),c(2,3,19:21)]
IntAg<-slopes[c(29:56),c(2,3,19:21)]


#intercept
X11(width=15,height=15)
par(mfrow=c(3,2),mai=c(0.2,0.65,0.2,0.05))
layout(matrix(c(1,2,3,4,5,6,7,7),ncol=2,byrow=T),heights=c(4,4,4,1))
for (j in 1:3){
  for (i in 1:7){
    IntC[,i] <- IntCRU[which(IntCRU$Dr==unique(IntCRU$Dr)[i]),j+2]
    IntA[,i] <- IntAg[which(IntAg$Dr==unique(IntAg$Dr)[i]),j+2]
  }
  rownames(IntC)<-unique(slopeAg$LUC)
  colnames(IntC)<-c(unique(slopeAg$Dr))
  rownames(IntA)<-unique(VarAg$LUC)
  colnames(IntA)<-c(unique(VarAg$Dr))
  
  
  barplot(t(IntC), beside=T, ylab="", xpd = FALSE,
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(range(IntC)[1]-0.5,range(IntC)[2]+0.005))
  box(bty="l")
  title(ylab="Intercept ", line=3, cex.lab=1.5)
  barplot(t(IntA), beside=TRUE, ylab="", xpd = FALSE,
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(range(IntA)[1]-0.5,range(IntA)[2]+0.005))
  title(ylab="Intercept", line=3, cex.lab=1.5)
  box(bty="l")
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=colnames(IntC), fill=col__, ncol=7, 
       bty="n",cex=1.3)



#Anova by crop

ANVsl<-list()
ANVrg<-list()
ANVint<-list()
for (i in 1:3){
  sl<-list()
  int<-list()
  rg<-list()
  for ( j in Clim){
  temp<-subset(slopes,Clim==j);temp1<-subset(Var,Clim==j)
  sl[[j]]<-aov(temp[,i+3]~LUC+Dr,data=temp)
  int[[j]]<-aov(temp[,i+18]~LUC+Dr,data=temp)
  rg[[j]]<-aov(temp1[,i+6]~LUC+Dr,data=temp1)
  }
  ANVsl[[i]]<-sl
  ANVint[[i]]<-int
  ANVrg[[i]]<-rg
}
summary(ANVsl[[1]][[2]])
summary(ANVint[[2]][[1]])
summary(ANVrg[[1]][[2]])


for (i in 1:3){
    ANVsl[[i]]<-aov(slopes[,i+3]~LUC+Dr+Clim*LUC+Clim*Dr+LUC*Dr,data=slopes)
    ANVint[[i]]<-aov(slopes[,i+18]~Clim+LUC+Dr+Clim*LUC+Clim*Dr+LUC*Dr,data=slopes)
    ANVrg[[i]]<-aov(Var[,i+6]~Clim+LUC+Dr+Clim*LUC+Clim*Dr+LUC*Dr,data=Var)
  }
#HSD test

slopes$Int<-with(slopes,interaction(Clim,LUC))
HSD<-HSD.test(aov(slopeW~Int, data=slopes), "Int",group =TRUE)
HSD

slopes$Int<-with(slopes,interaction(LUC,Dr))
HSD<-HSD.test(aov(RefW~Int, data=slopes), "Int",group =TRUE)
HSD

Var$Int<-with(Var,interaction(Clim,LUC))
HSD<-HSD.test(aov(RanW~Int, data=Var), "Int",group =TRUE)
HSD

bartlett.test(slopes[,i+3] ~ interaction(Clim,LUC), data=slopes)
leveneTest(ANVsl[[1]])
test<-TukeyHSD(aov())


