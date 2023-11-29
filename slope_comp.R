library(car)
library(foreign)
library(agricolae)
library(plyr)
library(dplyr)
library(scales)


#Calculating slopes
Clim<-c("CRU","Ag")
LUC<-paste0(c("LUH"),c(1:4))
Driver<-c("Allcons","Allvar","CO2var","Fervar","Precvar","Radvar","Tempvar")
Comb<-paste0(rep(Clim,each=28),rep(rep(LUC,each=7),2),rep(Driver,8))
Treats<-data.frame("Clim"=rep(Clim,each=28),"LUC"=rep(rep(LUC,each=7),2),"Dr"=rep(Driver,8))

Series<-c(Global_data,Global_data_A)
names(Series)[c(29:56)]<-paste0("Ag",names(Series)[c(1:28)])
Series_<-list()

#Calculating the number of years to equilibrium or inflection
Inflection<-data.frame(Series=names(Series))
for (i in 1:28){
  Series_[[i]]<-Series[[i]][-1,]
  Series_[[i+28]]<-Series[[i+28]][-c(1,2),]
}
for (i in 1:56){
  for (j in 1:3){
  Inflection[i,j+1]<-Series_[[i]][which(Series_[[i]][,j+1]==min(Series_[[i]][,j+1])),1]
  }
}
colnames(Inflection)[c(2:4)]<-c("Wheat","Maize","Rice")
Inflection$LC<-rep(rep(c("LCcrop","LCnat","LCnatpas","LCluh"),each=7),2)
Inflection$DRI<-rep(c("Allcons","Allvar","CO2var","Fervar","Prectvar","Radvar","Tempvar"),8)
Inflection_CRU<-Inflection[c(1:28),c(5,6,2,3,4)]
Inflection_Ag<-Inflection[c(29:56),c(5,6,2,3,4)]


InfC<-data.frame(matrix(nrow=4,ncol=1))
InfA<-data.frame(matrix(nrow=4,ncol=1))
X11(width=15,height=15)
par(mfrow=c(3,2),mai=c(0.2,0.65,0.2,0.05))
layout(matrix(c(1,2,3,4,5,6,7,7),ncol=2,byrow=T),heights=c(4,4,4,1))
for (j in 1:3){
  for (i in 1:7){
    InfC[,i] <- Inflection_CRU[which(Inflection_CRU$DRI==unique(Inflection_CRU$DRI)[i]),j+2]
    InfA[,i] <- Inflection_Ag[which(Inflection_Ag$DRI==unique(Inflection_Ag$DRI)[i]),j+2]
  }
  rownames(InfC)<-c("LC-1","LC-2","LC-3","LC-4")
  colnames(InfC)<-c(unique(Inflection_CRU$DRI))
  rownames(InfA)<-c("LC-1","LC-2","LC-3","LC-4")
  colnames(InfA)<-c(unique(Inflection_CRU$DRI))
  
  
  barplot(t(InfC), beside=T, ylab="", xpd = FALSE,
          names.arg=expression(LC[crop],LC[nat],LC[pas],LC[LUH]),
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(range(InfC)[1]-2,range(InfC)[2]+5))
  box(bty="l")
  title(ylab=ifelse(j==2,expression(paste(bold("Inflection year"))),""), line=3.5, cex.lab=1.7)
  barplot(t(InfA), beside=TRUE, ylab="", xpd = FALSE, 
          names.arg=expression(LC[crop],LC[nat],LC[pas],LC[LUH]),
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(range(InfA)[1]-2,range(InfA)[2]+5))
  title(ylab=ifelse(j==2,expression(paste(bold("Inflection year"))),""), line=3.5, cex.lab=1.7)
  box(bty="l")
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=colnames(InfC), fill=col__, ncol=7, 
       bty="n",cex=1.3)


#Analysis of Covariance
Chap_2<-Series
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

#Tables
#Anova(Ancov_CRU[[3]])
cbind(anova(Ancov_CRU[[1]])[5],anova(Ancov_Ag[[1]])[5],
      anova(Ancov_CRU[[2]])[5],anova(Ancov_Ag[[2]])[5],
      anova(Ancov_CRU[[3]])[5],anova(Ancov_Ag[[3]])[5])

Dumm_Ag<-list()
Dumm_CRU<-list()
for (i in 1:3){
Dumm_Ag[[i]]<-lm(Yields_Ag[,i+4]~DL1+DL2+DL3+Year+Year*DL1+Year*DL2+Year*DL3,data=Yields_Ag)
Dumm_CRU[[i]]<-lm(Yields_CRU[,i+4]~DL1+DL2+DL3+Year+Year*DL1+Year*DL2+Year*DL3,data=Yields_CRU)
}
cbind(summary(Dumm_CRU[[1]])$coefficients[,4],summary(Dumm_Ag[[1]])$coefficients[,4],
      summary(Dumm_CRU[[2]])$coefficients[,4],summary(Dumm_Ag[[2]])$coefficients[,4],
      summary(Dumm_CRU[[3]])$coefficients[,4],summary(Dumm_Ag[[3]])$coefficients[,4])

Driver_<-c("Allcons","Allvar","CO2var","Fervar","Precvar","Radvar","Tempvar")
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

cbind(summary(Dumm2_CRU[[1]])$coefficients[,4],summary(Dumm2_Ag[[1]])$coefficients[,4],
      summary(Dumm2_CRU[[2]])$coefficients[,4],summary(Dumm2_Ag[[2]])$coefficients[,4],
      summary(Dumm2_CRU[[3]])$coefficients[,4],summary(Dumm2_Ag[[3]])$coefficients[,4])

#Plots

col_<-c("Red","Blue","forestgreen")
col_2<-c("Red","Blue","forestgreen","darkorange3","gold3","gray54")
text_a<-c(2.7,5,3.5);text_b<-c(1.4,2.7,2.7)
text_a1<-c(3,5.5,4);text_b1<-c(1.5,3,2.8)
letters<-c("b","b","a")


X11(width=12,height=12)
par(mfrow=c(3,2),mai=c(0.3,0.57,0.18,0.08))
layout(matrix(c(1,2,3,4,5,6,7,8),ncol=2,byrow=T),heights=c(4,4,4,1))
for (i in 1:3){
  lims<-c(min(Yields_CRU[,i+4]),max(Yields_CRU[,i+4]))
  temp_<-subset(Yields_CRU,LUC=="LUH1")
  temp<-aggregate(temp_,list(temp_$year),FUN=mean)
  plot(temp[,i+5]~temp$year,ylab=substitute(paste(bold("Yield (T/ha)"))),xlab="",main="",cex.lab=1.4,
       cex.axis=1.3,cex.main=1.5,pch=16,col=alpha("black",0.9),ylim=lims-c(0.5,0.7))
  coeff<-Dumm_CRU[[i]]$coefficients[1]+Dumm_CRU[[i]]$coefficients[5]*(-1960)
  abline(coeff,Dumm_CRU[[i]]$coefficients[5],lwd=2)
  text(x=1968,y=text_b[i],"a",cex=1.5,font=2)
  
  for (j in 1:3){
    a=LUC[j+1]
    temp_<-subset(Yields_CRU,LUC==a)
    temp<-aggregate(temp_,list(temp_$year),FUN=mean)
    points(temp$year,temp[,i+5],col=alpha(col_[j],0.6),pch=16)
    coeff_<-Dumm_CRU[[i]]$coefficients[1]+Dumm_CRU[[i]]$coefficients[j+1]+(Dumm_CRU[[i]]$coefficients[5]+Dumm_CRU[[i]]$coefficients[j+5])*(-1960)
    abline(coeff_,Dumm_CRU[[i]]$coefficients[5]+Dumm_CRU[[i]]$coefficients[j+5],col=col_[j],lwd=2)
    a=letters[j]
    text(x=ifelse(j==3,1970,1966+2*j),y=ifelse(j==3,text_b[i],text_a[i]),letters[[j]],cex=1.5,col=col_[j],font=2)
  }
  temp_<-subset(Yields_CRU,Dr=="Allcons")
  temp<-aggregate(temp_,list(temp_$year),FUN=mean)
  plot(temp[,i+5]~temp$year,type="p",ylab="",xlab="",col=alpha("black",0.9),
       main="",cex.lab=1.4,cex.axis=1.3,cex.main=1.5,pch=16,ylim=lims-c(0.3,0))
  coeff<-Dumm2_CRU[[i]]$coefficients[1]+Dumm2_CRU[[i]]$coefficients[8]*(-1960)
  abline(coeff,Dumm2_CRU[[i]]$coefficients[8],lwd=2)
  text(x=1968,y=text_a1[i],"a",cex=1.5,font=2,col="red")
  b=1966
  for (j in 1:6){
    a=Driver_[j+1]
    b=ifelse(j==3&i==2,b,b+2)
    temp_<-subset(Yields_CRU,Dr==a)
    temp<-aggregate(temp_,list(temp_$year),FUN=mean)
    points(temp$year,temp[,i+5],col=alpha(col_2[j],0.6),pch=16)
    coeff_<-Dumm2_CRU[[i]]$coefficients[1]+Dumm2_CRU[[i]]$coefficients[j+1]+(Dumm2_CRU[[i]]$coefficients[8]+Dumm2_CRU[[i]]$coefficients[j+8])*(-1960)
    abline(coeff_,Dumm2_CRU[[i]]$coefficients[8]+Dumm2_CRU[[i]]$coefficients[j+8],col=col_2[j],lwd=2,xpd=F) 
    text(x=b,y=ifelse(j==3&i==2,text_a1[i],text_b1[i]),
         ifelse(j==3&i==2,"a","b"),cex=1.5,col=ifelse(j==1,"black",col_2[j]),font=2)
    
  }
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=c(as.expression(bquote('LC'['crop'])),
                            as.expression(bquote('LC'['nat'])),
                            as.expression(bquote('LC'['natpas'])),
                            as.expression(bquote('LC'['LUH']))), 
       fill=c("Black",col_), ncol=4, 
       bty="n",cex=1.3)
par(mai=c(0,0,0,0))
plot.new()
legend(x="top", legend=Driver_[c(1:4)], fill=c("Black",col_2[c(1:3)]), ncol=4, bty="n",cex=1.3)
legend(x="bottom", legend=Driver_[c(5:7)], fill=col_2[c(4:6)], ncol=3, bty="n",cex=1.3)



text_a<-c(3.3,5.8,3.7);text_b<-c(2.7,4.8,3.1)
text_a1<-c(3.4,5.7,4);text_b1<-c(2.6,4.7,3)
letters<-c("b","b","a")
X11(width=12,height=12)
par(mfrow=c(3,2),mai=c(0.3,0.57,0.18,0.08))
layout(matrix(c(1,2,3,4,5,6,7,8),ncol=2,byrow=T),heights=c(4,4,4,1))
for (i in 1:3){
  lims<-c(min(Yields_Ag[,i+4]),max(Yields_Ag[,i+4]))
  temp_<-subset(Yields_Ag,LUC=="LUH1")
  temp<-aggregate(temp_,list(temp_$year),FUN=mean)
  plot(temp[,i+5]~temp$year,ylab=substitute(paste(bold("Yield (T/ha)"))),xlab="",main="",cex.lab=1.4,
       cex.axis=1.3,cex.main=1.5,pch=16,col=alpha("black",0.9),ylim=lims)
  coeff<-Dumm_Ag[[i]]$coefficients[1]+Dumm_Ag[[i]]$coefficients[5]*(-1980)
  abline(coeff,Dumm_Ag[[i]]$coefficients[5],lwd=2)
  text(x=1987,y=text_b[i],"a",cex=1.5,font=2)
  
  for (j in 1:3){
  a=LUC[j+1]
  temp_<-subset(Yields_Ag,LUC==a)
  temp<-aggregate(temp_,list(temp_$year),FUN=mean)
  points(temp$year,temp[,i+5],col=alpha(col_[j],0.6),pch=16)
  coeff_<-Dumm_Ag[[i]]$coefficients[1]+Dumm_Ag[[i]]$coefficients[j+1]+(Dumm_Ag[[i]]$coefficients[5]+Dumm_Ag[[i]]$coefficients[j+5])*(-1980)
  abline(coeff_,Dumm_Ag[[i]]$coefficients[5]+Dumm_Ag[[i]]$coefficients[j+5],col=col_[j],lwd=2)
  a=letters[j]
  text(x=ifelse(j==3,1989,1985+2*j),y=ifelse(j==3,text_b[i],text_a[i]),letters[[j]],cex=1.5,col=col_[j],font=2)
  }
  temp_<-subset(Yields_Ag,Dr=="Allvar")
  temp<-aggregate(temp_,list(temp_$year),FUN=mean)
  plot(temp[,i+5]~temp$year,type="p",ylab="",xlab="",col=alpha("black",0.9),
       main="",cex.lab=1.4,cex.axis=1.3,cex.main=1.5,pch=16,ylim=lims-c(0.3,0))
  coeff<-Dumm2_Ag[[i]]$coefficients[1]+Dumm2_Ag[[i]]$coefficients[8]*(-1980)
  abline(coeff,Dumm2_Ag[[i]]$coefficients[8],lwd=2)
  text(x=1987,y=text_a1[i],"a",cex=1.5,font=2,col="red")
  b=1985
  for (j in 1:6){
    a=Driver_[j+1]
    b=ifelse(j==3&i==2|j==2&i==1,b,b+2)
    temp_<-subset(Yields_Ag,Dr==a)
    temp<-aggregate(temp_,list(temp_$year),FUN=mean)
    points(temp$year,temp[,i+5],col=alpha(col_2[j],0.6),pch=16)
    coeff_<-Dumm2_Ag[[i]]$coefficients[1]+Dumm2_Ag[[i]]$coefficients[j+1]+(Dumm2_Ag[[i]]$coefficients[8]+Dumm2_Ag[[i]]$coefficients[j+8])*(-1980)
    abline(coeff_,Dumm2_Ag[[i]]$coefficients[8]+Dumm2_Ag[[i]]$coefficients[j+8],col=col_2[j],lwd=2,xpd=F) 
    text(x=ifelse(j==2&i==1,1989,b),y=ifelse(j==3&i==2|j==2&i==1,text_a1[i],text_b1[i]),
         ifelse(j==3&i==2|j==2&i==1,"a","b"),cex=1.5,col=ifelse(j==1,"black",col_2[j]),font=2)
    
  }
  }

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=c(as.expression(bquote('LC'['crop'])),
       as.expression(bquote('LC'['nat'])),
       as.expression(bquote('LC'['natpas'])),
       as.expression(bquote('LC'['LUH']))), 
fill=c("Black",col_), ncol=4, 
       bty="n",cex=1.3)
par(mai=c(0,0,0,0))
plot.new()
legend(x="top", legend=Driver_[c(1:4)], fill=c("Black",col_2[c(1:3)]), ncol=4, bty="n",cex=1.3)
legend(x="bottom", legend=Driver_[c(5:7)], fill=col_2[c(4:6)], ncol=3, bty="n",cex=1.3)


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

Yields_CRU_detr$LUC<-factor(Yields_CRU_detr$LUC)
Yields_CRU_detr$Dr<-factor(Yields_CRU_detr$Dr)

hist(Yields_Ag_detr$Rres)
shapiro.test(Yields_Ag_detr$Wres)

leveneTest(Wres~interaction(Dr,LUC),data=Yields_Ag_detr)
LevDR_CRU<-list()
LevDR_Ag<-list()
LevLUC_CRU<-list()
LevLUC_Ag<-list()
for (i in 1:3){
LevDR_Ag[[i]]<-leveneTest(Yields_Ag_detr[,i+4]~Dr,data=Yields_Ag_detr)
LevLUC_Ag[[i]]<-leveneTest(Yields_Ag_detr[,i+4]~LUC,data=Yields_Ag_detr)
LevDR_CRU[[i]]<-leveneTest(Yields_CRU_detr[,i+4]~Dr,data=Yields_CRU_detr)
LevLUC_CRU[[i]]<-leveneTest(Yields_CRU_detr[,i+4]~LUC,data=Yields_CRU_detr)
}

df<-data.frame("a"=c(LevLUC_CRU[[1]]$`Pr(>F)`[1],LevDR_CRU[[1]]$`Pr(>F)`[1]),"b"=c(LevLUC_Ag[[1]]$`Pr(>F)`[1],LevDR_Ag[[1]]$`Pr(>F)`[1]),
          "c"=c(LevLUC_CRU[[2]]$`Pr(>F)`[1],LevDR_CRU[[2]]$`Pr(>F)`[1]),"d"=c(LevLUC_Ag[[2]]$`Pr(>F)`[1],LevDR_Ag[[2]]$`Pr(>F)`[1]),
          "e"=c(LevLUC_CRU[[3]]$`Pr(>F)`[1],LevDR_CRU[[3]]$`Pr(>F)`[1]),"f"=c(LevLUC_Ag[[3]]$`Pr(>F)`[1],LevDR_Ag[[3]]$`Pr(>F)`[1]))
      


names(Chap_2detr)<-Comb
slopes$RefW<-slopes$IntW+1980*slopes$slopeW
slopes$RefM<-slopes$IntM+1980*slopes$slopeM
slopes$RefR<-slopes$IntR+1980*slopes$slopeR




Var<-cbind(Treats,matrix(ncol = 6, nrow = 56))
colnames(Var)[c(4:9)]<-c("varW","varM","varR","RanW","RanM","RanR")

#Calculating detrended variance and range
for (j in 1:length(Chap_2detr)){
  for (i in 1:3){
    Var[j,i+3]<-sd(Chap_2detr[[j]][,i+4])/mean(Chap_2[[j]][,i+4])
    Var[j,i+6]<-range(Chap_2detr[[j]][,i+4])[2]-range(Chap_2detr[[j]][,i+4])[1]
  }
}

slopW<-data.frame(matrix(nrow=4,ncol=1))
VarW<-data.frame(matrix(nrow=4,ncol=1))
slopeCRU<-slopes[c(1:28),c(1:6)]
VarCRU<-Var[c(1:28),c(1:6)]
col__<-c("Black",col_2)

#variance plot comparison
Var_ts_Ag<-subset(Yields_Ag_detr[-c(6,7)],Dr=="Fervar")
Var_ts_CRU<-subset(Yields_CRU_detr[-c(6,7)],Dr=="Fervar")

X11(width=8,height=3)
par(mfrow=c(2,2),omi=c(0,0,0,0.1),mai=c(0,0.7,0.2,0.1))
layout(matrix(c(1,2,3,3),ncol=2,byrow=T),heights=c(3,1))
plot(Var_ts_CRU[which(Var_ts_CRU$LUC=="LUH1"),]$Year, 
     Var_ts_CRU[which(Var_ts_CRU$LUC=="LUH1"),]$Wres,type="l",lwd=3,
     ylab="Detrended Yield (T/ha)",main="CRUNCEP",
     cex.lab=1.2,font.lab=2,xlab="")
abline(h=0,type="l",lwd=3,lty=2)
a=0
for (i in LUC[-1]){
  a=a+1
  temp<-Var_ts_CRU[which(Var_ts_CRU$LUC==i),]
  lines(temp$Year,temp$Wres,type="l", col=col_[a],lwd=3)
  
}

plot(Var_ts_Ag[which(Var_ts_Ag$LUC=="LUH1"),]$Year, ylim=c(-0.12,0.15),
     Var_ts_Ag[which(Var_ts_Ag$LUC=="LUH1"),]$Wres,type="l",lwd=3,
     ylab="Detrended Yield (T/ha)",main="AgMERRA",
     cex.lab=1.2,font.lab=2, xlab="")
abline(h=0,type="l",lwd=3,lty=2)
a=0
for (i in LUC[-1]){
  a=a+1
  temp<-Var_ts_Ag[which(Var_ts_Ag$LUC==i),]
  lines(temp$Year,temp$Wres,type="l", col=col_[a],lwd=3)
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="bottom", legend=c(expression(LC[crop],LC[nat],LC[pas],LC[LUH])), lwd=3, 
       col=col__[c(1:4)], ncol=4,bty="n",cex=1.3,lty=c(1,1,1,1))


#barplot
#CRU
X11(width=15,height=15)
par(mfrow=c(3,2),mai=c(0.2,0.75,0.05,0.05))
layout(matrix(c(1,2,3,4,5,6,7,7),ncol=2,byrow=T),heights=c(4,4,4,1))
for (j in 1:3){
for (i in 1:7){
slopW[,i] <- slopeCRU[which(slopeCRU$Dr==unique(slopeCRU$Dr)[i]),j+3]
VarW[,i] <- VarCRU[which(VarCRU$Dr==unique(VarCRU$Dr)[i]),j+3]
}
rownames(slopW)<-c(as.expression(bquote('LC'['crop'])),
                   as.expression(bquote('LC'['nat'])),
                   as.expression(bquote('LC'['natpas'])),
                   as.expression(bquote('LC'['LUH'])))
colnames(slopW)<-c(unique(slopeCRU$Dr))
rownames(VarW)<-c(as.expression(bquote('LC'['crop'])),
                  as.expression(bquote('LC'['nat'])),
                  as.expression(bquote('LC'['natpas'])),
                  as.expression(bquote('LC'['LUH'])))
colnames(VarW)<-c(unique(VarCRU$Dr))

barplot(t(slopW), beside=TRUE, ylab="",names.arg=expression(LC[crop],LC[nat],LC[pas],LC[LUH]),
        cex.names=1.5, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
        ylim=c(range(slopW)[1]-0.005,range(slopW)[2]+0.005))
box(bty="l")
title(ylab=ifelse(j==2,expression(paste(bold(Slope ("T*"*ha^-1~"*"*year^-1)))),""), line=3.4, cex.lab=1.5)
barplot(t(VarW), beside=TRUE, ylab="", names.arg=expression(LC[crop],LC[nat],LC[pas],LC[LUH]),
        cex.names=1.5, las=1, col=col__, cex.lab=1.4,cex.axis=1.1,
        ylim=c(0.000,range(VarW)[2]+0.0005))
title(ylab=ifelse(j==2,expression(paste(bold("Coefficient of Variance"))),""), line=4.1, cex.lab=1.5)
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
par(mfrow=c(3,2),mai=c(0.2,0.75,0.05,0.05))
layout(matrix(c(1,2,3,4,5,6,7,7),ncol=2,byrow=T),heights=c(4,4,4,1))
for (j in 1:3){
  for (i in 1:7){
    slopW[,i] <- slopeAg[which(slopeAg$Dr==unique(slopeAg$Dr)[i]),j+3]
    VarW[,i] <- VarAg[which(VarAg$Dr==unique(VarAg$Dr)[i]),j+3]
  }
  rownames(slopW)<-c("LC-1","LC-2","LC-3","LC-4")
  colnames(slopW)<-c(unique(slopeAg$Dr))
  rownames(VarW)<-c("LC-1","LC-2","LC-3","LC-4")
  colnames(VarW)<-c(unique(VarAg$Dr))
  
  
  barplot(t(slopW), beside=TRUE, ylab="", names.arg=expression(LC[crop],LC[nat],LC[pas],LC[LUH]),
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(range(slopW)[1]-0.005,range(slopW)[2]+0.005))
  box(bty="l")
  title(ylab=ifelse(j==2,expression(paste(bold(Slope ("T*"*ha^-1~"*"*year^-1)))),""), line=3.7, cex.lab=1.5,font=2)
  barplot(t(VarW), beside=TRUE, ylab="", names.arg=expression(LC[crop],LC[nat],LC[pas],LC[LUH]),
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.1,
          ylim=c(0,range(VarW)[2]+0.0005))
  title(ylab=ifelse(j==2,expression(paste(bold("Coefficient of Variation"))),""), line=4, cex.lab=1.5)
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
  rownames(IntC)<-c("LC-1","LC-2","LC-3","LC-4")
  colnames(IntC)<-c(unique(slopeAg$Dr))
  rownames(IntA)<-c("LC-1","LC-2","LC-3","LC-4")
  colnames(IntA)<-c(unique(VarAg$Dr))
  
  
  barplot(t(IntC), beside=T, ylab="", xpd = FALSE,
          names.arg=expression(LC[crop],LC[nat],LC[pas],LC[LUH]),
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(range(IntC)[1]-0.5,range(IntC)[2]+0.005))
  box(bty="l")
  title(ylab=substitute(paste(bold("Intercept"))), line=3, cex.lab=1.5)
  barplot(t(IntA), beside=TRUE, ylab="", xpd = FALSE, 
          names.arg=expression(LC[crop],LC[nat],LC[pas],LC[LUH]),
          cex.names=1.4, las=1, col=col__, cex.lab=1.4,cex.axis=1.3,
          ylim=c(range(IntA)[1]-0.5,range(IntA)[2]+0.005))
  title(ylab="", line=3, cex.lab=1.5)
  box(bty="l")
}

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", legend=colnames(IntC), fill=col__, ncol=7, 
       bty="n",cex=1.3)



