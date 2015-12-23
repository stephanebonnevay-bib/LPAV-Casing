#---------------------------------------------------------Modèle 3-----------------------------------------------------------------------

library(stats)   
library(cluster)
library(graphics)
library(bnlearn)
library(gRain)
library(RODBC)
library(FSelector)
library(brglm)
library(minqa)
library(nloptr)
library(quantreg)
library(class)
library(car)
library(Rcmdr)
library(caret)
library(tseries)
library(rpart) 
library(arules)
library(rJava)
library(FSelector)
library(randomForest)
library('nnet')
library('neuralnet')
library("DAAG", lib.loc="~/R/win-library/3.2")
library("cvTools", lib.loc="~/R/win-library/3.2")
library('e1071')
library("ROCR", lib.loc="~/R/win-library/3.2")
library("mda", lib.loc="~/R/win-library/3.2")
library("varSelRF", lib.loc="~/R/win-library/3.2")
library("FSelector", lib.loc="~/R/win-library/3.2")
library("smbinning", lib.loc="~/R/win-library/3.2")
library("discretization", lib.loc="~/R/win-library/3.2")
library("infotheo", lib.loc="~/R/win-library/3.2")
library("arules", lib.loc="~/R/win-library/3.2")
library("entropy", lib.loc="~/R/win-library/3.2")
library("hash", lib.loc="~/R/win-library/3.2")
library("arules", lib.loc="~/R/win-library/3.2")
library("tis", lib.loc="~/R/win-library/3.2")
library(rapport)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")







#---------------------------------------------------------------Insertion des données----------------------------------------------------------

echantillon_RB<-read.table("~/Michelin/Etude_R/Donnees_test.txt",header=TRUE,sep=";",na.strings = "")
View(echantillon_RB)


donnees<-echantillon_RB
View(donnees)

data=donnees

#---------------------------------------------------------------Création des dates--------------------------------------------------------------

data$Month.Mount<-gsub(10,"G",data$Month.Mount)
data$Month.Mount<-gsub(11,"H",data$Month.Mount)
data$Month.Mount<-gsub(12,"I",data$Month.Mount)
data$Month.Mount<-gsub(1,"01",data$Month.Mount)
data$Month.Mount<-gsub(2,"02",data$Month.Mount)
data$Month.Mount<-gsub(3,"03",data$Month.Mount)
data$Month.Mount<-gsub(4,"04",data$Month.Mount)
data$Month.Mount<-gsub(5,"05",data$Month.Mount)
data$Month.Mount<-gsub(6,"06",data$Month.Mount)
data$Month.Mount<-gsub(7,"07",data$Month.Mount)
data$Month.Mount<-gsub(8,"08",data$Month.Mount)
data$Month.Mount<-gsub(9,"09",data$Month.Mount)
data$Month.Mount<-gsub("I","12",data$Month.Mount)
data$Month.Mount<-gsub("H","11",data$Month.Mount)
data$Month.Mount<-gsub("G","10",data$Month.Mount)


data$Month.Dismount<-gsub(10,"G",data$Month.Dismount)
data$Month.Dismount<-gsub(11,"H",data$Month.Dismount)
data$Month.Dismount<-gsub(12,"I",data$Month.Dismount)
data$Month.Dismount<-gsub(1,"01",data$Month.Dismount)
data$Month.Dismount<-gsub(2,"02",data$Month.Dismount)
data$Month.Dismount<-gsub(3,"03",data$Month.Dismount)
data$Month.Dismount<-gsub(4,"04",data$Month.Dismount)
data$Month.Dismount<-gsub(5,"05",data$Month.Dismount)
data$Month.Dismount<-gsub(6,"06",data$Month.Dismount)
data$Month.Dismount<-gsub(7,"07",data$Month.Dismount)
data$Month.Dismount<-gsub(8,"08",data$Month.Dismount)
data$Month.Dismount<-gsub(9,"09",data$Month.Dismount)
data$Month.Dismount<-gsub("I","12",data$Month.Dismount)
data$Month.Dismount<-gsub("H","11",data$Month.Dismount)
data$Month.Dismount<-gsub("G","10",data$Month.Dismount)



data$Month.Reception.at.plant<-gsub(10,"G",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(11,"H",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(12,"I",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(1,"01",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(2,"02",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(3,"03",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(4,"04",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(5,"05",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(6,"06",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(7,"07",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(8,"08",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub(9,"09",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub("I","12",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub("H","11",data$Month.Reception.at.plant)
data$Month.Reception.at.plant<-gsub("G","10",data$Month.Reception.at.plant)

data$Date.Mount<-paste(data$Year.Mount,data$Month.Mount,"01",sep="-")
data$Date.Dismount<-paste(data$Year.Dismount,data$Month.Dismount,"01",sep="-")
data$Date.Reception.at.plant<-paste(data$Year.Reception.at.plant,data$Month.Reception.at.plant,"01",sep="-")

#---------------------------------------------------------------Rajout Bias/Radial-------------------------------------------------------------



               
data$Tire.type<-substr(as.character(data$Part.Number), 1, 1)
data$Tire.type<-gsub("M","X",data$Tire.type)
data$Tire.type<-gsub("0","B",data$Tire.type)

# pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)
# 
# for (i in 1:length(data$Part.Number))
#   
# {
# 
#   if(!is.na(match(strsplit(as.character(data$Part.Number), split="")[[i]],"M")[1])){data$Tire.type[i]<-"X"}
#   else(data$Tire.type[i]<-"B")
# 
#   info <- sprintf("%d%% done", round(100*(i/length(data$Part.Number))))
#   setWinProgressBar(pb, 100*(i/length(data$Part.Number)), label=info)
# }  
# 
# close(pb)

#---------------------------------------------------------------Rajout des ventes--------------------------------------------------------------

datacut<-data.frame(data$Date.Mount,data$Part.Number,data$Codeclient,data$Plant.manufacturing,data$Tire.type)
colnames(datacut)<-c("Date","PN","Client","Plant","Tiretype")
datacut$Compteur<-rep_len(1,length.out=length(data$Part.Number))


#---------------------------------------------------------------Vente.pn-----------------------------------------------------------------------

Ventes.pn<-aggregate(datacut$Compteur,by=list(datacut$Date,datacut$PN),FUN=sum)
colnames(Ventes.pn)<-c("Date","PN","Ventes.pn")

Ventes.pn$Clef<-paste(Ventes.pn$Date,Ventes.pn$PN,sep='_')
data$Clef.pn<-paste(data$Date.Mount,data$Part.Number,sep='_')




data$Ventes.pn<-c()

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Clef.pn))
  
{
  
  if(!is.na(match(data$Clef.pn[i],Ventes.pn$Clef))){data$Ventes.pn[i]<-Ventes.pn$Ventes.pn[match(data$Clef.pn[i],Ventes.pn$Clef)]}

  
  info <- sprintf("%d%% done", round(100*(i/length(data$Clef.pn))))
  setWinProgressBar(pb, 100*(i/length(data$Clef.pn)), label=info)
}  

close(pb)

#---------------------------------------------------------------Vente.client----------------------------------------------------------------------------------

Ventes.client<-aggregate(datacut$Compteur,by=list(datacut$Date,datacut$Client),FUN=sum)
colnames(Ventes.client)<-c("Date","Client","Ventes.client")

Ventes.client$Clef<-paste(Ventes.client$Date,Ventes.client$Client,sep='_')
data$Clef.client<-paste(data$Date.Mount,data$Codeclient,sep='_')

data$Ventes.client<-c(rep_len(NA, length.out=length(data$Clef.client)))

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Clef.client))
  
{
  
  if(!is.na(match(data$Clef.client[i],Ventes.client$Clef))){data$Ventes.client[i]<-Ventes.client$Ventes.client[match(data$Clef.client[i],Ventes.client$Clef)]}
  
  
  info <- sprintf("%d%% done", round(100*(i/length(data$Clef.client))))
  setWinProgressBar(pb, 100*(i/length(data$Clef.client)), label=info)
}  

close(pb)

#---------------------------------------------------------------Vente.pn.client-------------------------------------------------------------------------------

Ventes.pn.client<-aggregate(datacut$Compteur,by=list(datacut$Date,datacut$PN,datacut$Client),FUN=sum)
colnames(Ventes.pn.client)<-c("Date","PN","Client","Ventes.pn.client")

Ventes.pn.client$Clef<-paste(Ventes.pn.client$Date,Ventes.pn.client$PN,Ventes.pn.client$Client,sep='_')
data$Clef.pn.client<-paste(data$Date.Mount,data$Part.Number,data$Codeclient,sep='_')


data$Ventes.pn.client<-c(rep_len(NA, length.out=length(data$Clef.pn.client)))

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Clef.pn.client))
  
{
  
  if(!is.na(match(data$Clef.pn.client[i],Ventes.pn.client$Clef))){data$Ventes.pn.client[i]<-Ventes.pn.client$Ventes.pn.client[match(data$Clef.pn.client[i],Ventes.pn.client$Clef)]}
  
  
  info <- sprintf("%d%% done", round(100*(i/length(data$Clef.pn.client))))
  setWinProgressBar(pb, 100*(i/length(data$Clef.pn.client)), label=info)
}  

close(pb)

#---------------------------------------------------------------Vente.pn.Plant--------------------------------------------------------------------------------

Ventes.pn.plant<-aggregate(datacut$Compteur,by=list(datacut$Date,datacut$PN,datacut$Plant),FUN=sum)
colnames(Ventes.pn.plant)<-c("Date","PN","Plant","Ventes.pn.plant")

Ventes.pn.plant$Clef<-paste(Ventes.pn.plant$Date,Ventes.pn.plant$PN,Ventes.pn.plant$Plant,sep='_')
data$Clef.pn.plant<-paste(data$Date.Mount,data$Part.Number,data$Plant.manufacturing,sep='_')


data$Ventes.pn.plant<-c(rep_len(NA, length.out=length(data$Clef.pn.plant)))

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Clef.pn.plant))
  
{
  
  if(!is.na(match(data$Clef.pn.plant[i],Ventes.pn.plant$Clef))){data$Ventes.pn.plant[i]<-Ventes.pn.plant$Ventes.pn.plant[match(data$Clef.pn.plant[i],Ventes.pn.plant$Clef)]}
  
  
  info <- sprintf("%d%% done", round(100*(i/length(data$Clef.pn.plant))))
  setWinProgressBar(pb, 100*(i/length(data$Clef.pn.plant)), label=info)
}  

close(pb)



#---------------------------------------------------------------Rajout des retours------------------------------------------------------------

datacut2<-data.frame(data$Date.Reception.at.plant,data$Part.Number,data$Codeclient,data$Plant.receiving.tire)
colnames(datacut2)<-c("Date","PN","Client","Plant")
datacut2$Compteur<-rep_len(1,length.out=length(data$Part.Number))

#---------------------------------------------------------------Retour.pn-----------------------------------------------------------------------

Retours.pn<-aggregate(datacut2$Compteur,by=list(datacut2$Date,datacut2$PN),FUN=sum)
colnames(Retours.pn)<-c("Date","PN","Retours.pn")


Retours.pn$Clef<-paste(Retours.pn$Date,Retours.pn$PN,sep='_')
data$Clef.pn<-paste(data$Date.Mount,data$Part.Number,sep='_')




data$Retours.pn<-c()

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Clef.pn))
  
{
  
  if(!is.na(match(data$Clef.pn[i],Retours.pn$Clef))){data$Retours.pn[i]<-Retours.pn$Retours.pn[match(data$Clef.pn[i],Retours.pn$Clef)]}
  
  
  info <- sprintf("%d%% done", round(100*(i/length(data$Clef.pn))))
  setWinProgressBar(pb, 100*(i/length(data$Clef.pn)), label=info)
}  

close(pb)

#---------------------------------------------------------------Retour.client----------------------------------------------------------------------------------

Retours.client<-aggregate(datacut2$Compteur,by=list(datacut2$Date,datacut2$Client),FUN=sum)
colnames(Retours.client)<-c("Date","Client","Retours.client")

Retours.client$Clef<-paste(Retours.client$Date,Retours.client$Client,sep='_')
data$Clef.client<-paste(data$Date.Mount,data$Codeclient,sep='_')

data$Retours.client<-c(rep_len(NA, length.out=length(data$Clef.client)))

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Clef.client))
  
{
  
  if(!is.na(match(data$Clef.client[i],Retours.client$Clef))){data$Retours.client[i]<-Retours.client$Retours.client[match(data$Clef.client[i],Retours.client$Clef)]}
  
  
  info <- sprintf("%d%% done", round(100*(i/length(data$Clef.client))))
  setWinProgressBar(pb, 100*(i/length(data$Clef.client)), label=info)
}  

close(pb)

#---------------------------------------------------------------Retour.pn.client-------------------------------------------------------------------------------

Retours.pn.client<-aggregate(datacut2$Compteur,by=list(datacut2$Date,datacut2$PN,datacut2$Client),FUN=sum)
colnames(Retours.pn.client)<-c("Date","PN","Client","Retours.pn.client")


Retours.pn.client$Clef<-paste(Retours.pn.client$Date,Retours.pn.client$PN,Retours.pn.client$Client,sep='_')
data$Clef.pn.client<-paste(data$Date.Mount,data$Part.Number,data$Codeclient,sep='_')


data$Retours.pn.client<-c(rep_len(NA, length.out=length(data$Clef.pn.client)))

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Clef.pn.client))
  
{
  
  if(!is.na(match(data$Clef.pn.client[i],Retours.pn.client$Clef))){data$Retours.pn.client[i]<-Retours.pn.client$Retours.pn.client[match(data$Clef.pn.client[i],Retours.pn.client$Clef)]}
  
  
  info <- sprintf("%d%% done", round(100*(i/length(data$Clef.pn.client))))
  setWinProgressBar(pb, 100*(i/length(data$Clef.pn.client)), label=info)
}  

close(pb)

#---------------------------------------------------------------Retour.pn.Plant--------------------------------------------------------------------------------

Retours.pn.plant<-aggregate(datacut2$Compteur,by=list(datacut2$Date,datacut2$PN,datacut2$Plant),FUN=sum)
colnames(Retours.pn.plant)<-c("Date","PN","Plant","Retours.pn.plant")


Retours.pn.plant$Clef<-paste(Retours.pn.plant$Date,Retours.pn.plant$PN,Retours.pn.plant$Plant,sep='_')
data$Clef.pn.plant<-paste(data$Date.Mount,data$Part.Number,data$Plant.manufacturing,sep='_')


data$Retours.pn.plant<-c(rep_len(NA, length.out=length(data$Clef.pn.plant)))

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Clef.pn.plant))
  
{
  
  if(!is.na(match(data$Clef.pn.plant[i],Retours.pn.plant$Clef))){data$Retours.pn.plant[i]<-Retours.pn.plant$Retours.pn.plant[match(data$Clef.pn.plant[i],Retours.pn.plant$Clef)]}
  
  
  info <- sprintf("%d%% done", round(100*(i/length(data$Clef.pn.plant))))
  setWinProgressBar(pb, 100*(i/length(data$Clef.pn.plant)), label=info)
}  

close(pb)




#---------------------------------------------------------------Valeur de l'échantillon-----------------------------------------------------------------------

#---------------------------------------------------------------Ventes sur data----------------------------------------------------------------

dv.pn<-data.frame(data$Date.Mount,data$Part.Number,data$Tire.type)
colnames(dv.pn)<-c("Date","PN","Tiretype")
dv.pn$Compteur<-rep_len(1,length.out=length(data$Part.Number))

dV.pn<-aggregate(dv.pn$Compteur,by=list(dv.pn$Date),FUN=sum)
colnames(dV.pn)<-c("Date","Ventes")

dV.pn.radial=subset(dv.pn,Tiretype =="X")
colnames(dV.pn.radial)<-c("Date","PN","Tiretype","Compteur")

somme.ventes.data<-dV.pn
sommes.ventes.data.radial<-aggregate(dV.pn.radial$Compteur,by=list(dV.pn.radial$Date),FUN=sum)

#---------------------------------------------------------------Ventes réelles-----------------------------------------------------------------

dv<-data.frame(vente_pn$Date,vente_pn$PN,vente_pn$VENTES,vente_pn$TECH)
colnames(dv)<-c("Date","PN","Ventes","TECH")
dV.radial<-subset(dv,TECH=="X")

somme.ventes.reelles<-aggregate(dv$Vente,by=list(dv$Date),FUN=sum)
sommes.ventes.reelles.radial<-aggregate(dV.radial$Ventes,by=list(dV.radial$Date),FUN=sum)

#---------------------------------------------------------------Pourcentage des ventes que représente l'échantillon-----------------------------


pourcentage.ventes.totales<-mean(somme.ventes.data[,2]/somme.ventes.reelles[37:95,2])
pourcentage.ventes.totales


pourcentage.ventes.totales.radial<-mean(sommes.ventes.data.radial[,2]/somme.ventes.reelles.radial[-59,2])
pourcentage.ventes.totales.radial


#---------------------------------------------------------------Représentation fidèle de la réalité ?-------------------------------------------

Date<-as.Date(subset(Ventes.pn,PN=="M01103-02")[6:(length(subset(Ventes.pn,PN=="M01103-02")[,1])-2),1])



remplacerparNA<-function(dF,charact)
  
{
  
  dF.2<-data.frame(matrix(nrow=length(Date),ncol=2))
  dF.2[,1]<-Date
  
  
  for(i in 1:length(Date))
    
  {
    
    if(!is.na(match(Date[i],as.Date(dF[,1],format="%Y-%m-%d")))){dF.2[i,2]<-dF[match(Date[i],as.Date(dF[,1],format="%Y-%m-%d")),2]}
    else(dF.2[i,2]<-NA)
    
  }
  
  colnames(dF.2)<-c("Date",charact)
  return(dF.2)
  
}


#Au PN : pas mal mais ventes pas incroyables

v=remplacerparNA(subset(Ventes.pn,PN=="M13901 IND 01")[,c(1,3)],"Ventes.pn")[,2]
r=remplacerparNA(subset(Retours.pn,PN=="M13901 IND 01")[,c(1,3)],"Retours.pn")[,2]
r[1]=0

vreel=remplacerparNA(liste.vente2$M13901,"Ventes")[,2]
rreel=remplacerparNA(liste.retour2$M13901,"Retours")[,2]


par(mfrow=c(2,1))

plot(v,type='l',col='blue',ylim=c(range(vreel,v)))
lines(vreel,col='blue',lty=2)

plot(r,type='l',col='red',ylim=c(range(rreel,r)))
lines(rreel,col='red',lty=2)


#Au PN / Client : Super

Date<-as.Date(subset(Retours.pn.client,PN=="M01103-02" & Client=="AVAFR")[3:(length(subset(Retours.pn.client,PN=="M01103-02" & Client=="AVAFR")[,1])),1])

v=remplacerparNA(subset(Ventes.pn.client,PN=="M01103-02" & Client=="AVAFR")[,c(1,4)],"Ventes.pn")[,2]
r=remplacerparNA(subset(Retours.pn.client,PN=="M01103-02" & Client=="AVAFR")[,c(1,4)],"Retours.pn")[,2]
r[1]=0
v[34]=v[35]=0


vreel=remplacerparNA(liste.pn.vente$AVAFR$M01103[,c(1,3)],"Ventes")[,2]
rreel=remplacerparNA(liste.pn.retour$AVAFR$M01103[,c(1,3)],"Retours")[,2]


par(mfrow=c(2,1))

plot(v,type='l',col='blue',ylim=c(range(vreel,v)))
lines(vreel,col='blue',lty=2)

plot(r,type='l',col='red',ylim=c(range(rreel,r)))
lines(rreel,col='red',lty=2)

#---------------------------------------------------------------Ajout de la flotte utilisée-------------------------------------------------------------------

datacut3<-data.frame(data$Date.Mount,data$Codeclient,data$Model)
colnames(datacut3)<-c("Date","Client","Model")
datacut3$Compteur<-rep_len(1,length.out=length(data$Part.Number))

Flotte.ut.client.model<-aggregate(datacut3$Compteur,by=list(datacut3$Date,datacut3$Client,datacut3$Model),FUN=sum)
colnames(Flotte.ut.client.model)<-c("Date","Client","Model","Flotte.ut")

fl<-subset(Flotte.ut.client.model,Client=="AVAFR"& Model=="747")[,4]

datacut4<-data.frame(data$Date.Mount,data$Codeclient)
colnames(datacut4)<-c("Date","Client")
datacut4$Compteur<-rep_len(1,length.out=length(data$Part.Number))

Flotte.ut.client<-aggregate(datacut4$Compteur,by=list(datacut4$Date,datacut4$Client),FUN=sum)
colnames(Flotte.ut.client)<-c("Date","Client","Flotte.ut")

fl<-subset(Flotte.ut.client,Client=="AVAFR")[,3]
v<-subset(Ventes.client,Client=="AVAFR")[,3]


#---------------------------------------------------------------Rajout du bussiness model---------------------------------------------------------------------

business_model<-read.table("~/Michelin/Etude_R/bussiness_model.csv",header=TRUE,sep=";",na.strings = "")
View(business_model)

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for(i in 1:length(data$Codeclient))
  
{
  
  
  
  if(is.na(match(data$Codeclient[i],business_model$CoClient))==FALSE)
    
  {
    
    data$Cargo[i]<-business_model$Cargo[match(data$Codeclient[i],business_model$CoClient)]
    data$Holiday.Charter[i]<-business_model$Holiday.Charter[match(data$Codeclient[i],business_model$CoClient)]
    data$Leaser[i]<-business_model$Leaser[match(data$Codeclient[i],business_model$CoClient)]
    data$Low.Cost[i]<-business_model$Low.Cost[match(data$Codeclient[i],business_model$CoClient)]
    
  }
  
  
  else 
    
  {
    
    data$Cargo[i]<-NA
    data$Holiday.Charter[i]<-NA
    data$Leaser[i]<-NA
    data$Low.Cost[i]<-NA
    
  }
  
  
   info <- sprintf("%d%% done", round(100*(i/length(data$Codeclient))))
   setWinProgressBar(pb, 100*(i/length(data$Codeclient)), label=info)
  
}

#---------------------------------------------------------------Rajout de la zone------------------------------------------------------------------------------




keys=c("Australia","Finland","Austria","Canada","China","France","Germany","Hong Kong","India","Italy","Japan","Kuwait","Malaysia","Netherlands","New Caledonia"
       ,"New Zealand","Philippines","Russian Federation","Singapore","South Korea","Spain","Thailand","United Arab Emirates"
       ,"United Kingdom","USA","El Salvador","Colombia","Bahrain","Ecuador","Portugal","Mexico","Jordan","Turkey","Tunisia","Sweden","Vietnam","Egypt","French Polynesia","Czech Republic","Israel")

valeurs=c("ANA","EUR","EUR","NCA","CHN","EUR","EUR","CHN","AIM","EUR","JPK","AIM","ANA","EUR","AIM","ANA","ANA","EUO"
          ,"ANA","JPK","EUR","ANA","AIM","EUR","NCA","ADS","ADS","AIM","ADS","EUR","ADS","AIM","EUR","AIM","EUR","ANA","AIM","ADS","EUR","AIM")

h=new.env(hash=TRUE)

for ( i in 1:length(keys))
{
  h[[keys[i]]]<-valeurs[i]

  cat(sprintf("%s", h[[keys[i]]])) 
  
}


data$Zone<-c(rep_len(NA, length.out=length(data$Operator.Country.DUS)))

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:length(data$Operator.Country.DUS))
  
{
  
  if((is.na(data$Operator.Country.DUS[i]))==TRUE){data$Zone[i]<-NA}
  else(data$Zone[i]<-h[[as.character(data$Operator.Country.DUS[i])]])

  info <- sprintf("%d%% done", round(100*(i/length(data$Codeclient))))
  setWinProgressBar(pb, 100*(i/length(data$Codeclient)), label=info)
  
  
}

#---------------------------------------------------------------Enlever des variables-------------------------------------------------------------------------

data$Asphalt[is.na(data$Asphalt)]<-0
data$Weighted.Surface.Runway.FrequencyOfOp.by.PN[is.na(data$Weighted.Surface.Runway.FrequencyOfOp.by.PN)]<-0
data$Concrete[is.na(data$Concrete)]<-0
data$Graded.or.rolled.earth[is.na(data$Graded.or.rolled.earth)]<-0
data$Gravel.Sand.Macadam[is.na(data$Gravel.Sand.Macadam)]<-0
data$Not.graded.or.rolled.earth[is.na(data$Not.graded.or.rolled.earth)]<-0
data$Other.Surfaces[is.na(data$Other.Surfaces)]<-0
data$Unknown.surface[is.na(data$Unknown.surface)]<-0

data$Other.Surfaces<-as.numeric(data$Other.Surfaces)+as.numeric(data$Unknown.surface)+as.numeric(data$Graded.or.rolled.earth)+as.numeric(data$Not.graded.or.rolled.earth)
data<-data[,!colnames(data)%in%c("Unknown.surface")]
data<-data[,!colnames(data)%in%c("Graded.or.rolled.earth")]
data<-data[,!colnames(data)%in%c("Not.graded.or.rolled.earth")]

data$Asphalt<-100*((as.numeric(data$Asphalt)/as.numeric(data$Weighted.Surface.Runway.FrequencyOfOp.by.PN)))
data$Concrete<-100*((as.numeric(data$Concrete)/as.numeric(data$Weighted.Surface.Runway.FrequencyOfOp.by.PN)))
data$Gravel.Sand.Macadam<-100*((as.numeric(data$Gravel.Sand.Macadam)/as.numeric(data$Weighted.Surface.Runway.FrequencyOfOp.by.PN)))
data$Other.Surfaces<-100-(data$Gravel.Sand.Macadam+data$Concrete+data$Asphalt)

data<-data[,!colnames(data)%in%c("Weighted.Surface.Runway.FrequencyOfOp.by.PN")]

data$Asphalt[is.na(data$Asphalt)]<-0
data$Concrete[is.na(data$Concrete)]<-0
data$Gravel.Sand.Macadam[is.na(data$Gravel.Sand.Macadam)]<-0
data$Other.Surfaces[is.na(data$Other.Surfaces)]<-0

#Données que j'aimerais garder mais pas assez de valeurs


data<-data[,!colnames(data)%in%c("LPC")]
data<-data[,!colnames(data)%in%c("Skid.loss.accum")]
data<-data[,!colnames(data)%in%c("Rubber.volume.loss")]

#---------------------------------------------------------------Garder les variables pour le test-------------------------------------------------------------

data.2=data

data.2<-data.2[,!colnames(data.2)%in%c("SN")]
data.2<-data.2[,!colnames(data.2)%in%c("Dimension.Number")]
data.2<-data.2[,!colnames(data.2)%in%c("Size")]
data.2<-data.2[,!colnames(data.2)%in%c("Scrap.Date")]
data.2<-data.2[,!colnames(data.2)%in%c("Premature.Scrap")]
data.2<-data.2[,!colnames(data.2)%in%c("Removal.code.Michelin.1")]
data.2<-data.2[,!colnames(data.2)%in%c("Reform.Code")]

#Regression logistique dessus sûrement intéressant

data.2<-data.2[,!colnames(data.2)%in%c("Flex.breaks")]
data.2<-data.2[,!colnames(data.2)%in%c("Leaks..bead.toe..liner."  )]
data.2<-data.2[,!colnames(data.2)%in%c("Others")]
data.2<-data.2[,!colnames(data.2)%in%c("Crack..cuts.bead.area")]
data.2<-data.2[,!colnames(data.2)%in%c("Ozone.cracks.sidewall")]
data.2<-data.2[,!colnames(data.2)%in%c("Shearo" )]
data.2<-data.2[,!colnames(data.2)%in%c("Sidewall.cuts.cracks" )]
data.2<-data.2[,!colnames(data.2)%in%c("Tread.cut")]
data.2<-data.2[,!colnames(data.2)%in%c("Worn.beyond.limits")]
data.2<-data.2[,!colnames(data.2)%in%c("Total.Scrap.A")]
data.2<-data.2[,!colnames(data.2)%in%c("Total.Scrap.B" )]
data.2<-data.2[,!colnames(data.2)%in%c("Total.Scrap.C")]
data.2<-data.2[,!colnames(data.2)%in%c("Total.Scrap.H" )]
data.2<-data.2[,!colnames(data.2)%in%c("Clef.pn.plant" )]
data.2<-data.2[,!colnames(data.2)%in%c("Clef.pn.client" )]
data.2<-data.2[,!colnames(data.2)%in%c("Clef.client" )]
data.2<-data.2[,!colnames(data.2)%in%c("Clef.pn" )]
data.2<-data.2[,!colnames(data.2)%in%c("Year.Mount" )]
data.2<-data.2[,!colnames(data.2)%in%c("Year.Dismount" )]
data.2<-data.2[,!colnames(data.2)%in%c("Month.Mount" )]
data.2<-data.2[,!colnames(data.2)%in%c("Month.Dismount" )]
data.2<-data.2[,!colnames(data.2)%in%c("Year.Reception.at.plant" )]
data.2<-data.2[,!colnames(data.2)%in%c("Month.Reception.at.plant" )]
data.2<-data.2[,!colnames(data.2)%in%c("Tire.Family" )]
data.2<-data.2[,!colnames(data.2)%in%c("Retours.client" )]
data.2<-data.2[,!colnames(data.2)%in%c("Retours.pn.plant"  )]
data.2<-data.2[,!colnames(data.2)%in%c("Retours.pn.client"  )]


#

data.2<-data.2[,!colnames(data.2)%in%c("OD..mm."  )]

#Ne garder que les données importantes renseignées



data.2<-data.2[!is.na(data.2[,colnames(data.2)%in%c("Codeclient")]),]
data.2<-data.2[!is.na(data.2[,colnames(data.2)%in%c("TAT.at.customer.estimate")]),]
data.2<-data.2[!is.na(data.2[,colnames(data.2)%in%c("LPT")]),]

data.2<-data.2[,!colnames(data.2)%in%c("GAvgFromAptTempWeighted"  )]
data.2<-data.2[,!colnames(data.2)%in%c("GAvgFromAptMinTempWeighted"   )]
data.2<-data.2[,!colnames(data.2)%in%c("GAvgFromAptMaxTempWeighted"   )]
data.2<-data.2[,!colnames(data.2)%in%c("GAvgFromAptDenWeighted"    )]
data.2<-data.2[,!colnames(data.2)%in%c( "GAvgFromPressAltWeighted"    )]
data.2<-data.2[,!colnames(data.2)%in%c( "GAvgFromDenAltWeighted"  )]

data.2<-na.omit(data.2)

boxplot(data.2[,15],col='pink',boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[15])
boxplot(data.2[,16],col='pink',boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[16])
boxplot(data.2[,17],col='pink',boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[17])
boxplot(data.2[,18:42],col='pink',boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[18])

for(i in 15:42)
  
{
  
  
boxplot(data.2[,i],col='pink',boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[i])
  
  
  
}

data.2<-data.2[(data.2[,colnames(data.2)%in%c("Tread.Depth.accum")]<27),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("TAT.at.customer.estimate" )]<1500),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("Transit.time.before.retreading.estimate" )]<750),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("LPT"    )]<1000),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("GAvgDistanceWeighted" )]<15000),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("MaxRunwayLength.m."  )]<4000),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c( "Skid.available..mm..accum")]<70),]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.2),ncol=1))

for (i in 1:ncol(data.2))

{
  
  longueur[i,1]<-length(unique(data.2[,i]))
  
  rownames(longueur)<-colnames(data.2)
  
  
}



data.2<-data.2[,!colnames(data.2)%in%c( "Gravel.Sand.Macadam"  )]

#---------------------------------------------------------------Création d'un modèle simple en ne gardant que ceux des plus faibles occurences------------------------------------------------------

data.3<-data.2


data.3=subset(data.3,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.3),ncol=1))

for (i in 1:ncol(data.3))
  
{
  
  longueur[i,1]<-length(unique(data.3[,i]))
  
  rownames(longueur)<-colnames(data.3)
  
  
}



data.3<-data.3[,!colnames(data.3)%in%c("Model")]
data.3<-data.3[,!colnames(data.3)%in%c("Part.Number")]
data.3<-data.3[,!colnames(data.3)%in%c("Ply.Rating")]
data.3<-data.3[,!colnames(data.3)%in%c("Codeclient")]
data.3<-data.3[,!colnames(data.3)%in%c("Date.Dismount")]
data.3<-data.3[,!colnames(data.3)%in%c("Date.Mount")]
data.3<-data.3[,!colnames(data.3)%in%c("Tire.type")]
data.3<-data.3[,!colnames(data.3)%in%c("Date.Reception.at.plant")]
data.3<-data.3[,!colnames(data.3)%in%c("Operator.Country.DUS")]
data.3<-data.3[,!colnames(data.3)%in%c("Transit.time.before.retreading.estimate")]
data.3<-data.3[,!colnames(data.3)%in%c("TO.run.ToPressAlt" )]
data.3<-data.3[,!colnames(data.3)%in%c("TO.run.FromDenAlt"  )]
data.3<-data.3[,!colnames(data.3)%in%c("Cargo"  )]
data.3<-data.3[,!colnames(data.3)%in%c("Holiday.Charter" )]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.3),ncol=1))

for (i in 1:ncol(data.3))
  
{
  
  longueur[i,1]<-length(unique(data.3[,i]))
  
  rownames(longueur)<-colnames(data.3)
  
  
}


data.3$TAT.at.customer.estimate<-discretize(data.3$TAT.at.customer.estimate,method="cluster",categories=4)



data.3$Tread.Depth.accum<-discretize(data.3$Tread.Depth.accum,method="cluster",categories=3)
data.3$AvgOfTire.age.when.received<-discretize(data.3$AvgOfTire.age.when.received,method="cluster",categories=3)
data.3$LPT<-discretize(data.3$LPT,method="cluster",categories=4)
data.3$Number.of.Flights.per.day.per.aircraft<-discretize(data.3$Number.of.Flights.per.day.per.aircraft,method="cluster",categories=4)

data.3$LPT<-discretize(data.3$LPT,method="cluster",categories=4)

data.3$AvgOfA.C.Maximum.Take.off.Weight.Kgs.<-discretize(data.3$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,method="cluster",categories=3)
data.3$GAvgToAptTempWeighted<-discretize(data.3$GAvgToAptTempWeighted,method="cluster",categories=4)
data.3$GAvgToAptMinTempWeighted<-discretize(data.3$GAvgToAptMinTempWeighted,method="cluster",categories=4)
data.3$GAvgToAptMaxTempWeighted<-discretize(data.3$GAvgToAptMaxTempWeighted,method="cluster",categories=4)
data.3$GAvgToAptDenWeighted<-discretize(data.3$GAvgToAptDenWeighted,method="cluster",categories=4)
data.3$GAvgToPressAltWeighted<-discretize(data.3$GAvgToPressAltWeighted,method="cluster",categories=4)
data.3$GAvgToDenAltWeighted<-discretize(data.3$GAvgToDenAltWeighted,method="cluster",categories=4)
data.3$TO.run.FromPressAlt<-discretize(data.3$TO.run.FromPressAlt,method="cluster",categories=4)
data.3$TO.run.ToDenAlt<-discretize(data.3$TO.run.ToDenAlt,method="cluster",categories=4)

data.3$GAvgDistanceWeighted<-discretize(data.3$GAvgDistanceWeighted,method="cluster",categories=4)
data.3$MinRunwayLength.m.<-discretize(data.3$MinRunwayLength.m.,method="cluster",categories=4)
data.3$MaxRunwayLength.m.<-discretize(data.3$MaxRunwayLength.m.,method="cluster",categories=4)
data.3$AvgRunwayLength.m.<-discretize(data.3$AvgRunwayLength.m.,method="cluster",categories=4)


data.3$Asphalt<-discretize(data.3$Asphalt,method="cluster",categories=2)
data.3$Concrete<-discretize(data.3$Concrete,method="cluster",categories=2)
data.3$Other.Surfaces<-discretize(data.3$Other.Surfaces,method="cluster",categories=2)

data.3$Section.Width..mm.<-discretize(data.3$Section.Width..mm.,method="cluster",categories=3)
data.3$Tire.mass..kg.<-discretize(data.3$Tire.mass..kg.,method="cluster",categories=3)

data.3$Leaser<-discretize(data.3$Leaser,method="cluster",categories=2)
data.3$Low.Cost<-discretize(data.3$Low.Cost,method="cluster",categories=2)

data.3$Skid.available..mm..accum<-discretize(data.3$Skid.available..mm..accum,method="cluster",categories=5)


Ventes=data.3[,c(37,39,40,41)]
data.3=data.3[,-c(37,39,40,41)]



dataVarSelRF<-data.3[,!colnames(data.3)%in%c("TAT.at.customer.estimate")]
TAT=data.3$TAT.at.customer.estimate




# Fonction d'evaluation qui construit 'test' et 'train' a chaque fois
evaluator1 <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits1 <- runif(nrow(data.3))
  results = sapply(1:k, function(i) {
    test.idx <- (splits1 >= (i - 1) / k) & (splits1 < i / k)
    train.idx <- !test.idx
    test <- data.3[test.idx, , drop=FALSE]
    train <- data.3[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "TAT.at.customer.estimate"), train)
    error.rate = sum(data.3$TAT.at.customer.estimate != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}
bestSubsetFWS <- forward.search(names(data.3)[-12], evaluator1)
bestSubsetFWS


poids <- information.gain(TAT~., data.3)
poids<-poids[-12,]
print(poids)
names(poids)=c("Rmax","T.Tech","SR","NorR","P.Manuf","P.Retr","P.Rece","Pos","Framer","Depth.ac","Age","LPT"
                   ,"Fly","MTOW","T","Tmin","Tmax","D","P","Dalt","TOP","TOD","Dist","MinD",
                   "MaxD","AvgD","Bitume","Concrete","AutresSur","Section","SkidD","Mass","R","Leaser","LowC")

barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]),col=c('black'))

#---------------------------------------------------------------Modèle un peu plus compliqué------------------------------------------------------------------------------------------


data.4<-data.2


data.4=subset(data.4,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.4),ncol=1))

for (i in 1:ncol(data.4))
  
{
  
  longueur[i,1]<-length(unique(data.4[,i]))
  
  rownames(longueur)<-colnames(data.4)
  
  
}




data.4<-data.4[,!colnames(data.4)%in%c("Ply.Rating")]
data.4<-data.4[,!colnames(data.4)%in%c("Date.Dismount")]
data.4<-data.4[,!colnames(data.4)%in%c("Date.Mount")]
data.4<-data.4[,!colnames(data.4)%in%c("Tire.type")]
data.4<-data.4[,!colnames(data.4)%in%c("Date.Reception.at.plant")]
data.4<-data.4[,!colnames(data.4)%in%c("Operator.Country.DUS")]
data.4<-data.4[,!colnames(data.4)%in%c("Transit.time.before.retreading.estimate")]
data.4<-data.4[,!colnames(data.4)%in%c("TO.run.ToPressAlt" )]
data.4<-data.4[,!colnames(data.4)%in%c("TO.run.FromDenAlt"  )]
data.4<-data.4[,!colnames(data.4)%in%c("Cargo"  )]
data.4<-data.4[,!colnames(data.4)%in%c("Holiday.Charter" )]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.4),ncol=1))

for (i in 1:ncol(data.4))
  
{
  
  longueur[i,1]<-length(unique(data.4[,i]))
  
  rownames(longueur)<-colnames(data.4)
  
  
}


data.4$TAT.at.customer.estimate<-discretize(data.4$TAT.at.customer.estimate,method="cluster",categories=4)



data.4$Tread.Depth.accum<-discretize(data.4$Tread.Depth.accum,method="cluster",categories=3)
data.4$AvgOfTire.age.when.received<-discretize(data.4$AvgOfTire.age.when.received,method="cluster",categories=3)
data.4$LPT<-discretize(data.4$LPT,method="cluster",categories=4)
data.4$Number.of.Flights.per.day.per.aircraft<-discretize(data.4$Number.of.Flights.per.day.per.aircraft,method="cluster",categories=4)

data.4$LPT<-discretize(data.4$LPT,method="cluster",categories=4)

data.4$AvgOfA.C.Maximum.Take.off.Weight.Kgs.<-discretize(data.4$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,method="cluster",categories=3)
data.4$GAvgToAptTempWeighted<-discretize(data.4$GAvgToAptTempWeighted,method="cluster",categories=4)
data.4$GAvgToAptMinTempWeighted<-discretize(data.4$GAvgToAptMinTempWeighted,method="cluster",categories=4)
data.4$GAvgToAptMaxTempWeighted<-discretize(data.4$GAvgToAptMaxTempWeighted,method="cluster",categories=4)
data.4$GAvgToAptDenWeighted<-discretize(data.4$GAvgToAptDenWeighted,method="cluster",categories=4)
data.4$GAvgToPressAltWeighted<-discretize(data.4$GAvgToPressAltWeighted,method="cluster",categories=4)
data.4$GAvgToDenAltWeighted<-discretize(data.4$GAvgToDenAltWeighted,method="cluster",categories=4)
data.4$TO.run.FromPressAlt<-discretize(data.4$TO.run.FromPressAlt,method="cluster",categories=4)
data.4$TO.run.ToDenAlt<-discretize(data.4$TO.run.ToDenAlt,method="cluster",categories=4)

data.4$GAvgDistanceWeighted<-discretize(data.4$GAvgDistanceWeighted,method="cluster",categories=4)
data.4$MinRunwayLength.m.<-discretize(data.4$MinRunwayLength.m.,method="cluster",categories=4)
data.4$MaxRunwayLength.m.<-discretize(data.4$MaxRunwayLength.m.,method="cluster",categories=4)
data.4$AvgRunwayLength.m.<-discretize(data.4$AvgRunwayLength.m.,method="cluster",categories=4)


data.4$Asphalt<-discretize(data.4$Asphalt,method="cluster",categories=2)
data.4$Concrete<-discretize(data.4$Concrete,method="cluster",categories=2)
data.4$Other.Surfaces<-discretize(data.4$Other.Surfaces,method="cluster",categories=2)

data.4$Section.Width..mm.<-discretize(data.4$Section.Width..mm.,method="cluster",categories=3)
data.4$Tire.mass..kg.<-discretize(data.4$Tire.mass..kg.,method="cluster",categories=3)

data.4$Leaser<-discretize(data.4$Leaser,method="cluster",categories=2)
data.4$Low.Cost<-discretize(data.4$Low.Cost,method="cluster",categories=2)

data.4$Skid.available..mm..accum<-discretize(data.4$Skid.available..mm..accum,method="cluster",categories=5)


Ventes=data.4[,c(40,42,43,44)]
data.4=data.4[,-c(40,42,43,44)]



dataVarSelRF<-data.4[,!colnames(data.4)%in%c("TAT.at.customer.estimate")]
TAT=data.4$TAT.at.customer.estimate




# Fonction d'evaluation qui construit 'test' et 'train' a chaque fois
evaluator1 <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits1 <- runif(nrow(data.4))
  results = sapply(1:k, function(i) {
    test.idx <- (splits1 >= (i - 1) / k) & (splits1 < i / k)
    train.idx <- !test.idx
    test <- data.4[test.idx, , drop=FALSE]
    train <- data.4[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "TAT.at.customer.estimate"), train)
    error.rate = sum(data.4$TAT.at.customer.estimate != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}
bestSubsetFWS <- forward.search(names(data.4)[-12], evaluator1)
bestSubsetFWS


poids <- information.gain(TAT~., data.4)
poids<-poids[-15,]
print(poids)
names(poids)=c("Rmax","Client","PN","T.Tech","SR","NorR","P.Manuf","P.Retr","P.Rece","Pos","Framer","Model","Depth.ac","Age","LPT"
               ,"Fly","MTOW","T","Tmin","Tmax","D","P","Dalt","TOP","TOD","Dist","MinD",
               "MaxD","AvgD","Bitume","Concrete","AutresSur","Section","SkidD","Mass","R","Leaser","LowC","Zone")

barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))



#---------------------------------------------------------------RF avec les paramètres importants-----------------------------------------------------------------------------------


#---------------------------------------------------------------Fonction LDA---------------------------------------------------------------------------------------------------------


LDA<-function(DF,name_var_cible)
  
{
 
   
sample<-sample(1:nrow(DF),nrow(DF)/2,replace=FALSE) 

train<-DF[sample,]
test<-DF[-sample,]

result<-list(LDA=NA,Match=NA)

r=pr=NULL

try(r<-lda(train[,colnames(train)%in%c(name_var_cible)] ~., data=train))

try(pr<-predict(r,test))

try(result[[1]]<-pr)

try(result[[2]]<-as.numeric(summary(test[,colnames(test)%in%c(name_var_cible)]==pr$class)[3])/(as.numeric(summary(test[,colnames(test)%in%c(name_var_cible)]==pr$class)[2])+as.numeric(summary(test[,colnames(test)%in%c(name_var_cible)]==pr$class)[3])))

 
return(result)

}  

#---------------------------------------------------------------RF Age--------------------------------------------------------------------------------------------------------------




DF<-data.frame(data.4$TAT.at.customer.estimate,data.4$AvgOfTire.age.when.received)
rf <- randomForest(DF$data.4.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

#---------------------------------------------------------------RF Age/Model--------------------------------------------------------------------------------------------------------------



DF<-data.frame(data.4$TAT.at.customer.estimate,data.4$Model)
rf <- randomForest(DF$data.4.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

#---------------------------------------------------------------RF Age/Model/Skid-------------------------------------------------------------------------------------------------

DF<-data.frame(data.4$TAT.at.customer.estimate,data.4$Model,data.4$Skid.available..mm..accum)
rf <- randomForest(DF$data.4.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

#---------------------------------------------------------------RF Age/Model/Skid/PN-----------------------------------------------------------------------------------------------


DF<-data.frame(data.4$TAT.at.customer.estimate,data.4$Model,data.4$Skid.available..mm..accum,data.4$Part.Number)
rf <- randomForest(DF$data.4.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

#---------------------------------------------------------------RF Age/Model/Skid/R-------------------------------------------------------------------------------------------------

DF<-data.frame(data.4$TAT.at.customer.estimate,data.4$Model,data.4$Skid.available..mm..accum,data.4$R_level.1)
rf <- randomForest(DF$data.4.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

#---------------------------------------------------------------RF Age/Model/Skid/R/Zone/AvgD---------------------------------------------------------------------------------------

DF<-data.frame(data.4$TAT.at.customer.estimate,data.4$Model,data.4$Skid.available..mm..accum,data.4$R_level.1,data.4$Zone,data.4$GAvgDistanceWeighted)
rf <- randomForest(DF$data.4.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)










# MDSplot(rf, DF$data.4.TAT.at.customer.estimate)
# 
# 
# #**************************
# #return the rules of a tree
# #**************************
# getConds<-function(tree){
#   #store all conditions into a list
#   conds<-list()
#   #start by the terminal nodes and find previous conditions
#   id.leafs<-which(tree$status==-1)
#   j<-0
#   for(i in id.leafs){
#     j<-j+1
#     prevConds<-prevCond(tree,i)
#     conds[[j]]<-prevConds$cond
#     while(prevConds$id>1){
#       prevConds<-prevCond(tree,prevConds$id)
#       conds[[j]]<-paste(conds[[j]]," & ",prevConds$cond)
#       if(prevConds$id==1){
#         conds[[j]]<-paste(conds[[j]]," => ",tree$prediction[i])
#         break()
#       }
#     }
#     
#   }
#   
#   return(conds)
# }
# 
# #**************************
# #find the previous conditions in the tree
# #**************************
# prevCond<-function(tree,i){
#   if(i %in% tree$right_daughter){
#     id<-which(tree$right_daughter==i)
#     cond<-paste(tree$split_var[id],">",tree$split_point[id])
#   }
#   if(i %in% tree$left_daughter){
#     id<-which(tree$left_daughter==i)
#     cond<-paste(tree$split_var[id],"<",tree$split_point[id])
#   }
#   
#   return(list(cond=cond,id=id))
# }
# 
# #remove spaces in a word
# collapse<-function(x){
#   x<-sub(" ","_",x)
#   
#   return(x)
# }
# 
# 
# 
# tree<-getTree(rf, k=1, labelVar=TRUE)
# #rename the name of the column
# colnames(tree)<-sapply(colnames(tree),collapse)
# rules<-getConds(tree)
# print(rules)
# 
# 





#---------------------------------------------------------------Changement au niveau de la discrétisation------------------------------------------------------------


data.5<-data.2


data.5=subset(data.5,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.5),ncol=1))

for (i in 1:ncol(data.5))
  
{
  
  longueur[i,1]<-length(unique(data.5[,i]))
  
  rownames(longueur)<-colnames(data.5)
  
  
}




data.5<-data.5[,!colnames(data.5)%in%c("Ply.Rating")]
data.5<-data.5[,!colnames(data.5)%in%c("Date.Dismount")]
data.5<-data.5[,!colnames(data.5)%in%c("Date.Mount")]
data.5<-data.5[,!colnames(data.5)%in%c("Tire.type")]
data.5<-data.5[,!colnames(data.5)%in%c("Date.Reception.at.plant")]
data.5<-data.5[,!colnames(data.5)%in%c("Operator.Country.DUS")]
data.5<-data.5[,!colnames(data.5)%in%c("Transit.time.before.retreading.estimate")]
data.5<-data.5[,!colnames(data.5)%in%c("TO.run.ToPressAlt" )]
data.5<-data.5[,!colnames(data.5)%in%c("TO.run.FromDenAlt"  )]
data.5<-data.5[,!colnames(data.5)%in%c("Cargo"  )]
data.5<-data.5[,!colnames(data.5)%in%c("Holiday.Charter" )]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.5),ncol=1))

for (i in 1:ncol(data.5))
  
{
  
  longueur[i,1]<-length(unique(data.5[,i]))
  
  rownames(longueur)<-colnames(data.5)
  
  
}



discret_var_arbitraire<-function(Y,p,q)
  
{  
  
  y<-c()
  
  
  for (i in 1:length(Y))
    
  {
    
    if(Y[i]<=p){y[i]<-"<=200"}
    if((Y[i]>p)&(Y[i]<=q)){y[i]<-"200<..<=500"}
    if(Y[i]>q){y[i]<-">500"}
    
  }
  
  return(y)
  
}


boxplot(data.2$TAT.at.customer.estimate,col='pink',xaxp=c(0,max(data.2$TAT.at.customer.estimate),12),boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[15])







data.5$TAT.at.customer.estimate<-discretize(data.5$TAT.at.customer.estimate,method="cluster",categories=7)



data.5$Tread.Depth.accum<-discretize(data.5$Tread.Depth.accum,method="cluster",categories=3)
data.5$AvgOfTire.age.when.received<-discretize(data.5$AvgOfTire.age.when.received,method="cluster",categories=3)
data.5$LPT<-discretize(data.5$LPT,method="cluster",categories=4)
data.5$Number.of.Flights.per.day.per.aircraft<-discretize(data.5$Number.of.Flights.per.day.per.aircraft,method="cluster",categories=4)

data.5$LPT<-discretize(data.5$LPT,method="cluster",categories=4)

data.5$AvgOfA.C.Maximum.Take.off.Weight.Kgs.<-discretize(data.5$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,method="cluster",categories=3)
data.5$GAvgToAptTempWeighted<-discretize(data.5$GAvgToAptTempWeighted,method="cluster",categories=4)
data.5$GAvgToAptMinTempWeighted<-discretize(data.5$GAvgToAptMinTempWeighted,method="cluster",categories=4)
data.5$GAvgToAptMaxTempWeighted<-discretize(data.5$GAvgToAptMaxTempWeighted,method="cluster",categories=4)
data.5$GAvgToAptDenWeighted<-discretize(data.5$GAvgToAptDenWeighted,method="cluster",categories=4)
data.5$GAvgToPressAltWeighted<-discretize(data.5$GAvgToPressAltWeighted,method="cluster",categories=4)
data.5$GAvgToDenAltWeighted<-discretize(data.5$GAvgToDenAltWeighted,method="cluster",categories=4)
data.5$TO.run.FromPressAlt<-discretize(data.5$TO.run.FromPressAlt,method="cluster",categories=4)
data.5$TO.run.ToDenAlt<-discretize(data.5$TO.run.ToDenAlt,method="cluster",categories=4)

data.5$GAvgDistanceWeighted<-discretize(data.5$GAvgDistanceWeighted,method="cluster",categories=4)
data.5$MinRunwayLength.m.<-discretize(data.5$MinRunwayLength.m.,method="cluster",categories=4)
data.5$MaxRunwayLength.m.<-discretize(data.5$MaxRunwayLength.m.,method="cluster",categories=4)
data.5$AvgRunwayLength.m.<-discretize(data.5$AvgRunwayLength.m.,method="cluster",categories=4)


data.5$Asphalt<-discretize(data.5$Asphalt,method="cluster",categories=2)
data.5$Concrete<-discretize(data.5$Concrete,method="cluster",categories=2)
data.5$Other.Surfaces<-discretize(data.5$Other.Surfaces,method="cluster",categories=2)

data.5$Section.Width..mm.<-discretize(data.5$Section.Width..mm.,method="cluster",categories=3)
data.5$Tire.mass..kg.<-discretize(data.5$Tire.mass..kg.,method="cluster",categories=3)

data.5$Leaser<-discretize(data.5$Leaser,method="cluster",categories=2)
data.5$Low.Cost<-discretize(data.5$Low.Cost,method="cluster",categories=2)

data.5$Skid.available..mm..accum<-discretize(data.5$Skid.available..mm..accum,method="cluster",categories=5)


data.5=data.5[,-c(40,42,43,44)]









TAT=data.5$TAT.at.customer.estimate
poids <- information.gain(TAT~., data.5)
poids<-poids[-15,]
print(poids)
names(poids)=c("Rmax","Client","PN","T.Tech","SR","NorR","P.Manuf","P.Retr","P.Rece","Pos","Framer","Model","Depth.ac","Age","LPT"
               ,"Fly","MTOW","T","Tmin","Tmax","D","P","Dalt","TOP","TOD","Dist","MinD",
               "MaxD","AvgD","Bitume","Concrete","AutresSur","Section","SkidD","Mass","R","Leaser","LowC","Zone")

barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))



DF<-data.frame(data.5$TAT.at.customer.estimate,data.5$Model,data.5$Skid.available..mm..accum,data.5$R_level.1,data.5$Zone,data.5$GAvgDistanceWeighted,data.5$MaxRunwayLength.m.)
rf <- randomForest(DF$data.5.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)
plot(rf)

#---------------------------------------------------------------Test de fiabilité pour les retours-----------------------

DF<-data.frame(data.5$TAT.at.customer.estimate,as.factor(as.character(data.5$Codeclient)),as.factor(as.character(data.5$Part.Number)),data.5$Zone)
rf1 <- randomForest(DF$data.5.TAT.at.customer.estimate ~ ., DF)
colMeans(rf1$err.rate)

DF<-data.frame(data.5$TAT.at.customer.estimate,as.factor(as.character(data.5$Codeclient)),as.factor(as.character(data.5$Part.Number)),data.5$Zone,data.5$AvgOfTire.age.when.received,data.5$Model,data.5$Skid.available..mm..accum,data.5$R_level.1,data.5$AvgRunwayLength.m.,data.5$MaxRunwayLength.m.)
rf <- randomForest(DF$data.5.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

DF<-data.frame(data.5$TAT.at.customer.estimate,as.factor(as.character(data.5$Part.Number)),data.5$Zone,data.5$AvgOfTire.age.when.received,data.5$Model,data.5$Skid.available..mm..accum,data.5$R_level.1,data.5$AvgRunwayLength.m.,data.5$MaxRunwayLength.m.)
rf3 <- randomForest(DF$data.5.TAT.at.customer.estimate ~ ., DF)
colMeans(rf3$err.rate)

DF<-data.frame(data.5$TAT.at.customer.estimate,as.factor(as.character(data.5$Part.Number)),data.5$Zone,data.5$AvgOfTire.age.when.received,data.5$Model,data.5$Skid.available..mm..accum,data.5$R_level.1)
rf4 <- randomForest(DF$data.5.TAT.at.customer.estimate ~ ., DF)
colMeans(rf4$err.rate)

compa=as.matrix(cbind(colMeans(rf$err.rate),colMeans(rf1$err.rate),colMeans(rf3$err.rate),colMeans(rf4$err.rate)))
compa=floor(100*t(compa))


barplot(sort(poids,decreasing = TRUE)[1:10],main="Importance des paramètres",names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


barplot(compa,axes=FALSE,names.arg = toupper(colnames(compa)),yaxp=c(0,80,10), beside = TRUE,xlab="TAT",main="Comparaison des modèles avec différents paramètres",col=c('red','blue','purple','yellow'),ylab="Taux d'erreur")

ticks <- seq(0, 100, 5)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   


legend("topleft",cex=0.8,legend=c("Client/PN/Zone/Age/Avion/Gomme/Distance/Piste/Rlevel","Client/PN/Zone/NorR","PN/Zone/Age/Avion/Gomme/Distance/Piste/Rlevel","PN/Zone/Age/Avion/Gomme/Rlevel"),col=c('red','blue','purple','yellow'),pch = 15)


pie(summary(data.6$TAT.at.customer.estimate),main="Répartition des TAT")




data.6=data.5
data.6$Codeclient<-as.factor(as.character(data.6$Codeclient))
data.6$Part.Number<-as.factor(as.character(data.6$Part.Number))
rff <- randomForest(data.6$TAT.at.customer.estimate ~ ., data.6)
colMeans(rff$err.rate)

#---------------------------------------------------------------Test sur les LPTs---------------------------------------------------------

LPT=data.5$LPT
poids <- information.gain(LPT~., data.5)
poids<-poids[,1]
names(poids)=c("Rmax","Client","PN","T.Tech","SR","NorR","P.Manuf","P.Retr","P.Rece","Pos","Framer","Model","Depth.ac","Age","LPT"
               ,"Fly","MTOW","T","Tmin","Tmax","D","P","Dalt","TOP","TOD","Dist","MinD",
               "MaxD","AvgD","Bitume","Concrete","AutresSur","Section","SkidD","Mass","R","Leaser","LowC","Zone")


barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


DF<-data.frame(data.5$LPT,as.factor(as.character(data.5$Codeclient)),as.factor(as.character(data.5$Part.Number)),data.5$Model,data.5$Tire.mass..kg.,data.5$GAvgDistanceWeighted,data.5$Zone,data.5$Position,data.5$Number.of.Flights.per.day.per.aircraft,data.5$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.5$Section.Width..mm.)
rf1 <- randomForest(DF$data.5.LPT ~ ., DF)
colMeans(rf1$err.rate)

DF<-data.frame(data.5$LPT,as.factor(as.character(data.5$Part.Number)),data.5$Model,data.5$Tire.mass..kg.,data.5$GAvgDistanceWeighted,data.5$Zone,data.5$Position,data.5$Number.of.Flights.per.day.per.aircraft,data.5$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.5$Section.Width..mm.)
rf2 <- randomForest(DF$data.5.LPT ~ ., DF)
colMeans(rf2$err.rate)

DF<-data.frame(data.5$LPT,as.factor(as.character(data.5$Part.Number)),data.5$Model,data.5$Tire.mass..kg.,data.5$Zone,data.5$Position,data.5$Section.Width..mm.)
rf3 <- randomForest(DF$data.5.LPT ~ ., DF)
colMeans(rf3$err.rate)


DF<-data.frame(data.5$LPT,data.5$Model)
rf4 <- randomForest(DF$data.5.LPT ~ ., DF)
colMeans(rf4$err.rate)


compa=as.matrix(cbind(colMeans(rf1$err.rate),colMeans(rf2$err.rate),colMeans(rf3$err.rate),colMeans(rf4$err.rate)))
compa=(100*t(compa))


barplot(sort(poids,decreasing = TRUE)[1:10],main="Importance des paramètres",names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


barplot(compa,axes=FALSE,names.arg = toupper(colnames(compa)),yaxp=c(0,80,10), beside = TRUE,xlab="LPT",main="Comparaison des modèles avec différents paramètres",col=c('red','blue','purple','yellow'),ylab="Taux d'erreur")

ticks <- seq(0, 100, 5)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   


legend("topleft",cex=0.8,legend=c("Client/PN/Model/Mass/Dist/Zone/Pos/Fly.day/MTOW/Section","PN/Model/Mass/Dist/Zone/Pos/Fly.day/MTOW/Section","PN/Model/Mass/Zone/Pos/Section","Model"),col=c('red','blue','purple','yellow'),pch = 15)


pie(summary(data.5$LPT),main="Répartition des LPT")


rf <- randomForest (DF$data.5.LPT ~ ., DF);
OOB.votes <- predict (rf,DF,type="prob");
OOB.pred <- OOB.votes[,2];

pred.obj <- prediction (OOB.pred,data.5$LPT)

RP.perf <- performance(pred.obj, "rec","prec");
plot (RP.perf);

ROC.perf <- performance(pred.obj, "fpr","tpr");
plot (ROC.perf);

plot  (RP.perf@alpha.values[[1]],RP.perf@x.values[[1]])
lines (RP.perf@alpha.values[[1]],RP.perf@y.values[[1]])
lines (ROC.perf@alpha.values[[1]],ROC.perf@x.values[[1]])

#---------------------------------------------------------------Changement de méthode de discrétisation (frequency)--------------------------------------------------



data.7<-data.2


data.7=subset(data.7,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.7),ncol=1))

for (i in 1:ncol(data.7))
  
{
  
  longueur[i,1]<-length(unique(data.7[,i]))
  
  rownames(longueur)<-colnames(data.7)
  
  
}




data.7<-data.7[,!colnames(data.7)%in%c("Ply.Rating")]
data.7<-data.7[,!colnames(data.7)%in%c("Date.Dismount")]
data.7<-data.7[,!colnames(data.7)%in%c("Date.Mount")]
data.7<-data.7[,!colnames(data.7)%in%c("Tire.type")]
data.7<-data.7[,!colnames(data.7)%in%c("Date.Reception.at.plant")]
data.7<-data.7[,!colnames(data.7)%in%c("Operator.Country.DUS")]
data.7<-data.7[,!colnames(data.7)%in%c("Transit.time.before.retreading.estimate")]
data.7<-data.7[,!colnames(data.7)%in%c("TO.run.ToPressAlt" )]
data.7<-data.7[,!colnames(data.7)%in%c("TO.run.FromDenAlt"  )]
data.7<-data.7[,!colnames(data.7)%in%c("Cargo"  )]
data.7<-data.7[,!colnames(data.7)%in%c("Holiday.Charter" )]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.7),ncol=1))

for (i in 1:ncol(data.7))
  
{
  
  longueur[i,1]<-length(unique(data.7[,i]))
  
  rownames(longueur)<-colnames(data.7)
  
  
}



discret_var_arbitraire<-function(Y,p,q)
  
{  
  
  y<-c()
  
  
  for (i in 1:length(Y))
    
  {
    
    if(Y[i]<=p){y[i]<-"<=200"}
    if((Y[i]>p)&(Y[i]<=q)){y[i]<-"200<..<=500"}
    if(Y[i]>q){y[i]<-">500"}
    
  }
  
  return(y)
  
}


boxplot(data.2$TAT.at.customer.estimate,col='pink',xaxp=c(0,max(data.2$TAT.at.customer.estimate),12),boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[15])







data.7$TAT.at.customer.estimate<-discretize(data.7$TAT.at.customer.estimate,method="frequency",categories=6)



data.7$Tread.Depth.accum<-discretize(data.7$Tread.Depth.accum,method="frequency",categories=3)
data.7$AvgOfTire.age.when.received<-discretize(data.7$AvgOfTire.age.when.received,method="frequency",categories=3)
data.7$LPT<-discretize(data.7$LPT,method="frequency",categories=4)
data.7$Number.of.Flights.per.day.per.aircraft<-discretize(data.7$Number.of.Flights.per.day.per.aircraft,method="frequency",categories=4)

data.7$LPT<-discretize(data.7$LPT,method="frequency",categories=4)

data.7$AvgOfA.C.Maximum.Take.off.Weight.Kgs.<-discretize(data.7$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,method="frequency",categories=3)
data.7$GAvgToAptTempWeighted<-discretize(data.7$GAvgToAptTempWeighted,method="frequency",categories=4)
data.7$GAvgToAptMinTempWeighted<-discretize(data.7$GAvgToAptMinTempWeighted,method="frequency",categories=4)
data.7$GAvgToAptMaxTempWeighted<-discretize(data.7$GAvgToAptMaxTempWeighted,method="frequency",categories=4)
data.7$GAvgToAptDenWeighted<-discretize(data.7$GAvgToAptDenWeighted,method="frequency",categories=4)
data.7$GAvgToPressAltWeighted<-discretize(data.7$GAvgToPressAltWeighted,method="frequency",categories=4)
data.7$GAvgToDenAltWeighted<-discretize(data.7$GAvgToDenAltWeighted,method="frequency",categories=4)
data.7$TO.run.FromPressAlt<-discretize(data.7$TO.run.FromPressAlt,method="frequency",categories=4)
data.7$TO.run.ToDenAlt<-discretize(data.7$TO.run.ToDenAlt,method="frequency",categories=4)

data.7$GAvgDistanceWeighted<-discretize(data.7$GAvgDistanceWeighted,method="frequency",categories=4)
data.7$MinRunwayLength.m.<-discretize(data.7$MinRunwayLength.m.,method="frequency",categories=4)
data.7$MaxRunwayLength.m.<-discretize(data.7$MaxRunwayLength.m.,method="frequency",categories=4)
data.7$AvgRunwayLength.m.<-discretize(data.7$AvgRunwayLength.m.,method="frequency",categories=4)


data.7$Asphalt<-discretize(data.7$Asphalt,method="frequency",categories=2)
data.7$Concrete<-discretize(data.7$Concrete,method="frequency",categories=2)
data.7$Other.Surfaces<-discretize(data.7$Other.Surfaces,method="frequency",categories=2)

data.7$Section.Width..mm.<-discretize(data.7$Section.Width..mm.,method="frequency",categories=3)
data.7$Tire.mass..kg.<-discretize(data.7$Tire.mass..kg.,method="frequency",categories=3)

data.7$Leaser<-discretize(data.7$Leaser,method="frequency",categories=2)
data.7$Low.Cost<-discretize(data.7$Low.Cost,method="frequency",categories=2)

data.7$Skid.available..mm..accum<-discretize(data.7$Skid.available..mm..accum,method="frequency",categories=5)


data.7=data.7[,-c(40,42,43,44)]


pie(summary(data.7$LPT),main="Répartition des LPT")
pie(summary(data.7$TAT.at.customer.estimate),main="Répartition des TAT")



TAT=data.7$TAT.at.customer.estimate
poids <- information.gain(TAT~., data.7)
poids<-poids[-15,]
print(poids)
names(poids)=c("Rmax","Client","PN","T.Tech","SR","NorR","P.Manuf","P.Retr","P.Rece","Pos","Framer","Model","Depth.ac","Age","LPT"
               ,"Fly","MTOW","T","Tmin","Tmax","D","P","Dalt","TOP","TOD","Dist","MinD",
               "MaxD","AvgD","Bitume","Concrete","AutresSur","Section","SkidD","Mass","R","Leaser","LowC","Zone")

barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


#Bleu
DF<-data.frame(data.7$TAT.at.customer.estimate,as.factor(as.character(data.7$Codeclient)),as.factor(as.character(data.7$Part.Number)),data.7$Zone)
rf1 <- randomForest(DF$data.7.TAT.at.customer.estimate ~ ., DF)
colMeans(rf1$err.rate)

#Rouge
DF<-data.frame(data.7$TAT.at.customer.estimate,as.factor(as.character(data.7$Codeclient)),as.factor(as.character(data.7$Part.Number)),data.7$Zone,data.7$AvgOfTire.age.when.received,data.7$Model,data.7$Skid.available..mm..accum,data.7$R_level.1,data.7$AvgRunwayLength.m.,data.7$MaxRunwayLength.m.)
rf <- randomForest(DF$data.7.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

#Violet
DF<-data.frame(data.7$TAT.at.customer.estimate,as.factor(as.character(data.7$Part.Number)),data.7$Zone,data.7$AvgOfTire.age.when.received,data.7$Model,data.7$Skid.available..mm..accum,data.7$R_level.1,data.7$AvgRunwayLength.m.,data.7$MaxRunwayLength.m.)
rf3 <- randomForest(DF$data.7.TAT.at.customer.estimate ~ ., DF)
colMeans(rf3$err.rate)

#Jaune
DF<-data.frame(data.7$TAT.at.customer.estimate,as.factor(as.character(data.7$Part.Number)),data.7$Zone,data.7$AvgOfTire.age.when.received,data.7$Model,data.7$Skid.available..mm..accum,data.7$R_level.1)
rf4 <- randomForest(DF$data.7.TAT.at.customer.estimate ~ ., DF)
colMeans(rf4$err.rate)

#Rose
DF<-data.frame(data.7$TAT.at.customer.estimate,data.7$Zone,data.7$AvgOfTire.age.when.received,data.7$Model,data.7$Skid.available..mm..accum,data.7$R_level.1)
rf5 <- randomForest(DF$data.7.TAT.at.customer.estimate ~ ., DF)
colMeans(rf5$err.rate)

compa=as.matrix(cbind(colMeans(rf$err.rate),colMeans(rf1$err.rate),colMeans(rf3$err.rate),colMeans(rf4$err.rate),colMeans(rf5$err.rate)))
compa=floor(100*t(compa))


barplot(sort(poids,decreasing = TRUE)[1:10],main="Importance des paramètres",names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


barplot(compa,axes=FALSE,names.arg = toupper(colnames(compa)),yaxp=c(0,80,10), beside = TRUE,xlab="TAT",main="Comparaison des modèles avec différents paramètres",col=c('red','blue','purple','yellow','pink'),ylab="Taux d'erreur")

ticks <- seq(0, 100, 5)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   


legend("topleft",cex=0.8,legend=c("Client/PN/Zone/Age/Avion/Gomme/Distance/Piste/Rlevel","Client/PN/Zone/NorR","PN/Zone/Age/Avion/Gomme/Distance/Piste/Rlevel","PN/Zone/Age/Avion/Gomme/Rlevel","Zone/Age/Avion/Gomme/Rlevel"),col=c('red','blue','purple','yellow','pink'),pch = 15)


getTree(rf)
importance(rf)
MDSplot(rf, DF$data.7.TAT.at.customer.estimate)

DF<-data.frame(as.factor(as.character(data.7$Codeclient)),as.factor(as.character(data.7$Part.Number)),data.7$Zone,data.7$AvgOfTire.age.when.received,data.7$Model,data.7$Skid.available..mm..accum,data.7$R_level.1,data.7$AvgRunwayLength.m.,data.7$MaxRunwayLength.m.)
data.7$TAT.at.customer.estimate
CV=rfcv(trainx=DF,trainy=data.7$TAT.at.customer.estimate,cv.fold=5)


#---------------------------------------------------------------Changement de méthode de discrétisation (fixed)--------------------------------------------------




data.8<-data.2


data.8=subset(data.8,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.8),ncol=1))

for (i in 1:ncol(data.8))
  
{
  
  longueur[i,1]<-length(unique(data.8[,i]))
  
  rownames(longueur)<-colnames(data.8)
  
  
}




data.8<-data.8[,!colnames(data.8)%in%c("Ply.Rating")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Dismount")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Mount")]
data.8<-data.8[,!colnames(data.8)%in%c("Tire.type")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Reception.at.plant")]
data.8<-data.8[,!colnames(data.8)%in%c("Operator.Country.DUS")]
data.8<-data.8[,!colnames(data.8)%in%c("Transit.time.before.retreading.estimate")]
data.8<-data.8[,!colnames(data.8)%in%c("TO.run.ToPressAlt" )]
data.8<-data.8[,!colnames(data.8)%in%c("TO.run.FromDenAlt"  )]
data.8<-data.8[,!colnames(data.8)%in%c("Cargo"  )]
data.8<-data.8[,!colnames(data.8)%in%c("Holiday.Charter" )]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.8),ncol=1))

for (i in 1:ncol(data.8))
  
{
  
  longueur[i,1]<-length(unique(data.8[,i]))
  
  rownames(longueur)<-colnames(data.8)
  
  
}



discret_var_arbitraire<-function(Y,p,q)
  
{  
  
  y<-c()
  
  
  for (i in 1:length(Y))
    
  {
    
    if(Y[i]<=p){y[i]<-"<=200"}
    if((Y[i]>p)&(Y[i]<=q)){y[i]<-"200<..<=500"}
    if(Y[i]>q){y[i]<-">500"}
    
  }
  
  return(y)
  
}


boxplot(data.2$TAT.at.customer.estimate,col='pink',xaxp=c(0,max(data.2$TAT.at.customer.estimate),12),boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[15])







data.8$TAT.at.customer.estimate<-discretize(data.8$TAT.at.customer.estimate,method="fixed",categories=c(16,100,150,220,300,350,450,550,750,1700))



data.8$Tread.Depth.accum<-discretize(data.8$Tread.Depth.accum,method="frequency",categories=3)
data.8$AvgOfTire.age.when.received<-discretize(data.8$AvgOfTire.age.when.received,method="frequency",categories=3)
data.8$LPT<-discretize(data.8$LPT,method="frequency",categories=4)
data.8$Number.of.Flights.per.day.per.aircraft<-discretize(data.8$Number.of.Flights.per.day.per.aircraft,method="frequency",categories=4)

data.8$LPT<-discretize(data.8$LPT,method="frequency",categories=4)

data.8$AvgOfA.C.Maximum.Take.off.Weight.Kgs.<-discretize(data.8$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,method="frequency",categories=3)
data.8$GAvgToAptTempWeighted<-discretize(data.8$GAvgToAptTempWeighted,method="frequency",categories=4)
data.8$GAvgToAptMinTempWeighted<-discretize(data.8$GAvgToAptMinTempWeighted,method="frequency",categories=4)
data.8$GAvgToAptMaxTempWeighted<-discretize(data.8$GAvgToAptMaxTempWeighted,method="frequency",categories=4)
data.8$GAvgToAptDenWeighted<-discretize(data.8$GAvgToAptDenWeighted,method="frequency",categories=4)
data.8$GAvgToPressAltWeighted<-discretize(data.8$GAvgToPressAltWeighted,method="frequency",categories=4)
data.8$GAvgToDenAltWeighted<-discretize(data.8$GAvgToDenAltWeighted,method="frequency",categories=4)
data.8$TO.run.FromPressAlt<-discretize(data.8$TO.run.FromPressAlt,method="frequency",categories=4)
data.8$TO.run.ToDenAlt<-discretize(data.8$TO.run.ToDenAlt,method="frequency",categories=4)

data.8$GAvgDistanceWeighted<-discretize(data.8$GAvgDistanceWeighted,method="frequency",categories=4)
data.8$MinRunwayLength.m.<-discretize(data.8$MinRunwayLength.m.,method="frequency",categories=4)
data.8$MaxRunwayLength.m.<-discretize(data.8$MaxRunwayLength.m.,method="frequency",categories=4)
data.8$AvgRunwayLength.m.<-discretize(data.8$AvgRunwayLength.m.,method="frequency",categories=4)


data.8$Asphalt<-discretize(data.8$Asphalt,method="frequency",categories=2)
data.8$Concrete<-discretize(data.8$Concrete,method="frequency",categories=2)
data.8$Other.Surfaces<-discretize(data.8$Other.Surfaces,method="frequency",categories=2)

data.8$Section.Width..mm.<-discretize(data.8$Section.Width..mm.,method="frequency",categories=3)
data.8$Tire.mass..kg.<-discretize(data.8$Tire.mass..kg.,method="frequency",categories=3)

data.8$Leaser<-discretize(data.8$Leaser,method="frequency",categories=2)
data.8$Low.Cost<-discretize(data.8$Low.Cost,method="frequency",categories=2)

data.8$Skid.available..mm..accum<-discretize(data.8$Skid.available..mm..accum,method="frequency",categories=5)


data.8=data.8[,-c(40,42,43,44)]


pie(summary(data.8$LPT),main="Répartition des LPT")
pie(summary(data.8$TAT.at.customer.estimate),main="Répartition des TAT")



TAT=data.8$TAT.at.customer.estimate
poids <- information.gain(TAT~., data.8)
poids<-poids[-15,]
print(poids)
names(poids)=c("Rmax","Client","PN","T.Tech","SR","NorR","P.Manuf","P.Retr","P.Rece","Pos","Framer","Model","Depth.ac","Age","LPT"
               ,"Fly","MTOW","T","Tmin","Tmax","D","P","Dalt","TOP","TOD","Dist","MinD",
               "MaxD","AvgD","Bitume","Concrete","AutresSur","Section","SkidD","Mass","R","Leaser","LowC","Zone")

barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


DF<-data.frame(data.8$TAT.at.customer.estimate,as.factor(as.character(data.8$Codeclient)),as.factor(as.character(data.8$Part.Number)),data.8$Zone)
rf1 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf1$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,as.factor(as.character(data.8$Codeclient)),as.factor(as.character(data.8$Part.Number)),data.8$Zone,data.8$AvgOfTire.age.when.received,data.8$Model,data.8$Skid.available..mm..accum,data.8$R_level.1,data.8$AvgRunwayLength.m.,data.8$MaxRunwayLength.m.)
rf <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,as.factor(as.character(data.8$Part.Number)),data.8$Zone,data.8$AvgOfTire.age.when.received,data.8$Model,data.8$Skid.available..mm..accum,data.8$R_level.1,data.8$AvgRunwayLength.m.,data.8$MaxRunwayLength.m.)
rf3 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf3$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,as.factor(as.character(data.8$Part.Number)),data.8$Zone,data.8$AvgOfTire.age.when.received,data.8$Model,data.8$Skid.available..mm..accum,data.8$R_level.1)
rf4 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf4$err.rate)

compa=as.matrix(cbind(colMeans(rf$err.rate),colMeans(rf1$err.rate),colMeans(rf3$err.rate),colMeans(rf4$err.rate)))
compa=floor(100*t(compa))


barplot(sort(poids,decreasing = TRUE)[1:10],main="Importance des paramètres",names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


barplot(compa,axes=FALSE,names.arg = toupper(colnames(compa)),yaxp=c(0,80,10), beside = TRUE,xlab="TAT",main="Comparaison des modèles avec différents paramètres",col=c('red','blue','purple','yellow'),ylab="Taux d'erreur")

ticks <- seq(0, 100, 5)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   


legend("topleft",cex=0.8,legend=c("Client/PN/Zone/Age/Avion/Gomme/Distance/Piste/Rlevel","Client/PN/Zone/NorR","PN/Zone/Age/Avion/Gomme/Distance/Piste/Rlevel","PN/Zone/Age/Avion/Gomme/Rlevel"),col=c('red','blue','purple','yellow'),pch = 15)

plot(data.2$TAT.at.customer.estimate)

ticks <- seq(0, 500, 50)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   

#---------------------------------------------------------------Random forest données qualitatives et quantitatives-------------------------------------------------------



data.9=data.2


data.9<-data.9[,!colnames(data.9)%in%c("Ply.Rating")]
data.9<-data.9[,!colnames(data.9)%in%c("Date.Dismount")]
data.9<-data.9[,!colnames(data.9)%in%c("Date.Mount")]
data.9<-data.9[,!colnames(data.9)%in%c("Tire.type")]
data.9<-data.9[,!colnames(data.9)%in%c("Date.Reception.at.plant")]
data.9<-data.9[,!colnames(data.9)%in%c("Operator.Country.DUS")]
data.9<-data.9[,!colnames(data.9)%in%c("Transit.time.before.retreading.estimate")]
data.9<-data.9[,!colnames(data.9)%in%c("TO.run.ToPressAlt" )]
data.9<-data.9[,!colnames(data.9)%in%c("TO.run.FromDenAlt"  )]
data.9<-data.9[,!colnames(data.9)%in%c("Cargo"  )]
data.9<-data.9[,!colnames(data.9)%in%c("Holiday.Charter" )]

data.9=data.9[,-c(40,42,43,44)]

TAT=data.9$TAT.at.customer.estimate
poids <- information.gain(TAT~., data.9)
poids<-poids[-15,]
print(poids)
names(poids)=c("Rmax","Client","PN","T.Tech","SR","NorR","P.Manuf","P.Retr","P.Rece","Pos","Framer","Model","Depth.ac","Age","LPT"
               ,"Fly","MTOW","T","Tmin","Tmax","D","P","Dalt","TOP","TOD","Dist","MinD",
               "MaxD","AvgD","Bitume","Concrete","AutresSur","Section","SkidD","Mass","R","Leaser","LowC","Zone")

barplot(sort(poids,decreasing = TRUE)[1:20],names.arg = names(sort(poids,decreasing = TRUE)[1:20]))


DF<-data.frame(data.9$TAT.at.customer.estimate,data.9$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.9$TO.run.FromPressAlt,data.9$Skid.available..mm..accum,as.factor(as.character(data.9$Codeclient)),data.9$GAvgDistanceWeighted,data.9$Concrete,data.9$AvgOfTire.age.when.received,data.9$Asphalt,data.9$LPT,data.9$Other.Surfaces,data.9$TO.run.ToDenAlt,data.9$Number.of.Flights.per.day.per.aircraft,as.factor(as.character(data.9$Part.Number)))
rfnum <- randomForest(DF,ntree=2)
colMeans(rfnum$err.rate)
DF$data.9.TAT.at.customer.estimate ~ .

DF<-data.frame(data.9$TAT.at.customer.estimate,data.9$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.9$TO.run.FromPressAlt,data.9$Skid.available..mm..accum,as.factor(as.character(data.9$Codeclient)))
rfnum <- randomForest(DF,ntree=2)
colMeans(rfnum$err.rate)

DF<-data.frame(data.9$TAT.at.customer.estimate,data.9$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.9$TO.run.FromPressAlt,data.9$Skid.available..mm..accum)
rfnum <- randomForest(DF,ntree=2)
colMeans(rfnum$err.rate)




DF<-data.frame(data.9$TAT.at.customer.estimate,data.9$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.9$TO.run.FromPressAlt,data.9$Skid.available..mm..accum,as.factor(as.character(data.9$Codeclient)),data.9$GAvgDistanceWeighted,data.9$Concrete,data.9$AvgOfTire.age.when.received,data.9$Asphalt,data.9$LPT,data.9$Other.Surfaces,data.9$TO.run.ToDenAlt,data.9$Number.of.Flights.per.day.per.aircraft,as.factor(as.character(data.9$Part.Number)))
sample<-sample(1:nrow(DF),floor(nrow(DF)/10),replace=FALSE) 
DF<-DF[sample,]
rfnum <- randomForest(DF,ntree=2000)
colMeans(rfnum$err.rate)


sample2<-sample(1:nrow(DF),floor(nrow(DF)/10),replace=FALSE) 
predict(rfnum, newdata = DF[sample2,])


data.9$TAT.at.customer.estimate<-discretize(data.9$TAT.at.customer.estimate,method="frequency",categories=6)
DF<-data.frame(data.9$TAT.at.customer.estimate,data.9$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.9$TO.run.FromPressAlt,data.9$Skid.available..mm..accum,as.factor(as.character(data.9$Codeclient)),data.9$GAvgDistanceWeighted,data.9$Concrete,data.9$AvgOfTire.age.when.received,data.9$Asphalt,data.9$LPT,data.9$Other.Surfaces,data.9$TO.run.ToDenAlt,data.9$Number.of.Flights.per.day.per.aircraft,as.factor(as.character(data.9$Part.Number)))
sample<-sample(1:nrow(DF),floor(nrow(DF)/10),replace=FALSE) 
DF<-DF[sample,]

rfnum1 <- randomForest(DF$data.9.TAT.at.customer.estimate ~ .,DF,ntree=200)
plot(rfnum)
sort(rfnum$importance)


rfnum2 <- randomForest(DF$data.9.TAT.at.customer.estimate ~ .,DF,ntree=200)
plot(rfnum2)


rfnum3<-combine(rfnum,rfnum2)
predict(rfnum3,type="prob")



getConfusionMatrix <- function(rf) {
  
  tbl = table(predict(rf), rf$y)
  class.error = vector()
  
  for (i in 1:nrow(tbl)) {
    rowSum = sum(tbl[i,])
    accurate = diag(tbl)[i]
    error = rowSum - accurate
    
    class.error[i] = error / rowSum
  }   
  return(cbind(tbl, class.error))
}


rfnum3<-combine(rfnum,rfnum2)
getConfusionMatrix(rfnum3)






DF<-data.frame(data.9$TAT.at.customer.estimate,data.9$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.9$TO.run.FromPressAlt,data.9$Skid.available..mm..accum,as.factor(as.character(data.9$Codeclient)),data.9$GAvgDistanceWeighted,data.9$Concrete,data.9$AvgOfTire.age.when.received,data.9$Asphalt,data.9$LPT,data.9$Other.Surfaces,data.9$TO.run.ToDenAlt,data.9$Number.of.Flights.per.day.per.aircraft,as.factor(as.character(data.9$Part.Number)))
sample<-sample(1:nrow(DF),floor(nrow(DF)/10),replace=FALSE) 
DF<-DF[sample,]

rfnum1 <- randomForest(DF$data.9.TAT.at.customer.estimate ~ .,DF,ntree=200)

rfnum2 <- randomForest(DF$data.9.TAT.at.customer.estimate ~ .,DF,ntree=200)







combinerarbre<-function(DF,cible,type="unsupervised",ntree,k,cut)
  
{


  
if(type=="supervised")
  
{
  
  
result<-list(error=NA,randomforest=NA)  
  
sample<-sample(1:nrow(DF),floor(nrow(DF)/cut),replace=FALSE) 
df<-DF[sample,]
  
rfnum1 <- randomForest(cible ~ .,df,ntree=ntree)
  
rfnum2 <- randomForest(cible ~ .,df,ntree=ntree)
  
  
rfnum.tot<-combine(rfnum1,rfnum2)

error<-c()

i=1

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

while(i<=k)
  
{
  
  rfnum<-NULL
  df<-DF
  
  sample<-sample(1:nrow(DF),floor(nrow(DF)/cut),replace=FALSE) 
  df<-DF[sample,]
  
  rfnum <- randomForest(cible ~ .,df,ntree=ntree)
  
  
  rfnum.tot<-combine(rfnum.tot,rfnum)
  error[i]<-mean(getConfusionMatrix( rfnum.tot)[,7])
  
  i<-i+1
  
  info <- sprintf("%d%% done", round(100*(i/k)))
  setWinProgressBar(pb, 100*(i/k), label=info)
  
}

result[[1]]<-error  
result[[2]]<-rfnum.tot

}
  

if(type=="unsupervised")  
  
{
 
  result<-list(randomforest=NA)
  
  sample<-sample(1:nrow(DF),floor(nrow(DF)/cut),replace=FALSE) 
  df<-DF[sample,]
  
  rfnum1 <- randomForest(df,ntree=ntree)
  
  rfnum2 <- randomForest(df,ntree=ntree)
  
  
  rfnum.tot<-combine(rfnum1,rfnum2)
  
  error<-c()
  
  i=1
  
  pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)
  
  while(i<=k)
    
  {
    
    rfnum<-NULL
    df<-DF
    
    sample<-sample(1:nrow(DF),floor(nrow(DF)/cut),replace=FALSE) 
    df<-DF[sample,]
    
    rfnum <- randomForest(df,ntree=ntree)
    
    
    rfnum.tot<-combine(rfnum.tot,rfnum)
    
    
    i<-i+1
    
    info <- sprintf("%d%% done", round(100*(i/k)))
    setWinProgressBar(pb, 100*(i/k), label=info)
    
  }
  
  
  result[[1]]<-rfnum.tot 
  
}

  
 return(result)
    
}




plot(error,type='l',main="Erreur moyenne en fonction du nombre d'arbres concaténés",xlab="Nombre d'arbres combinés",ylab="Taux derreur en %")





DF<-data.frame(data.9$TAT.at.customer.estimate,data.9$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.9$TO.run.FromPressAlt,data.9$Skid.available..mm..accum,as.factor(as.character(data.9$Codeclient)),data.9$GAvgDistanceWeighted,data.9$Concrete,data.9$AvgOfTire.age.when.received,data.9$Asphalt,data.9$LPT,data.9$Other.Surfaces,data.9$TO.run.ToDenAlt,data.9$Number.of.Flights.per.day.per.aircraft,as.factor(as.character(data.9$Part.Number)))

rftot<-combinerarbre(DF,ntree = 200,k=30,cut=50,type="unsupervised")

names=c("Age","TAT","TOtoD","TOP","Dist","Concrete","Asphalt","Fly/Day","LPT","Other.Sufaces","MTOW","Client","Skid","PN")
names<-names[c(14:1)]
varImpPlot(rftot$randomforest,pch=15,col='blue',labels=names,main="Variables Importance")



#---------------------------------------------------------------Tentative de modèle mars-----------------------------------------


data.10<-data.frame(data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft,data.2$LPT,data.2$AvgOfA.C.Maximum.Take.off.Weight.Kgs.)
cible=data.2$TAT.at.customer.estimate


sample<-sample(1:nrow(data.10),nrow(data.10)/3,replace=FALSE) 

data.10train<-data.10[-sample,]
cibletrain<-cible[-sample]

cibletest=cible[sample]
data.10test=data.10[sample,]



modelemars=mars(x = data.10train,y=cibletrain)

prmodelemars=predict(modelemars,data.10test)

plot(prmodelemars)
points(cibletest,col='red')
mean(100*abs(prmodelemars-cibletest)/cibletest)

plot(100*abs(prmodelemars-cibletest)/cibletest,ylim=c(0,200),xlim=c(0,5000))

ticks <- seq(0, 100, 10)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   

pie(round(100*table(discretize(floor(100*abs(prmodelemars-cibletest)/cibletest),categories=10,method="cluster"))/sum(table(discretize(floor(100*abs(prmodelemars-cibletest)/cibletest),categories=10,method="cluster"))),digits = 2))


View(cbind(prmodelemars,cibletest,100*abs(prmodelemars-cibletest)/cibletest))

erreur<-(prmodelemars-cibletest)^2
plot(erreur,ylim=c(0,100))



data.11<-data.frame(data.2$TAT.at.customer.estimate,data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft,data.2$LPT,data.2$AvgOfA.C.Maximum.Take.off.Weight.Kgs.)

sample<-sample(1:nrow(data.11),nrow(data.11)/3,replace=FALSE) 

data.11train<-data.11[-sample,]

data.11test=data.11[sample,]



modelelm=lm(data.11train$data.2.TAT.at.customer.estimate~.,data.11train)

prmodelelm=predict(modelelm,data.10test)

mean(100*abs(prmodelelm-cibletest)/cibletest)


modelelmforcv <- lm(data.11$data.2.TAT.at.customer.estimate~.,data.11) 
CV=CVlm(data.11, form.lm=formula(lm(data.11$data.2.TAT.at.customer.estimate~.,data.11))) 


cvFit(modelelmforcv, data = data.11, y = data.11$data.2.TAT.at.customer.estimate, cost = rtmspe,K = 5, R = 10)



modelemars=mars(x = data.10,y=cible)

CV=cvFit(modelemars,  data.10, y = cible, cost = rtmspe,K = 5, R = 10)      

#---------------------------------------------------------------Cross Validation--------------------------------------------------------------------


CV<-function(df,k,cible,type="lm")
  
{
 
result=NULL  
   
if(type=="lm") 
  
{  
 

  
  index=cvFolds(nrow(df),K=k,type="random")

  mse<-list()
  mse2<-list()
  mean.mse<-c()
  mean.mse2<-c()
  mean.var2<-c()
  occurence.30<-c()
  occurence.60<-c()
  occurence.90<-c()
  occurence.120<-c()
  w<-matrix(data=c(NA),ncol=k,nrow=max(summary(as.factor(index$which))))
  
  i=1
  
while(i<=k)
  
{
  
  
  sample<-index$which==i
  
  data.train<-df[-sample,]
  data.test<-df[sample,]
  
  
  modelelm <- modelelm <- lm(cible[-sample]~.,data.train[,-which(as.vector(unique(cible[-sample]==data.train)[1,])==TRUE)]) 
  prmodelelm=predict(modelelm,data.test)
  
  v<-100*abs(prmodelelm-cible[sample])/cible[sample] 
  
  
  cible2<-cible[sample]
  for(j in 1:length(prmodelelm))
  {
  w[j,i]<-prmodelelm[j]-cible2[j]
  }
  
  mse[[i]]<-(prmodelelm-cible[sample])^2
  mse2[[i]]<-v[which(v<150)]
  
  occurence.30[i]<-occurence(abs(w[!is.na(w[,i]),i]),limite1=0,limite2=30)
  occurence.60[i]<-occurence(abs(w[!is.na(w[,i]),i]),limite1=30,limite2=60)
  occurence.90[i]<-occurence(abs(w[!is.na(w[,i]),i]),limite1=60,limite2=90)
  occurence.120[i]<-occurence(abs(w[!is.na(w[,i]),i]),limite1=90,limite2=120)
  
  i<-i+1
  
}  
  
  for(j in 1:k){mean.mse[j]<-mean(mse[[j]])}
  for(j in 1:k){mean.mse2[j]<-mean(mse2[[j]])}
  for(j in 1:k){mean.var2[j]<-sd(mse2[[j]])}
  
result=list(sqrt(mean(mean.mse)),mean(mean.mse2),mean(mean.var2),mean(occurence.30),mean(occurence.60),mean(occurence.90),mean(occurence.120),rowMeans(w,na.rm=TRUE)) 
names(result)<-c("MSE","MRE","MSD","Dans le bon mois","1 à 2 mois","2 à 3 mois","3 à 4 mois","Residus")

}

  
  if(type=="mars") 
    
  {  
    
    
    index=cvFolds(nrow(df),K=k,type="random")
    i=1
    
    mse<-list()
    mse2<-list()
    mean.mse<-c()
    mean.mse2<-c()
    mean.var2<-c()
    occurence.30<-c()
    occurence.60<-c()
    occurence.90<-c()
    occurence.120<-c()
    w<-matrix(data=c(NA),ncol=k,nrow=max(summary(as.factor(index$which))))
    
    while(i<=k)
      
    {
      
      
      sample<-index$which==i
      
      data.train<-df[-sample,]
      data.test<-df[sample,]
      
      
      modelemars <- mars(x=data.train,y=cible[-sample]) 
      prmodelemars=predict(modelemars,data.test)

      v<-100*abs(prmodelemars-cible[sample])/cible[sample] 
      
      
      cible2<-cible[sample]
      for(j in 1:length(prmodelemars))
      {
        w[j,i]<-prmodelemars[j]-cible2[j]
      }
   
      
      mse[[i]]<-(prmodelemars-cible[sample])^2
      mse2[[i]]<-v[which(v<150)]
      
      occurence.30[i]<-occurence(abs(w[!is.na(w[,i]),i]),limite1=0,limite2=30)
      occurence.60[i]<-occurence(abs(w[!is.na(w[,i]),i]),limite1=30,limite2=60)
      occurence.90[i]<-occurence(abs(w[!is.na(w[,i]),i]),limite1=60,limite2=90)
      occurence.120[i]<-occurence(abs(w[!is.na(w[,i]),i]),limite1=90,limite2=120)

    i<-i+1

    }  
    
    for(j in 1:k){mean.mse[j]<-mean(mse[[j]])}
    for(j in 1:k){mean.mse2[j]<-mean(mse2[[j]])}
    for(j in 1:k){mean.var2[j]<-sd(mse2[[j]])}
    
    result=list(sqrt(mean(mean.mse)),mean(mean.mse2),mean(mean.var2),mean(occurence.30),mean(occurence.60),mean(occurence.90),mean(occurence.120),rowMeans(w,na.rm=TRUE))  
    names(result)<-c("MSE","MRE","MSD","Dans le bon mois","1 à 2 mois","2 à 3 mois","3 à 4 mois","Residus")
    
  }
  
  
  
  return(result)
  
}

#Avec LPT

data.10<-data.frame(data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft,data.2$LPT,data.2$AvgOfA.C.Maximum.Take.off.Weight.Kgs.)
cible=data.2$TAT.at.customer.estimate

CV2mars.tot=CV(df=data.10,cible=data.2$TAT.at.customer.estimate,k=7,type="mars")

data.11<-data.frame(data.2$TAT.at.customer.estimate,data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft,data.2$LPT,data.2$AvgOfA.C.Maximum.Take.off.Weight.Kgs.)
modelelm=lm(cible~.,data.11)

CV2lm.tot=CV(df=data.11,cible=data.11$data.2.TAT.at.customer.estimate,k=7,type="lm")

#Sans LPT

data.10<-data.frame(data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft,data.2$AvgOfA.C.Maximum.Take.off.Weight.Kgs.)
cible=data.2$TAT.at.customer.estimate

CV2mars.sans.lpt=CV(df=data.10,cible=data.2$TAT.at.customer.estimate,k=7,type="mars")

data.11<-data.frame(data.2$TAT.at.customer.estimate,data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft,data.2$AvgOfA.C.Maximum.Take.off.Weight.Kgs.)
modelelm=lm(cible~.,data.11)

CV2lm.sans.lpt=CV(df=data.11,cible=data.11$data.2.TAT.at.customer.estimate,k=7,type="lm")


#Sans MTOW

data.10<-data.frame(data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft)
cible=data.2$TAT.at.customer.estimate

CV2mars.sans.mtow=CV(df=data.10,cible=data.2$TAT.at.customer.estimate,k=7,type="mars")

data.11<-data.frame(data.2$TAT.at.customer.estimate,data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft)
modelelm=lm(cible~.,data.11)

CV2lm.sans.mtow=CV(df=data.11,cible=data.11$data.2.TAT.at.customer.estimate,k=7,type="lm")

#Sans AGE

data.10<-data.frame(data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft)
cible=data.2$TAT.at.customer.estimate

CV2mars.sans.age=CV(df=data.10,cible=data.2$TAT.at.customer.estimate,k=7,type="mars")

data.11<-data.frame(data.2$TAT.at.customer.estimate,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Concrete,data.2$Number.of.Flights.per.day.per.aircraft)
modelelm=lm(cible~.,data.11)

CV2lm.sans.age=CV(df=data.11,cible=data.11$data.2.TAT.at.customer.estimate,k=7,type="lm")


#Sans Piste

data.10<-data.frame(data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Number.of.Flights.per.day.per.aircraft)
cible=data.2$TAT.at.customer.estimate

CV2mars.sans.piste=CV(df=data.10,cible=data.2$TAT.at.customer.estimate,k=7,type="mars")

data.11<-data.frame(data.2$TAT.at.customer.estimate,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Number.of.Flights.per.day.per.aircraft)
modelelm=lm(cible~.,data.11)

CV2lm.sans.piste=CV(df=data.11,cible=data.11$data.2.TAT.at.customer.estimate,k=7,type="lm")

#Sans piste mais pareil que modèle MARS3

data.10<-data.frame(data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Number.of.Flights.per.day.per.aircraft)
cible=data.2$TAT.at.customer.estimate

CV2mars=CV(df=data.10,cible=data.2$TAT.at.customer.estimate,k=7,type="mars")




compa=data.frame(as.matrix(CV2lm.sans.piste),as.matrix(CV2mars.sans.piste),
                 as.matrix(CV2lm.sans.age),as.matrix(CV2mars.sans.age),
                 as.matrix(CV2lm.sans.mtow),as.matrix(CV2mars.sans.mtow),
                 as.matrix(CV2lm.sans.lpt),as.matrix(CV2mars.sans.lpt),
                 as.matrix(CV2lm.tot),as.matrix(CV2mars.tot))
# compa<-compa[-(6:7),]
compa<-t(as.matrix(compa))
compa2=matrix(data=NA,ncol=ncol(compa),nrow=nrow(compa))
              
for(i in 1:ncol(compa))
{
  for(j in 1:nrow(compa))
  {
    compa2[j,i]<-unlist(compa[j,i])
  }
} 

colnames(compa2)<-colnames(compa)
rownames(compa2)<-rownames(compa)
compa2<-compa2[,-c(1,3,6,7)]

#Tout modèle

couleurs==c('yellow4','blue4',
      'yellow3','blue3',
      'yellow2','blue2',
      'yellow1','blue1',
      'yellow','blue')
couleurs2=as.vector(colors())[c(7,12,17,24,30,36,47,53,68,509)]
barplot(compa2,names.arg = toupper(colnames(compa2)),col=couleurs2, beside = TRUE,main="Comparaison des modèles avec différents paramètres",ylab="Taux (%)")

ticks <- seq(0, 50, 4)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   

legend("top",cex=0.8,legend=c("LM:TOD/TOP/Dist/Fly.day","MARS:TOD/TOP/Dist/Fly.day",
                                   "LM:TOD/TOP/Dist/Fly.day/Piste","MARS:TOD/TOP/Dist/Fly.day/Piste"
                                   ,"LM:TOD/TOP/Dist/Fly.day/Piste/Age","MARS:TOD/TOP/Dist/Fly.day/Piste/Age"
                                   ,"LM:TOD/TOP/Dist/Fly.day/Piste/Age/MTOW","MARS:TOD/TOP/Dist/Fly.day/Piste/Age/MTOW"
                                   ,"LM:TOD/TOP/Dist/Fly.day/Piste/Age/LPT","MARS:TOD/TOP/Dist/Fly.day/Piste/Age/LPT")
                                   ,col=couleurs2
                                   ,pch = 15)


#Modèle MARS

compa=data.frame(as.matrix(CV2lm.sans.piste),as.matrix(CV2mars.sans.piste),
                 as.matrix(CV2lm.sans.age),as.matrix(CV2mars.sans.age),
                 as.matrix(CV2lm.sans.mtow),as.matrix(CV2mars.sans.mtow),
                 as.matrix(CV2lm.sans.lpt),as.matrix(CV2mars.sans.lpt),
                 as.matrix(CV2lm.tot),as.matrix(CV2mars.tot))
# compa<-compa[-(6:7),]
compa<-t(as.matrix(compa))
compa2=matrix(data=NA,ncol=ncol(compa),nrow=nrow(compa))

for(i in 1:ncol(compa))
{
  for(j in 1:nrow(compa))
  {
    compa2[j,i]<-unlist(compa[j,i])
  }
} 

colnames(compa2)<-colnames(compa)
rownames(compa2)<-rownames(compa)
compa2<-compa2[-c(1,3,5,7,9),-c(1,3)]



couleurs2=c(1:5)
barplot(compa2,names.arg = toupper(colnames(compa2)),col=couleurs2, beside = TRUE,main="Comparaison des modèles avec différents paramètres",ylab="Taux (%)")

ticks <- seq(0, 40, 5)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   

legend("top",cex=0.9,legend=c("MARS:TOD/TOP/Dist/Fly.day",
                              "MARS:TOD/TOP/Dist/Fly.day/Piste",
                              "MARS:TOD/TOP/Dist/Fly.day/Piste/Age",
                              "MARS:TOD/TOP/Dist/Fly.day/Piste/Age/MTOW",
                              "MARS:TOD/TOP/Dist/Fly.day/Piste/Age/MTOW/LPT")
       ,col=couleurs2
       ,pch = 15)





#--------------------------------------------------Test avec modèle MARS TOD/TOP/Dist/Fly.Day/Age---------------------------------------------

#--------------------------------------------------Prédiction des retours pour un Client pool neuf--------------------------------------------

#AVAFR

data.10<-data.frame(data.2$AvgOfTire.age.when.received,data.2$TO.run.ToDenAlt,data.2$TO.run.ToPressAlt,data.2$GAvgDistanceWeighted,data.2$Number.of.Flights.per.day.per.aircraft)
cible=data.2$TAT.at.customer.estimate



v_AVAFR<-subset(Ventes.client,Client=='AVAFR')
v_AVAFR<-remplacerparNA(v_AVAFR[,c(1,3)],"Ventes")[11:50,]

AVAFR<-liste.client$AVAFR


modele=mars(x=data.10,y=cible)


datacut<-data.frame(data$Date.Mount,data$Part.Number,data$Codeclient,data$Plant.manufacturing,data$Tire.type,data$New.or.Retread)
colnames(datacut)<-c("Date","PN","Client","Plant","Tiretype","NorR")
datacut$Compteur<-rep_len(1,length.out=length(data$Part.Number))


Ventes.client.neuf<-aggregate(datacut$Compteur,by=list(datacut$Date,datacut$Client,datacut$NorR),FUN=sum)
colnames(Ventes.client.neuf)<-c("Date","Client","NorR","Ventes.client")
v_AVAFR<-subset(Ventes.client.neuf,Client=='AVAFR' & NorR=='N')
v_AVAFR<-remplacerparNA(v_AVAFR[,c(1,4)],"Ventes")[11:50,]
v_AVAFR



datacut2<-data.frame(data$Date.Reception.at.plant,data$Part.Number,data$Codeclient,data$Plant.receiving.tire,data$New.or.Retread)
colnames(datacut2)<-c("Date","PN","Client","Plant","NorR")
datacut2$Compteur<-rep_len(1,length.out=length(data$Part.Number))
Retours.client.neuf<-aggregate(datacut2$Compteur,by=list(datacut2$Date,datacut2$Client,datacut2$NorR),FUN=sum)
colnames(Retours.client.neuf)<-c("Date","Client","NorR","Retours.client")

r_AVAFR<-subset(Retours.client.neuf,Client=='AVAFR' & NorR=='N')
r_AVAFR<-remplacerparNA(r_AVAFR[,c(1,4)],"Retours")[11:50,]
r_AVAFR

dg=data.frame(r_AVAFR,v_AVAFR)[(21-11):(50-10),c(1,2,4)]

dot1=subset(data,Codeclient=='AVAFR' & New.or.Retread=='N')


TOD=mean(dot1$TO.run.ToDenAlt)
Age=mean(dot1$AvgOfTire.age.when.received,na.rm=TRUE)
TOP=mean(dot1$TO.run.ToPressAlt)
Dist=mean(dot1$GAvgDistanceWeighted)
Fly=mean(dot1$Number.of.Flights.per.day.per.aircraft,na.rm=TRUE)


newdata=data.frame(Age,TOD,TOP,Dist,Fly)
colnames(newdata)<-colnames(data.10)

newdata2=rbind(data.10,newdata)
modele=mars(x=data.10,y=cible)
prmodele=predict(modele,newdata)
mean(dot1$TAT.at.customer.estimate)

dg$retours.prevus<-
  
dg.2=data.frame(subset(dg,Date==as.Date(paste(substr(dg$Date[1]+prmodele,1,8),'01',sep="")))$Ventes)

dg.2=data.frame(matrix(data=c(NA),nrow=length(paste(substr(dg$Date+prmodele,1,8),'01',sep="")),ncol=2))
dg.2[,1]<-as.Date(paste(substr(dg$Date+prmodele,1,8),'01',sep=""))
dg.2[,2]<-subset(dg,Date==(paste(substr(dg$Date+prmodele,1,8),'01',sep="")))$Ventes



v<-c(NA)
length(v)=length(dg$Date)




dot1=subset(data,Codeclient=='AVAFR' & New.or.Retread=='N' & Part.Number=="M01103-02")


boxplot(dot1$TO.run.ToDenAlt)
boxplot(dot1$AvgOfTire.age.when.received,na.rm=TRUE)
boxplot(dot1$TO.run.ToPressAlt)
boxplot(dot1$GAvgDistanceWeighted)
boxplot(dot1$Number.of.Flights.per.day.per.aircraft,na.rm=TRUE)
boxplot(dot1$TAT.at.customer.estimate)




#Relation age niveau de rechappage

boxplot(data$AvgOfTire.age.when.received~data$R_level.1,data=data,xlab="Niveau de rechappage",ylab = "Age du pneu",main="Age en fonction du Rlevel",col="cornflowerblue",ylim=c(0,5))
boxplot(data$Skid.available..mm..accum~data$R_level.1,data=data,xlab="Niveau de rechappage",ylab = "Skid disp",main="Skid disp en fonction du Rlevel",col="cornflowerblue")
boxplot(data$TAT.at.customer.estimate~data$R_level.1,data=data,xlab="Niveau de rechappage",ylab = "TAT",main="TAT en fonction du Rlevel",col="cornflowerblue")

plot(data$TO.run.ToDenAlt~data$TO.run.ToPressAlt)
cor(data$TO.run.ToDenAlt,data$TO.run.ToPressAltn,na.rm=TRUE)


summary(as.factor(data.2$R_level.1))










#-------------------------------------------Refaire le travil de tri des données en évitant les na.omit------------------------------------------------------



data.2=data

data.2<-data.2[,!colnames(data.2)%in%c("SN")]
data.2<-data.2[,!colnames(data.2)%in%c("Dimension.Number")]
data.2<-data.2[,!colnames(data.2)%in%c("Size")]
data.2<-data.2[,!colnames(data.2)%in%c("Scrap.Date")]
data.2<-data.2[,!colnames(data.2)%in%c("Premature.Scrap")]
data.2<-data.2[,!colnames(data.2)%in%c("Removal.code.Michelin.1")]
data.2<-data.2[,!colnames(data.2)%in%c("Reform.Code")]

#Regression logistique dessus sûrement intéressant

data.2<-data.2[,!colnames(data.2)%in%c("Flex.breaks")]
data.2<-data.2[,!colnames(data.2)%in%c("Leaks..bead.toe..liner."  )]
data.2<-data.2[,!colnames(data.2)%in%c("Others")]
data.2<-data.2[,!colnames(data.2)%in%c("Crack..cuts.bead.area")]
data.2<-data.2[,!colnames(data.2)%in%c("Ozone.cracks.sidewall")]
data.2<-data.2[,!colnames(data.2)%in%c("Shearo" )]
data.2<-data.2[,!colnames(data.2)%in%c("Sidewall.cuts.cracks" )]
data.2<-data.2[,!colnames(data.2)%in%c("Tread.cut")]
data.2<-data.2[,!colnames(data.2)%in%c("Worn.beyond.limits")]
data.2<-data.2[,!colnames(data.2)%in%c("Total.Scrap.A")]
data.2<-data.2[,!colnames(data.2)%in%c("Total.Scrap.B" )]
data.2<-data.2[,!colnames(data.2)%in%c("Total.Scrap.C")]
data.2<-data.2[,!colnames(data.2)%in%c("Total.Scrap.H" )]
data.2<-data.2[,!colnames(data.2)%in%c("Clef.pn.plant" )]
data.2<-data.2[,!colnames(data.2)%in%c("Clef.pn.client" )]
data.2<-data.2[,!colnames(data.2)%in%c("Clef.client" )]
data.2<-data.2[,!colnames(data.2)%in%c("Clef.pn" )]
data.2<-data.2[,!colnames(data.2)%in%c("Year.Mount" )]
data.2<-data.2[,!colnames(data.2)%in%c("Year.Dismount" )]
data.2<-data.2[,!colnames(data.2)%in%c("Month.Mount" )]
data.2<-data.2[,!colnames(data.2)%in%c("Month.Dismount" )]
data.2<-data.2[,!colnames(data.2)%in%c("Year.Reception.at.plant" )]
data.2<-data.2[,!colnames(data.2)%in%c("Month.Reception.at.plant" )]
data.2<-data.2[,!colnames(data.2)%in%c("Tire.Family" )]
data.2<-data.2[,!colnames(data.2)%in%c("Retours.client" )]
data.2<-data.2[,!colnames(data.2)%in%c("Retours.pn.plant"  )]
data.2<-data.2[,!colnames(data.2)%in%c("Retours.pn.client"  )]


#

data.2<-data.2[,!colnames(data.2)%in%c("OD..mm."  )]

#Ne garder que les données importantes renseignées



data.2<-data.2[!is.na(data.2[,colnames(data.2)%in%c("Codeclient")]),]
data.2<-data.2[!is.na(data.2[,colnames(data.2)%in%c("TAT.at.customer.estimate")]),]
data.2<-data.2[!is.na(data.2[,colnames(data.2)%in%c("LPT")]),]

data.2<-data.2[,!colnames(data.2)%in%c("GAvgFromAptTempWeighted"  )]
data.2<-data.2[,!colnames(data.2)%in%c("GAvgFromAptMinTempWeighted"   )]
data.2<-data.2[,!colnames(data.2)%in%c("GAvgFromAptMaxTempWeighted"   )]
data.2<-data.2[,!colnames(data.2)%in%c("GAvgFromAptDenWeighted"    )]
data.2<-data.2[,!colnames(data.2)%in%c( "GAvgFromPressAltWeighted"    )]
data.2<-data.2[,!colnames(data.2)%in%c( "GAvgFromDenAltWeighted"  )]



summary(is.na(data.2))

data.2<-data.2[,!colnames(data.2)%in%c( "MinRunwayLength.m."  )]
data.2<-data.2[,!colnames(data.2)%in%c( "MaxRunwayLength.m."  )]
data.2<-data.2[,!colnames(data.2)%in%c( "TO.run.ToDenAlt"  )]
data.2<-data.2[,!colnames(data.2)%in%c( "TO.run.FromDenAlt"  )]
data.2<-data.2[,!colnames(data.2)%in%c( "Plant.retreading.tire"  )]
data.2<-data.2[,!colnames(data.2)%in%c( "Tread.Depth.accum"  )]

data.2<-na.omit(data.2)


for(i in 1:ncol(data.2))
  
{
  
  if(is.numeric(data.2[,i]))
  
  {
  boxplot(data.2[,i],col='pink',boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[i])
  }
  
  
}

data.2<-data.2[(data.2[,colnames(data.2)%in%c("Tread.Depth.accum")]<27),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("TAT.at.customer.estimate" )]<1500),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("Transit.time.before.retreading.estimate" )]<750),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("LPT"    )]<1000),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("GAvgDistanceWeighted" )]<15000),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c("MaxRunwayLength.m."  )]<4000),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c( "Skid.available..mm..accum")]<70),]
data.2<-data.2[(data.2[,colnames(data.2)%in%c( "AvgOfTire.age.when.received"  )]>0),]



data.2<-data.2[,!colnames(data.2)%in%c( "Gravel.Sand.Macadam"  )]
data.2<-data.2[,!colnames(data.2)%in%c( "Leaser"  )]
data.2<-data.2[,!colnames(data.2)%in%c( "GAvgToAptDenWeighted"   )]
data.2<-data.2[,!colnames(data.2)%in%c(  "GAvgToDenAltWeighted"  )]
data.2<-data.2[,!colnames(data.2)%in%c( "Tread.Depth.accum"  )]
data.2<-data.2[,!colnames(data.2)%in%c( "Tread.Depth.accum"  )]
data.2<-data.2[,!colnames(data.2)%in%c( "Tread.Depth.accum"  )]



data.2<-read.csv("Data_numerique.csv")

data.8<-data.2


data.8=subset(data.8,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.8),ncol=1))

for (i in 1:ncol(data.8))
  
{
  
  longueur[i,1]<-length(unique(data.8[,i]))
  
  rownames(longueur)<-colnames(data.8)
  
  
}




data.8<-data.8[,!colnames(data.8)%in%c("Ply.Rating")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Dismount")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Mount")]
data.8<-data.8[,!colnames(data.8)%in%c("Tire.type")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Reception.at.plant")]
data.8<-data.8[,!colnames(data.8)%in%c("Operator.Country.DUS")]
data.8<-data.8[,!colnames(data.8)%in%c("Transit.time.before.retreading.estimate")]
data.8<-data.8[,!colnames(data.8)%in%c("TO.run.ToPressAlt" )]
data.8<-data.8[,!colnames(data.8)%in%c("TO.run.FromDenAlt"  )]
data.8<-data.8[,!colnames(data.8)%in%c("Cargo"  )]
data.8<-data.8[,!colnames(data.8)%in%c("Holiday.Charter" )]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.8),ncol=1))

for (i in 1:ncol(data.8))
  
{
  
  longueur[i,1]<-length(unique(data.8[,i]))
  
  rownames(longueur)<-colnames(data.8)
  
  
}



discret_var_arbitraire<-function(Y,p,q)
  
{  
  
  y<-c()
  
  
  for (i in 1:length(Y))
    
  {
    
    if(Y[i]<=p){y[i]<-"<=200"}
    if((Y[i]>p)&(Y[i]<=q)){y[i]<-"200<..<=500"}
    if(Y[i]>q){y[i]<-">500"}
    
  }
  
  return(y)
  
}


boxplot(data.2$TAT.at.customer.estimate,col='pink',xaxp=c(0,max(data.2$TAT.at.customer.estimate),12),boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[15])







data.8$TAT.at.customer.estimate<-discretize(data.8$TAT.at.customer.estimate,method="frequency",categories=10)




data.8$AvgOfTire.age.when.received<-discretize(data.8$AvgOfTire.age.when.received,method="frequency",categories=3)
data.8$LPT<-discretize(data.8$LPT,method="frequency",categories=4)
data.8$Number.of.Flights.per.day.per.aircraft<-discretize(data.8$Number.of.Flights.per.day.per.aircraft,method="frequency",categories=4)

data.8$LPT<-discretize(data.8$LPT,method="frequency",categories=4)

data.8$AvgOfA.C.Maximum.Take.off.Weight.Kgs.<-discretize(data.8$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,method="frequency",categories=3)
data.8$GAvgToAptTempWeighted<-discretize(data.8$GAvgToAptTempWeighted,method="frequency",categories=4)
data.8$GAvgToAptMinTempWeighted<-discretize(data.8$GAvgToAptMinTempWeighted,method="frequency",categories=4)
data.8$GAvgToAptMaxTempWeighted<-discretize(data.8$GAvgToAptMaxTempWeighted,method="frequency",categories=4)
data.8$GAvgToPressAltWeighted<-discretize(data.8$GAvgToPressAltWeighted,method="frequency",categories=4)
data.8$TO.run.FromPressAlt<-discretize(data.8$TO.run.FromPressAlt,method="frequency",categories=4)


data.8$GAvgDistanceWeighted<-discretize(data.8$GAvgDistanceWeighted,method="frequency",categories=4)
data.8$AvgRunwayLength.m.<-discretize(data.8$AvgRunwayLength.m.,method="frequency",categories=4)


data.8$Asphalt<-discretize(data.8$Asphalt,method="frequency",categories=2)
data.8$Concrete<-discretize(data.8$Concrete,method="frequency",categories=2)
data.8$Other.Surfaces<-discretize(data.8$Other.Surfaces,method="frequency",categories=2)

data.8$Section.Width..mm.<-discretize(data.8$Section.Width..mm.,method="frequency",categories=3)
data.8$Tire.mass..kg.<-discretize(data.8$Tire.mass..kg.,method="frequency",categories=3)

data.8$Low.Cost<-discretize(data.8$Low.Cost,method="frequency",categories=2)

data.8$Skid.available..mm..accum<-discretize(data.8$Skid.available..mm..accum,method="frequency",categories=5)


data.8=data.8[,-c(32,34,35,36)]


pie(summary(data.8$LPT),main="Répartition des LPT")
pie(summary(data.8$TAT.at.customer.estimate),main="Répartition des TAT")



TAT=data.8$TAT.at.customer.estimate
poids <- information.gain(TAT~., data.8)
poids<-poids[-13,]
print(poids)
names(poids)=c("Rmax",
               "Client",
                "PN",
                "T.Tech",
                "SR",
                "NorR",
                "P.Manuf",
                "P.Rece",
                "Pos",
                "Framer",
                "Model",
                "Age",
                "LPT"
               ,"Fly",
                "MTOW",
                "T",
                "Tmin",
                "Tmax",
                "P",
                "TOP",
                "Dist"
               ,"Runway",
                "Bitume",
                "Concrete",
                "AutresSur",
                "Section",
                "SkidD",
                "Mass",
                "R",
                "LowC",
                "Zone")

barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


DF<-data.frame(data.8$TAT.at.customer.estimate,as.factor(as.character(data.8$Codeclient)),as.factor(as.character(data.8$Part.Number)),data.8$Zone,data.8$New.or.Retread)
rf <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,as.factor(as.character(data.8$Codeclient)),data.8$Skid.available..mm..accum,data.8$R_level.1,data.8$AvgOfTire.age.when.received,data.8$New.or.Retread,data.8$Model,as.factor(as.character(data.8$Part.Number)),data.8$TO.run.FromPressAlt,data.8$Zone,data.8$Plant.receiving.tire)
rf1 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf1$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,data.8$Skid.available..mm..accum,data.8$R_level.1,data.8$AvgOfTire.age.when.received,data.8$New.or.Retread,data.8$Model,as.factor(as.character(data.8$Part.Number)),data.8$TO.run.FromPressAlt,data.8$Zone,data.8$Plant.receiving.tire)
rf3 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf3$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,data.8$R_level.1,data.8$Model,as.factor(as.character(data.8$Part.Number)),data.8$TO.run.FromPressAlt,data.8$Zone)
rf4 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf4$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,data.8$R_level.1,data.8$Model,as.factor(as.character(data.8$Part.Number)),data.8$Zone)
rf5 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf5$err.rate)


compa=as.matrix(cbind(colMeans(rf$err.rate),colMeans(rf1$err.rate),colMeans(rf3$err.rate),colMeans(rf4$err.rate),colMeans(rf5$err.rate)))
compa=floor(100*t(compa))

barplot(compa,axes=FALSE,names.arg = toupper(colnames(compa)),yaxp=c(0,80,10), beside = TRUE,xlab="TAT",main="Comparaison des modèles avec différents paramètres",col=c('red','blue','purple','yellow','pink'),ylab="Taux d'erreur")

ticks <- seq(0, 100, 5)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   


legend("topleft",cex=0.8,legend=c("Client/PN/Zone/NorR","Client/SkidD/R/Age/NorR/Model/PN/TOP/Zone/P.rece","SkidD/Age/R/NorR/Model/PN/TOP/Zone","R/Model/PN/TOP/Zone","R/Model/PN/Zone"),col=c('red','blue','purple','yellow','pink'),pch = 15)




data.8<-data.2


data.8=subset(data.8,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.8),ncol=1))

for (i in 1:ncol(data.8))
  
{
  
  longueur[i,1]<-length(unique(data.8[,i]))
  
  rownames(longueur)<-colnames(data.8)
  
  
}




data.8<-data.8[,!colnames(data.8)%in%c("Ply.Rating")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Dismount")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Mount")]
data.8<-data.8[,!colnames(data.8)%in%c("Tire.type")]
data.8<-data.8[,!colnames(data.8)%in%c("Date.Reception.at.plant")]
data.8<-data.8[,!colnames(data.8)%in%c("Operator.Country.DUS")]
data.8<-data.8[,!colnames(data.8)%in%c("Transit.time.before.retreading.estimate")]
data.8<-data.8[,!colnames(data.8)%in%c("TO.run.ToPressAlt" )]
data.8<-data.8[,!colnames(data.8)%in%c("TO.run.FromDenAlt"  )]
data.8<-data.8[,!colnames(data.8)%in%c("Cargo"  )]
data.8<-data.8[,!colnames(data.8)%in%c("Holiday.Charter" )]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.8),ncol=1))

for (i in 1:ncol(data.8))
  
{
  
  longueur[i,1]<-length(unique(data.8[,i]))
  
  rownames(longueur)<-colnames(data.8)
  
  
}



discret_var_arbitraire<-function(Y,p,q)
  
{  
  
  y<-c()
  
  
  for (i in 1:length(Y))
    
  {
    
    if(Y[i]<=p){y[i]<-"<=200"}
    if((Y[i]>p)&(Y[i]<=q)){y[i]<-"200<..<=500"}
    if(Y[i]>q){y[i]<-">500"}
    
  }
  
  return(y)
  
}


boxplot(data.2$TAT.at.customer.estimate,col='pink',xaxp=c(0,max(data.2$TAT.at.customer.estimate),12),boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[15])







data.8$TAT.at.customer.estimate<-discretize(data.8$TAT.at.customer.estimate,method="frequency",categories=5)




data.8$AvgOfTire.age.when.received<-discretize(data.8$AvgOfTire.age.when.received,method="frequency",categories=3)
data.8$LPT<-discretize(data.8$LPT,method="frequency",categories=4)
data.8$Number.of.Flights.per.day.per.aircraft<-discretize(data.8$Number.of.Flights.per.day.per.aircraft,method="frequency",categories=4)

data.8$LPT<-discretize(data.8$LPT,method="frequency",categories=4)

data.8$AvgOfA.C.Maximum.Take.off.Weight.Kgs.<-discretize(data.8$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,method="frequency",categories=3)
data.8$GAvgToAptTempWeighted<-discretize(data.8$GAvgToAptTempWeighted,method="frequency",categories=4)
data.8$GAvgToAptMinTempWeighted<-discretize(data.8$GAvgToAptMinTempWeighted,method="frequency",categories=4)
data.8$GAvgToAptMaxTempWeighted<-discretize(data.8$GAvgToAptMaxTempWeighted,method="frequency",categories=4)
data.8$GAvgToPressAltWeighted<-discretize(data.8$GAvgToPressAltWeighted,method="frequency",categories=4)
data.8$TO.run.FromPressAlt<-discretize(data.8$TO.run.FromPressAlt,method="frequency",categories=4)


data.8$GAvgDistanceWeighted<-discretize(data.8$GAvgDistanceWeighted,method="frequency",categories=4)
data.8$AvgRunwayLength.m.<-discretize(data.8$AvgRunwayLength.m.,method="frequency",categories=4)


data.8$Asphalt<-discretize(data.8$Asphalt,method="frequency",categories=2)
data.8$Concrete<-discretize(data.8$Concrete,method="frequency",categories=2)
data.8$Other.Surfaces<-discretize(data.8$Other.Surfaces,method="frequency",categories=2)

data.8$Section.Width..mm.<-discretize(data.8$Section.Width..mm.,method="frequency",categories=3)
data.8$Tire.mass..kg.<-discretize(data.8$Tire.mass..kg.,method="frequency",categories=3)

data.8$Low.Cost<-discretize(data.8$Low.Cost,method="frequency",categories=2)

data.8$Skid.available..mm..accum<-discretize(data.8$Skid.available..mm..accum,method="frequency",categories=5)


data.8=data.8[,-c(32,34,35,36)]


pie(summary(data.8$LPT),main="Répartition des LPT")
pie(summary(data.8$TAT.at.customer.estimate),main="Répartition des TAT")



TAT=data.8$TAT.at.customer.estimate
poids <- information.gain(TAT~., data.8)
poids<-poids[-13,]
print(poids)
names(poids)=c("Rmax",
               "Client",
               "PN",
               "T.Tech",
               "SR",
               "NorR",
               "P.Manuf",
               "P.Rece",
               "Pos",
               "Framer",
               "Model",
               "Age",
               "LPT"
               ,"Fly",
               "MTOW",
               "T",
               "Tmin",
               "Tmax",
               "P",
               "TOP",
               "Dist"
               ,"Runway",
               "Bitume",
               "Concrete",
               "AutresSur",
               "Section",
               "SkidD",
               "Mass",
               "R",
               "LowC",
               "Zone")

barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))


DF<-data.frame(data.8$TAT.at.customer.estimate,as.factor(as.character(data.8$Codeclient)),as.factor(as.character(data.8$Part.Number)),data.8$Zone,data.8$New.or.Retread)
rf <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,as.factor(as.character(data.8$Codeclient)),data.8$Skid.available..mm..accum,data.8$R_level.1,data.8$AvgOfTire.age.when.received,data.8$New.or.Retread,data.8$Model,as.factor(as.character(data.8$Part.Number)),data.8$TO.run.FromPressAlt,data.8$Zone,data.8$Plant.receiving.tire)
rf1 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf1$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,data.8$Skid.available..mm..accum,data.8$R_level.1,data.8$AvgOfTire.age.when.received,data.8$New.or.Retread,data.8$Model,as.factor(as.character(data.8$Part.Number)),data.8$TO.run.FromPressAlt,data.8$Zone,data.8$Plant.receiving.tire)
rf3 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf3$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,data.8$R_level.1,data.8$Model,as.factor(as.character(data.8$Part.Number)),data.8$TO.run.FromPressAlt,data.8$Zone)
rf4 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf4$err.rate)

DF<-data.frame(data.8$TAT.at.customer.estimate,data.8$R_level.1,data.8$Model,as.factor(as.character(data.8$Part.Number)),data.8$Zone)
rf5 <- randomForest(DF$data.8.TAT.at.customer.estimate ~ ., DF)
colMeans(rf5$err.rate)


compa=as.matrix(cbind(colMeans(rf$err.rate),colMeans(rf1$err.rate),colMeans(rf3$err.rate),colMeans(rf4$err.rate),colMeans(rf5$err.rate)))
compa=floor(100*t(compa))

barplot(compa,axes=FALSE,names.arg = toupper(colnames(compa)),yaxp=c(0,80,10), beside = TRUE,xlab="TAT",main="Comparaison des modèles avec différents paramètres",col=c('red','blue','purple','yellow','pink'),ylab="Taux d'erreur")

ticks <- seq(0, 100, 5)       # sequence for ticks and labels
axis(2, at = ticks,         # y-Axis
     labels = ticks)
box()   


legend("topleft",cex=0.8,legend=c("Client/PN/Zone/NorR","Client/SkidD/R/Age/NorR/Model/PN/TOP/Zone/P.rece","SkidD/Age/R/NorR/Model/PN/TOP/Zone","R/Model/PN/TOP/Zone","R/Model/PN/Zone"),col=c('red','blue','purple','yellow','pink'),pch = 15)

#--------------------------------------------------------Occurence-------------------------------------------------------------

occurence<-function(v,limite1,limite2)
  
{
  
  compteur<-0
  N<-length(v)
  limite1vec=limite2vec=c()
  
  for(i in 1:length(v))
    
  {
    limite2vec[i]<-limite2
    limite1vec[i]<-limite1
    
  }
  
  for (i in 1:N)
  {
    
    if( ((v[i]<=limite2vec[i])==TRUE) & ((v[i]>=limite1vec[i])==TRUE) ) 
      
    {
      
      compteur<-compteur+1
      
    }
    
    
  }
  
  
  
  return (100*(compteur/N))
  
}  



#----------------------------------------------------Numérique-------------------------------------------------



data.9<-data.2
data.9=subset(data.9,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.9),ncol=1))

for (i in 1:ncol(data.9))
  
{
  
  longueur[i,1]<-length(unique(data.9[,i]))
  
  rownames(longueur)<-colnames(data.9)
  
  
}



data.9<-data.9[,!colnames(data.9)%in%c("X")]
data.9<-data.9[,!colnames(data.9)%in%c("Ply.Rating")]
data.9<-data.9[,!colnames(data.9)%in%c("Date.Dismount")]
data.9<-data.9[,!colnames(data.9)%in%c("Date.Mount")]
data.9<-data.9[,!colnames(data.9)%in%c("Tire.type")]
data.9<-data.9[,!colnames(data.9)%in%c("Date.Reception.at.plant")]
data.9<-data.9[,!colnames(data.9)%in%c("Operator.Country.DUS")]
data.9<-data.9[,!colnames(data.9)%in%c("Transit.time.before.retreading.estimate")]
data.9<-data.9[,!colnames(data.9)%in%c("TO.run.ToPressAlt" )]
data.9<-data.9[,!colnames(data.9)%in%c("TO.run.FromDenAlt"  )]
data.9<-data.9[,!colnames(data.9)%in%c("Cargo"  )]
data.9<-data.9[,!colnames(data.9)%in%c("Holiday.Charter" )]
data.9$Other.Surfaces<-replace(floor(data.9$Other.Surfaces),which(floor(data.9$Other.Surfaces)==-1),0)
data.9$Concrete<-floor(data.9$Concrete)
data.9$Asphalt<-floor(data.9$Asphalt)


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.9),ncol=1))

for (i in 1:ncol(data.9))
  
{
  
  longueur[i,1]<-length(unique(data.9[,i]))
  
  rownames(longueur)<-colnames(data.9)
  
  
}


data.9=data.9[,-c(32,34,35,36)]

TAT=data.9$TAT.at.customer.estimate
poids <- information.gain(TAT~., data.9)
poids<-poids[-13,]
print(poids)
names(poids)=c("Rmax",
               "Client",
               "PN",
               "T.Tech",
               "SR",
               "NorR",
               "P.Manuf",
               "P.Rece",
               "Pos",
               "Framer",
               "Model",
               "Age",
               "LPT",
               "Fly",
               "MTOW",
               "T",
               "Tmin",
               "Tmax",
               "P",
               "TOP",
               "Dist"
               ,"Runway",
               "Bitume",
               "Concrete",
               "AutresSur",
               "Section",
               "SkidD",
               "Mass",
               "R",
               "LowC",
               "Zone")


barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))



#Avec LPT

data.10<-data.frame(data.9$GAvgToPressAltWeighted,data.9$TO.run.FromPressAlt,data.9$Asphalt,data.9$AvgRunwayLength.m.,data.9$Number.of.Flights.per.day.per.aircraft,data.9$Skid.available..mm..accum,data.9$GAvgDistanceWeighted,data.9$Concrete,data.9$Section.Width..mm.,data.9$LPT)
cible=data.9$TAT.at.customer.estimate

CV2mars.1=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")


#Sans LPT

data.10<-data.frame(data.9$GAvgToPressAltWeighted,data.9$TO.run.FromPressAlt,data.9$Asphalt,data.9$AvgRunwayLength.m.,data.9$Number.of.Flights.per.day.per.aircraft,data.9$Skid.available..mm..accum,data.9$GAvgDistanceWeighted,data.9$Concrete,data.9$Section.Width..mm.)
cible=data.9$TAT.at.customer.estimate

CV2mars.2=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")


#Sans MTOW

data.10<-data.frame(data.9$GAvgToPressAltWeighted,data.9$TO.run.FromPressAlt,data.9$Asphalt,data.9$AvgRunwayLength.m.,data.9$Number.of.Flights.per.day.per.aircraft,data.9$Skid.available..mm..accum,data.9$GAvgDistanceWeighted,data.9$Concrete)
cible=data.9$TAT.at.customer.estimate

CV2mars.3=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")

#Sans AGE

data.10<-data.frame(data.9$GAvgToPressAltWeighted,data.9$TO.run.FromPressAlt,data.9$Asphalt,data.9$AvgRunwayLength.m.,data.9$Number.of.Flights.per.day.per.aircraft,data.9$Skid.available..mm..accum,data.9$GAvgDistanceWeighted)
cible=data.9$TAT.at.customer.estimate

CV2mars.4=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")


#Sans Piste

data.10<-data.frame(data.9$GAvgToPressAltWeighted,data.9$TO.run.FromPressAlt,data.9$Asphalt,data.9$AvgRunwayLength.m.,data.9$Number.of.Flights.per.day.per.aircraft,data.9$Skid.available..mm..accum)
cible=data.9$TAT.at.customer.estimate

CV2mars.5=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")

#Sans piste mais pareil que modèle MARS3

data.10<-data.frame(data.9$GAvgToPressAltWeighted,data.9$TO.run.FromPressAlt,data.9$Asphalt,data.9$AvgRunwayLength.m.,data.9$Number.of.Flights.per.day.per.aircraft)
cible=data.9$TAT.at.customer.estimate

CV2mars.6=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")

#

data.10<-data.frame(data.9$GAvgToPressAltWeighted,data.9$TO.run.FromPressAlt,data.9$Asphalt,data.9$AvgRunwayLength.m.)
cible=data.9$TAT.at.customer.estimate

CV2mars.7=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")

#

data.10<-data.frame(data.9$GAvgToPressAltWeighted,data.9$TO.run.FromPressAlt,data.9$Asphalt)
cible=data.9$TAT.at.customer.estimate

CV2mars.8=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")

#


data.10<-data.frame(data.9$TO.run.FromPressAlt,data.9$GAvgDistanceWeighted,data.9$Tire.mass..kg.,data.9$AvgRunwayLength.m.,data.9$Skid.available..mm..accum,data.9$AvgOfTire.age.when.received)
cible=data.9$TAT.at.customer.estimate

CV2mars.9=CV(df=data.10,cible=data.9$TAT.at.customer.estimate,k=7,type="mars")



compa=data.frame(CV2mars.1$Residus,
                 CV2mars.2$Residus,
                 CV2mars.3$Residus,
                 CV2mars.4$Residus,
                 CV2mars.5$Residus,
                 CV2mars.6$Residus,
                 CV2mars.7$Residus,
                 CV2mars.8$Residus,
                 CV2mars.9$Residus
                 )
plot(density(compa[,1]),main="Comparaison des différents modèles",lwd=2,xlab="Erreur sur le TAT")
couleurs2=as.vector(colors())[c(7,12,17,24,30,36,47,53,68,509)]
lines(density(compa[,2]),col=couleurs2[2],lwd=2)
lines(density(compa[,3]),col=couleurs2[3],lwd=2)
lines(density(compa[,4]),col=couleurs2[9],lwd=2)
lines(density(compa[,5]),col=couleurs2[5],lwd=2)
lines(density(compa[,6]),col=couleurs2[6],lwd=2)
lines(density(compa[,7]),col=couleurs2[7],lwd=2)
lines(density(compa[,8]),col=couleurs2[8],lwd=2)
lines(density(compa[,9]),col=couleurs2[9],lwd=2)

# abline(v=0,lty=2)
# abline(v=-30,lty=4,col='blue')
# abline(v=30,lty=4,col='blue')
# 
# abline(v=-100,lty=5,col='red')
# abline(v=100,lty=5,col='red')

legend("topleft",legend=c("P/TOP/Bitume/Runway/Fly/Skid/Dist/Concrete/Section/LPT",
                          "P/TOP/Bitume/Runway/Fly/Skid/Dist/Concrete/Section",
                          "P/TOP/Bitume/Runway/Fly/Skid/Dist/Concrete",
                          "P/TOP/Bitume/Runway/Fly/Skid/Dist",
                          "P/TOP/Bitume/Runway/Fly/Skid",
                          "P/TOP/Bitume/Runway/Fly",
                          "P/TOP/Bitume/Runway",
                          "P/TOP/Bitume",
                          "TOP/Dist/Mass/Runway/SkidD/Age"
                          ),col=c(couleurs2[1],couleurs2[2],couleurs2[3],
                                  couleurs2[9],couleurs2[5],
                                  couleurs2[6],couleurs2[7],
                                  couleurs2[8],couleurs2[9]
                                  ),lty=1,lwd=2,cex = 0.8)

ticks <- seq(-400, 400, 50)       # sequence for ticks and labels
axis(1, at = ticks,         # y-Axis
     labels = ticks)
box()   


do.it <- function (i, col,erreur) {
  
  d=density(erreur)
  x <- d[[1]][i]
  y <- d[[2]][i]
  polygon( c(x,rev(x)), c(rep(0,length(x)),rev(y)), border=NA, col=col )
}


d=density(compa[,5])

plot(d,col=couleurs2[5],lwd=2)
do.it((d$x<=30)&(d$x>=-30),couleurs2[5],erreur=compa[,5])
text(0, 0.003, paste(as.character(floor(100*sum(lintegrate(x=d$x,y=d$y,xint = d$x[(d$x<=30)&(d$x>=-30)])))),"%",sep=""),cex=1.5,col='white')

plot.d<-function(erreur,text=0.003,borneinf,bornesup,couleur=couleurs2[5],main="Répartition de l'erreur",xlab="Erreurs sur TAT")
  
{
  
d=density(erreur)  
plot(d,col=couleur,lwd=2,main=main,xlab=xlab)

do.it((d[[1]]<=bornesup)&(d[[1]]>=borneinf),couleur,erreur=erreur)

text(0, text, paste(as.character(floor(100*sum(lintegrate(x=d[[1]],y=d[[2]],xint = d[[1]][(d[[1]]<=bornesup)&(d[[1]]>=borneinf)])))),"%",sep=""),cex=1.5,col='white')


ticks <- c(borneinf, bornesup)       # sequence for ticks and labels
axis(1, at = ticks,         # y-Axis
     labels = ticks)
box() 

ticks <- seq(-400, 400, 100)       # sequence for ticks and labels
axis(1, at = ticks,         # y-Axis
     labels = ticks)
box() 


}


plot.d(compa[,9],borneinf=-30,bornesup=30,couleur=couleurs2[9])
legend("topleft",legend=c("TOP/Dist/Mass/Runway/SkidD/Age"),col=c(couleurs2[9]),lty=1,lwd=2,cex=1)

plot.d(compa[,9],borneinf=-60,bornesup=60,couleur=couleurs2[9])
legend("topleft",legend=c("TOP/Dist/Mass/Runway/SkidD/Age"),col=c(couleurs2[9]),lty=1,lwd=2,cex=1)

plot.d(compa[,9],borneinf=-90,bornesup=90,couleur=couleurs2[9])
legend("topleft",legend=c("TOP/Dist/Mass/Runway/SkidD/Age"),col=c(couleurs2[9]),lty=1,lwd=2,cex=1)




plot.d(compa[,5],borneinf=-30,bornesup=30)
legend("topleft",legend=c("P/TOP/Bitume/Runway/Fly/Skid"),col=c(couleurs2[5]),lty=1,lwd=2,cex=1)

par(mfrow=c(2,2))

plot.d(erreur=compa[,5],borneinf=-30,bornesup=30)
legend("topleft",legend=c("P/TOP/Bitume/Runway/Fly/Skid"),col=c(couleurs2[5]),lty=1,lwd=2,cex=1)

plot.d(erreur=compa[,5],borneinf=-60,bornesup=60)
legend("topleft",legend=c("P/TOP/Bitume/Runway/Fly/Skid"),col=c(couleurs2[5]),lty=1,lwd=2,cex=1)

plot.d(erreur=compa[,8],text=0.002,borneinf=-30,bornesup=30,couleur=couleurs2[8])
legend("topleft",legend=c("P/TOP/Bitume"),col=c(couleurs2[8]),lty=1,lwd=2,cex=1)

plot.d(erreur=compa[,8],text=0.002,borneinf=-60,bornesup=60,couleur=couleurs2[8])
legend("topleft",legend=c("P/TOP/Bitume"),col=c(couleurs2[8]),lty=1,lwd=2,cex=1)


par(mfrow=c(2,3))

plot.d(erreur=compa[,5],borneinf=-30,bornesup=30)
legend("topleft",legend=c("P/TOP/Bitume/Runway/Fly/Skid"),col=c(couleurs2[5]),lty=1,lwd=2,cex=1)

plot.d(erreur=compa[,5],borneinf=-60,bornesup=60)
legend("topleft",legend=c("P/TOP/Bitume/Runway/Fly/Skid"),col=c(couleurs2[5]),lty=1,lwd=2,cex=1)

plot.d(erreur=compa[,8],text=0.002,borneinf=-30,bornesup=30,couleur=couleurs2[8])
legend("topleft",legend=c("P/TOP/Bitume"),col=c(couleurs2[8]),lty=1,lwd=2,cex=1)

plot.d(erreur=compa[,8],text=0.002,borneinf=-60,bornesup=60,couleur=couleurs2[8])
legend("topleft",legend=c("P/TOP/Bitume"),col=c(couleurs2[8]),lty=1,lwd=2,cex=1)


plot.d(erreur=compa[,1],text=0.002,borneinf=-30,bornesup=30,couleur=couleurs2[1])
legend("topleft",legend=c("P/TOP/Bitume/Runway/Fly/Skid/Dist/Concrete/Section/LPT"),col=c(couleurs2[1]),lty=1,lwd=2,cex=1)

plot.d(erreur=compa[,1],text=0.002,borneinf=-60,bornesup=60,couleur=couleurs2[1])
legend("topleft",legend=c("P/TOP/Bitume/Runway/Fly/Skid/Dist/Concrete/Section/LPT"),col=c(couleurs2[1]),lty=1,lwd=2,cex=1)


#---------------------


data.7<-data.2


data.7=subset(data.7,Tire.type=="X")

longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.7),ncol=1))

for (i in 1:ncol(data.7))
  
{
  
  longueur[i,1]<-length(unique(data.7[,i]))
  
  rownames(longueur)<-colnames(data.7)
  
  
}




data.7<-data.7[,!colnames(data.7)%in%c("Ply.Rating")]
data.7<-data.7[,!colnames(data.7)%in%c("Date.Dismount")]
data.7<-data.7[,!colnames(data.7)%in%c("Date.Mount")]
data.7<-data.7[,!colnames(data.7)%in%c("Tire.type")]
data.7<-data.7[,!colnames(data.7)%in%c("Date.Reception.at.plant")]
data.7<-data.7[,!colnames(data.7)%in%c("Operator.Country.DUS")]
data.7<-data.7[,!colnames(data.7)%in%c("Transit.time.before.retreading.estimate")]
data.7<-data.7[,!colnames(data.7)%in%c("TO.run.ToPressAlt" )]
data.7<-data.7[,!colnames(data.7)%in%c("TO.run.FromDenAlt"  )]
data.7<-data.7[,!colnames(data.7)%in%c("Cargo"  )]
data.7<-data.7[,!colnames(data.7)%in%c("Holiday.Charter" )]


longueur<-as.data.frame(matrix(data=NA,nrow=ncol(data.7),ncol=1))

for (i in 1:ncol(data.7))
  
{
  
  longueur[i,1]<-length(unique(data.7[,i]))
  
  rownames(longueur)<-colnames(data.7)
  
  
}



discret_var_arbitraire<-function(Y,p,q)
  
{  
  
  y<-c()
  
  
  for (i in 1:length(Y))
    
  {
    
    if(Y[i]<=p){y[i]<-"<=200"}
    if((Y[i]>p)&(Y[i]<=q)){y[i]<-"200<..<=500"}
    if(Y[i]>q){y[i]<-">500"}
    
  }
  
  return(y)
  
}


boxplot(data.2$TAT.at.customer.estimate,col='pink',xaxp=c(0,max(data.2$TAT.at.customer.estimate),12),boxwex=0.3,varwidth=FALSE,horizontal=TRUE,main = colnames(data.2)[15])







data.7$TAT.at.customer.estimate<-discretize(data.7$TAT.at.customer.estimate,method="cluster",categories=4)



data.7$Tread.Depth.accum<-discretize(data.7$Tread.Depth.accum,method="frequency",categories=3)
data.7$AvgOfTire.age.when.received<-discretize(data.7$AvgOfTire.age.when.received,method="frequency",categories=3)
data.7$LPT<-discretize(data.7$LPT,method="frequency",categories=4)
data.7$Number.of.Flights.per.day.per.aircraft<-discretize(data.7$Number.of.Flights.per.day.per.aircraft,method="frequency",categories=4)

data.7$LPT<-discretize(data.7$LPT,method="frequency",categories=4)

data.7$AvgOfA.C.Maximum.Take.off.Weight.Kgs.<-discretize(data.7$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,method="frequency",categories=3)
data.7$GAvgToAptTempWeighted<-discretize(data.7$GAvgToAptTempWeighted,method="frequency",categories=4)
data.7$GAvgToAptMinTempWeighted<-discretize(data.7$GAvgToAptMinTempWeighted,method="frequency",categories=4)
data.7$GAvgToAptMaxTempWeighted<-discretize(data.7$GAvgToAptMaxTempWeighted,method="frequency",categories=4)
data.7$GAvgToAptDenWeighted<-discretize(data.7$GAvgToAptDenWeighted,method="frequency",categories=4)
data.7$GAvgToPressAltWeighted<-discretize(data.7$GAvgToPressAltWeighted,method="frequency",categories=4)
data.7$GAvgToDenAltWeighted<-discretize(data.7$GAvgToDenAltWeighted,method="frequency",categories=4)
data.7$TO.run.FromPressAlt<-discretize(data.7$TO.run.FromPressAlt,method="frequency",categories=4)
data.7$TO.run.ToDenAlt<-discretize(data.7$TO.run.ToDenAlt,method="frequency",categories=4)

data.7$GAvgDistanceWeighted<-discretize(data.7$GAvgDistanceWeighted,method="frequency",categories=4)
data.7$MinRunwayLength.m.<-discretize(data.7$MinRunwayLength.m.,method="frequency",categories=4)
data.7$MaxRunwayLength.m.<-discretize(data.7$MaxRunwayLength.m.,method="frequency",categories=4)
data.7$AvgRunwayLength.m.<-discretize(data.7$AvgRunwayLength.m.,method="frequency",categories=4)


data.7$Asphalt<-discretize(data.7$Asphalt,method="frequency",categories=2)
data.7$Concrete<-discretize(data.7$Concrete,method="frequency",categories=2)
data.7$Other.Surfaces<-discretize(data.7$Other.Surfaces,method="frequency",categories=2)

data.7$Section.Width..mm.<-discretize(data.7$Section.Width..mm.,method="frequency",categories=3)
data.7$Tire.mass..kg.<-discretize(data.7$Tire.mass..kg.,method="frequency",categories=3)

data.7$Leaser<-discretize(data.7$Leaser,method="frequency",categories=2)
data.7$Low.Cost<-discretize(data.7$Low.Cost,method="frequency",categories=2)

data.7$Skid.available..mm..accum<-discretize(data.7$Skid.available..mm..accum,method="frequency",categories=5)

colnames(data.7)
data.7=data.7[,-c(33,35,36,37)]


pie(summary(data.7$LPT),main="Répartition des LPT")
pie(summary(data.7$TAT.at.customer.estimate),main="Répartition des TAT")



nom<-dv2[order(dv2[,2],decreasing = TRUE)[1:12],1]
data.7prime<-subset(data.7,Part.Number=='M01103-02')
data.7prime<-data.7prime[,-1]

TAT=data.7prime$TAT.at.customer.estimate
poids <- information.gain(TAT~., data.7prime)
poids<-poids[-c(13,3),]
print(poids)
names(poids)=c("Rmax",
               "Client",
               # "PN",
               "T.Tech",
               "SR",
               "NorR",
               "P.Manuf",
               "P.Rece",
               "Pos",
               "Framer",
               "Model",
               "Age",
               "LPT"
               ,"Fly",
               "MTOW",
               "T",
               "Tmin",
               "Tmax",
               "P",
               "TOP",
               "Distance",
               "AvgRunway",
               "Bitume",
               "Concrete",
               "AutresSur",
               "Section",
               "SkidD",
               "Mass",
               "R",
               "LowC",
               "Zone")

barplot(sort(poids,decreasing = TRUE)[1:10],names.arg = names(sort(poids,decreasing = TRUE)[1:10]))

#Relation age niveau de rechappage



datata<-subset(data.2,Part.Number=='M01103-02'&Codeclient=='AVAFR')

boxplot(datata$AvgOfTire.age.when.received~datata$R_level.1,data=datata,xlab="Niveau de rechappage",ylab = "Age du pneu",main="Age en fonction du Rlevel",col="cornflowerblue",ylim=c(0,5))
boxplot(datata$Skid.available..mm..accum~datata$R_level.1,data=datata,xlab="Niveau de rechappage",ylab = "Skid disp",main="Skid disp en fonction du Rlevel",col="cornflowerblue")
boxplot(datata$TAT.at.customer.estimate~datata$R_level.1,data=datata,xlab="Niveau de rechappage",ylab = "TAT",main="TAT en fonction du Rlevel",col="cornflowerblue")




#Bleu
DF<-data.frame(data.7prime$TAT.at.customer.estimate,as.factor(as.character(data.7prime$Codeclient)),as.factor(as.character(data.7prime$Part.Number)),data.7prime$Zone)
rf1 <- randomForest(DF$data.7prime.TAT.at.customer.estimate ~ ., DF)
colMeans(rf1$err.rate)

#Rouge
DF<-data.frame(data.7prime$TAT.at.customer.estimate,as.factor(as.character(data.7prime$Part.Number)),data.7prime$Zone,data.7prime$AvgOfTire.age.when.received)
rf <- randomForest(DF$data.7prime.TAT.at.customer.estimate ~ ., DF)
colMeans(rf$err.rate)

#Rouge
DF<-data.frame(data.7prime$TAT.at.customer.estimate,as.factor(as.character(data.7prime$Low.Cost)),as.factor(as.character(data.7prime$Part.Number)),data.7prime$Zone,data.7prime$AvgOfTire.age.when.received)
rf3 <- randomForest(DF$data.7prime.TAT.at.customer.estimate ~ ., DF)
colMeans(rf3$err.rate)


#Rouge
DF<-data.frame(data.7prime$TAT.at.customer.estimate,as.factor(as.character(data.7prime$AvgOfA.C.Maximum.Take.off.Weight.Kgs.)),as.factor(as.character(data.7prime$Low.Cost)),as.factor(as.character(data.7prime$Part.Number)),data.7prime$Zone,data.7prime$AvgOfTire.age.when.received)
rf4 <- randomForest(DF$data.7prime.TAT.at.customer.estimate ~ ., DF)
colMeans(rf4$err.rate)

#Rouge
DF<-data.frame(data.7prime$TAT.at.customer.estimate,as.factor(as.character(data.7prime$Codeclient)),as.factor(as.character(data.7prime$Part.Number)),data.7prime$Zone,data.7prime$AvgOfTire.age.when.received)
rf2 <- randomForest(DF$data.7prime.TAT.at.customer.estimate ~ ., DF)
colMeans(rf2$err.rate)


#Rouge
DF<-data.frame(data.7prime$TAT.at.customer.estimate,as.factor(as.character(data.7prime$AvgOfA.C.Maximum.Take.off.Weight.Kgs.)),as.factor(as.character(data.7prime$Low.Cost)),data.7prime$Zone,data.7prime$AvgOfTire.age.when.received)
rf5 <- randomForest(DF$data.7prime.TAT.at.customer.estimate ~ ., DF)
colMeans(rf5$err.rate)


#Rouge
DF<-data.frame(data.7prime$TAT.at.customer.estimate,data.7prime$Zone,data.7prime$AvgOfTire.age.when.received)
rf5 <- randomForest(DF$data.7prime.TAT.at.customer.estimate ~ ., DF)
colMeans(rf5$err.rate)


#Rouge
DF<-data.frame(data.7prime$Codeclient,data.7prime$AvgOfTire.age.when.received,data.7prime$New.or.Retread,data.7prime$AvgOfA.C.Maximum.Take.off.Weight.Kgs.,data.7prime$TAT.at.customer.estimate,data.7prime$Zone,data.7prime$R_level.1)
rf5 <- randomForest(DF$data.7prime.TAT.at.customer.estimate ~ ., DF)
colMeans(rf5$err.rate)
rf5$confusion
rf5$votes[1,]















