#-----------------------------------------------------------Modèle 1 et modèle 2-------------------------------------------------------------------

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
library(caret)
library(forecast)
library('nnet')
library('neuralnet')
library("cvTools", lib.loc="~/R/win-library/3.2")
library("strucchange", lib.loc="~/R/win-library/3.2")
library("vars", lib.loc="~/R/win-library/3.2")
library("tsDyn", lib.loc="~/R/win-library/3.2")
library('e1071')
library("varSelRF", lib.loc="~/R/win-library/3.2")
library("FSelector", lib.loc="~/R/win-library/3.2")
library("smbinning", lib.loc="~/R/win-library/3.2")
library("discretization", lib.loc="~/R/win-library/3.2")
library("infotheo", lib.loc="~/R/win-library/3.2")
library("arules", lib.loc="~/R/win-library/3.2")
library("zoo", lib.loc="C:/Program Files/R/R-3.2.0/library")



retour_pn<-read.csv("~/Michelin/Etude_R/retour_pn.csv",header=TRUE,sep=";",na.strings = "")
vente_pn<-read.csv("~/Michelin/Etude_R/vente_pn.csv",header=TRUE,sep=";",na.strings = "")

#--------------------------------------------------------------------Première préparation----------------------------------------------

retour_pn$Datereception.Mois<-gsub("Janvier","01",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Fevrier","02",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Février","02",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Mars","03",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Avril","04",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Mai","05",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Juin","06",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Juillet","07",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Aout","08",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Août","08",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Septembre","09",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Octobre","10",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Novembre","11",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Décembre","12",retour_pn$Datereception.Mois)
retour_pn$Datereception.Mois<-gsub("Decembre","12",retour_pn$Datereception.Mois)



retour_pn$Date<-paste(retour_pn$Datereception.Année,retour_pn$Datereception.Mois,"01",sep="-")
retour_pn$CLIENT<-paste("AV",retour_pn$Codeclient,sep="")

vente_pn$Month<-gsub(10,"G",vente_pn$Month)
vente_pn$Month<-gsub(11,"H",vente_pn$Month)
vente_pn$Month<-gsub(12,"I",vente_pn$Month)
vente_pn$Month<-gsub(1,"01",vente_pn$Month)
vente_pn$Month<-gsub(2,"02",vente_pn$Month)
vente_pn$Month<-gsub(3,"03",vente_pn$Month)
vente_pn$Month<-gsub(4,"04",vente_pn$Month)
vente_pn$Month<-gsub(5,"05",vente_pn$Month)
vente_pn$Month<-gsub(6,"06",vente_pn$Month)
vente_pn$Month<-gsub(7,"07",vente_pn$Month)
vente_pn$Month<-gsub(8,"08",vente_pn$Month)
vente_pn$Month<-gsub(9,"09",vente_pn$Month)
vente_pn$Month<-gsub("I","12",vente_pn$Month)
vente_pn$Month<-gsub("H","11",vente_pn$Month)
vente_pn$Month<-gsub("G","10",vente_pn$Month)

vente_pn$Date<-paste(vente_pn$Year,vente_pn$Month,"01",sep="-")

#--------------------------------------------------------------------Deuxième préparation-------------------------------------------------

dv<-data.frame(vente_pn$Date,vente_pn$PN,vente_pn$VENTES,vente_pn$CLIENT,vente_pn$ZONE)
dr<-data.frame(retour_pn$Date,retour_pn$PN,retour_pn$compteur,retour_pn$CLIENT,retour_pn$Codesite)

colnames(dv)<-c("Date","PN","Ventes","Client","Zone")
colnames(dr)<-c("Date","PN","Retours","Client","Site")



dV<-aggregate(dv$Vente,by=list(dv$Date,dv$Client,dv$PN,dv$Zone),FUN=sum)
dR<-aggregate(dr$Retours,by=list(dr$Date,dr$Client,dr$PN,dr$Site),FUN=sum)
colnames(dV)<-c("Date","Client","PN","Zone","Ventes")
colnames(dR)<-c("Date","Client","PN","Site","Retours")

dV$Zone<-gsub("EMA","URG",dV$Zone)
dV$Zone<-gsub("FEO","NKE",dV$Zone)
dV$Zone<-gsub("NAM","NWD",dV$Zone)

dR$Site<-gsub("KCY","NWD",dR$Site)


subset(dR,Client=="AVUSA" & PN=="M01103" & Site=="NWD")

#--------------------------------------------------------------------Maille client + PN + Client/PN---------------------------------------------------------------

#Attention por charger ces listes charger dv<-data.frame(vente_pn$Date,vente_pn$PN,vente_pn$VENTES,vente_pn$CLIENT)


liste.vente<-list()
nom<-c()

for(j in 1:length(unique(dV$Client)))
  
{
  
  
  liste.vente[[j]]<-subset(dV, Client==unique(dV$Client)[j])[,-2]
  nom<-c(nom,as.character(unique(dV$Client)[j]))
  names(liste.vente)<-nom
  
  
  
  
}


liste.retour<-list()
nom<-c()

for(j in 1:length(unique(dR$Client)))
  
{
  
  
  liste.retour[[j]]<-subset(dR, Client==unique(dR$Client)[j])[,-2]
  nom<-c(nom,as.character(unique(dR$Client)[j]))
  names(liste.retour)<-nom
  
  
  
  
}


liste.pn.retour<-list(list())
length(liste.pn.retour)=length(liste.retour)
nom<-c()


for(j in 1:length(liste.retour))
  
{
  
  for(i in 1:length(unique(liste.retour[[j]][,2])))
  
  {
    
  liste.pn.retour[[j]][[i]]<-subset(liste.retour[[j]], PN==as.vector(unique(liste.retour[[j]][,2]))[i])[,-2]
  
  
  }
  
  
}

names(liste.pn.retour)<-as.character(names(liste.retour))


for(k in 1:length(names(liste.pn.retour)))
  
{
  
  names(liste.pn.retour[[k]])<-as.vector(unique(liste.retour[[k]][,2]))
  
  
}



liste.pn.vente<-list(list())
length(liste.pn.vente)=length(liste.vente)
nom<-c()


for(j in 1:length(liste.vente))
  
{
  
  for(i in 1:length(unique(liste.vente[[j]][,2])))
    
  {
    
    liste.pn.vente[[j]][[i]]<-subset(liste.vente[[j]], PN==as.vector(unique(liste.vente[[j]][,2]))[i])[,-2]
    
    
  }
  
  
}

names(liste.pn.vente)<-as.character(names(liste.vente))


for(k in 1:length(names(liste.pn.vente)))
  
{
  
  names(liste.pn.vente[[k]])<-as.vector(unique(liste.vente[[k]][,2]))
  
  
}



Date<-as.Date(liste.pn.vente$AVAFR$M01103[,1])[37:101]



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


for(j in 1:length(liste.pn.vente))
  
{
  
  for(i in 1:length(liste.pn.vente[[j]]))
    
  {
    
    liste.pn.vente[[j]][[i]]<-remplacerparNA(liste.pn.vente[[j]][[i]],"Ventes")
    
  }
  
  
}  

#--------------------------------------------------------------------Liste finale maille client PN---------------

liste.pn.client<-liste.pn.retour

for(j in 1:length(liste.pn.client))
  
{
  
  for(i in 1:length(liste.pn.client[[j]]))
    
  {
    
    liste.pn.client[[j]][[i]]<-remplacerparNA(liste.pn.client[[j]][[i]],"Retours")
    
    
    liste.pn.client[[j]][[i]]$Ventes<-liste.pn.vente[[match(names(liste.pn.client)[j],names(liste.pn.vente))]][[match(names(liste.pn.client[[j]])[i],names(liste.pn.vente[[match(names(liste.pn.client)[j],names(liste.pn.vente))]]))]][,2]
      
  }
  
}


#--------------------------------------------------------------------Liste finale maille PN zone commerciale----------------------------------------

dv<-data.frame(vente_pn$Date,vente_pn$PN,vente_pn$VENTES,vente_pn$ZONE)
dr<-data.frame(retour_pn$Date,retour_pn$PN,retour_pn$compteur,retour_pn$Zone)

colnames(dv)<-c("Date","PN","Ventes","Zone")
colnames(dr)<-c("Date","PN","Retours","Zone")


dV<-aggregate(dv$Vente,by=list(dv$Date,dv$PN,dv$Zone),FUN=sum)
dR<-aggregate(dr$Retours,by=list(dr$Date,dr$PN,dr$Zone),FUN=sum)
colnames(dV)<-c("Date","PN","Zone","Ventes")
colnames(dR)<-c("Date","PN","Zone","Retours")

summary(subset(dR, PN=="M01103" & Zone=="EMA"))
summary(subset(dV, PN=="M01103" & Zone=="EMA"))


liste.vente.zone<-list()
nom<-c()

for(j in 1:length(unique(dV$PN)))
  
{
  
  
  liste.vente.zone[[j]]<-subset(dV, PN==unique(dV$PN)[j])[,-2]
  nom<-c(nom,as.character(unique(dV$PN)[j]))
  names(liste.vente.zone)<-nom
  
  
  
  
}


liste.retour.zone<-list()
nom<-c()

for(j in 1:length(unique(dR$PN)))
  
{
  
  
  liste.retour.zone[[j]]<-subset(dR, PN==unique(dR$PN)[j])[,-2]
  nom<-c(nom,as.character(unique(dR$PN)[j]))
  names(liste.retour.zone)<-nom
  
  
  
  
}

liste.pn.retour.zone<-list(list())
length(liste.pn.retour.zone)=length(liste.retour.zone)
nom<-c()


for(j in 1:length(liste.retour.zone))
  
{
  
  for(i in 1:length(unique(liste.retour.zone[[j]][,2])))
    
  {
    
    liste.pn.retour.zone[[j]][[i]]<-subset(liste.retour.zone[[j]], Zone==as.vector(unique(liste.retour.zone[[j]][,2]))[i])[,-2]
    
    
  }
  
  
}

names(liste.pn.retour.zone)<-as.character(names(liste.retour.zone))


for(k in 1:length(names(liste.pn.retour.zone)))
  
{
  
  names(liste.pn.retour.zone[[k]])<-as.vector(unique(liste.retour.zone[[k]][,2]))
  
  
}



liste.pn.vente.zone<-list(list())
length(liste.pn.vente.zone)=length(liste.vente.zone)
nom<-c()


for(j in 1:length(liste.vente.zone))
  
{
  
  for(i in 1:length(unique(liste.vente.zone[[j]][,2])))
    
  {
    
    liste.pn.vente.zone[[j]][[i]]<-subset(liste.vente.zone[[j]], Zone==as.vector(unique(liste.vente.zone[[j]][,2]))[i])[,-2]
    
    
  }
  
  
}

names(liste.pn.vente.zone)<-as.character(names(liste.vente.zone))


for(k in 1:length(names(liste.pn.vente.zone)))
  
{
  
  names(liste.pn.vente.zone[[k]])<-as.vector(unique(liste.vente.zone[[k]][,2]))
  
  
}

  


for(j in 1:length(liste.pn.retour.zone))
  
{
  
  for(i in 1:length(liste.pn.retour.zone[[j]]))
    
  {
    
    liste.pn.retour.zone[[j]][[i]]<-remplacerparNA(liste.pn.retour.zone[[j]][[i]],"Retours")
    
  }
  
}  



for(j in 1:length(liste.pn.vente.zone))
  
{
  
  for(i in 1:length(liste.pn.vente.zone[[j]]))
    
  {
    
    liste.pn.vente.zone[[j]][[i]]<-remplacerparNA(liste.pn.vente.zone[[j]][[i]],"Ventes")
    
  }
  
}  



liste.pn.zone<-liste.pn.retour.zone

for(j in 1:length(liste.pn.zone))
  
{
  
  for(i in 1:length(liste.pn.zone[[j]]))
    
  {
    
    
    
    liste.pn.zone[[j]][[i]]$Ventes<-liste.pn.vente.zone[[match(names(liste.pn.zone)[j],names(liste.pn.vente.zone))]][[match(names(liste.pn.zone[[j]])[i],names(liste.pn.vente.zone[[match(names(liste.pn.zone)[j],names(liste.pn.vente.zone))]]))]][,2]
    
  }
  
}






#--------------------------------------------------------------------Liste maille PN---------------------------


liste.vente2<-list()
nom<-c()

for(j in 1:length(unique(dV$PN)))
  
{
  
  
  liste.vente2[[j]]<-subset(dV, PN==unique(dV$PN)[j])[,-2]
  nom<-c(nom,as.character(unique(dV$PN)[j]))
  names(liste.vente2)<-nom
  
  
  
  
}


liste.retour2<-list()
nom<-c()

for(j in 1:length(unique(dR$PN)))
  
{
  
  
  liste.retour2[[j]]<-subset(dR, PN==unique(dR$PN)[j])[,-2]
  nom<-c(nom,as.character(unique(dR$PN)[j]))
  names(liste.retour2)<-nom
  
  
  
  
}


liste.pn.retour2<-list()

{

for(j in 1:length((names(liste.retour2))))
  
{
  
liste.pn.retour2[[j]]<-aggregate(liste.retour2[[j]][,3],by=list(liste.retour2[[j]][,1]),FUN=sum)
liste.pn.retour2[[j]]<-remplacerparNA(liste.pn.retour2[[j]],"Retours")

}

names(liste.pn.retour2)<-names(liste.retour2)



}


liste.pn.vente2<-list()

{

for(j in 1:length((names(liste.vente2))))
  
{
  
  liste.pn.vente2[[j]]<-aggregate(liste.vente2[[j]][,3],by=list(liste.vente2[[j]][,1]),FUN=sum)
  liste.pn.vente2[[j]]<-remplacerparNA(liste.pn.vente2[[j]],"ventes")
  
}

names(liste.pn.vente2)<-names(liste.vente2)



}


liste.pn<-liste.pn.retour2

for(j in 1:length(liste.pn))
  
{
  

  
    
    liste.pn[[j]]$Ventes<-liste.pn.vente2[[match(names(liste.pn)[j],names(liste.pn.vente2))]][,2]
    

  
}

#--------------------------------------------------------------------Liste maille client-----------------------

liste.vente3<-list()
nom<-c()

for(j in 1:length(unique(dV$Client)))
  
{
  
  
  liste.vente3[[j]]<-subset(dV, Client==unique(dV$Client)[j])[,-2]
  nom<-c(nom,as.character(unique(dV$Client)[j]))
  names(liste.vente3)<-nom
  
  
  
  
}


liste.retour3<-list()
nom<-c()

for(j in 1:length(unique(dR$Client)))
  
{
  
  
  liste.retour3[[j]]<-subset(dR, Client==unique(dR$Client)[j])[,-2]
  nom<-c(nom,as.character(unique(dR$Client)[j]))
  names(liste.retour3)<-nom
  
  
  
  
}


liste.pn.retour3<-list()

{

for(j in 1:length((names(liste.retour3))))
  
{
  
  liste.pn.retour3[[j]]<-aggregate(liste.retour3[[j]][,3],by=list(liste.retour3[[j]][,1]),FUN=sum)
  liste.pn.retour3[[j]]<-remplacerparNA(liste.pn.retour3[[j]],"Retours")
  
}

names(liste.pn.retour3)<-names(liste.retour3)



}


liste.pn.vente3<-list()

{

for(j in 1:length((names(liste.vente3))))
  
{
  
  liste.pn.vente3[[j]]<-aggregate(liste.vente3[[j]][,3],by=list(liste.vente3[[j]][,1]),FUN=sum)
  liste.pn.vente3[[j]]<-remplacerparNA(liste.pn.vente3[[j]],"ventes")
  
}

names(liste.pn.vente3)<-names(liste.vente3)



}


liste.client<-liste.pn.retour3

for(j in 1:length(liste.client))
  
{
  
  
  
  
  liste.client[[j]]$Ventes<-liste.pn.vente3[[match(names(liste.client)[j],names(liste.pn.vente3))]][,2]
  
  
  
}

#--------------------------------------------------------------------Modèle 1-------------------------------------------------------------------------------------

#--------------------------------------------------------------------Choisir coeff ARIMA--------------------------------------------------------------------------

choisircoeff1<-function(cible)
{
  
  a <- c()
  for (p in 0:2) {
    for (d in 0:2) {
      for (q in 0:2) {
        for (P in 0:2) {
          for (D in 0:2) {
            for (Q in 0:2) {
              r <- list(aic=NA)
              try( 
                r <- arima( cible, 
                            order=c(p,d,q),
                            list(order=c(P,D,Q), period=12) 
                )
              )
              a[Q+D*(3^1)+P*(3^2)+q*(3^3)+d*(3^4)+p*(3^5)] <- r$aic
              cat(r$aic); cat("\n")
            }
          }
        }
      }
    }
  }  
  
  
  return(a)
  
}

a<-c()

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for (i in 1:728)
{
 
  try(r<-arima(cible,order=c(retrouveordre(i)[1],retrouveordre(i)[2],retrouveordre(i)[3]),
               list(order=c(retrouveordre(i)[4],retrouveordre(i)[5],retrouveordre(i)[6]),period=12) ) )
  try(a[i]<-r$aic)
  
  info <- sprintf("%d%% done", round(100*(i/728)))
  setWinProgressBar(pb, 100*(i/728), label=info)
  
}

choisircoeff2<-function(cible)
{
  
  a <- c()
  for (p in 0:1) {
    for (d in 0:2) {
      for (q in 0:1) {
        for (P in 0:1) {
          for (D in 0:2) {
            for (Q in 0:1) {
              r <- list(aic=NA)
              try( 
                r <- arima( cible, 
                            order=c(p,d,q),
                            list(order=c(P,D,Q), period=12) 
                )
              )
              a[Q+D*(3^1)+P*(3^2)+q*(3^3)+d*(3^4)+p*(3^5)] <- r$aic
              cat(r$aic); cat("\n")
            }
          }
        }
      }
    }
  }  
  
  
  return(a)
  
}



retrouveordre<-function(x)
  
{
  
  result<-c()
  
  
  
  result[1]<-x%/%(3^5)
  result[2]<-(x-result[1]*(3^5))%/%(3^4)
  result[3]<-(x-result[1]*(3^5)-result[2]*(3^4))%/%(3^3)
  result[4]<-(x-result[1]*(3^5)-result[2]*(3^4)-result[3]*(3^3))%/%(3^2)
  result[5]<-(x-result[1]*(3^5)-result[2]*(3^4)-result[3]*(3^3)-result[4]*(3^2))%/%3
  result[6]<-x-result[1]*(3^5)-result[2]*(3^4)-result[3]*(3^3)-result[4]*(3^2)-result[5]*3
  
  
  
  names(result)<-c("p","d","q","P","D","Q")
  
  return(result)
}


pqdesmin<-function(a,l)
  
{
  a2<-a
  
  min<-matrix(data=c(0),ncol=2,nrow=l)
  min[1,]<-c(which.min(a2),a[which.min(a2)])
  Min<-matrix(data=c(0),ncol=7,nrow=l)
  
  k<-2
  
  
  
  while(k<=l)
    
  {
    
    a2<-a2[-which.min(a2)]
    min[k,]<-c(which(a==a2[which.min(a2)]),a2[which.min(a2)])
    
    
    k<-k+1
  }
  
  for(k in 1:l)
    
  {
    
    Min[k,]<-c(retrouveordre(min[k,1]),min[k,2])
    
  }
  
  colnames(Min)<-c("p","d","q","P","D","Q","AIC")
  
  return(Min)
}  


#--------------------------------------------------------------------Calcul modèle 1--------------------------------------

calculmodele2<-function(x,coupe,n.ahead=0,train=12,method="mean",fenetre=48,nbmodel=200)
  
{
  x2<-window(x,start=(length(x)-fenetre),end=length(x))
  rn<-x2[!is.na(x2)]
  N<-length(rn)
  rn1<-rn[1:coupe]
  rn1.2<-rn[(coupe+1):(coupe+train)]
  rn2<-rn[((coupe+train+1):N)]
  rn3<-c(rn1,rn1.2,rn2)
  
  serie_rn1<-serie(rn1)
  
  t<-c(1:length(serie_rn1))
  
  
  lin_serie_rn1<-lm(serie_rn1 ~ t + I(t^2)) 
  
  predict_serie_rn1 <- predict( lm(serie_rn1 ~ t + I(t^2)) )
  
  
  En<-lin_serie_rn1$residuals
  
  deltaEn<-diff(En)
  
  COEFF<-choisircoeff1(deltaEn)
  best<-pqdesmin(COEFF,nbmodel)
  
  
  k<-1
  erreurinst<-c()
  erreurmoyenne<-c()
  modele<-matrix(data=c(0),ncol=nbmodel,nrow=N)
  modele1<-matrix(data=c(NA),ncol=nbmodel,nrow=N+n.ahead)
  
  
if(method=="mean")

{  
  while(k<=nrow(best))  
    
  {
    
    
    
    
    try(rdeltaEn <- arima(deltaEn, order=c(best[k,1],best[k,2],best[k,3]), list(order=c(best[k,4],best[k,5],best[k,6]), period=12 ) ))
    
    try(pdeltaEn<-predict(rdeltaEn,n.ahead=N-coupe+1+n.ahead))
    
    try(
      for(i in 1:coupe)
        
      {
        modele[i,k]<-rn1[i]  
        
        
      }  
    )
    
    try(
      for(i in ((coupe+1):N))
        
        
      {
        
        modele[i,k]<-(lin_serie_rn1$coefficients[3]*(2*i-1)+lin_serie_rn1$coefficients[2]+as.vector(pdeltaEn$pred)[i-coupe])
        
        
      }
    )
    
    try(
      for(i in ((coupe+1):(N+n.ahead)))
        
        
      {
        
        modele1[i,k]<-(lin_serie_rn1$coefficients[3]*(2*i-1)+lin_serie_rn1$coefficients[2]+as.vector(pdeltaEn$pred)[i-coupe])
        
        
      }
    )
    
    rn3<-rn3[!is.na(rn3)]
    
    try(erreurinst<-100*abs(modele[(coupe+1):(coupe+train),k]-rn3[(coupe+1):(coupe+train)])/rn3[(coupe+1):(coupe+train)] )     
    try(erreurmoyenne[k]<-mean(erreurinst)) 
    
    k<-k+1  
    rdeltaEn<-0
    
  
    
  }  
  
  result<-list(mod=NA,emin=NA,type=NA,E.esti=NA)
  result[[1]]<-modele1[,which.min(erreurmoyenne)]
  
  modelemin<-c()
  modelemin<-modele1[,which.min(erreurmoyenne)]
  erreurveritable<-c()
  
  for( i in (coupe+train+1):N ){erreurveritable[i]<-100*abs(modelemin[i]-rn3[i])/rn3[i]}
  

  
  result[[2]]<-erreurmoyenne[which.min(erreurmoyenne)]
  result[[3]]<-c(best[which.min(erreurmoyenne),1],best[which.min(erreurmoyenne),2],best[which.min(erreurmoyenne),3],best[which.min(erreurmoyenne),4],best[which.min(erreurmoyenne),5],best[which.min(erreurmoyenne),6])
  result[[4]]<-mean(erreurveritable,na.rm=TRUE)
  
}  
  
  
if(method=="aic")

{
  
k=1
modele<-c()
modele1<-c()
    
    
    try(rdeltaEn <- arima(deltaEn, order=c(best[k,1],best[k,2],best[k,3]), list(order=c(best[k,4],best[k,5],best[k,6]), period=12 ) ))
    
    try(pdeltaEn<-predict(rdeltaEn,n.ahead=N-coupe+1+n.ahead))
    
    try(
      for(i in 1:coupe)
        
      {
        modele[i]<-rn1[i]  
        
        
      }  
    )
    
    try(
      for(i in ((coupe+1):N))
        
        
      {
        
        modele[i]<-(lin_serie_rn1$coefficients[3]*(2*i-1)+lin_serie_rn1$coefficients[2]+as.vector(pdeltaEn$pred)[i-coupe])
        
        
      }
    )
    
    try(
      for(i in ((coupe+1):(N+n.ahead)))
        
        
      {
        
        modele1[i]<-(lin_serie_rn1$coefficients[3]*(2*i-1)+lin_serie_rn1$coefficients[2]+as.vector(pdeltaEn$pred)[i-coupe])
        
        
      }
    )
    

    
  result<-list(mod=NA,type=NA)
  result[[1]]<-modele1
  result[[2]]<-c(best[k,1],best[k,2],best[k,3],best[k,4],best[k,5],best[k,6])
  
}    
  
  
  
  
  
  
return(result)
  
}





#-------------------------------------------------------------------Occurences------------------------------------------------------------------------------------

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



intervalleconfiance<-function(xreel,xmodele,epsilon)
  
  
{
  
  
  k<-0
  
  while ( (occurence(xreel,(1-k)*xmodele,(1+k)*xmodele)>=epsilon)==FALSE )
    
  {
    
    
    k<-k+0.02
    
    
  }
  
  
  return(k)
  
  
  
}  







#-------------------------------------------------------------------Modèle 2------------------------------------------------------------------------------

#------------------------------------------------------------------Choisir coeff VAR----------------------------------------------------

choisircoeff1VAR<-function(cible)
  
{
  
  a <- c()
  r <- 0
  
  for( saison in 0:12)
    
  {
    
    
    if(saison==0){saison<-NULL}
    
    for (k in 0:12) 
      
    {
      
      
      r<-0
      try( r <- VAR( cible, p = k,type = c("const", "trend", "both", "none"), season = saison ,exogen=NULL,lag.max=NULL,ic=c("AIC")) )
      
      if((is.null(saison)==TRUE)&((class(try(AIC(r)))=="try-error")==TRUE)){a[k+13*0] <- NA}
      if((is.null(saison)==TRUE)&((class(try(AIC(r)))=="try-error")==FALSE)){a[k+13*0] <-AIC(r)}
      if((is.null(saison)==FALSE)&((class(try(AIC(r)))=="try-error")==TRUE)){a[k+13*saison] <-NA}
      if((is.null(saison)==FALSE)&((class(try(AIC(r)))=="try-error")==FALSE)){a[k+13*saison] <-AIC(r)}
      
      
      
    }
    
  }
  
  return(a)
  
}

#------------------------------------------------------------------Retrouver ordre VAR-------------------------------------------------

retrouveordreVAR<-function(x)
  
{
  
  result<-c()
  
  
  
  result[1]<-x%%13
  result[2]<-(x-result[1])%/%13
  
  
  
  
  names(result)<-c("ordre","saison")
  
  return(result)
}

#------------------------------------------------------------------Trouver les mins----------------------------------------------------

pqdesminVAR<-function(a,l)
  
{
  a2<-a
  
  min<-matrix(data=c(0),ncol=2,nrow=l)
  min[1,]<-c(which.min(a2),a2[which.min(a2)])
  Min<-matrix(data=c(0),ncol=3,nrow=l)
  
  k<-2
  
  
  
  while(k<=l)
    
  {
    
    a2<-a2[-which.min(a2)]
    min[k,]<-c(which(a==a2[which.min(a2)]),a2[which.min(a2)])
    
    
    k<-k+1
  }
  
  for(k in 1:l)
    
  {
    
    Min[k,]<-c(retrouveordreVAR(as.numeric(min[k,1])),min[k,2])
    
  }
  
  colnames(Min)<-c("ordre","saison","AIC")
  
  return(Min)
}  

#------------------------------------------------------------------Calcul du modèle 2--------------------------------------------------

cross_valid<-function(v,r,p,saison,debut=5,fin=12)
  
{
  
  kCV<-list()
  
  n<-length(r)
  
  
  for(k in debut:fin)
    
  {
    
    kCV2<-list(modele=matrix(NA,floor(n/k)-1,k))
    
    
    
    
    for(i in 1:(floor(n/k)-1))
    {
      xshort <- window(r, end=i*k)
      xnext <- window(r, start=(i*k)+1, end=(i+1)*k)
      yshort <- window(v, end=i*k)
      ynext <- window(v, start=i*k+1, end=(i+1)*k)
      
      
      
      try(fit1 <- VAR(data.frame(xshort,yshort), p = p, type = "both",season =saison, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")))
      try(fcast1 <- predict(fit1, n.ahead=k)$fcst$xshort[,1])
      
      kCV2$modele[i,] <- abs(fcast1-xnext)
      
      
    }
    
    names(kCV2)<-c(paste("modele",k,sep=""))
    kCV<-c(kCV,kCV2)
    
    
    
    
    
  }  
  
  Mean<-c()
  
  for(j in 1:length(kCV))
    
  {
    
    Mean[j]<-mean(colMeans(kCV[[j]],na.rm=TRUE))
    
    kCV[[j]]<-list(kCV[[j]],Mean[!is.na(Mean)])
    
    Mean<-c()
    
  }
  
  
  for(j in 1:length(kCV))
    
  {
    Mean[j]<-kCV[[j]][[2]]
    
  }  
  
  return(list(kCV,mean(Mean,na.rm=TRUE)))
  
}



kCV<-cross_valid(v,r,3,10)



calculmodeleVAR<-function(vente,retour,coupe,debut=5,fin=12,lvl=0.95,method="kfolds",depart=12,n.ahead=(nrow(data.frame(vente,retour))-coupe+1))
  
{
  
  rn<-data.frame(vente,retour)
  N<-nrow(rn)
  rn1<-rn[1:coupe,]
  rn1.2<-rn[(coupe+1):(coupe+depart),]
  rn2<-rn[((coupe+depart+1):N),]
  rn3<-rbind(rn1,rn1.2,rn2)
  
  if((method=="kfolds")==TRUE)
    
  {
    COEFF<-choisircoeff1VAR(rn1)
    best<-pqdesminVAR(COEFF,length(COEFF[!is.na(COEFF)]))
    kCV<-c()
    k0<-0
    k<-1
    erreurinst<-c()
    erreurmoyenne<-c()
    modele<-c()
    up<-c()
    down<-c()
    
    while(k<=nrow(best))  
      
    {
      
      if((best[k,2]==0)==TRUE){try(kCV[k]<-cross_valid(rn1[,1],rn1[,2],best[k,1],NULL,debut,fin)[[2]])}
      if((best[k,2]==0)==FALSE){try(kCV[k]<-cross_valid(rn1[,1],rn1[,2],best[k,1],best[k,2],debut,fin)[[2]])}
      
      k<-k+1
      
    }
    
    k0<-which.min(kCV)
    
    if((best[k0,2]==0)==TRUE){try(var.test <- VAR(rn1, p = best[k0,1], type = "both",season =NULL, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")))}
    if((best[k0,2]==0)==FALSE){try(var.test <- VAR(rn1, p = best[k0,1], type ="both",season =best[k0,2], exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")))}
    
    
    try(p.var.test<-predict(var.test,n.ahead=N-coupe+1+n.ahead,ci=lvl))
    
    try
    
    (
      
      for(i in 1:coupe)
        
      {
        modele[i]<-rn1[i,2]   
        up[i]<-rn1[i,2]
        down[i]<-rn1[i,2]
        
      }  
    )
    
    try
    
    (
      
      for(i in ((coupe+1):(N-coupe+1+n.ahead)))
        
        
      {
        
        modele[i]<-as.vector(p.var.test$fcst[[2]][,1])[i-coupe]
        up[i]<-as.vector(p.var.test$fcst[[2]][,3])[i-coupe]
        down[i]<-as.vector(p.var.test$fcst[[2]][,2])[i-coupe]
        
      }
      
    )
    
    
    
    erreurinst<-abs(modele-rn3[,2])      
    erreurmoyenne<-mean(erreurinst[(coupe+1):N])
    
    result<-list(mod=NA,up=NA,down=NA,e=NA,ordre=NA,saison=NA,KFolds=NA)
    result[[1]]<-modele
    result[[2]]<-up
    result[[3]]<-down
    result[[4]]<-erreurmoyenne
    result[[5]]<-best[k0,1]
    result[[6]]<-best[k0,2]   
    result[[7]]<-kCV[k0] 
  }
  
  
  
  if((method=="leave-out")==TRUE)
    
  {
    
    
    COEFF<-choisircoeff1VAR(rn1)
    best<-pqdesminVAR(COEFF,length(COEFF[!is.na(COEFF)]))
    
    
    k<-1
    erreurinst<-c()
    erreurmoyenne<-c()
    modele<-matrix(data=c(0),ncol=length(COEFF[!is.na(COEFF)]),nrow=N)
    modele1<-matrix(data=c(NA),ncol=length(COEFF[!is.na(COEFF)]),nrow=N+n.ahead)
    up<-matrix(data=c(0),ncol=length(COEFF[!is.na(COEFF)]),nrow=N+n.ahead)
    down<-matrix(data=c(0),ncol=length(COEFF[!is.na(COEFF)]),nrow=N+n.ahead)
    
    while(k<=nrow(best))  
      
    {
      
      
      
      if((best[k,2]==0)==TRUE){try(var.test <- VAR(rn1, p = best[k,1], type = "both",season =NULL, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")))}
      
      if((best[k,2]==0)==FALSE){try(var.test <- VAR(rn1, p = best[k,1], type ="both",season =best[k,2], exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")))}
      
      try(p.var.test<-predict(var.test,n.ahead=(N-coupe+1+n.ahead)))
      
      try
      
      (
        
        for(i in 1:coupe)
          
        {
          modele[i,k]<-rn1[i,2]   
          up[i,k]<-rn1[i,2]
          down[i,k]<-rn1[i,2]
          
        }  
      )
      
      try
      
      (
        
        for(i in ((coupe+1):N))
          
          
        {
          
          modele[i,k]<-as.vector(p.var.test$fcst[[2]][,1])[i-coupe]
          up[i,k]<-as.vector(p.var.test$fcst[[2]][,3])[i-coupe]
          down[i,k]<-as.vector(p.var.test$fcst[[2]][,2])[i-coupe]
          
        }
        
      )
      
      try
      
      (
        
        for(i in ((coupe+1):(N+n.ahead)))
          
          
        {
          
          modele1[i,k]<-as.vector(p.var.test$fcst[[2]][,1])[i-coupe]
          up[i,k]<-as.vector(p.var.test$fcst[[2]][,3])[i-coupe]
          down[i,k]<-as.vector(p.var.test$fcst[[2]][,2])[i-coupe]
          
        }
        
      )
      
      try(erreurinst<-abs(modele[(coupe+1):(coupe+depart),k]-rn3[(coupe+1):(coupe+depart),2]) )     
      try(erreurmoyenne[k]<-mean(erreurinst)) 
      
      k<-k+1  
      var.test<-0
      
      
      
    }  
    
    result<-list(mod=NA,up=NA,down=NA,e=NA,ordre=NA,saison=NA)
    result[[1]]<-modele1[,which.min(erreurmoyenne)]
    result[[2]]<-up[,which.min(erreurmoyenne)]
    result[[3]]<-down[,which.min(erreurmoyenne)]
    result[[4]]<-erreurmoyenne[which.min(erreurmoyenne)]
    result[[5]]<-best[which.min(erreurmoyenne),1]
    result[[6]]<-best[which.min(erreurmoyenne),2]
    
    
    
    
    
    
    
  }  
  
  
  if((method=="AIC")==TRUE)
    
  {
    
    
    COEFF<-choisircoeff1VAR(rn1)
    best<-pqdesminVAR(COEFF,length(COEFF[!is.na(COEFF)]))
    
    
    k<-1
    erreurinst<-c()
    erreurmoyenne<-c()
    modele<-matrix(data=c(0),ncol=length(COEFF[!is.na(COEFF)]),nrow=N)
    modele1<-matrix(data=c(0),ncol=length(COEFF[!is.na(COEFF)]),nrow=N+n.ahead)
    up<-matrix(data=c(0),ncol=length(COEFF[!is.na(COEFF)]),nrow=N+n.ahead)
    down<-matrix(data=c(0),ncol=length(COEFF[!is.na(COEFF)]),nrow=N+n.ahead)

      
      
      if((best[k,2]==0)==TRUE){try(var.test <- VAR(rn1, p = best[k,1], type = "both",season =NULL, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")))}
      
      if((best[k,2]==0)==FALSE){try(var.test <- VAR(rn1, p = best[k,1], type ="both",season =best[k,2], exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")))}
      
      try(p.var.test<-predict(var.test,n.ahead=(N-coupe+1)))
      
      try
      
      (
        
        for(i in 1:coupe)
          
        {
          modele[i,k]<-rn1[i,2]   
          up[i,k]<-rn1[i,2]
          down[i,k]<-rn1[i,2]
          
        }  
      )
      
      try
      
      (
        
        for(i in ((coupe+1):N))
          
          
        {
          
          modele[i,k]<-as.vector(p.var.test$fcst[[2]][,1])[i-coupe]
          up[i,k]<-as.vector(p.var.test$fcst[[2]][,3])[i-coupe]
          down[i,k]<-as.vector(p.var.test$fcst[[2]][,2])[i-coupe]
          
        }
        
      )
    
    try
    
    (
      
      for(i in ((coupe+1):(N+n.ahead)))
        
        
      {
        
        modele1[i,k]<-as.vector(p.var.test$fcst[[2]][,1])[i-coupe]
        up[i,k]<-as.vector(p.var.test$fcst[[2]][,3])[i-coupe]
        down[i,k]<-as.vector(p.var.test$fcst[[2]][,2])[i-coupe]
        
      }
      
    )
      
      try(erreurinst<-abs(modele[(coupe+1):N,k]-rn3[(coupe+1):N,2]) )     
      try(erreurmoyenne[k]<-mean(erreurinst)) 
      
      k<-k+1  
      var.test<-0
      
      
      
      
    
    result<-list(mod=NA,up=NA,down=NA,e=NA,ordre=NA,saison=NA)
    result[[1]]<-modele1[,which.min(erreurmoyenne)]
    result[[2]]<-up[,which.min(erreurmoyenne)]
    result[[3]]<-down[,which.min(erreurmoyenne)]
    result[[4]]<-erreurmoyenne[which.min(erreurmoyenne)]
    result[[5]]<-best[which.min(erreurmoyenne),1]
    result[[6]]<-best[which.min(erreurmoyenne),2]
    
    
    
    
    
    
    
  }    
  
  
  
  return(result)
  
}






#-------------------------------------------------------------------Tests------------------------------------------------------------------------------------------




#-------------------------------------------------------------------Test maille client-----------------------------------------------------------------------------

#-------------------------------------------------------------------AVEZY------------------------------------------------------------------------------------------

v<-replace(liste.client$AVEZY[,3],which(is.na(liste.client$AVEZY[,3])),0)
r<-replace(liste.client$AVEZY[,2],which(is.na(liste.client$AVEZY[,2])),0)
plot(v,type='l',main="Easy Jet",xlab="mois",ylab="ventes")
lines(r,col='red')
cor(r,v)



mod2AVEZY<-calculmodele2(r,48)

plot(r,type='l',xlim=c(48,65),main="Easy Jet",xlab="mois",ylab="retours")
lines(mod2AVEZY$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2AVEZY$mod[48:61],80))*mod2AVEZY$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2AVEZY$mod[48:61],80))*mod2AVEZY$mod,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 1","20%"),col=c('black','red','red'),lty=c(1,1,2))



var<-calculmodeleVAR(v,r,48,5,7,0.70)

plot(r,type='l',xlim=c(48,65),main="AVEZY")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,type='l',xlim=c(48,65),main="AVEZY")
lines(var$mod,col="red")
lines(mod2AVEZY$mod,col="blue")
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

impulsion<-irf(VAR(data.frame(v,r), p = var$ordre, type = "both",season =var$saison, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")),impulse='v')
plot(impulsion$irf$v[,2],type='l',main="Impulsion vente AVAFR",ylab="Intensité",xlab="Months")

#-------------------------------------------------------------------AVAFR-------------------------------------------------------------

v<-replace(liste.client$AVAFR[,3],which(is.na(liste.client$AVAFR[,3])),0)
r<-replace(liste.client$AVAFR[,2],which(is.na(liste.client$AVAFR[,2])),0)
plot(r,type='l',main="Air France",xlim=c(11,65),xlab="Months",ylab="Number of tires")
legend("topleft",legend=c("Casing returns","Sales"),col=c('black','red'),lty=c(1,1,2))
lines(v,col='red')



cor(r,v)



mod2AVAFR<-calculmodele2(r,48)

plot(r,type='l',xlim=c(40,65),main="Air France",xlab="months",ylab="Casing returns")
lines(mod2AVAFR$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2AVAFR$mod[48:61],80))*mod2AVAFR$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2AVAFR$mod[48:61],80))*mod2AVAFR$mod,col="red",lty=2)
legend("topleft",legend=c("Actual","ARIMA Model (1)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))



var<-calculmodeleVAR(v,r,48,6,12,0.70)

plot(r,type='l',xlim=c(40,65),main="Air France",xlab="months",ylab="Casing returns")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Actual","VAR Model (2)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))

plot(r,type='l',xlim=c(48,65),main="AVAFR")
lines(var$mod,col="red")
lines(mod2AVAFR$mod,col="blue")
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

impulsion<-irf(VAR(data.frame(v,r), p = var$ordre, type = "both",season =var$saison, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")),impulse='v')
plot(impulsion$irf$v[,2],type='l',main="Impulsion vente AVAFR",ylab="Intensité",xlab="Months")

#-------------------------------------------------------------------AVUSA-------------------------------

v<-replace(liste.client$AVUSA[,3],which(is.na(liste.client$AVUSA[,3])),0)
r<-replace(liste.client$AVUSA[,2],which(is.na(liste.client$AVUSA[,2])),0)
plot(v,type='l',main="AVUSA")
lines(r,col='red')
cor(r,v)



mod2AVUSA<-calculmodele2(r,48)

plot(r,type='l',xlim=c(40,65),main="US Airways",xlab="months",ylab="Casing returns")
lines(mod2AVUSA$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2AVUSA$mod[48:61],80))*mod2AVUSA$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2AVUSA$mod[48:61],80))*mod2AVUSA$mod,col="red",lty=2)
legend("topleft",legend=c("Actual","ARIMA Model (1)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))



var<-calculmodeleVAR(vente=v,retour=r,coupe=48,method="leave-out")

plot(r,type='l',xlim=c(48,65),main="AVUSA")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,type='l',xlim=c(48,65),main="AVUSA")
lines(v,col='purple')
lines(var$mod,col="red")
lines(mod2AVUSA$mod,col="blue")
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

impulsion<-irf(VAR(data.frame(v,r), p = var$ordre, type = "both",season =var$saison, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")),impulse='v')
plot(impulsion$irf$v[,2],type='l',main="Impulsion vente AVUSA")

#-------------------------------------------------------------------AVBAW------------------------------------------------------------------------------------------

v<-replace(liste.client$AVBAW[,3],which(is.na(liste.client$AVBAW[,3])),0)
r<-replace(liste.client$AVBAW[,2],which(is.na(liste.client$AVBAW[,2])),0)
plot(r,type='l',main="British Airways",xlab="Months",ylab="Number of tires")
lines(v,col='red')
legend("topleft",legend=c("Casing returns","Sales"),col=c('black','red'),lty=c(1,1,2))
cor(r,v)



mod2AVBAW<-calculmodele2(r,48)

plot(r,type='l',xlim=c(48,65),main="AVBAW")
lines(mod2AVBAW$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2AVBAW$mod[48:61],80))*mod2AVBAW$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2AVBAW$mod[48:61],80))*mod2AVBAW$mod,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 1","20%"),col=c('black','red','red'),lty=c(1,1,2))



var<-calculmodeleVAR(v,r,48,6,12,0.70)

plot(r,type='l',xlim=c(48,65),main="AVBAW")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,type='l',xlim=c(48,65),main="AVBAW")
lines(v,col='purple')
lines(var$mod,col="red")
lines(mod2AVBAW$mod,col="blue")
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

impulsion<-irf(VAR(data.frame(v,r), p = var$ordre, type = "both",season =var$saison, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")),impulse='v')
plot(impulsion$irf$v[,2],type='l',main="Impulsion vente British airways",xlab="mois",ylab="Intensité")




#-------------------------------------------------------------------Test maille PN----------------------------------------------------------------------------------

#-------------------------------------------------------------------M01103------------------------------------------------------------------------------------------

v<-replace(liste.pn$M01103[,3],which(is.na(liste.pn$M01103[,3])),0)
r<-replace(liste.pn$M01103[,2],which(is.na(liste.pn$M01103[,2])),0)
plot(v,type='l',main="M01103")
lines(r,col='red')
cor(r,v)



mod2M01103<-calculmodele2(r,48)

plot(r,type='l',xlim=c(40,65),main="M01103",xlab="months",ylab="Casing returns")
lines(mod2M01103$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2M01103$mod[48:61],80))*mod2M01103$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2M01103$mod[48:61],80))*mod2M01103$mod,col="red",lty=2)
legend("topleft",legend=c("Actual","ARIMA Model (1)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))

# write.csv(data.frame(r,mod2M01103$mod)),file="donnees",row.names = TRUE)


var<-calculmodeleVAR(vente=v,retour=r,coupe=48,method="leave-out")



plot(r,type='l',xlim=c(40,65),main="M01103",xlab="months",ylab="Casing returns")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Actual","VAR Model (2)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))

impulsion<-irf(VAR(data.frame(v,r), p = var$ordre, type = "both",season =var$saison, exogen = NULL, lag.max = NULL,ic = c("AIC", "HQ", "SC", "FPE")),impulse='v')
plot(impulsion$irf$v[,2],type='l',main="Impulsion vente M01103")

#-------------------------------------------------------------------M08201------------------------------------------------------------------------------------------


v<-replace(liste.pn$M08201[,3],which(is.na(liste.pn$M08201[,3])),0)
r<-replace(liste.pn$M08201[,2],which(is.na(liste.pn$M08201[,2])),0)
plot(v,type='l',main="M08201")
lines(r,col='red')
cor(r,v)



mod2M08201<-calculmodele2(r,48)

plot(r,type='l',xlim=c(48,65),main="M08201")
lines(mod2M08201$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2M08201$mod[48:61],80))*mod2M08201$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2M08201$mod[48:61],80))*mod2M08201$mod,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 1","20%"),col=c('black','red','red'),lty=c(1,1,2))



var<-calculmodeleVAR(v,r,48,5,7,0.70)

plot(r,type='l',xlim=c(48,65),main="M08201")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,type='l',xlim=c(48,65),main="M08201")
lines(var$mod,col="red")
lines(mod2M08201$mod,col="blue")
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))





#-------------------------------------------------------------------Test maille client/PN---------------------------------------------------------------------------

#-------------------------------------------------------------------AVAFR M01103------------------------------------------------------------------------------------


v<-replace(liste.pn.client$AVAFR$M01103[,3],which(is.na(liste.pn.client$AVAFR$M01103[,3])),0)
r<-replace(liste.pn.client$AVAFR$M01103[,2],which(is.na(liste.pn.client$AVAFR$M01103[,2])),0)
plot(v,type='l',main="M01103-AVAFR")
lines(r,col='red')
cor(r,v)



mod2M01103AVAFR<-calculmodele2(r,48)

plot(r,type='l',xlim=c(40,65),main="M01103-Air France")
lines(mod2M01103AVAFR$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2M01103AVAFR$mod[48:61],80))*mod2M01103AVAFR$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2M01103AVAFR$mod[48:61],80))*mod2M01103AVAFR$mod,col="red",lty=2)
legend("topleft",legend=c("Actual","ARIMA Model (1)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))



var<-calculmodeleVAR(v,r,48,5,7,0.70)

plot(r,type='l',xlim=c(48,65),main="M01103AVAFR")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,type='l',xlim=c(48,65),main="M01103AVAFR")
lines(var$mod,col="red")
lines(mod2M01103AVAFR$mod,col="blue")
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

#-------------------------------------------------------------------AVEZY M08201-------------------------------------------------------------------------------------

v<-replace(liste.pn.client$AVEZY$M08201[,3],which(is.na(liste.pn.client$AVEZY$M08201[,3])),0)
r<-replace(liste.pn.client$AVEZY$M08201[,2],which(is.na(liste.pn.client$AVEZY$M08201[,2])),0)
plot(v,type='l',main="AVEZY-M08201")
lines(r,col='red')
cor(r,v)



mod2M08201AVEZY<-calculmodele2(r,48)

plot(r,type='l',xlim=c(48,65),main="M08201AVEZY")
lines(mod2M08201AVEZY$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2M08201AVEZY$mod[48:61],80))*mod2M08201AVEZY$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2M08201AVEZY$mod[48:61],80))*mod2M08201AVEZY$mod,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 1","20%"),col=c('black','red','red'),lty=c(1,1,2))



var<-calculmodeleVAR(v,r,48,5,7,0.70)

plot(r,type='l',xlim=c(48,65),main="M08201AVEZY")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,type='l',xlim=c(48,65),main="M08201AVEZY")
lines(var$mod,col="red")
lines(mod2M08201AVEZY$mod,col="blue")
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

#-------------------------------------------------------------------AVUSA M01103------------------------------------------------------------------------------------

v<-replace(liste.pn.client$AVUSA$M01103[,3],which(is.na(liste.pn.client$AVUSA$M01103[,3])),0)
r<-replace(liste.pn.client$AVUSA$M01103[,2],which(is.na(liste.pn.client$AVUSA$M01103[,2])),0)
plot(v,type='l',main="M01103-AVUSA",xlab="mois",ylab="ventes")
lines(r,col='red')
cor(r,v)



mod2M01103AVUSA<-calculmodele2(r,48)

plot(r,type='l',xlim=c(48,65),main="M01103AVUSA")
lines(mod2M01103AVUSA$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2M01103AVUSA$mod[48:61],80))*mod2M01103AVUSA$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2M01103AVUSA$mod[48:61],80))*mod2M01103AVUSA$mod,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 1","20%"),col=c('black','red','red'),lty=c(1,1,2))



var<-calculmodeleVAR(v,r,48,5,7,0.70)

plot(r,type='l',xlim=c(48,65),main="M01103AVUSA")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,type='l',xlim=c(48,65),main="M01103AVUSA")
lines(var$mod,col="red")
lines(mod2M01103AVUSA$mod,col="blue")
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

plot(r,type='l',col='blue')
lines(v)





v<-replace(liste.client$AVUSA[,3],which(is.na(liste.client$AVUSA[,3])),0)
r<-replace(liste.client$AVUSA[,2],which(is.na(liste.client$AVUSA[,2])),0)
plot(v,type='l',main="US Airways",xlab="mois",ylab="ventes")
lines(r,col='red')

xshort <- r[30:65]
yshort <- v[30:65]

var<-NULL
var<-calculmodeleVAR(yshort,xshort,coupe=24,method="leave-out")


plot(as.vector(xshort),type='l',main="AVEZY")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
lines(yshort,col='purple')



#-------------------------------------------------------------------Test maille PN/Zone-----------------------------------------------------------------------------

#-------------------------------------------------------------------M01103 Norwood----------------------------------------------------------------------------------

v<-replace(liste.pn.site$M01103$NWD[,3],which(is.na(liste.pn.site$M01103$NWD[,3])),0)
r<-replace(liste.pn.site$M01103$NWD[,2],which(is.na(liste.pn.site$M01103$NWD[,2])),0)
plot(v,type='l',main="M01103-NWD")
lines(r,col='red')
cor(r,v)



mod2M01103NWD<-calculmodele2(r,53)


plot(r,type='l',xlim=c(50,65),ylim=range(c(v)),main="M01103-Norwood")
lines(mod2M01103NWD$mod,col="red")
lines((1-intervalleconfiance(r[49:61],mod2M01103NWD$mod[49:61],80))*mod2M01103NWD$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[49:61],mod2M01103NWD$mod[49:61],80))*mod2M01103NWD$mod,col="red",lty=2)
legend("topleft",legend=c("Actual","ARIMA Model (1)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))


var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=53,method="leave-out")

plot(r,type='l',xlim=c(48,65),main="M01103-Norwood")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,ylim=range(c(v)),type='l',xlim=c(48,65),main="M01103-Norwood")
lines(var$mod,col="red")
lines(mod2M01103NWD$mod,col="blue")
lines(v,col='purple')
legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))


limit1<-c()
limit2<-c()

for(i in 1:length(v))
{
limit1[i]<-mean(v)-1.96*sd(v)
limit2[i]<-mean(v)+1.96*sd(v)
}

plot(v,type='l')
lines(limit1,col='red')
lines(limit2,col='red')

#-------------------------------------------------------------------M01103 Bourges---------------------------------------------------------------


v<-replace(liste.pn.site$M01103$URG[,3],which(is.na(liste.pn.site$M01103$URG[,3])),0)
r<-replace(liste.pn.site$M01103$URG[,2],which(is.na(liste.pn.site$M01103$URG[,2])),0)
plot(v,type='l',main="M01103-URG")
lines(r,col='red')
cor(r,v)

limit1<-c()
limit2<-c()

for(i in 1:length(v))
{
  limit1[i]<-mean(v)-1.96*sd(v)
  limit2[i]<-mean(v)+1.96*sd(v)
}

plot(v,type='l')
lines(limit1,col='red')
lines(limit2,col='red')



mod2M01103URG<-calculmodele2(r,53)


plot(r,type='l',xlim=c(50,65),ylim=range(c(v)),main="M01103-Norwood")
lines(mod2M01103URG$mod,col="red")
lines((1-intervalleconfiance(r[49:61],mod2M01103URG$mod[49:61],80))*mod2M01103URG$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[49:61],mod2M01103URG$mod[49:61],80))*mod2M01103URG$mod,col="red",lty=2)
legend("topleft",legend=c("Actual","ARIMA Model (1)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))


var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=49,method="leave-out")

plot(r,type='l',xlim=c(48,65),main="M01103-Norwood")
lines(var$mod,col="red")
lines(smooth.spline(c(1:65),var$mod),col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,ylim=range(c(v)),type='l',xlim=c(48,65),main="M01103-Norwood")
lines(var$mod,col="red")
lines(mod2M01103URG$mod,col="blue")
lines(v,col='purple')
lines(limit1,col='purple')
lines(limit2,col='purple')


legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

#-------------------------------------------------------------------M12301 Bourges----------------------------------------------------------------------------------

v<-replace(liste.pn.site$M12301$URG[,3],which(is.na(liste.pn.site$M12301$URG[,3])),0)
r<-replace(liste.pn.site$M12301$URG[,2],which(is.na(liste.pn.site$M12301$URG[,2])),0)
plot(v,type='l',main="M12301-URG")
lines(r,col='red')
cor(r,v)

limit1<-c()
limit2<-c()

for(i in 1:length(v))
{
  limit1[i]<-mean(v)-1.96*sd(v)
  limit2[i]<-mean(v)+1.96*sd(v)
}

plot(v,type='l')
lines(limit1,col='red')
lines(limit2,col='red')



mod2M12301URG<-calculmodele2(r,48)


plot(r,type='l',xlim=c(45,65),ylim=range(c(v,r)),main="M12301-Bourges")
lines(mod2M12301URG$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2M12301URG$mod[48:61],80))*mod2M12301URG$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2M12301URG$mod[48:61],80))*mod2M12301URG$mod,col="red",lty=2)
legend("topleft",legend=c("Actual","ARIMA Model (1)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))


var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=48,method="leave-out")

plot(r,type='l',xlim=c(45,65),main="M12301-Norwood")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,ylim=range(c(v,r)),type='l',xlim=c(48,65),main="M12301-Bourges")
lines(var$mod,col="red")
lines(mod2M12301URG$mod,col="blue")
lines(v,col='purple')
lines(limit1,col='purple')
lines(limit2,col='purple')


legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

#-------------------------------------------------------------------M12301 Norwood-----------------------------------------------------------------------------------

v<-replace(liste.pn.site$M12301$NWD[,3],which(is.na(liste.pn.site$M12301$NWD[,3])),0)
r<-replace(liste.pn.site$M12301$NWD[,2],which(is.na(liste.pn.site$M12301$NWD[,2])),0)
plot(v,type='l',main="M12301-NWD")
lines(r,col='red')
cor(r,v)

limit1<-c()
limit2<-c()

for(i in 1:length(v))
{
  limit1[i]<-mean(v)-1.96*sd(v)
  limit2[i]<-mean(v)+1.96*sd(v)
}

plot(v,type='l')
lines(limit1,col='red')
lines(limit2,col='red')



limit1<-c()
limit2<-c()

for(i in 1:length(v))
{
  limit1[i]<-mean(r)-1.96*sd(r)
  limit2[i]<-mean(r)+1.96*sd(r)
}

plot(r,type='l')
lines(limit1,col='red')
lines(limit2,col='red')



mod2M12301NWD<-calculmodele2(r,48)


plot(r,type='l',xlim=c(45,65),ylim=range(c(v,r)),main="M12301-Norwood")
lines(mod2M12301NWD$mod,col="red")
lines((1-intervalleconfiance(r[48:61],mod2M12301NWD$mod[48:61],80))*mod2M12301NWD$mod,col="red",lty=2)
lines((1+intervalleconfiance(r[48:61],mod2M12301NWD$mod[48:61],80))*mod2M12301NWD$mod,col="red",lty=2)
legend("topleft",legend=c("Actual","ARIMA Model (1)","Confidence Interval 80%"),col=c('black','red','red'),lty=c(1,1,2))


var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=48,method="leave-out")

plot(r,type='l',xlim=c(45,65),main="M12301-Norwood")
lines(var$mod,col="red")
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
legend("topleft",legend=c("Retours","Modèle 2","20%"),col=c('black','red','red'),lty=c(1,1,2))


plot(r,ylim=range(c(v,r)),type='l',xlim=c(48,65),main="M12301-Norwood")
lines(var$mod,col="red")
lines(mod2M12301NWD$mod,col="blue")
lines(v,col='purple')
lines(limit1,col='purple')
lines(limit2,col='purple')


legend("topleft",legend=c("Retours","Modèle 2","Modèle 1"),col=c('black','red','blue'),lty=c(1,1,1))

indice<-c(1:length(r))
R<-as.matrix(r)
R


#-------------------------------------------------------------------Valeurs abbérantes--------------------------------------------------

#-------------------------------------------------------------------Recherche d'un critère----------------------------------------------




intervalle.sigma<-function(xreel,epsilon)
  
{
  
  k<-1
  
  while ( (occurence(xreel,mean(xreel)-k*sd(xreel),mean(xreel)+k*sd(xreel))>=epsilon)==FALSE )
    
  {
    
    
    k<-k+0.05
    
    
  }
  
  
  return(k)
  
  
  
}  

#Maille pneu



y1<-c()
z1<-c()
for(i in 2: length(liste.pn))
  
{
  
  compteur<-0
  
  for(j in 1:length(liste.pn[[i]][,3]))
    
  {
  
    if( (replace(liste.pn[[i]][,3],which(is.na(liste.pn[[i]][,3])),0)[j]==0)==TRUE ){compteur<-compteur+1}
    
  }
  
  if((compteur<5)==TRUE)
    
  {
    
  y1[i]<-intervalle.sigma(replace(liste.pn[[i]][,3],which(is.na(liste.pn[[i]][,3])),0),70)
  z1[i]<-intervalle.sigma(replace(liste.pn[[i]][,3],which(is.na(liste.pn[[i]][,3])),0),95)
  
  }
  
  
}

Y1=mean(y1,na.rm=TRUE)
Z1=mean(z1,na.rm=TRUE)


#Maille client


y2<-c()
z2<-c()
for(i in 1: length(liste.client))
  
{
  
  compteur<-0
  
  if( (class(try(liste.client[[i]][,3]))=="try-error")==FALSE )
    
  {
  
  for(j in 1:length(liste.client[[i]][,3]))
    
  {
    
    if( (replace(liste.client[[i]][,3],which(is.na(liste.client[[i]][,3])),0)[j]==0)==TRUE ){compteur<-compteur+1}
    
  }
  

  
    if((compteur<5)==TRUE)
    
  {
    
    y2[i]<-intervalle.sigma(replace(liste.client[[i]][,3],which(is.na(liste.client[[i]][,3])),0),70)
    z2[i]<-intervalle.sigma(replace(liste.client[[i]][,3],which(is.na(liste.client[[i]][,3])),0),95)
    
  }
  
  }
  
}

y2
z2

Y2=mean(y2,na.rm=TRUE)
Z2=mean(z2,na.rm=TRUE)

#Maille client/pn



t<-c()

for(k  in 1:length(liste.pn.client))
  
{
  
  
  t[k]<-length(liste.pn.client[[k]])
  
  
}


y3<-matrix(data=NA,nrow=length(liste.pn.client),ncol=max(t))
z3<-matrix(data=NA,nrow=length(liste.pn.client),ncol=max(t))

for(i in 1: length(liste.pn.client))
  
{

for(k in 1:length(liste.pn.client[[k]]))  
  
{
  compteur<-0
  
  if( (class(try(liste.pn.client[[i]][[k]][,3]))=="try-error")==FALSE )
    
  {
    
    for(j in 1:length(liste.pn.client[[i]][[k]][,3]))
      
    {
      
      if( (replace(liste.pn.client[[i]][[k]][,3],which(is.na(liste.pn.client[[i]][[k]][,3])),0)[j]==0)==TRUE ){compteur<-compteur+1}
      
    }
    
    
    
    if((compteur<5)==TRUE)
      
    {
      
      y3[i,k]<-intervalle.sigma(replace(liste.pn.client[[i]][[k]][,3],which(is.na(liste.pn.client[[i]][[k]][,3])),0),70)
      z3[i,k]<-intervalle.sigma(replace(liste.pn.client[[i]][[k]][,3],which(is.na(liste.pn.client[[i]][[k]][,3])),0),95)
      
    }
    
  }

}  
}

y3
z3

Y3=mean(colMeans(y3,na.rm=TRUE),na.rm=TRUE)
Z3=mean(colMeans(z3,na.rm=TRUE),na.rm=TRUE)



#Maille pn/zone



t<-c()

for(k  in 1:length(liste.pn.site))
  
{
  
  
  t[k]<-length(liste.pn.site[[k]])
  
  
}


y4<-matrix(data=NA,nrow=length(liste.pn.site),ncol=max(t))
z4<-matrix(data=NA,nrow=length(liste.pn.site),ncol=max(t))

for(i in 1: length(liste.pn.site))
  
{
  
  for(k in 1:length(liste.pn.site[[k]]))  
    
  {
    compteur<-0
    
    if( (class(try(liste.pn.site[[i]][[k]][,3]))=="try-error")==FALSE )
      
    {
      
      for(j in 1:length(liste.pn.site[[i]][[k]][,3]))
        
      {
        
        if( (replace(liste.pn.site[[i]][[k]][,3],which(is.na(liste.pn.site[[i]][[k]][,3])),0)[j]==0)==TRUE ){compteur<-compteur+1}
        
      }
      
      
      
      if((compteur<5)==TRUE)
        
      {
        
        y4[i,k]<-intervalle.sigma(replace(liste.pn.site[[i]][[k]][,3],which(is.na(liste.pn.site[[i]][[k]][,3])),0),70)
        z4[i,k]<-intervalle.sigma(replace(liste.pn.site[[i]][[k]][,3],which(is.na(liste.pn.site[[i]][[k]][,3])),0),95)
        
      }
      
    }
    
  }  
}

y4
z4

Y4=mean(colMeans(y4,na.rm=TRUE),na.rm=TRUE)
Z4=mean(colMeans(z4,na.rm=TRUE),na.rm=TRUE)

#-------------------------------------------------------------------Critères limites------------------------------------------------------

limite_extreme<-function(vec,type)
  
{
  
  result<-c()
  
  if(type=="sup")
    
  {
    for (i in 1:length(vec))
      
    {
      
    result[i]<-mean(vec)+0.25*(Z1+Z2+Z3+Z4)*sd(vec)
      
    }
      
  }
  
  if(type=="inf")
    
  {
    for (i in 1:length(vec))
      
    {
      
      result[i]<-mean(vec)-0.25*(Z1+Z2+Z3+Z4)*sd(vec)
      
    }
    
  }
  
  return(result)
  
}

limite_erreur<-function(vec,type)
  
{
  
  result<-c()
  
  if(type=="sup")
    
  {
    for (i in 1:length(vec))
      
    {
      
      result[i]<-mean(vec)+0.25*(Y1+Y2+Y3+Y4)*sd(vec)
      
    }
    
  }
  
  if(type=="inf")
    
  {
    for (i in 1:length(vec))
      
    {
      
      result[i]<-mean(vec)-0.25*(Y1+Y2+Y3+Y4)*sd(vec)
      
    }
    
  }
  
  return(result)
  
}

#-------------------------------------------------------------------Vecteur des mois----------------------------------------------------

quel.mois<-function(vec)
  
{
 
  return(rep_len(1:12, length.out=length(vec)))
  
  
}


valeur.mois<-function(vec,mois)
  
{
  result<-c()
  
  
  for (i in 1:length(vec))
    
  {
   
      if((quel.mois(vec)[i]==mois)==TRUE)
        
      {
          
      result[i]<-vec[i]
    
      }
    
  }
  
  
  
  return(result[!is.na(result)])
  
  
}

plot(valeur.mois(r,12),type='l',ylim=range(c(limite(valeur.mois(r,12),"sup"),limite(valeur.mois(r,12),"inf"))))
lines(limite_extreme(valeur.mois(r,12),"sup"),col='red')
lines(limite_extreme(valeur.mois(r,12),"inf"),col='red')

#-------------------------------------------------------------------Compare à un mois----------------------------------------------------

compare.mois<-function(vec,valeur)
  
{
  result<-"Normal"
 
  
  if(((valeur>=limite_extreme(vec,"sup")[1]) | (valeur<=limite_extreme(vec,"inf")[1])) ==TRUE){result<- "Très attention"}
  if((((valeur>=limite_erreur(vec,"sup")[1]) | (valeur<=limite_erreur(vec,"inf")[1])) & !((valeur>=limite_extreme(vec,"sup")[1]) | (valeur<=limite_extreme(vec,"inf")[1]))) ==TRUE){result<- "Attention"}
  
  
  
  return(result)
    
}



pourcentage.valeur.risque<-function(v)
  
{
  
  k<-0
  l<-0
  valeursk<-c()
  valeursl<-c()
  
  
  result<-list(Valeur.aberrante=list(Pourcentage=NA,Valeur=NA),Valeur.très.aberrante=list(Pourcentage=NA,Valeur=NA))
  
  for(i in 1:length(v))
    
    
    
  {
    
    
    if((compare.mois(v,v[i])=="Attention")==TRUE)
      
    {
      
      k<-k+1
      valeursk<-c(valeursk,v[i])
      result[[1]][[2]]<-valeursk
    
    }
    
    if((compare.mois(v,v[i])=="Très attention")==TRUE)
      
    {
      l<-l+1
      valeursl<-c(valeursl,v[i])
      result[[2]][[2]]<-valeursl
      
    }
    
   
  }
  
  result[[1]][[1]]<-100*(k/length(v))
  result[[2]][[1]]<-100*(l/length(v))
  
  return(result)
  
}


risque<-pourcentage.valeur.risque(v)

#-------------------------------------------------------------------Fonction plot les points aberrants------------------------------------




point.aberrant<-function(v,y1=min(w[w>1]),y2=max(w),debut=50,fin=65,main="Main",xlab="xlab",ylab="ylab")
  
{

  
  
risque<-pourcentage.valeur.risque(v)
  
z=w=lim1=lim2=lim3=lim4=c()
for(i in 1:length(v))
  
{
  
  if( (!is.na(match(v[i],risque$Valeur.aberrante$Valeur))  & is.na(match(v[i],risque$Valeur.très.aberrante$Valeur))) ){w[i]<-v[i]}
  
  
}

for(i in 1:length(v))
  
{
  
  if(!is.na(match(v[i],risque$Valeur.très.aberrante$Valeur))){z[i]<-v[i]}
  
  
}


  lim1<-limite_extreme(v,"sup")
  lim2<-limite_extreme(v,"inf")
  lim3<-limite_erreur(v,"sup")
  lim4<-limite_erreur(v,"inf")
  



plot(w,col='blue',ylim=c(y1,y2),lwd=4,xlim=c(debut,fin),main=main,xlab=xlab,ylab=ylab)
points(z,col='red',pch=24,lwd=4)
lines(v)
lines(v,xlim=c(debut,fin),type='o')
lines(lim1,col='red')
lines(lim2,col='red')
lines(lim3,col='purple')
lines(lim4,col='purple')

ticks <- seq(0, length(v), 10)       # sequence for ticks and labels
axis(1, at = ticks,         # y-Axis
     labels = ticks)
box()   



}

#-------------------------------------------------------------------Etalon de stabilité-------------------------------------------------------------------------------

v<-replace(liste.client$AVEZY[,3],which(is.na(liste.client$AVEZY[,3])),0)
smooth=smooth.spline(c(1:65),v)

plot(v,type='l',main="Easy Jet",xlab="mois",ylab="ventes")
lines(smooth$y,col='red',lwd=3)

pourcentage.valeur.risque(smooth$y)
point.aberrant(smooth$y,y1=250,y2=700,debut=1,fin=65)


v<-replace(liste.pn$M01103[,3],which(is.na(liste.pn$M01103[,3])),0)
smooth=smooth.spline(c(1:65),v)

plot(v,type='l',main="Easy Jet",xlab="mois",ylab="ventes")
lines(smooth$y,col='red',lwd=3)

pourcentage.valeur.risque(smooth$y)
point.aberrant(smooth$y-tendance,y1=-700,y2=700,debut=1,fin=65)

tendance<-lm( smooth$y ~ c(1:65))$fitted.values










v<-replace(liste.client$AVUSA[,3],which(is.na(liste.client$AVUSA[,3])),0)
r<-replace(liste.client$AVUSA[,2],which(is.na(liste.client$AVUSA[,2])),0)
smooth=smooth.spline(c(1:65),r)
tendance<-lm( smooth$y ~ c(1:65))$fitted.values
var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=49,method="leave-out")

par(mfrow=c(1,1))

pourcentage.valeur.risque(r)
point.aberrant(r,y1=450,y2=1800,debut=1,fin=65)
lines(var$mod,col='red',lty=2,ltw=3)
lines(mod2AVUSA$mod,col="blue",lty=2,ltw=3)

point.aberrant(v,y1=450,y2=2500,debut=1,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
legend("topleft",legend=c("Actual","Limite valeurs habituelles","Limite valeurs anormales","Valeurs inhabituelles","Valeurs très inhabituelles"),col=c('black','purple','red','blue','red'),pch=c(1,1,1,20,24))

#-------------------------------------------------------------------Tests----------------------------------------------------------------

v<-replace(liste.pn$M01103[,3],which(is.na(liste.pn$M01103[,3])),0)
r<-replace(liste.pn$M01103[,2],which(is.na(liste.pn$M01103[,2])),0)

var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=49,method="leave-out")

par(mfrow=c(2,1))

point.aberrant(r,y1=min(r),y2=max(r),debut=1,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
legend("topleft",legend=c("Actual","Limite valeurs habituelles","Limite valeurs anormales","Valeurs inhabituelles","Valeurs très inhabituelles"),col=c('black','purple','red','blue','red'),pch=c(1,1,1,20,24))

point.aberrant(r,y1=min(r),y2=max(r),debut=49,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(var$mod,col="red",lty=4)


v<-replace(liste.client$AVAFR[,3],which(is.na(liste.client$AVAFR[,3])),0)
r<-replace(liste.client$AVAFR[,2],which(is.na(liste.client$AVAFR[,2])),0)

var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=49,method="leave-out")

par(mfrow=c(2,1))

point.aberrant(r,y1=min(r),y2=max(r),debut=1,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
legend("topleft",legend=c("Actual","Limite valeurs habituelles","Limite valeurs anormales","Valeurs inhabituelles","Valeurs très inhabituelles"),col=c('black','purple','red','blue','red'),pch=c(1,1,1,20,24))

point.aberrant(r,y1=min(r),y2=max(r),debut=49,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(var$mod,col="red",lty=4)


v<-replace(liste.pn.site$M12301$NWD[,3],which(is.na(liste.pn.site$M12301$NWD[,3])),0)
r<-replace(liste.pn.site$M12301$NWD[,2],which(is.na(liste.pn.site$M12301$NWD[,2])),0)

var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=49,method="leave-out")

par(mfrow=c(2,1))

point.aberrant(r,y1=min(r),y2=max(r),debut=1,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
legend("topleft",legend=c("Actual","Limite valeurs habituelles","Limite valeurs anormales","Valeurs inhabituelles","Valeurs très inhabituelles"),col=c('black','purple','red','blue','red'),pch=c(1,1,1,20,24))

point.aberrant(r,y1=min(r),y2=max(r),debut=49,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(var$mod,col="red",lty=4)


v<-replace(liste.pn.site$M12301$URG[,3],which(is.na(liste.pn.site$M12301$URG[,3])),0)
r<-replace(liste.pn.site$M12301$URG[,2],which(is.na(liste.pn.site$M12301$URG[,2])),0)

var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=48,method="leave-out")

par(mfrow=c(2,1))

point.aberrant(r,y1=min(r),y2=max(r),debut=1,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
legend("topleft",legend=c("Actual","Limite valeurs habituelles","Limite valeurs anormales","Valeurs inhabituelles","Valeurs très inhabituelles"),col=c('black','purple','red','blue','red'),pch=c(1,1,1,20,24))

point.aberrant(r,y1=min(r),y2=max(r),debut=48,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(var$mod,col="red",lty=4)
#lines(v,col='green')



v<-replace(liste.client$AVDLH[,3],which(is.na(liste.client$AVDLH[,3])),0)
r<-replace(liste.client$AVDLH[,2],which(is.na(liste.client$AVDLH[,2])),0)

var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=57,method="leave-out")

par(mfrow=c(2,1))

point.aberrant(r,y1=min(r),y2=max(r),debut=1,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
legend("topleft",legend=c("Actual","Limite valeurs habituelles","Limite valeurs anormales","Valeurs inhabituelles","Valeurs très inhabituelles"),col=c('black','purple','red','blue','red'),pch=c(1,1,1,20,24))

point.aberrant(r,y1=min(r),y2=max(r),debut=57,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(var$mod,col="red",lty=4)
lines(var$up,col="red",lty=2)
lines(var$down,col="red",lty=2)
#lines(v,col='green')



v<-replace(liste.client$AVDLH[,3],which(is.na(liste.client$AVDLH[,3])),0)
r<-replace(liste.client$AVDLH[,2],which(is.na(liste.client$AVDLH[,2])),0)

var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=57,method="leave-out")
mod2AVDLH<-calculmodele2(r,57)


par(mfrow=c(2,1))

point.aberrant(r,y1=min(r),y2=max(r),debut=1,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
legend("topleft",legend=c("Actual","Limite valeurs habituelles","Limite valeurs anormales","Valeurs inhabituelles","Valeurs très inhabituelles"),col=c('black','purple','red','blue','red'),pch=c(1,1,1,20,24))

point.aberrant(r,y1=min(r),y2=max(r),debut=57,fin=65,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(var$mod,col="red",lty=4)
lines(mod2AVDLH$mod,col="blue",lty=4)
#lines(var$up,col="red",lty=2)
#lines(var$down,col="red",lty=2)
#lines(v,col='green')

v<-replace(liste.pn.site$M01103$URG[,3],which(is.na(liste.pn.site$M01103$URG[,3])),0)
r<-replace(liste.pn.site$M01103$URG[,2],which(is.na(liste.pn.site$M01103$URG[,2])),0)

var=NULL
var<-calculmodeleVAR(vente=v,retour=r,coupe=48,method="leave-out",depart=12,n.ahead=25)
mod2M01103URG<-calculmodele2(r,48,depart=12,n.ahead=25)




par(mfrow=c(1,1))

point.aberrant(r,y1=min(r),y2=max(r),debut=1,fin=65+10,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(var$mod,col="red",lty=2)
lines(mod2M01103URG$mod,col="blue",lty=2)
#lines(v,col='green')

#-------------------------------------------------------------------Préparation des données VB-----------------------------------------


r_M01103_URG<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M01103$URG[,2])$y,40,depart=25,n.ahead=19)
r_M01103_URG_2<-calculmodeleVAR(vente=smooth.spline(c(1:65),liste.pn.vente.site$M01103$URG[,2])$y,retour =smooth.spline(c(1:65),liste.pn.retour.site$M01103$URG[,2])$y,coupe= 40,method = "leave-out",depart = 25,n.ahead = 19)



r_M01103_NKE<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M01103$NKE[,2])$y,40,depart=25,n.ahead=19)
r_M01103_NKE_2<-calculmodeleVAR(vente=smooth.spline(c(1:65),liste.pn.vente.site$M01103$NKE[,2])$y,retour =smooth.spline(c(1:65),liste.pn.retour.site$M01103$NKE[,2])$y,coupe= 40,method = "leave-out",depart = 25,n.ahead = 19)

r_M01103_NKE<-calculmodele2(liste.pn.retour.site$M01103$NKE[,2],40,depart=25,n.ahead=19)
r_M01103_NKE_2<-calculmodeleVAR(vente=liste.pn.vente.site$M01103$NKE[,2],retour =liste.pn.retour.site$M01103$NKE[,2],coupe= 40,method = "leave-out",depart = 25,n.ahead = 19)


r_M01103_NWD<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M01103$NWD[,2],df=40)$y,40,depart=25,n.ahead=19)
r_M01103_NWD_2<-calculmodeleVAR(vente=smooth.spline(c(1:65),liste.pn.vente.site$M01103$NWD[,2],df=40)$y,retour =smooth.spline(c(1:65),liste.pn.retour.site$M01103$NWD[,2],df=40)$y,coupe= 40,method = "leave-out",depart = 25,n.ahead = 19)

r_M01103_NWD<-calculmodele2(liste.pn.retour.site$M01103$NWD[,2],40,depart=25,n.ahead=19)
r_M01103_NWD_2<-calculmodeleVAR(vente=liste.pn.vente.site$M01103$NWD[,2],retour =liste.pn.retour.site$M01103$NWD[,2],coupe= 40,method = "leave-out",depart = 25,n.ahead = 19)







r_M05102_URG<-calculmodele2(liste.pn.retour.site$M05102$URG[,2],57,depart=3,n.ahead=19)

r_M05102_URG<-calculmodele2(liste.pn.retour.site$M05102$URG[,2],40,depart=10,n.ahead=19)
r_M05102_URG_2<-calculmodeleVAR(vente=liste.pn.vente.site$M05102$URG[,2],retour=liste.pn.retour.site$M05102$URG[,2],coupe= 40,method = "leave-out",depart = 10,n.ahead = 19)





r_M05102_NKE<-calculmodele2(liste.pn.retour.site$M05102$NKE[,2],57,depart=3,n.ahead=19)

r_M05102_NKE<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M05102$NKE[,2],df=50)$y,44,depart=11,n.ahead=19)
r_M05102_NKE_2<-calculmodeleVAR(vente=liste.pn.vente.site$M05102$NKE[,2],retour=liste.pn.retour.site$M05102$NKE[,2],coupe= 40,method = "leave-out",depart = 10,n.ahead = 19)




r_M05102_NWD<-calculmodele2(liste.pn.retour.site$M05102$NWD[,2],57,depart=3,n.ahead=19)


plot(liste.pn.site$M05102$NWD[,2],type='l')
lines(smooth.spline(c(1:length(liste.pn.site$M05102$NWD[,2])),liste.pn.site$M05102$NWD[,2]),col='red',type='l')

r_M05102_NWD<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M05102$NWD[,2],df=50)$y,44,depart=11,n.ahead=19)
r_M05102_NWD_2<-calculmodeleVAR(vente=liste.pn.vente.site$M05102$NWD[,2],retour=liste.pn.retour.site$M05102$NWD[,2],coupe= 40,method = "leave-out",depart = 10,n.ahead = 19)




r_M08201_URG<-calculmodele2(liste.pn.retour.site$M08201$URG[,2],57,depart=3,n.ahead=19)

plot(liste.pn.site$M08201$URG[,2],type='l')
lines(smooth.spline(c(1:length(liste.pn.site$M08201$URG[,2])),liste.pn.site$M08201$URG[,2]),col='red',type='l')

r_M08201_URG<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M08201$URG[,2],df=50)$y,48,depart=8,n.ahead=19)
r_M08201_URG_2<-calculmodeleVAR(vente=liste.pn.vente.site$M08201$URG[,2],retour=liste.pn.retour.site$M08201$URG[,2],coupe= 48,method = "leave-out",depart = 8,n.ahead = 19)



r_M08201_NKE<-calculmodele2(liste.pn.retour.site$M08201$NKE[,2],57,depart=3,n.ahead=19)

plot(liste.pn.site$M08201$NKE[,2],type='l')
lines(smooth.spline(c(1:length(liste.pn.site$M08201$NKE[,2])),liste.pn.site$M08201$NKE[,2]),col='red',type='l')

r_M08201_NKE<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M08201$NKE[,2],df=50)$y,48,depart=8,n.ahead=19)
r_M08201_NKE_2<-calculmodeleVAR(vente=liste.pn.vente.site$M08201$NKE[,2],retour=liste.pn.retour.site$M08201$NKE[,2],coupe= 48,method = "leave-out",depart = 8,n.ahead = 19)









r_M08201_NWD<-calculmodele2(liste.pn.retour.site$M08201$NWD[,2],57,depart=3,n.ahead=19)

plot(liste.pn.site$M08201$NWD[,2],type='l')
lines(smooth.spline(c(1:length(liste.pn.site$M08201$NWD[,2])),liste.pn.site$M08201$NWD[,2]),col='red',type='l')

r_M08201_NWD<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M08201$NWD[,2],df=50)$y,48,depart=8,n.ahead=19)
r_M08201_NWD_2<-calculmodeleVAR(vente=liste.pn.vente.site$M08201$NWD[,2],retour=liste.pn.retour.site$M08201$NWD[,2],coupe= 48,method = "leave-out",depart = 8,n.ahead = 19)







r_M12301_URG<-calculmodele2(liste.pn.retour.site$M12301$URG[,2],57,depart=3,n.ahead=19)

plot(liste.pn.site$M12301$URG[,2],type='l')
lines(smooth.spline(c(1:length(liste.pn.site$M12301$URG[,2])),liste.pn.site$M12301$URG[,2]),col='red',type='l')

r_M12301_URG<-calculmodele2(smooth.spline(c(1:65),liste.pn.retour.site$M12301$URG[,2],df=50)$y,56,depart=8,n.ahead=19)
r_M12301_URG_2<-calculmodeleVAR(vente=liste.pn.vente.site$M12301$URG[,2],retour=liste.pn.retour.site$M12301$URG[,2],coupe= 56,method = "leave-out",depart = 8,n.ahead = 19)








r_M12301_NKE<-calculmodele2(liste.pn.retour.site$M12301$NKE[,2],57,depart=3,n.ahead=19)
r_M12301_NWD<-calculmodele2(liste.pn.retour.site$M12301$NWD[,2],57,depart=3,n.ahead=19)

r_M20101_URG<-calculmodele2(liste.pn.retour.site$M20101$URG[,2],57,depart=3,n.ahead=19)
r_M20101_NKE<-calculmodele2(liste.pn.retour.site$M20101$NKE[,2],57,depart=3,n.ahead=19)
r_M20101_NWD<-calculmodele2(liste.pn.retour.site$M20101$NWD[,2],57,depart=3,n.ahead=19)

r_M18602_URG<-calculmodele2(liste.pn.retour.site$M18602$URG[,2],57,depart=3,n.ahead=19)
r_M18602_NKE<-calculmodele2(liste.pn.retour.site$M18602$NKE[,2],57,depart=3,n.ahead=19)
r_M18602_NWD<-calculmodele2(liste.pn.retour.site$M18602$NWD[,2],57,depart=3,n.ahead=19)


r_M13901_NWD<-calculmodele2(liste.pn.retour.site$M13901$NWD[,2],57,depart=3,n.ahead=19)
r_M13901_NKE<-calculmodele2(liste.pn.retour.site$M13901$NKE[,2],57,depart=3,n.ahead=19)

point.aberrant(liste.pn.retour.site$M01103$URG[,2],y1=min(liste.pn.retour.site$M01103$URG[,2]),y2=max(liste.pn.retour.site$M01103$URG[,2]),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(r_M01103_URG$mod,col="blue",lty=2)
lines(r_M01103_URG_2$mod,col="purple",lty=4)
lines(smooth.spline(c(1:65),liste.pn.retour.site$M01103$URG[,2]),col='red')
lines(smooth.spline(c(1:length(r_M01103_URG$mod)),r_M01103_URG$mod),col='purple')



point.aberrant(liste.pn.retour.site$M01103$NKE[,2],y1=min(liste.pn.retour.site$M01103$NKE[,2]),y2=max(liste.pn.retour.site$M01103$NKE[,2]),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(r_M01103_NKE$mod,col="blue",lty=2)
lines(r_M01103_NKE_2$mod,col="purple",lty=4)
lines(smooth.spline(c(1:65),liste.pn.retour.site$M01103$NKE[,2],df=40)$y,col='red')

point.aberrant(liste.pn.retour.site$M01103$NWD[,2],y1=min(liste.pn.retour.site$M01103$NWD[,2]),y2=max(liste.pn.retour.site$M01103$NWD[,2]),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(r_M01103_NWD$mod,col="blue",lty=2)
lines(r_M01103_NWD_2$mod,col="purple",lty=4)
lines(smooth.spline(c(1:65),liste.pn.retour.site$M01103$NWD[,2],df=40)$y,col='red')

point.aberrant(liste.pn.retour.site$M05102$URG[,2],y1=min(liste.pn.retour.site$M05102$URG[,2]),y2=max(liste.pn.retour.site$M05102$URG[,2]),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(r_M05102_URG$mod,col="blue",lty=2)
lines(r_M05102_URG_2$mod,col="purple",lty=4)
lines(rowMeans(as.matrix(data.frame(r_M05102_URG$mod,r_M05102_URG_2$mod))),col="red",lty=8)

point.aberrant(liste.pn.retour.site$M05102$NKE[,2],y1=min(liste.pn.retour.site$M05102$NKE[,2]),y2=max(liste.pn.retour.site$M05102$NKE[,2]),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(r_M05102_NKE$mod,col="blue",lty=2)
lines(r_M05102_NKE_2$mod,col="purple",lty=4)
lines(rowMeans(as.matrix(data.frame(r_M05102_NKE$mod,r_M05102_NKE_2$mod))),col="red",lty=8)


point.aberrant(liste.pn.retour.site$M08201$URG[,2],y1=min(liste.pn.retour.site$M08201$URG[,2]),y2=max(liste.pn.retour.site$M08201$URG[,2]),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(r_M08201_URG$mod,col="blue",lty=2)
lines(r_M08201_URG_2$mod,col="purple",lty=4)
lines(rowMeans(as.matrix(data.frame(r_M08201_URG$mod,r_M08201_URG_2$mod))),col="red",lty=8)


point.aberrant(liste.pn.retour.site$M12301$URG[,2],y1=min(liste.pn.retour.site$M12301$URG[,2]),y2=max(liste.pn.retour.site$M12301$URG[,2]),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(r_M12301_URG$mod,col="blue",lty=2)
lines(r_M12301_URG_2$mod,col="purple",lty=4)
lines(rowMeans(as.matrix(data.frame(r_M12301_URG$mod,r_M12301_URG_2$mod))),col="red",lty=8)















plot(liste.pn.retour.site$M01103$NKE[,2],type='l')
lines(smooth.spline(c(1:length(liste.pn.retour.site$M01103$NKE[,2])),liste.pn.retour.site$M01103$NKE[,2]),col='red',type='l')

plot(liste.pn$M01103[,2],type='l')
lines(smooth.spline(c(1:length(liste.pn$M01103[,2])),liste.pn$M01103[,2]),col='red',type='l')


plot(liste.pn.site$M05102$URG[,2],type='l')
lines(smooth.spline(c(1:length(liste.pn.site$M05102$URG[,2])),liste.pn.site$M05102$URG[,2]),col='red',type='l')




point.aberrant(liste.pn.retour.site$M05102$NWD[,2],y1=min(liste.pn.retour.site$M05102$NWD[,2]),y2=max(liste.pn.retour.site$M05102$NWD[,2]),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")

#-------------------------------------------------------------------Calcul du modèle final-------------------------------------------------------

v_structure<-function(vecteur)
  
  
{
  
  liste.vstructure.montante<-list(NA)
  liste.vstructure.descendante<-list(NA)
  
  for(i in 2:(length(vecteur)-1))
    
    
  {
    
    
    if( (vecteur[i]<vecteur[i-1])&(vecteur[i]<vecteur[i+1]) )
      
    {
      
      liste.vstructure.montante[[i]]<-c(vecteur[i-1],vecteur[i],vecteur[i+1])
      
    }
    
    else(liste.vstructure.montante[[i]]=NA)
    
    if( (vecteur[i]>vecteur[i-1])&(vecteur[i]>vecteur[i+1]) )
      
    {
      
      liste.vstructure.descendante[[i]]<-c(vecteur[i-1],vecteur[i],vecteur[i+1])
      
    }
    else(liste.vstructure.descendante[[i]]=NA)
    
    
    
    
    
  }
  
  
  
  result<-list(liste.vstructure.montante,liste.vstructure.descendante)
  names(result)<-c("Vmontant","Vdescendant")
  

  
  return(result)
  
}





trouve.depart<-function(vecteur)
  
{
  
  debut.recherche<-floor(length(vecteur)/2)
  vecteur2=vecteur[(debut.recherche:length(vecteur))]
  moyenne.v<-v_structure(vecteur2)
  sd.v<-v_structure(vecteur2)
  
  for(j in 1:2)
  {
    for(i in 1:(length(v_structure(vecteur2)[[j]])))
    {
      
      if(is.na(v_structure(vecteur2)[[j]][[i]])){moyenne.v[[j]][[i]]=NA}
      
      else(moyenne.v[[j]][[i]]=abs(mean(v_structure(vecteur2)[[j]][[i]])-mean(vecteur2)) )
  
    
    }
  
  }
  
  for(j in 1:2)
  {
    for(i in 1:(length(v_structure(vecteur2)[[j]])))
    {
      
      if(is.na(v_structure(vecteur2)[[j]][[i]])){moyenne.v[[j]][[i]]=NA}
      
      else(sd.v[[j]][[i]]=sd(v_structure(vecteur2)[[j]][[i]]) )
      
      
    }
    
  }
  
  
  
  if(which.min(c(which.min(sd.v[[1]]),which.min(sd.v[[2]])))==1){indice.depart=which.min(sd.v[[1]])+1+debut.recherche-1}
  else(indice.depart=which.min(sd.v[[2]])+debut.recherche)
  
  
return(indice.depart)
  
}

w=c(rep_len(NA,length.out=length(vecteur)))
w[trouve.depart(vecteur)]=vecteur[trouve.depart(vecteur)]
plot(vecteur,type='l')
lines(rep_len(mean(vecteur),length.out = length(vecteur)),col='red')
points(w,col='red')
points(vecteur)

modelefinal<-function(x,train=12,n.ahead=0,method="mean")
  
{
  
  
  N=length(x)
  smooth=smooth.spline(c(1:length(x)),x)
  begin=trouve.depart(x)
  
    if(smooth$df<=5)
      
    {
      result=list()
      
      pred<-predict(smooth,c(length(x):(length(x)+n.ahead)))
      
      result[[1]]<-"Retours trop aléatoires"
      result[[2]]<-pred$y
      
      result[[3]]<-0.70*pred$y
      result[[4]]<-1.30*pred$y
      
      names(result)<-c("Warning","Modele","Down","Up")
      
      
    }
  
  
  if(smooth$df>5)
    
  {
    
    
    
   
    if((begin+train)<N)
      
    {
      
    result=list()
      
    pred<-calculmodele2(x,coupe= begin,train=train ,n.ahead=n.ahead,method=method)
   
    
    result[[1]]<-"No Warning"
    result[[2]]<-pred
    
    confidence=pred$e/mean(as.vector(window(x,start=(begin+1),end=N)))
    
    result[[3]]<-(1-confidence)*pred$mod
    result[[4]]<-(1+confidence)*pred$mod
    
    names(result)<-c("Warning","Modele","Down","Up")
    
    }
    
    if((begin+train)>=N)
      
    {
      
      result=list()
      
      pred<-calculmodele2(x,coupe= begin,train=(N-begin),n.ahead=n.ahead,method=method)
      
      if((N-begin)<=6)
      {
      result[[1]]<-"Mauvaise confiance en la prédiction"
      }
      
      if((N-begin)>6)
      {
        result[[1]]<-"Confiance moyenne en la prediction"
      }
      
      result[[2]]<-pred
      
     
      confidence=pred$e/mean(as.vector(window(x,start=(begin+1),end=N)))
      
      result[[3]]<-(1-confidence)*pred$mod
      result[[4]]<-(1+confidence)*pred$mod
      
      names(result)<-c("Warning","Modele","Down","Up")
      
    }
    
   

    
        
  }
  
  
  
  
  return(result) 
  
}

x=liste.pn.retour.site$M05102$URG[,2]
n.ahead=12
train=12

smooth.spline(c(1:length(x)),x)

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(smooth.spline(c(1:length(x)),x),col='red')

mod_x=modelefinal(x,n.ahead=12)
mod_x_2=calculmodele2(x,coupe=53,train=12,n.ahead=12)

smooth=smooth.spline(c(1:length(x)),x)
pred=predict(smooth,c(length(x):(length(x)+n.ahead)))

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(mod_x$Modele$mod,col="blue")
lines(mod_x$Down,col="blue",lty=2)
lines(mod_x$Up,col="blue",lty=2)

lines(mod_x_2$mod,col="purple",lty=4)
lines(pred$y,col="red")
lines(smooth.spline(result$Up),col="red",lty=8)
lines(smooth.spline(result$Down),col="red",lty=8)



x=liste.pn$M01103[,2]
n.ahead=12
train=12

smooth.spline(c(1:length(x)),x)

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(smooth.spline(c(1:length(x)),x),col='red')

mod_x=modelefinal(x,n.ahead=12)
mod_x_2=calculmodele2(x,coupe=53,train=12,n.ahead=12)

smooth=smooth.spline(c(1:length(x)),x)
pred=predict(smooth,c(length(x):(length(x)+n.ahead)))

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(mod_x$Modele$mod,col="blue")
lines(mod_x$Down,col="blue",lty=2)
lines(mod_x$Up,col="blue",lty=2)

lines(mod_x_2$mod,col="purple",lty=4)
lines(pred$y,col="red")
lines(smooth.spline(result$Up),col="red",lty=8)
lines(smooth.spline(result$Down),col="red",lty=8)

x=liste.client$AVAFR[,2]
n.ahead=12
train=12

smooth.spline(c(1:length(x)),x)

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(smooth.spline(c(1:length(x)),x),col='red')

mod_x=modelefinal(x,n.ahead=12)
mod_x_2=calculmodele2(x,coupe=48,train=17,n.ahead=12)

smooth=smooth.spline(c(1:length(x)),x)
pred=predict(smooth,c(length(x):(length(x)+n.ahead)))

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(mod_x$Modele$mod,col="blue")
lines(mod_x$Down,col="blue",lty=2)
lines(mod_x$Up,col="blue",lty=2)

lines(mod_x_2$mod,col="purple",lty=4)
lines(pred$y,col="red")
lines(smooth.spline(result$Up),col="red",lty=8)
lines(smooth.spline(result$Down),col="red",lty=8)


x=liste.client$AVEZY[,2]
n.ahead=12
train=12

smooth.spline(c(1:length(x)),x)

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(smooth.spline(c(1:length(x)),x),col='red')

mod_x=modelefinal(x,n.ahead=12)
mod_x_2=calculmodele2(x,coupe=40,train=17,n.ahead=12)

smooth=smooth.spline(c(1:length(x)),x)
pred=predict(smooth,c(length(x):(length(x)+n.ahead)))

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(mod_x$Modele$mod,col="blue")
lines(mod_x$Down,col="blue",lty=2)
lines(mod_x$Up,col="blue",lty=2)

lines(mod_x_2$mod,col="purple",lty=4)
lines(pred$y,col="red")
lines(smooth.spline(result$Up),col="red",lty=8)
lines(smooth.spline(result$Down),col="red",lty=8)


x=liste.client$AVUAE[(30:65),3]
n.ahead=12
train=12

smooth.spline(c(1:length(x)),x)

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(smooth.spline(c(1:length(x)),x),col='red')

mod_x=modelefinal(x,n.ahead=12)
mod_x_2=calculmodele2(x,coupe=26,train=10,n.ahead=12)

smooth=smooth.spline(c(1:length(x)),x)
pred=predict(smooth,c(length(x):(length(x)+n.ahead)))

point.aberrant(x,y1=min(x),y2=max(x),debut=1,fin=65+19,main="Retours XXX",ylab="Nombres",xlab="Mois")
lines(mod_x$Modele$mod,col="blue")
lines(mod_x$Down,col="blue",lty=2)
lines(mod_x$Up,col="blue",lty=2)

lines(mod_x_2$mod,col="purple")
lines(pred$y,col="red")
lines(smooth.spline(result$Up),col="red",lty=8)
lines(smooth.spline(result$Down),col="red",lty=8)










#-------------------------------------------------------------------Calcul sur tous les PN/Site sur 12 mois-----------------------------------------------------------------------

forecast<-liste.pn.site

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in deb:length(liste.pn.site))

{
  
  for (j in 1:length(liste.pn.site[[i]]))
    
  {
    
  x=NULL
  
  x=remplacerparNA(liste.pn.site[[i]][[j]][,-3],"Retours")[,2] 
  x=replace(x,which(is.na(x)),0)
  
  try(forecast[[i]][[j]]<-modelefinal(x,n.ahead = 12))  
  
    
  }
    
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn.site)-deb))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn.site)-deb)), label=info)
  
  
}

deb=i
deb2=j
  
names(forecast)<-names(liste.pn.site) 
 
plot(liste.pn.site$M01103$URG[,2],type='l',xlim=c(0,80))
lines(forecast$M01103$URG$Modele$mod,col='red')

plot(liste.pn.site$M01103$NKE[,2],type='l',xlim=c(0,80))
lines(forecast$M01103$NKE$Modele$mod,col='red')

plot(liste.pn.site$M01103$NWD[,2],type='l',xlim=c(0,80))
lines(forecast$M01103$NWD$Modele$mod,col='red')

plot(liste.pn.site$M08201$URG[,2],type='l',xlim=c(0,80))
lines(forecast$M08201$URG$Modele$mod,col='red')

plot(liste.pn.site$M08201$NKE[,2],type='l',xlim=c(0,80))
lines(forecast$M08201$NKE$Modele$mod,col='red')

plot(liste.pn.site$M08201$NWD[,2],type='l',xlim=c(0,80))
lines(forecast$M08201$NWD$Modele$mod,col='red')

plot(liste.pn.site$M12301$URG[,2],type='l',xlim=c(0,80))
lines(forecast$M12301$URG$Modele$mod,col='red')

x=liste.pn.site$M12301$URG[,2]
mod_x=forecast$M12301$URG
mod_x_2=calculmodele2(liste.pn.site$M12301$URG[,2],n.ahead=24,method="aic",coupe=56)

plot(liste.pn.site$M12301$URG[,2],type='l',xlim=c(0,80),ylim=c(-100,500))
points(liste.pn.site$M12301$URG[,2])
lines(forecast$M12301$URG$Modele$mod,col='red')
lines(mod_x_2$mod,col='purple')


x=liste.pn.site$M12301$URG[40:length(x),2]
plot(x,type='l',xlim=c(0,80),ylim=c(-100,500))
spec.pgram(ts(x))



x=liste.pn$M01103[,3]
plot(x,type='l',xlim=c(0,80))
spec.pgram(ts(x))
mod_x=modelefinal(x,n.ahead=12)
lines(mod_x$Modele$mod,col='red')

#-------------------------------------------------------------------Test sur zone commerciale-----------------------------------------------------------

x.M01103.EMA=liste.pn.zone$M01103$EMA
x.M01103.EMA=x.M01103.EMA[-(1:3),]

mod.x.M01103.EMA<-calculmodele2(x.M01103.EMA[,2],coupe=59,train=5,n.ahead=12)
mod.x.M01103.EMA.var<-modele.instable(x.M01103.EMA[,3],x.M01103.EMA[,2],n.ahead=12)
valid.M01103.EMA.var<-validation(x.M01103.EMA[,3],x.M01103.EMA[,2],n.ahead=12)



plot(x.M01103.EMA[,2],type='l',xlim=c(0,80))
lines(mod.x.M01103.EMA$Modele$mod,col='red')
lines(mod.x.M01103.EMA.var$mod.r,col='blue',lty=4)


#

x.M01103.FEO=liste.pn.zone$M01103$FEO
x.M01103.FEO=x.M01103.FEO[-(1:3),]

mod.x.M01103.FEO<-modelefinal(x.M01103.FEO[,2],n.ahead=12)
mod.x.M01103.FEO.var<-modele.instable(x.M01103.FEO[,3],x.M01103.FEO[,2],n.ahead=12)

valid.M01103.NAM.var<-validation(x.M01103.NAM[,3],x.M01103.NAM[,2],n.ahead=12)



plot(x.M01103.FEO[,2],type='l',xlim=c(0,80))
lines(mod.x.M01103.FEO$Modele$mod,col='red')
lines(mod.x.M01103.FEO.var$mod.r,col='blue',lty=4)


#


x.M01103.NAM=liste.pn.zone$M01103$NAM
x.M01103.NAM=x.M01103.NAM[-(1:3),]

mod.x.M01103.NAM<-modelefinal(x.M01103.NAM[,2],n.ahead=12)
mod.x.M01103.NAM.var<-modele.instable(x.M01103.NAM[,3],x.M01103.NAM[,2],n.ahead=12)

valid.M01103.FEO.var<-validation(x.M01103.FEO[,3],x.M01103.FEO[,2],n.ahead=12)


plot(x.M01103.NAM[,2],type='l',xlim=c(0,80))
lines(mod.x.M01103.NAM$Modele$mod,col='red')
lines(mod.x.M01103.NAM.var$mod.r,col='blue',lty=4)

#

x.M05102.EMA=liste.pn.zone$M05102$EMA
x.M05102.EMA=x.M05102.EMA[-(1:3),]

mod.x.M05102.EMA<-modelefinal(x.M05102.EMA[,2],n.ahead=12)
mod.x.M05102.EMA.var<-modele.instable(x.M05102.EMA[,3],x.M05102.EMA[,2],coupe=22,n.ahead=3)
mod.x.M05102.EMA.var<-modeleVAR(x.M05102.EMA[,3],x.M05102.EMA[,2],coupe=23,n.ahead=3,ordre=6)


valid.M05102.EMA.var<-validation(x.M05102.EMA[,3],x.M05102.EMA[,2],kmin=3,kmax=9,n.ahead=3)
valid.M05102.EMA.var$mean



plot(x.M05102.EMA[,2],type='l',xlim=c(0,80),xaxp=c(0,80,80))
lines(x.M05102.EMA[,3],col='red')
lines(mod.x.M05102.EMA$Modele$mod,col='red')
lines(mod.x.M05102.EMA.var$mod.r,col='blue',lty=4)
points(x.M05102.EMA[,2])
points(mod.x.M05102.EMA.var$modele$mod.r,col='blue')


  



#

x.M05102.FEO=liste.pn.zone$M05102$FEO
x.M05102.FEO=x.M05102.FEO[-(1:3),]

mod.x.M05102.FEO<-calculmodele2(x.M05102.FEO[,2],coupe=47,train=18,n.ahead=12)
mod.x.M05102.FEO.var<-modele.instable(x.M05102.FEO[,3],x.M05102.FEO[,2],n.ahead=12)

plot(x.M05102.FEO[,2],type='l',xlim=c(0,80))
lines(mod.x.M05102.FEO$Modele$mod,col='red')
lines(mod.x.M05102.FEO.var$mod.r,col='blue',lty=4)


#


x.M05102.NAM=liste.pn.zone$M05102$NAM
x.M05102.NAM=x.M05102.NAM[-(1:3),]

mod.x.M05102.NAM<-modelefinal(x.M05102.NAM[,2],n.ahead=12)
mod.x.M05102.NAM.var<-modele.instable(x.M05102.NAM[,3],x.M05102.NAM[,2],coupe=48,n.ahead=12)

plot(x.M05102.NAM[,2],type='l',xlim=c(0,80))
lines(x.M05102.NAM[,3],col='red')
lines(mod.x.M05102.NAM$Modele$mod,col='red')
lines(mod.x.M05102.NAM.var$modele$mod.r,col='blue',lty=4)
points(mod.x.M05102.NAM.var$modele$mod.r,col='blue')
points(x.M05102.NAM[,2])




#

x.M08201.EMA=liste.pn.zone$M08201$EMA
x.M08201.EMA=x.M08201.EMA[-(1:3),]

mod.x.M08201.EMA<-modelefinal(x.M08201.EMA[,2],n.ahead=12)
mod.x.M08201.EMA.var<-modele.instable(x.M08201.EMA[,3],x.M08201.EMA[,2],n.ahead=12)

plot(x.M08201.EMA[,2],type='l',xlim=c(0,80))
lines(mod.x.M08201.EMA$Modele$mod,col='red')
lines(mod.x.M08201.EMA.var$mod.r,col='blue',lty=4)


#

x.M08201.FEO=liste.pn.zone$M08201$FEO
x.M08201.FEO=x.M08201.FEO[-(1:3),]

mod.x.M08201.FEO<-modelefinal(x.M08201.FEO[,2],n.ahead=12)
mod.x.M08201.FEO.var<-modele.instable(x.M08201.FEO[,3],x.M08201.FEO[,2],n.ahead=12)

plot(x.M08201.FEO[,2],type='l',xlim=c(0,80))
lines(mod.x.M08201.FEO$Modele$mod,col='red')
lines(mod.x.M08201.FEO.var$mod.r,col='blue',lty=4)


#


x.M08201.NAM=liste.pn.zone$M08201$NAM
x.M08201.NAM=x.M08201.NAM[-(1:3),]

mod.x.M08201.NAM<-modelefinal(x.M08201.NAM[,2],n.ahead=12)
mod.x.M08201.NAM.var<-modele.instable(x.M08201.NAM[,3],x.M08201.NAM[,2],n.ahead=12)

plot(x.M08201.NAM[,2],type='l',xlim=c(0,80))
lines(mod.x.M08201.NAM$Modele$mod,col='red')
lines(mod.x.M08201.NAM.var$mod.r,col='blue',lty=4)


#------------------------------------------------------------Ordonne---------------------------------------------------
ordonne<-function(vente,retour,ordre,N)
  
  
{
  
  
  result<-c()
  
  result[1]<-vente[N-1]
  
  k<-1
  
  while (k<=(ordre-1))
    
  {
    
    result[2*k+1]<-vente[N-(k+1)]
    result[2*k]<-retour[N-k]
    
    k<-k+1
    
  }
  
  result[2*ordre]<-retour[N-ordre]
  
  return(result)
  
  
}

#------------------------------------------------------------coeff.r---------------------------------------------------


coeff.r<-function(vente,retour,ordre)
  
{
  rn<-data.frame(vente,retour)
  
  varvec=VAR( rn, p = ordre,type = c("const", "trend", "both", "none"), season = NULL ,exogen=NULL,lag.max=NULL,ic=c("AIC")) 
  
  coeff.r<-c()
  
  coeff.r[1]<-varvec[[1]][[2]][[1]][1] #vente
  
  k<-1
  
  while(k<=(ordre-1))
    
  {
    
    coeff.r[2*k+1]<-varvec[[1]][[2]][[1]][2*k+1] #vente
    coeff.r[2*k]<-varvec[[1]][[2]][[1]][2*k] #retour
    k<-k+1
    
  }
  
  coeff.r[2*ordre]<-varvec[[1]][[2]][[1]][2*ordre] #retour
  
  
  return(coeff.r)
  
  
}


#------------------------------------------------------------coeff.v---------------------------------------------------


coeff.v<-function(vente,retour,ordre)
  
{
  rn<-data.frame(vente,retour)
  
  varvec=VAR( rn, p = ordre,type = c("const", "trend", "both", "none"), season = NULL ,exogen=NULL,lag.max=NULL,ic=c("AIC")) 
  
  coeff.v<-c()
  
  coeff.v[1]<-varvec[[1]][[1]][[1]][1] #vente
  
  k<-1
  
  while(k<=(ordre-1))
    
  {
    
    coeff.v[2*k+1]<-varvec[[1]][[1]][[1]][2*k+1] #vente
    coeff.v[2*k]<-varvec[[1]][[1]][[1]][2*k] #retour
    k<-k+1
    
  }
  
  coeff.v[2*ordre]<-varvec[[1]][[1]][[1]][2*ordre] #retour
  
  
  return(coeff.v)
  
  
}

#------------------------------------------------------------constante.r---------------------------------------------------


constante.r<-function(vente,retour,ordre)
  
{
  rn<-data.frame(vente,retour)
  
  varvec=VAR( rn, p = ordre,type = c("const", "trend", "both", "none"), season = NULL ,exogen=NULL,lag.max=NULL,ic=c("AIC")) 
  
  cte<-varvec[[1]][[2]][[1]][2*ordre+1]
  
  return(cte)
  
  
}

#------------------------------------------------------------constante.v---------------------------------------------------


constante.v<-function(vente,retour,ordre)
  
{
  rn<-data.frame(vente,retour)
  
  varvec=VAR( rn, p = ordre,type = c("const", "trend", "both", "none"), season = NULL ,exogen=NULL,lag.max=NULL,ic=c("AIC")) 
  
  cte<-varvec[[1]][[1]][[1]][2*ordre+1]
  
  return(cte)
  
  
}

#------------------------------------------------------------modeleVAR---------------------------------------------------


modeleVAR<-function(vente,retour,ordre,coupe,n.ahead,fenetre=48)
  
{
  result=list(mod.r=NA,actuals.r=NA,error.r=NA,mean.error.r=NA,mod.v=NA,actuals.v=NA,error.v=NA,mean.error.v=NA)
  vente2=retour2=c()
  vente2<-window(vente,start=(length(vente)-fenetre)+1,end=length(vente))
  retour2<-window(retour,start=(length(retour)-fenetre)+1,end=length(retour))
  rn<-data.frame(vente2,retour2)
  N<-nrow(rn)
  rn1<-rn[1:coupe,]
  rn2<-rn[((coupe+1):N),]
  rn3<-rn
  
  
  
  
  pred.r<-c()
  pred.v<-c()
  vnprime<-rn1[,1]
  rnprime<-rn1[,2]
  
  k<-1  
  vecteur<-ordonne(vnprime,rnprime,ordre,coupe+1)
  
  while (k<=n.ahead)
    
  {
    
    if(class(try(coeff.r(rn1[,1],rn1[,2],ordre)))=="try-error")
      
    {
      vnprime[length(vnprime)+1]<-NA
      rnprime[length(rnprime)+1]<-NA
      k<-k+1
    }
    
    if(!class(try(coeff.r(rn1[,1],rn1[,2],ordre)))=="try-error")
    {
    
      pred.r[k]<-coeff.r(rn1[,1],rn1[,2],ordre)%*%vecteur+constante.r(rn1[,1],rn1[,2],ordre)
      pred.v[k]<-coeff.v(rn1[,1],rn1[,2],ordre)%*%vecteur+constante.v(rn1[,1],rn1[,2],ordre)
      rnprime[length(rnprime)+1]<-pred.r[k]
      vnprime[length(vnprime)+1]<-pred.v[k]
      
      vecteur<-ordonne(vnprime,rnprime,ordre,coupe+k+1)
      
      k<-k+1
      
    }
    
   
    
  }
  
  vnmodele<-c()
  vnreel<-c()
  rnmodele<-c()
  rnreel<-c()
  
  result[[1]]<-rnprime
  result[[2]]<-rn3[,2]
  
  if(coupe<length(rn3[,2]))
  {
    
    if(coupe+n.ahead<=length(rn3[,2]))
      
    {
      
      rnmodele<-as.vector(window(rnprime,start=coupe+1,end=coupe+n.ahead))
      rnreel<-as.vector(window(as.vector(rn3[,2]),start=coupe+1,end=coupe+n.ahead))
      result[[3]]<-100*abs(rnmodele-rnreel)/rnreel
    }
    
    
    if(coupe+n.ahead>length(rn3[,2]))
      
    {
      
      rnmodele<-as.vector(window(rnprime,start=coupe+1,end=length(rn3[,2])))
      rnreel<-as.vector(window(as.vector(rn3[,2]),start=coupe+1,end=length(rn3[,2])))
      result[[3]]<-100*abs(rnmodele-rnreel)/rnreel
      
    }
    
    
    
    result[[4]]<-mean(100*abs(rnmodele-rnreel)/rnreel,na.rm=TRUE)
    
  }
  else(result[[4]]<-"No Error")
  
  result[[5]]<-vnprime
  result[[6]]<-rn1[,1]
  
  if(coupe<length(rn3[,1]))
  {
    
    if(coupe+n.ahead<=length(rn3[,1]))
      
    {
      
      vnmodele<-as.vector(window(vnprime,start=coupe+1,end=coupe+n.ahead))
      vnreel<-as.vector(window(as.vector(rn3[,1]),start=coupe+1,end=coupe+n.ahead))
      result[[7]]<-100*abs(vnmodele-vnreel)/vnreel
      
    }
    
    
    if(coupe+n.ahead>length(rn3[,1]))
      
    {
      
      vnmodele<-as.vector(window(vnprime,start=coupe+1,end=length(rn3[,1])))
      vnreel<-as.vector(window(as.vector(rn3[,1]),start=coupe+1,end=length(rn3[,1])))
      result[[7]]<-100*abs(vnmodele-vnreel)/vnreel
    }
    
    
    
    
    result[[8]]<-mean(100*abs(vnmodele-vnreel)/vnreel,na.rm=TRUE)
    
  }
  else(result[[8]]<-"No Error")
  
  
  
  
  return(result)
  
  
}

#------------------------------------------------------------Test---------------------------------------------------






validation<-function(vente,retour,n.ahead,coupemin=34,coupemax=46,kmin=6,kmax=15,fenetre=48)
  
{
  
  result<-list(mean=NA,sd=NA,ordremin=NA)  
  mean<-c()
  sd<-c()
  
  
  for( k in kmin:kmax)
    
  {
    
    
    
   
      
      evolution.erreur<-c()
    
for(i in coupemin:coupemax)
  
    {
      
      var=NULL
      var=modeleVAR(vente,retour,coupe=i,ordre=k,n.ahead = n.ahead,fenetre=fenetre)
      
      evolution.erreur[i]<-var$mean.error.r
      i<-i+1
      
    }
   
    
    
    mean[k]<-mean(evolution.erreur,na.rm=TRUE)
    sd[k]<-sd(evolution.erreur,na.rm=TRUE)
    k=k+1
    
  }
  
  result[[1]]=mean
  result[[2]]=sd
  result[[3]]=which.min(mean)
  
  return(result)
  
}


#---------------------------------------------------------------------------------------------------


ordre_var<-function(vente,retour,n.ahead=12)
  
{
  
  valid<-validation(v,r,n.ahead=n.ahead)
  
  
  result<-which.min(valid$mean)
  
  return(result)
  
}

modele.instable<-function(vente,retour,coupe=length(retour),n.ahead)
  
{
  
  ordre2=ordre_var(vente,retour,n.ahead=n.ahead)
  result<-list(modele=NA,ordre=NA)
  result[[1]]<-modeleVAR(vente=vente,retour=retour,ordre=ordre2,n.ahead=n.ahead,coupe=coupe)
  result[[2]]<-ordre2
  
  
  return(result)  
  
  
  
}




#--------------------------------Estimation de l'erreur de VAR sur un cas simple-----------------------------------

forecast.var<-liste.pn.zone

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 2:length(names(liste.pn.zone)))
  
{
  c<-liste.pn.zone[[names(liste.pn.zone)[i]]]
  
  for (j in 1:length(c))
  
  
  {
  
    x=NULL
    y=NULL
    if( (ncol(c[[names(c)[j]]])<3) ){j<-j+1}
    
    x=remplacerparNA(c[[names(c)[j]]][,-3],"Retours")[-(1:3),2] 
    
    y=remplacerparNA(c[[names(c)[j]]][,-2],"Ventes")[-(1:3),2]
    x=replace(x,which(is.na(x)),0)
    y=replace(y,which(is.na(y)),0)
    
    try(forecast.var[[i]][[j]]<-modele.instable(y,x,n.ahead = 3))  
    
    
  }
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn.site)-deb))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn.site)-deb)), label=info)
  
  
}




valid.var<-liste.pn.zone

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 2:length(liste.pn.zone))
  
{
 
  
  for (j in 1:length(liste.pn.zone[[i]]))
    
    
  {
    
    x=NULL
    y=NULL
    v=NULL
    
    if( (ncol(liste.pn.zone[[i]][[j]])<3) ){j<-j+1}
    
    x=remplacerparNA(liste.pn.zone[[i]][[j]][,-3],"Retours")[,2] 
    
    y=remplacerparNA(liste.pn.zone[[i]][[j]][,-2],"Ventes")[,2]
    x=replace(x,which(is.na(x)),0)
    y=replace(y,which(is.na(y)),0)
    
   try( v<-validation(vente=x,retour=y,kmin=2,n.ahead = 3,coupemin=23,coupemax=35,fenetre=36) )
    
    valid.var[[i]][[j]]<-list(v$mean,v$ordremin)  
    
    
  }
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn.zone)))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn.zone))), label=info)
  
  
}


valid.var.12<-liste.pn.zone

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 2:length(liste.pn.zone))
  
{
  
  
  for (j in 1:length(liste.pn.zone[[i]]))
    
    
  {
    
    x=NULL
    y=NULL
    v=NULL
    
    if( (ncol(liste.pn.zone[[i]][[j]])<3) ){j<-j+1}
    
    x=remplacerparNA(liste.pn.zone[[i]][[j]][,-3],"Retours")[,2] 
    
    y=remplacerparNA(liste.pn.zone[[i]][[j]][,-2],"Ventes")[,2]
    x=replace(x,which(is.na(x)),0)
    y=replace(y,which(is.na(y)),0)
    
    try( v<-validation(vente=x,retour=y,kmin=2,n.ahead = 12) )
    
    valid.var.12[[i]][[j]]<-list(v$mean,v$ordremin)  
    
    
  }
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn.zone)))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn.zone))), label=info)
  
  
}

valid.var.pn<-liste.pn

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 2:length(liste.pn))
  
{
  
  
  
    
    x=NULL
    y=NULL
    v=NULL
  
    
    x=remplacerparNA(liste.pn[[i]][,-3],"Retours")[,2] 
    
    y=remplacerparNA(liste.pn[[i]][,-2],"Ventes")[,2]
    x=replace(x,which(is.na(x)),0)
    y=replace(y,which(is.na(y)),0)
    
    try( v<-validation(vente=x,retour=y,kmin=2,n.ahead = 3) )
    
    valid.var.pn[[i]]<-list(v$mean,v$ordremin)  
    
    
  
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn)))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn))), label=info)
  
  
}

valid.var.pn.12<-liste.pn

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 2:length(liste.pn))
  
{
  
  
  
  
  x=NULL
  y=NULL
  v=NULL
  
  
  x=remplacerparNA(liste.pn[[i]][,-3],"Retours")[,2] 
  
  y=remplacerparNA(liste.pn[[i]][,-2],"Ventes")[,2]
  x=replace(x,which(is.na(x)),0)
  y=replace(y,which(is.na(y)),0)
  
  try( v<-validation(vente=x,retour=y,kmin=2,n.ahead = 12) )
  
  valid.var.pn.12[[i]]<-list(v$mean,v$ordremin)  
  
  
  
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn)))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn))), label=info)
  
  
}



valid.var.pn.8<-liste.pn

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 2:length(liste.pn))
  
{
  
  
  
  
  x=NULL
  y=NULL
  v=NULL
  
  
  x=remplacerparNA(liste.pn[[i]][,-3],"Retours")[,2] 
  
  y=remplacerparNA(liste.pn[[i]][,-2],"Ventes")[,2]
  x=replace(x,which(is.na(x)),0)
  y=replace(y,which(is.na(y)),0)
  
  try( v<-validation(vente=x,retour=y,kmin=2,n.ahead = 8) )
  
  valid.var.pn.8[[i]]<-list(v$mean,v$ordremin)  
  
  
  
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn)))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn))), label=info)
  
  
}



erreurpire<-valid.var

for(i in 3:length(liste.pn.zone))
  
{
  
  for(j in 1:length(liste.pn.zone[[i]]))
    
  {
    
    erreurpire[[i]][[j]]<-which.min(valid.var[[i]][[j]])
    
  }
  
  
  
}

volumetrie<-liste.pn.zone

for(i in 1:length(liste.pn.zone))
  
{
  
  for(j in 1:length(liste.pn.zone[[i]]))
    
  {
    
    try(volumetrie[[i]][[j]]<-mean(liste.pn.zone[[i]][[j]][,2],na.rm=T))
    
    
  }
  
  
  
}


valid.var.3.36<-liste.pn.zone

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 2:length(liste.pn.zone))
  
{
  
  
  for (j in 1:length(liste.pn.zone[[i]]))
    
    
  {
    
    x=NULL
    y=NULL
    v=NULL
    
    if( (ncol(liste.pn.zone[[i]][[j]])<3) ){j<-j+1}
    
    x=remplacerparNA(liste.pn.zone[[i]][[j]][,-3],"Retours")[,2] 
    
    y=remplacerparNA(liste.pn.zone[[i]][[j]][,-2],"Ventes")[,2]
    x=replace(x,which(is.na(x)),0)
    y=replace(y,which(is.na(y)),0)
    
    try( v<-validation(vente=x,retour=y,kmin=1,n.ahead = 3,coupemin=23,coupemax=35,fenetre=36) )
    
    valid.var.3.36[[i]][[j]]<-list(v$mean,v$ordremin)  
    
    
  }
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn.zone)))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn.zone))), label=info)
  
  
}



valid.var.3.1<-liste.pn.zone

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 2:length(liste.pn.zone))
  
{
  
  
  for (j in 1:length(liste.pn.zone[[i]]))
    
    
  {
    
    x=NULL
    y=NULL
    v=NULL
    
    if( (ncol(liste.pn.zone[[i]][[j]])<3) ){j<-j+1}
    
    x=remplacerparNA(liste.pn.zone[[i]][[j]][,-3],"Retours")[,2] 
    
    y=remplacerparNA(liste.pn.zone[[i]][[j]][,-2],"Ventes")[,2]
    x=replace(x,which(is.na(x)),0)
    y=replace(y,which(is.na(y)),0)
    
    try( v<-validation(vente=x,retour=y,kmin=1,n.ahead = 3) )
    
    valid.var.3.1[[i]][[j]]<-list(v$mean,v$ordremin)  
    
    
  }
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn.zone)))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn.zone))), label=info)
  
  
}
















#-------------------

m01103<-liste.pn$M01103
m01103.fcst<-modelefinal(m01103[,2],n.ahead=8)

plot(m01103[,2],type='l',xlim=c(0,80))
lines(m01103.fcst$Modele$mod,col='red')


m08201<-liste.pn$M08201
m08201.fcst<-calculmodele2(m08201[,2],coupe=49,train=12,n.ahead=8)
plot(m08201[,2],type='l',xlim=c(0,80),xaxp=c(0,80,80))
points(m08201[,2])
lines(m08201.fcst$mod,col='red')


m05102<-liste.pn$M05102
m05102.fcst<-calculmodele2(m05102[,2],coupe=51,train=12,n.ahead=8)
m05102.fcst.2<-modele.instable(m05102[,3],m05102[,2],n.ahead=12)
m05102.fcst.3<-rowMeans(cbind(m05102.fcst$mod[66:73],m05102.fcst.2$mod.r[66:73]))

plot(m05102[,2],type='l',xlim=c(0,80),xaxp=c(0,80,80),yaxp=c(800,1600,10))
points(m05102[,2])
lines(m05102.fcst$mod,col='red')
lines(m05102.fcst.2$mod.r,col='blue')
points(m05102.fcst$mod,col='red')
lines(c(rep_len(NA,length.out=65),m05102.fcst.3),col='purple')
points(c(rep_len(NA,length.out=65)))

pierre<-data.frame(c(rep_len(NA,length.out=65),m05102.fcst.3),m08201.fcst$mod,m01103.fcst$Modele$mod)

write.csv(pierre,file="fcst.titouan.csv")


m18602EMA<-liste.pn.zone$M18602$EMA

modm18602EMA<-modele.instable(m18602EMA[-(1:18),3],m18602EMA[-(1:18),2],n.ahead=6,coupe=33)
modm18602EMA
plot(m18602EMA[-(1:18),2],type='l')
lines(modm18602EMA$mod.r,col='red')
points(modm18602EMA$mod.r,col='red')
points(m18602EMA[-(1:18),2])



m12301FEO<-liste.pn.zone$M12301$FEO
modm12301FEO<-modele.instable(vente=m12301FEO[-(1:3),3],retour=m12301FEO[-(1:3),2],coupe=53,n.ahead=2)
plot(liste.pn.zone$M12301$FEO[-(1:3),2],xlim=c(0,80),type='l',lwd=2)
lines(modm12301FEO$mod.r,lty=2,col='red')


#----------------------------------------Liste des PN les plus importants pour le 80/20-------------------------------------------


View(vente_pn)
dv<-data.frame(vente_pn$Date,vente_pn$PN,vente_pn$VENTES)
dv2=aggregate(dv$vente_pn.VENTES,by=list(dv$vente_pn.Date,dv$vente_pn.PN),FUN=sum)
dv3=aggregate(dv$vente_pn.VENTES,by=list(dv$vente_pn.Date),FUN=sum)
colnames(dv2)<-c("Date","PN","Ventes")
colnames(dv3)<-c("Date","Ventes.totales")

for (i in 1:nrow(dv2))
  
{
  
  dv2[i,3]<-100*dv2[i,3]/dv3[match(dv2[i,1],dv3[,1]),2]
  
  
}

colnames(dv2)<-c("Date","PN","Rapport.ventes")



lespn<-matrix(data=c(NA),nrow=length(unique(dv2$PN)),ncol=1)

for(j in 1:length(unique(dv2$PN)))

{

dv4=subset(dv2,PN==unique(dv2$PN)[j])
lespn[j,1]<-round(mean(aggregate(dv4$Rapport.ventes,by=list(dv4$Date),FUN=mean)[,2]),1)


}

rownames(lespn)<-unique(dv2$PN)

lespn<-lespn[which(lespn[,1]>1),]

barplot(lespn)


#--------------------------

vente_pn2<-subset(vente_pn,Year>2010 & TECH=="X" )
dv<-data.frame(vente_pn2$Date,vente_pn2$PN,vente_pn2$VENTES)
dv<-subset(dv,!vente_pn2.PN=="#N/A")

dv2=aggregate(dv$vente_pn2.VENTES,by=list(dv$vente_pn2.PN),FUN=sum)
somme=sum(dv2[,2])
colnames(dv2)<-c("PN","Ventes")


for (i in 1:nrow(dv2))
  
{
  
  dv2[i,2]<-round(100*dv2[i,2]/somme,1)
  
  
}

colnames(dv2)<-c("PN","Rapport.ventes")
dv2<-dv2[which(dv2$Rapport.ventes>=1),]

barplot(dv2$Rapport.ventes,names.arg = dv2$PN)
pie(dv2$Rapport.ventes,labels = dv2$PN)


nom<-dv2[order(dv2[,2],decreasing = TRUE)[1:12],1]

valid.var[[as.character(nom)[1]]]
valid.var[[as.character(nom)[2]]]
valid.var[[as.character(nom)[3]]]
valid.var[[as.character(nom)[4]]]
valid.var[[as.character(nom)[5]]]
valid.var[[as.character(nom)[6]]]
valid.var[[as.character(nom)[7]]]
valid.var[[as.character(nom)[8]]]
valid.var[[as.character(nom)[9]]]
valid.var[[as.character(nom)[10]]]
valid.var[[as.character(nom)[11]]]
valid.var[[as.character(nom)[12]]]

valid.var.pn[[as.character(nom)[1]]]
valid.var.pn[[as.character(nom)[2]]]
valid.var.pn[[as.character(nom)[3]]]
valid.var.pn[[as.character(nom)[4]]]
valid.var.pn[[as.character(nom)[5]]]
valid.var.pn[[as.character(nom)[6]]]
valid.var.pn[[as.character(nom)[7]]]
valid.var.pn[[as.character(nom)[8]]]
valid.var.pn[[as.character(nom)[9]]]
valid.var.pn[[as.character(nom)[10]]]
valid.var.pn[[as.character(nom)[11]]]
valid.var.pn[[as.character(nom)[12]]]


#---------------------------


m01103ema.r<-liste.pn.zone$M01103$EMA[,2]
m01103feo.r<-liste.pn.zone$M01103$FEO[,2]
m01103nam.r<-liste.pn.zone$M01103$NAM[,2]

m01103ema.v<-liste.pn.zone$M01103$EMA[,3]
m01103feo.v<-liste.pn.zone$M01103$FEO[,3]
m01103nam.v<-liste.pn.zone$M01103$NAM[,3]



m01103ema.mod45<-modeleVAR(m01103ema.v,m01103ema.r,ordre=valid.var$M01103$EMA[[2]],coupe=45,n.ahead=4)
m01103feo.mod45<-modeleVAR(m01103feo.v,m01103feo.r,ordre=valid.var$M01103$FEO[[2]],coupe=45,n.ahead=4)
m01103nam.mod45<-modeleVAR(m01103nam.v,m01103nam.r,ordre=valid.var$M01103$NAM[[2]],coupe=45,n.ahead=4)

m01103ema.mod45$mod.r
m01103feo.mod45$mod.r
m01103nam.mod45$mod.r

m01103ema.mod46<-modeleVAR(m01103ema.v,m01103ema.r,ordre=valid.var$M01103$EMA[[2]],coupe=46,n.ahead=4)
m01103feo.mod46<-modeleVAR(m01103feo.v,m01103feo.r,ordre=valid.var$M01103$FEO[[2]],coupe=46,n.ahead=4)
m01103nam.mod46<-modeleVAR(m01103nam.v,m01103nam.r,ordre=valid.var$M01103$NAM[[2]],coupe=46,n.ahead=4)

m01103ema.mod46$mod.r
m01103feo.mod46$mod.r
m01103nam.mod46$mod.r

m01103ema.mod47<-modeleVAR(m01103ema.v,m01103ema.r,ordre=valid.var$M01103$EMA[[2]],coupe=47,n.ahead=4)
m01103feo.mod47<-modeleVAR(m01103feo.v,m01103feo.r,ordre=valid.var$M01103$FEO[[2]],coupe=47,n.ahead=4)
m01103nam.mod47<-modeleVAR(m01103nam.v,m01103nam.r,ordre=valid.var$M01103$NAM[[2]],coupe=47,n.ahead=4)

m01103ema.mod47$mod.r
m01103feo.mod47$mod.r
m01103nam.mod47$mod.r





m08201ema.r<-liste.pn.zone$M08201$EMA[,2]
m08201feo.r<-liste.pn.zone$M08201$FEO[,2]
m08201nam.r<-liste.pn.zone$M08201$NAM[,2]

m08201ema.v<-liste.pn.zone$M08201$EMA[,3]
m08201feo.v<-liste.pn.zone$M08201$FEO[,3]
m08201nam.v<-liste.pn.zone$M08201$NAM[,3]



m08201ema.mod45<-modeleVAR(m08201ema.v,m08201ema.r,ordre=valid.var$M08201$EMA[[2]],coupe=45,n.ahead=4)
m08201feo.mod45<-modeleVAR(m08201feo.v,m08201feo.r,ordre=valid.var$M08201$FEO[[2]],coupe=45,n.ahead=4)
m08201nam.mod45<-modeleVAR(m08201nam.v,m08201nam.r,ordre=valid.var$M08201$NAM[[2]],coupe=45,n.ahead=4)

m08201ema.mod45$mod.r
m08201feo.mod45$mod.r
m08201nam.mod45$mod.r

m08201ema.mod46<-modeleVAR(m08201ema.v,m08201ema.r,ordre=valid.var$M08201$EMA[[2]],coupe=46,n.ahead=4)
m08201feo.mod46<-modeleVAR(m08201feo.v,m08201feo.r,ordre=valid.var$M08201$FEO[[2]],coupe=46,n.ahead=4)
m08201nam.mod46<-modeleVAR(m08201nam.v,m08201nam.r,ordre=valid.var$M08201$NAM[[2]],coupe=46,n.ahead=4)

m08201ema.mod46$mod.r
m08201feo.mod46$mod.r
m08201nam.mod46$mod.r

m08201ema.mod47<-modeleVAR(m08201ema.v,m08201ema.r,ordre=valid.var$M08201$EMA[[2]],coupe=47,n.ahead=4)
m08201feo.mod47<-modeleVAR(m08201feo.v,m08201feo.r,ordre=valid.var$M08201$FEO[[2]],coupe=47,n.ahead=4)
m08201nam.mod47<-modeleVAR(m08201nam.v,m08201nam.r,ordre=valid.var$M08201$NAM[[2]],coupe=47,n.ahead=4)

m08201ema.mod47$mod.r
m08201feo.mod47$mod.r
m08201nam.mod47$mod.r





m05102ema.r<-liste.pn.zone$M05102$EMA[,2]
m05102feo.r<-liste.pn.zone$M05102$FEO[,2]
m05102nam.r<-liste.pn.zone$M05102$NAM[,2]

m05102ema.v<-liste.pn.zone$M05102$EMA[,3]
m05102feo.v<-liste.pn.zone$M05102$FEO[,3]
m05102nam.v<-liste.pn.zone$M05102$NAM[,3]



m05102ema.mod45<-modeleVAR(m05102ema.v,m05102ema.r,ordre=valid.var$M05102$EMA[[2]],coupe=45,n.ahead=4)
m05102feo.mod45<-modeleVAR(m05102feo.v,m05102feo.r,ordre=valid.var$M05102$FEO[[2]],coupe=45,n.ahead=4)
m05102nam.mod45<-modeleVAR(m05102nam.v,m05102nam.r,ordre=valid.var$M05102$NAM[[2]],coupe=45,n.ahead=4)

m05102ema.mod45$mod.r
m05102feo.mod45$mod.r
m05102nam.mod45$mod.r

m05102ema.mod46<-modeleVAR(m05102ema.v,m05102ema.r,ordre=valid.var$M05102$EMA[[2]],coupe=46,n.ahead=4)
m05102feo.mod46<-modeleVAR(m05102feo.v,m05102feo.r,ordre=valid.var$M05102$FEO[[2]],coupe=46,n.ahead=4)
m05102nam.mod46<-modeleVAR(m05102nam.v,m05102nam.r,ordre=valid.var$M05102$NAM[[2]],coupe=46,n.ahead=4)

m05102ema.mod46$mod.r
m05102feo.mod46$mod.r
m05102nam.mod46$mod.r

m05102ema.mod47<-modeleVAR(m05102ema.v,m05102ema.r,ordre=valid.var$M05102$EMA[[2]],coupe=47,n.ahead=4)
m05102feo.mod47<-modeleVAR(m05102feo.v,m05102feo.r,ordre=valid.var$M05102$FEO[[2]],coupe=47,n.ahead=4)
m05102nam.mod47<-modeleVAR(m05102nam.v,m05102nam.r,ordre=valid.var$M05102$NAM[[2]],coupe=47,n.ahead=4)

m05102ema.mod47$mod.r
m05102feo.mod47$mod.r
m05102nam.mod47$mod.r


glissante<-as.data.frame(matrix(data=c(NA),nrow=51,ncol=3*3*3+3*3))

glissante[,1]<-c(m01103ema.mod45$mod.r,NA,NA)
glissante[,2]<-c(m01103feo.mod45$mod.r,NA,NA)
glissante[,3]<-c(m01103nam.mod45$mod.r,NA,NA)

glissante[,4]<-c(m01103ema.mod46$mod.r,NA)
glissante[,5]<-c(m01103feo.mod46$mod.r,NA)
glissante[,6]<-c(m01103nam.mod46$mod.r,NA)

glissante[,7]<-c(m01103ema.mod47$mod.r)
glissante[,8]<-c(m01103feo.mod47$mod.r)
glissante[,9]<-c(m01103nam.mod47$mod.r)

glissante[,10]<-c(m08201ema.mod45$mod.r,NA,NA)
glissante[,11]<-c(m08201feo.mod45$mod.r,NA,NA)
glissante[,12]<-c(m08201nam.mod45$mod.r,NA,NA)

glissante[,13]<-c(m08201ema.mod46$mod.r,NA)
glissante[,14]<-c(m08201feo.mod46$mod.r,NA)
glissante[,15]<-c(m08201nam.mod46$mod.r,NA)

glissante[,16]<-c(m08201ema.mod47$mod.r)
glissante[,17]<-c(m08201feo.mod47$mod.r)
glissante[,18]<-c(m08201nam.mod47$mod.r)

glissante[,19]<-c(m05102ema.mod45$mod.r,NA,NA)
glissante[,20]<-c(m05102feo.mod45$mod.r,NA,NA)
glissante[,21]<-c(m05102nam.mod45$mod.r,NA,NA)

glissante[,22]<-c(m05102ema.mod46$mod.r,NA)
glissante[,23]<-c(m05102feo.mod46$mod.r,NA)
glissante[,24]<-c(m05102nam.mod46$mod.r,NA)

glissante[,25]<-c(m05102ema.mod47$mod.r)
glissante[,26]<-c(m05102feo.mod47$mod.r)
glissante[,27]<-c(m05102nam.mod47$mod.r)

glissante[,28]<-c(window(m01103ema.r,start=length(m01103ema.r)-47,end=length(m01103ema.r)),NA,NA,NA)
glissante[,29]<-c(window(m01103feo.r,start=length(m01103feo.r)-47,end=length(m01103feo.r)),NA,NA,NA)
glissante[,30]<-c(window(m01103nam.r,start=length(m01103nam.r)-47,end=length(m01103nam.r)),NA,NA,NA)

glissante[,31]<-c(window(m08201ema.r,start=length(m08201ema.r)-47,end=length(m08201ema.r)),NA,NA,NA)
glissante[,32]<-c(window(m08201feo.r,start=length(m08201feo.r)-47,end=length(m08201feo.r)),NA,NA,NA)
glissante[,33]<-c(window(m08201nam.r,start=length(m08201nam.r)-47,end=length(m08201nam.r)),NA,NA,NA)

glissante[,34]<-c(window(m05102ema.r,start=length(m05102ema.r)-47,end=length(m05102ema.r)),NA,NA,NA)
glissante[,35]<-c(window(m05102feo.r,start=length(m05102feo.r)-47,end=length(m05102feo.r)),NA,NA,NA)
glissante[,36]<-c(window(m05102nam.r,start=length(m05102nam.r)-47,end=length(m05102nam.r)),NA,NA,NA)

date2<-c(window(Date,start=length(Date)-47,end=length(Date)),as.Date("2015-10-01"),as.Date("2015-11-01"),as.Date("2015-12-01"))
glissante[,37]<-date2

              
colnames(glissante)<-c("M01103EMA.45",
                       "M01103FEO.45",
                       "M01103NAM.45",
                       "M01103EMA.46",
                       "M01103FEO.46",
                       "M01103NAM.46",
                       "M01103EMA.47",
                       "M01103FEO.47",
                       "M01103NAM.47",
                       "M08201EMA.45",
                       "M08201FEO.45",
                       "M08201NAM.45",
                       "M08201EMA.46",
                       "M08201FEO.46",
                       "M08201NAM.46",
                       "M08201EMA.47",
                       "M08201FEO.47",
                       "M08201NAM.47",
                       "M05102EMA.45",
                       "M05102FEO.45",
                       "M05102NAM.45",
                       "M05102EMA.46",
                       "M05102FEO.46",
                       "M05102NAM.46",
                       "M05102EMA.47",
                       "M05102FEO.47",
                       "M05102NAM.47",
                       "M01103EMA",
                       "M01103FEO",
                       "M01103NAM",
                       "M08201EMA",
                       "M08201FEO",
                       "M08201NAM",
                       "M05102EMA",
                       "M05102FEO",
                       "M05102NAM",
                       "Date"
                       )



# table.glissante<-data.frame(matrix(data=c(NA),ncol=5,nrow=3*3*3*length(date2)))
table.glissante<-data.frame(matrix(ncol=5))
PN<-c("M01103","M08201","M05102")
Zone<-c("EMA","FEO","NAM")
coupecoupe<-c(45,46,47)
Deb<-c(as.Date("2015-06-01"),as.Date("2015-07-01"),as.Date("2015-08-01"))
colnames(table.glissante)<-c("Date","PN","Zone","Debut.prevision","Retours")

for(i in 1:length(date2))
  
{
  k<-1
  
  while(k<=length(PN))
    
  {
    
    p<-1
    
    while(p<=length(Zone))
      
    {
      
      l<-1
      
      while(l<=length(Deb))
        
      {
        
      mod=NULL
      mod=modeleVAR(liste.pn.zone[[PN[k]]][[Zone[p]]][,3],liste.pn.zone[[PN[k]]][[Zone[p]]][,2],ordre=valid.var[[PN[k]]][[Zone[[p]]]][[2]],coupe=coupecoupe[l],n.ahead=4)
      
      table.glissante<-rbind(table.glissante,c(as.character(date2[i]),PN[k],Zone[p],as.character(Deb[l]),mod$mod.r[i]))
      
      l<-l+1
      
      }
      
      p<-p+1
      
    }
    
    k<-k+1
    
  }
 
  
  
  
  
}

table.glissante<-table.glissante[-1,]
View(table.glissante)
write.csv(table.glissante,file = "glissante.csv")



#----------------------------------------------Comparaison avec le modèle LPAV--------------------------------------------------------------------

forecast.LPAV<-read.csv(file="forecast.lpav.csv",sep=";",header = TRUE)
View(forecast.LPAV)
length(sort(as.Date(unique(retour_pn$Date)),decreasing = FALSE))
forecast.LPAV$Date<-sort(as.Date(unique(retour_pn$Date)),decreasing = FALSE)[-67]
Date<-sort(as.Date(unique(retour_pn$Date)),decreasing = FALSE)

nom<-dv2[order(dv2[,2],decreasing = TRUE)[1:12],1]

liste.reel<-liste.pn.zone[as.character(nom)[-12]]
liste.reel$M01103$EMA






#Transformer forecast.LPAV en liste
(substr(colnames(forecast.LPAV)[1],1,6)%in%names(liste.reel)[i])
(substr(colnames(forecast.LPAV)[1],7,9)%in%names(liste.reel[[i]])[j])
Zone<-c("EMA","FEO","NAM")
Date<-as.Date(as.character(forecast.LPAV$Date))
  
liste.forecast<-list()

{

  for (i in 1:((ncol(forecast.LPAV)-1)/3))
    
  {
    
    
    liste.forecast[[substr(colnames(forecast.LPAV)[3*i-2],1,6)]]<-list()
    
    
    
    for( j in 1:3)
      
    {
  
liste.forecast[[substr(colnames(forecast.LPAV)[3*i-2],1,6)]][[Zone[j]]]<-forecast.LPAV[,3*i-2+(j-1)]          

      
    }
    
    
  }


}



liste.LPAV<-liste.forecast

for(i in 1:length(liste.forecast))
  
{
  

  
  for(j in 1:length(liste.forecast[[i]]))
    
  {
    
    liste.LPAV[[i]][[j]]<-cbind(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][-(1:3),2], liste.forecast[[i]][[j]],100*abs(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][-(1:3),2]-liste.forecast[[i]][[j]])/liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][-(1:3),2])
    # colnames(liste.LPAV[[i]][[j]])<-c("Actuals","Forecast")                                         
    
  }
  
  
}



liste.erreur.LPAV<-liste.forecast

for(i in 1:length(liste.forecast))
  
{

  
  for(j in 1:length(liste.forecast[[i]]))
    
  {
    
liste.erreur.LPAV[[i]][[j]]<-mean(100*(abs(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][-(1:3),2][34:66]-
                                   liste.forecast[[i]][[j]][34:66])/liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][-(1:3),2][34:66]),na.rm=T)
 
  }
  
  

  
}






#-----------------------------

dv<-data.frame(vente_pn$Date,vente_pn$PN,vente_pn$VENTES)
dv2=aggregate(dv$vente_pn.VENTES,by=list(dv$vente_pn.Date,dv$vente_pn.PN),FUN=sum)
dv3=aggregate(dv$vente_pn.VENTES,by=list(dv$vente_pn.Date),FUN=sum)
colnames(dv2)<-c("Date","PN","Ventes")
colnames(dv3)<-c("Date","Ventes.totales")

for (i in 1:nrow(dv2))
  
{
  
  dv2[i,3]<-100*dv2[i,3]/dv3[match(dv2[i,1],dv3[,1]),2]
  
  
}

colnames(dv2)<-c("Date","PN","Rapport.ventes")

nom<-dv2[order(dv2[,2],decreasing = TRUE)[1:12],1]

liste.validation<-liste.pn.zone[nom]

mod.M01103.EMA.12<-calculmodele2(liste.validation$M01103$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M01103.FEO.12<-calculmodele2(liste.validation$M01103$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M01103.NAM.12<-calculmodele2(liste.validation$M01103$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M08201.EMA.12<-calculmodele2(liste.validation$M08201$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M08201.FEO.12<-calculmodele2(liste.validation$M08201$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M08201.NAM.12<-calculmodele2(liste.validation$M08201$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M05102.EMA.12<-calculmodele2(liste.validation$M05102$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M05102.FEO.12<-calculmodele2(liste.validation$M05102$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M05102.NAM.12<-calculmodele2(liste.validation$M05102$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M12301.EMA.12<-calculmodele2(liste.validation$M12301$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M12301.FEO.12<-calculmodele2(liste.validation$M12301$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M12301.NAM.12<-calculmodele2(liste.validation$M12301$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M01103.EMA.12
mod.M01103.FEO.12
mod.M01103.NAM.12
mod.M08201.EMA.12
mod.M08201.FEO.12
mod.M08201.EMA.12
mod.M05102.EMA.12
mod.M05102.FEO.12
mod.M05102.NAM.12
mod.M12301.EMA.12
mod.M12301.FEO.12
mod.M12301.NAM.12


mod.M01103.EMA.13<-calculmodele2(liste.validation$M01103$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M01103.FEO.13<-calculmodele2(liste.validation$M01103$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M01103.NAM.13<-calculmodele2(liste.validation$M01103$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M08201.EMA.13<-calculmodele2(liste.validation$M08201$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M08201.FEO.13<-calculmodele2(liste.validation$M08201$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M08201.NAM.13<-calculmodele2(liste.validation$M08201$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M05102.EMA.13<-calculmodele2(liste.validation$M05102$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M05102.FEO.13<-calculmodele2(liste.validation$M05102$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M05102.NAM.13<-calculmodele2(liste.validation$M05102$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M12301.EMA.13<-calculmodele2(liste.validation$M12301$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M12301.FEO.13<-calculmodele2(liste.validation$M12301$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M12301.NAM.13<-calculmodele2(liste.validation$M12301$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M01103.EMA.13
mod.M01103.FEO.13
mod.M01103.NAM.13
mod.M08201.EMA.13
mod.M08201.FEO.13
mod.M08201.EMA.13
mod.M05102.EMA.13
mod.M05102.FEO.13
mod.M05102.NAM.13
mod.M12301.EMA.13
mod.M12301.FEO.13
mod.M12301.NAM.13


mod.M01103.EMA.14<-calculmodele2(liste.validation$M01103$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M01103.FEO.14<-calculmodele2(liste.validation$M01103$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M01103.NAM.14<-calculmodele2(liste.validation$M01103$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M08201.EMA.14<-calculmodele2(liste.validation$M08201$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M08201.FEO.14<-calculmodele2(liste.validation$M08201$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M08201.NAM.14<-calculmodele2(liste.validation$M08201$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M05102.EMA.14<-calculmodele2(liste.validation$M05102$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M05102.FEO.14<-calculmodele2(liste.validation$M05102$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M05102.NAM.14<-calculmodele2(liste.validation$M05102$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M12301.EMA.14<-calculmodele2(liste.validation$M12301$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M12301.FEO.14<-calculmodele2(liste.validation$M12301$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M12301.NAM.14<-calculmodele2(liste.validation$M12301$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M01103.EMA.14
mod.M01103.FEO.14
mod.M01103.NAM.14
mod.M08201.EMA.14
mod.M08201.FEO.14
mod.M08201.EMA.14
mod.M05102.EMA.14
mod.M05102.FEO.14
mod.M05102.NAM.14
mod.M12301.EMA.14
mod.M12301.FEO.14
mod.M12301.NAM.14


mod.M01103.EMA.9<-calculmodele2(liste.validation$M01103$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M01103.FEO.9<-calculmodele2(liste.validation$M01103$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M01103.NAM.9<-calculmodele2(liste.validation$M01103$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M08201.EMA.9<-calculmodele2(liste.validation$M08201$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M08201.FEO.9<-calculmodele2(liste.validation$M08201$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M08201.NAM.9<-calculmodele2(liste.validation$M08201$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M05102.EMA.9<-calculmodele2(liste.validation$M05102$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M05102.FEO.9<-calculmodele2(liste.validation$M05102$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M05102.NAM.9<-calculmodele2(liste.validation$M05102$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M12301.EMA.9<-calculmodele2(liste.validation$M12301$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M12301.FEO.9<-calculmodele2(liste.validation$M12301$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M12301.NAM.9<-calculmodele2(liste.validation$M12301$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M01103.EMA.9
mod.M01103.FEO.9
mod.M01103.NAM.9
mod.M08201.EMA.9
mod.M08201.FEO.9
mod.M08201.EMA.9
mod.M05102.EMA.9
mod.M05102.FEO.9
mod.M05102.NAM.9
mod.M12301.EMA.9
mod.M12301.FEO.9
mod.M12301.NAM.9

mod.M01103.9<-calculmodele2(liste.pn$M01103[,2],coupe=28,n.ahead=2,train=12)
mod.M05102.9<-calculmodele2(liste.pn$M05102[,2],coupe=28,n.ahead=2,train=12)
mod.M08201.9<-calculmodele2(liste.pn$M08201[,2],coupe=28,n.ahead=2,train=12)
mod.M12301.9<-calculmodele2(liste.pn$M12301[,2],coupe=28,n.ahead=2,train=12)


mod.M01103.11<-calculmodele2(liste.pn$M01103[,2],coupe=31,n.ahead=2,train=12)
mod.M01103.12<-calculmodele2(liste.pn$M01103[,2],coupe=32,n.ahead=3,train=12)
mod.M01103.13<-calculmodele2(liste.pn$M01103[,2],coupe=33,n.ahead=4,train=12)
mod.M01103.14<-calculmodele2(liste.pn$M01103[,2],coupe=34,n.ahead=5,train=12)


mod.M05102.11<-calculmodele2(liste.pn$M05102[,2],coupe=31,n.ahead=2,train=12)
mod.M05102.12<-calculmodele2(liste.pn$M05102[,2],coupe=32,n.ahead=3,train=12)
mod.M05102.13<-calculmodele2(liste.pn$M05102[,2],coupe=33,n.ahead=4,train=12)
mod.M05102.14<-calculmodele2(liste.pn$M05102[,2],coupe=34,n.ahead=5,train=12)


mod.M08201.11<-calculmodele2(liste.pn$M08201[,2],coupe=31,n.ahead=2,train=12)
mod.M08201.12<-calculmodele2(liste.pn$M08201[,2],coupe=32,n.ahead=3,train=12)
mod.M08201.13<-calculmodele2(liste.pn$M08201[,2],coupe=33,n.ahead=4,train=12)
mod.M08201.14<-calculmodele2(liste.pn$M08201[,2],coupe=34,n.ahead=5,train=12)

mod.M12301.11<-calculmodele2(liste.pn$M12301[,2],coupe=31,n.ahead=2,train=12)
mod.M12301.12<-calculmodele2(liste.pn$M12301[,2],coupe=32,n.ahead=3,train=12)
mod.M12301.13<-calculmodele2(liste.pn$M12301[,2],coupe=33,n.ahead=4,train=12)
mod.M12301.14<-calculmodele2(liste.pn$M12301[,2],coupe=34,n.ahead=5,train=12)

mod.M01103.12
mod.M01103.13
mod.M01103.14
mod.M05102.12
mod.M05102.13
mod.M05102.14
mod.M08201.12
mod.M08201.13
mod.M08201.14
mod.M12301.12
mod.M12301.13
mod.M12301.14



plot(ylab="Retours",xlim=c(0,80),as.vector(window(liste.pn.zone$M08201$EMA[,2],start=length(liste.pn.zone$M08201$EMA[,2])-48,end=length(liste.pn.zone$M08201$EMA[,2]))),type='l')
lines(mod.M08201.EMA.12$mod,col='red')
lines(mod.M08201.EMA.13$mod,col='blue')
lines(mod.M08201.EMA.14$mod,col='purple')




plot(ylab="Retours",xlim=c(0,80),as.vector(window(liste.pn.zone$M01103$EMA[,2],start=length(liste.pn.zone$M01103$EMA[,2])-48,end=length(liste.pn.zone$M01103$EMA[,2]))),type='l')
lines(mod.M01103.EMA.12$mod,col='red')
lines(mod.M01103.EMA.13$mod,col='blue')
lines(mod.M01103.EMA.14$mod,col='purple')

plot(ylab="Retours",xlim=c(0,80),as.vector(window(liste.pn.zone$M01103$FEO[,2],start=length(liste.pn.zone$M01103$FEO[,2])-48,end=length(liste.pn.zone$M01103$FEO[,2]))),type='l')
lines(mod.M01103.FEO.9$mod,col='red')
lines(mod.M01103.FEO.13$mod,col='blue')
lines(mod.M01103.FEO.14$mod,col='purple')





plot(ylab="Retours",xlim=c(0,80),as.vector(window(liste.pn.zone$M08201$EMA[,2],start=length(liste.pn.zone$M08201$EMA[,2])-48,end=length(liste.pn.zone$M08201$EMA[,2]))),type='l')
lines(mod.M08201.EMA.12$mod,col='red')
lines(mod.M08201.EMA.13$mod,col='blue')




plot(ylab="Retours",xlim=c(0,80),as.vector(window(liste.pn$M01103[,2],start=length(liste.pn$M01103[,2])-48,end=length(liste.pn$M01103[,2]))),type='l')
lines(mod.M01103.12$mod,col='red')
lines(mod.M01103.13$mod,col='blue')
lines(mod.M01103.14$mod,col='purple')


plot(ylab="Retours",xlim=c(0,80),as.vector(window(liste.pn$M12301[,2],start=length(liste.pn$M12301[,2])-48,end=length(liste.pn$M12301[,2]))),type='l')
lines(mod.M12301.12$mod,col='red')
lines(mod.M12301.13$mod,col='blue')
lines(mod.M12301.14$mod,col='purple')

plot(ylab="Retours",xlim=c(0,80),as.vector(window(liste.pn$M08201[,2],start=length(liste.pn$M08201[,2])-48,end=length(liste.pn$M08201[,2]))),type='l')
lines(mod.M08201.12$mod,col='red')
lines(mod.M08201.13$mod,col='blue')
lines(mod.M08201.14$mod,col='purple')

plot(ylab="Retours",xlim=c(0,80),as.vector(window(liste.pn$M08201[,2],start=length(liste.pn$M08201[,2])-48,end=length(liste.pn$M08201[,2]))),type='l')
lines(mod.M08201.9$mod,col='red')
lines(mod.M08201.13$mod,col='blue')
lines(mod.M08201.14$mod,col='purple')





#--------------



mod.M18201.EMA.12<-calculmodele2(liste.validation$M18201$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M18201.FEO.12<-calculmodele2(liste.validation$M18201$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M18201.NAM.12<-calculmodele2(liste.validation$M18201$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M18602.EMA.12<-calculmodele2(liste.validation$M18602$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M18602.FEO.12<-calculmodele2(liste.validation$M18602$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M18602.NAM.12<-calculmodele2(liste.validation$M18602$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M13901.EMA.12<-calculmodele2(liste.validation$M13901$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M13901.FEO.12<-calculmodele2(liste.validation$M13901$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M13901.NAM.12<-calculmodele2(liste.validation$M13901$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M10001.EMA.12<-calculmodele2(liste.validation$M10001$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M10001.FEO.12<-calculmodele2(liste.validation$M10001$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M10001.NAM.12<-calculmodele2(liste.validation$M10001$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M18201.EMA.12
mod.M18201.FEO.12
mod.M18201.NAM.12
mod.M18602.EMA.12
mod.M18602.FEO.12
mod.M18602.EMA.12
mod.M13901.EMA.12
mod.M13901.FEO.12
mod.M13901.NAM.12
mod.M10001.EMA.12
mod.M10001.FEO.12
mod.M10001.NAM.12


mod.M18201.EMA.13<-calculmodele2(liste.validation$M18201$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M18201.FEO.13<-calculmodele2(liste.validation$M18201$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M18201.NAM.13<-calculmodele2(liste.validation$M18201$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M18602.EMA.13<-calculmodele2(liste.validation$M18602$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M18602.FEO.13<-calculmodele2(liste.validation$M18602$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M18602.NAM.13<-calculmodele2(liste.validation$M18602$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M13901.EMA.13<-calculmodele2(liste.validation$M13901$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M13901.FEO.13<-calculmodele2(liste.validation$M13901$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M13901.NAM.13<-calculmodele2(liste.validation$M13901$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M10001.EMA.13<-calculmodele2(liste.validation$M10001$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M10001.FEO.13<-calculmodele2(liste.validation$M10001$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M10001.NAM.13<-calculmodele2(liste.validation$M10001$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M18201.EMA.13
mod.M18201.FEO.13
mod.M18201.NAM.13
mod.M18602.EMA.13
mod.M18602.FEO.13
mod.M18602.EMA.13
mod.M13901.EMA.13
mod.M13901.FEO.13
mod.M13901.NAM.13
mod.M10001.EMA.13
mod.M10001.FEO.13
mod.M10001.NAM.13


mod.M18201.EMA.14<-calculmodele2(liste.validation$M18201$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M18201.FEO.14<-calculmodele2(liste.validation$M18201$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M18201.NAM.14<-calculmodele2(liste.validation$M18201$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M18602.EMA.14<-calculmodele2(liste.validation$M18602$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M18602.FEO.14<-calculmodele2(liste.validation$M18602$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M18602.NAM.14<-calculmodele2(liste.validation$M18602$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M13901.EMA.14<-calculmodele2(liste.validation$M13901$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M13901.FEO.14<-calculmodele2(liste.validation$M13901$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M13901.NAM.14<-calculmodele2(liste.validation$M13901$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M10001.EMA.14<-calculmodele2(liste.validation$M10001$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M10001.FEO.14<-calculmodele2(liste.validation$M10001$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M10001.NAM.14<-calculmodele2(liste.validation$M10001$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M18201.EMA.14
mod.M18201.FEO.14
mod.M18201.NAM.14
mod.M18602.EMA.14
mod.M18602.FEO.14
mod.M18602.EMA.14
mod.M13901.EMA.14
mod.M13901.FEO.14
mod.M13901.NAM.14
mod.M10001.EMA.14
mod.M10001.FEO.14
mod.M10001.NAM.14


mod.M18201.EMA.9<-calculmodele2(liste.validation$M18201$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M18201.FEO.9<-calculmodele2(liste.validation$M18201$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M18201.NAM.9<-calculmodele2(liste.validation$M18201$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M18602.EMA.9<-calculmodele2(liste.validation$M18602$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M18602.FEO.9<-calculmodele2(liste.validation$M18602$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M18602.NAM.9<-calculmodele2(liste.validation$M18602$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M13901.EMA.9<-calculmodele2(liste.validation$M13901$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M13901.FEO.9<-calculmodele2(liste.validation$M13901$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M13901.NAM.9<-calculmodele2(liste.validation$M13901$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M10001.EMA.9<-calculmodele2(liste.validation$M10001$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M10001.FEO.9<-calculmodele2(liste.validation$M10001$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M10001.NAM.9<-calculmodele2(liste.validation$M10001$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M18201.EMA.9
mod.M18201.FEO.9
mod.M18201.NAM.9
mod.M18602.EMA.9
mod.M18602.FEO.9
mod.M18602.EMA.9
mod.M13901.EMA.9
mod.M13901.FEO.9
mod.M13901.NAM.9
mod.M10001.EMA.9
mod.M10001.FEO.9
mod.M10001.NAM.9

mod.M18201.9<-calculmodele2(liste.pn$M18201[,2],coupe=28,n.ahead=2,train=12)
mod.M13901.9<-calculmodele2(liste.pn$M13901[,2],coupe=28,n.ahead=2,train=12)
mod.M18602.9<-calculmodele2(liste.pn$M18602[,2],coupe=28,n.ahead=2,train=12)
mod.M10001.9<-calculmodele2(liste.pn$M10001[,2],coupe=28,n.ahead=2,train=12)


mod.M18201.11<-calculmodele2(liste.pn$M18201[,2],coupe=31,n.ahead=2,train=12)
mod.M18201.12<-calculmodele2(liste.pn$M18201[,2],coupe=32,n.ahead=3,train=12)
mod.M18201.13<-calculmodele2(liste.pn$M18201[,2],coupe=33,n.ahead=4,train=12)
mod.M18201.14<-calculmodele2(liste.pn$M18201[,2],coupe=34,n.ahead=5,train=12)


mod.M13901.11<-calculmodele2(liste.pn$M13901[,2],coupe=31,n.ahead=2,train=12)
mod.M13901.12<-calculmodele2(liste.pn$M13901[,2],coupe=32,n.ahead=3,train=12)
mod.M13901.13<-calculmodele2(liste.pn$M13901[,2],coupe=33,n.ahead=4,train=12)
mod.M13901.14<-calculmodele2(liste.pn$M13901[,2],coupe=34,n.ahead=5,train=12)


mod.M18602.11<-calculmodele2(liste.pn$M18602[,2],coupe=31,n.ahead=2,train=12)
mod.M18602.12<-calculmodele2(liste.pn$M18602[,2],coupe=32,n.ahead=3,train=12)
mod.M18602.13<-calculmodele2(liste.pn$M18602[,2],coupe=33,n.ahead=4,train=12)
mod.M18602.14<-calculmodele2(liste.pn$M18602[,2],coupe=34,n.ahead=5,train=12)

mod.M10001.11<-calculmodele2(liste.pn$M10001[,2],coupe=31,n.ahead=2,train=12)
mod.M10001.12<-calculmodele2(liste.pn$M10001[,2],coupe=32,n.ahead=3,train=12)
mod.M10001.13<-calculmodele2(liste.pn$M10001[,2],coupe=33,n.ahead=4,train=12)
mod.M10001.14<-calculmodele2(liste.pn$M10001[,2],coupe=34,n.ahead=5,train=12)

mod.M18201.12
mod.M18201.13
mod.M18201.14
mod.M13901.12
mod.M13901.13
mod.M13901.14
mod.M18602.12
mod.M18602.13
mod.M18602.14
mod.M10001.12
mod.M10001.13
mod.M10001.14




#--------------



mod.M07601.EMA.12<-calculmodele2(liste.validation$M07601$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M07601.FEO.12<-calculmodele2(liste.validation$M07601$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M07601.NAM.12<-calculmodele2(liste.validation$M07601$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M20101.EMA.12<-calculmodele2(liste.validation$M20101$EMA[,2],coupe=32,n.ahead=3,train=12)
mod.M20101.FEO.12<-calculmodele2(liste.validation$M20101$FEO[,2],coupe=32,n.ahead=3,train=12)
mod.M20101.NAM.12<-calculmodele2(liste.validation$M20101$NAM[,2],coupe=32,n.ahead=3,train=12)
mod.M07601.EMA.12
mod.M07601.FEO.12
mod.M07601.NAM.12
mod.M20101.EMA.12
mod.M20101.FEO.12
mod.M20101.EMA.12



mod.M07601.EMA.13<-calculmodele2(liste.validation$M07601$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M07601.FEO.13<-calculmodele2(liste.validation$M07601$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M07601.NAM.13<-calculmodele2(liste.validation$M07601$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M20101.EMA.13<-calculmodele2(liste.validation$M20101$EMA[,2],coupe=33,n.ahead=4,train=12)
mod.M20101.FEO.13<-calculmodele2(liste.validation$M20101$FEO[,2],coupe=33,n.ahead=4,train=12)
mod.M20101.NAM.13<-calculmodele2(liste.validation$M20101$NAM[,2],coupe=33,n.ahead=4,train=12)
mod.M07601.EMA.13
mod.M07601.FEO.13
mod.M07601.NAM.13
mod.M20101.EMA.13
mod.M20101.FEO.13
mod.M20101.EMA.13
mod.M13901.EMA.13



mod.M07601.EMA.14<-calculmodele2(liste.validation$M07601$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M07601.FEO.14<-calculmodele2(liste.validation$M07601$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M07601.NAM.14<-calculmodele2(liste.validation$M07601$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M20101.EMA.14<-calculmodele2(liste.validation$M20101$EMA[,2],coupe=34,n.ahead=5,train=12)
mod.M20101.FEO.14<-calculmodele2(liste.validation$M20101$FEO[,2],coupe=34,n.ahead=5,train=12)
mod.M20101.NAM.14<-calculmodele2(liste.validation$M20101$NAM[,2],coupe=34,n.ahead=5,train=12)
mod.M07601.EMA.14
mod.M07601.FEO.14
mod.M07601.NAM.14
mod.M20101.EMA.14
mod.M20101.FEO.14
mod.M20101.EMA.14



mod.M07601.EMA.9<-calculmodele2(liste.validation$M07601$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M07601.FEO.9<-calculmodele2(liste.validation$M07601$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M07601.NAM.9<-calculmodele2(liste.validation$M07601$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M20101.EMA.9<-calculmodele2(liste.validation$M20101$EMA[,2],coupe=28,n.ahead=5,train=12)
mod.M20101.FEO.9<-calculmodele2(liste.validation$M20101$FEO[,2],coupe=28,n.ahead=5,train=12)
mod.M20101.NAM.9<-calculmodele2(liste.validation$M20101$NAM[,2],coupe=28,n.ahead=5,train=12)
mod.M07601.EMA.9
mod.M07601.FEO.9
mod.M07601.NAM.9
mod.M20101.EMA.9
mod.M20101.FEO.9
mod.M20101.EMA.9


mod.M07601.9<-calculmodele2(liste.pn$M07601[,2],coupe=28,n.ahead=2,train=12)
mod.M20101.9<-calculmodele2(liste.pn$M20101[,2],coupe=28,n.ahead=2,train=12)



mod.M07601.11<-calculmodele2(liste.pn$M07601[,2],coupe=31,n.ahead=2,train=12)
mod.M07601.12<-calculmodele2(liste.pn$M07601[,2],coupe=32,n.ahead=3,train=12)
mod.M07601.13<-calculmodele2(liste.pn$M07601[,2],coupe=33,n.ahead=4,train=12)
mod.M07601.14<-calculmodele2(liste.pn$M07601[,2],coupe=34,n.ahead=5,train=12)



mod.M20101.11<-calculmodele2(liste.pn$M20101[,2],coupe=31,n.ahead=2,train=12)
mod.M20101.12<-calculmodele2(liste.pn$M20101[,2],coupe=32,n.ahead=3,train=12)
mod.M20101.13<-calculmodele2(liste.pn$M20101[,2],coupe=33,n.ahead=4,train=12)
mod.M20101.14<-calculmodele2(liste.pn$M20101[,2],coupe=34,n.ahead=5,train=12)



mod.M07601.12
mod.M07601.13
mod.M07601.14
mod.M20101.12
mod.M20101.13
mod.M20101.14



#------------------------



#PN court
mean(c(mod.M01103.12$E.esti,mod.M01103.13$E.esti,mod.M01103.14$E.esti))
mean(c(mod.M08201.12$E.esti,mod.M08201.13$E.esti,mod.M08201.14$E.esti))
mean(c(mod.M12301.12$E.esti,mod.M12301.13$E.esti,mod.M12301.14$E.esti))
mean(c(mod.M05102.12$E.esti,mod.M05102.13$E.esti,mod.M05102.14$E.esti))
mean(c(mod.M18201.12$E.esti,mod.M18201.13$E.esti,mod.M18201.14$E.esti))
mean(c(mod.M18602.12$E.esti,mod.M18602.13$E.esti,mod.M18602.14$E.esti))
mean(c(mod.M13901.12$E.esti,mod.M13901.13$E.esti,mod.M13901.14$E.esti))
mean(c(mod.M10001.12$E.esti,mod.M10001.13$E.esti,mod.M10001.14$E.esti))



#PN long
mod.M01103.9$E.esti
mod.M08201.9$E.esti
mod.M12301.9$E.esti
mod.M05102.9$E.esti
mod.M18201.9$E.esti
mod.M18602.9$E.esti
mod.M13901.9$E.esti
mod.M10001.9$E.esti

#PN-zone court
mean(c(mod.M01103.EMA.12$E.esti,mod.M01103.EMA.13$E.esti,mod.M01103.EMA.14$E.esti))
mean(c(mod.M01103.FEO.12$E.esti,mod.M01103.FEO.13$E.esti,mod.M01103.FEO.14$E.esti))
mean(c(mod.M01103.NAM.12$E.esti,mod.M01103.NAM.13$E.esti,mod.M01103.NAM.14$E.esti))

mean(c(mod.M08201.EMA.12$E.esti,mod.M08201.EMA.13$E.esti,mod.M08201.EMA.14$E.esti))
mean(c(mod.M08201.FEO.12$E.esti,mod.M08201.FEO.13$E.esti,mod.M08201.FEO.14$E.esti))
mean(c(mod.M08201.NAM.12$E.esti,mod.M08201.NAM.13$E.esti,mod.M08201.NAM.14$E.esti))


mean(c(mod.M12301.EMA.12$E.esti,mod.M12301.EMA.13$E.esti,mod.M12301.EMA.14$E.esti))
mean(c(mod.M12301.FEO.12$E.esti,mod.M12301.FEO.13$E.esti,mod.M12301.FEO.14$E.esti))
mean(c(mod.M12301.NAM.12$E.esti,mod.M12301.NAM.13$E.esti,mod.M12301.NAM.14$E.esti))

mean(c(mod.M05102.EMA.12$E.esti,mod.M05102.EMA.13$E.esti,mod.M05102.EMA.14$E.esti))
mean(c(mod.M05102.FEO.12$E.esti,mod.M05102.FEO.13$E.esti,mod.M05102.FEO.14$E.esti))
mean(c(mod.M05102.NAM.12$E.esti,mod.M05102.NAM.13$E.esti,mod.M05102.NAM.14$E.esti))



mean(c(mod.M18201.EMA.12$E.esti,mod.M18201.EMA.13$E.esti,mod.M18201.EMA.14$E.esti))
mean(c(mod.M18201.FEO.12$E.esti,mod.M18201.FEO.13$E.esti,mod.M18201.FEO.14$E.esti))
mean(c(mod.M18201.NAM.12$E.esti,mod.M18201.NAM.13$E.esti,mod.M18201.NAM.14$E.esti))

mean(c(mod.M18602.EMA.12$E.esti,mod.M18602.EMA.13$E.esti,mod.M18602.EMA.14$E.esti))
mean(c(mod.M18602.FEO.12$E.esti,mod.M18602.FEO.13$E.esti,mod.M18602.FEO.14$E.esti))
mean(c(mod.M18602.NAM.12$E.esti,mod.M18602.NAM.13$E.esti,mod.M18602.NAM.14$E.esti))


mean(c(mod.M13901.EMA.12$E.esti,mod.M13901.EMA.13$E.esti,mod.M13901.EMA.14$E.esti))
mean(c(mod.M13901.FEO.12$E.esti,mod.M13901.FEO.13$E.esti,mod.M13901.FEO.14$E.esti))
mean(c(mod.M13901.NAM.12$E.esti,mod.M13901.NAM.13$E.esti,mod.M13901.NAM.14$E.esti))

mean(c(mod.M10001.EMA.12$E.esti,mod.M10001.EMA.13$E.esti,mod.M10001.EMA.14$E.esti))
mean(c(mod.M10001.FEO.12$E.esti,mod.M10001.FEO.13$E.esti,mod.M10001.FEO.14$E.esti))
mean(c(mod.M10001.NAM.12$E.esti,mod.M10001.NAM.13$E.esti,mod.M10001.NAM.14$E.esti))


#PN-zone long

mod.M01103.EMA.9$E.esti
mod.M01103.FEO.9$E.esti
mod.M01103.NAM.9$E.esti

mod.M08201.EMA.9$E.esti
mod.M08201.FEO.9$E.esti
mod.M08201.NAM.9$E.esti

mod.M05102.EMA.9$E.esti
mod.M05102.FEO.9$E.esti
mod.M05102.NAM.9$E.esti

mod.M12301.EMA.9$E.esti
mod.M12301.FEO.9$E.esti
mod.M12301.NAM.9$E.esti

mod.M18201.EMA.9$E.esti
mod.M18201.FEO.9$E.esti
mod.M18201.NAM.9$E.esti

mod.M18602.EMA.9$E.esti
mod.M18602.FEO.9$E.esti
mod.M18602.NAM.9$E.esti

mod.M13901.EMA.9$E.esti
mod.M13901.FEO.9$E.esti
mod.M13901.NAM.9$E.esti

mod.M10001.EMA.9$E.esti
mod.M10001.FEO.9$E.esti
mod.M10001.NAM.9$E.esti


#-----------

#PN court
mean(c(mod.M20101.12$E.esti,mod.M20101.13$E.esti,mod.M20101.14$E.esti))
mean(c(mod.M07601.12$E.esti,mod.M07601.13$E.esti,mod.M07601.14$E.esti))

#PN long
mod.M20101.9$E.esti
mod.M07601.9$E.esti

#PN-zone court

mean(c(mod.M20101.EMA.12$E.esti,mod.M20101.EMA.13$E.esti,mod.M20101.EMA.14$E.esti))
mean(c(mod.M20101.FEO.12$E.esti,mod.M20101.FEO.13$E.esti,mod.M20101.FEO.14$E.esti))
mean(c(mod.M20101.NAM.12$E.esti,mod.M20101.NAM.13$E.esti,mod.M20101.NAM.14$E.esti))

mean(c(mod.M07601.EMA.12$E.esti,mod.M07601.EMA.13$E.esti,mod.M07601.EMA.14$E.esti))
mean(c(mod.M07601.FEO.12$E.esti,mod.M07601.FEO.13$E.esti,mod.M07601.FEO.14$E.esti))
mean(c(mod.M07601.NAM.12$E.esti,mod.M07601.NAM.13$E.esti,mod.M07601.NAM.14$E.esti))


#PN-zone long

mod.M20101.EMA.9$E.esti
mod.M20101.FEO.9$E.esti
mod.M20101.NAM.9$E.esti

mod.M07601.EMA.9$E.esti
mod.M07601.FEO.9$E.esti
mod.M07601.NAM.9$E.esti


#------------Creation des listes errreur LPAV


#Modele 2 court : 23-35 sur 36 mois 

liste.erreur.LPAV<-liste.forecast

for(i in 1:length(liste.forecast))
  
{
  
  
  for(j in 1:length(liste.forecast[[i]]))
    
  {
    
    liste.erreur.LPAV[[i]][[j]]<-mean(100*(abs(as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-36,end=66))[23:35]-
                                                 as.vector(window(liste.forecast[[i]][[j]],start=66-36,end=66))[23:35])/as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-36,end=66))[23:35]),na.rm=TRUE)
    
  }
  
  
  
  
}

liste.erreur.LPAV[nom]

#Modele 2 long : 34-46 sur 48 mois 

liste.erreur.LPAV<-liste.forecast

for(i in 1:length(liste.forecast))
  
{
  
  
  for(j in 1:length(liste.forecast[[i]]))
    
  {
  
    liste.erreur.LPAV[[i]][[j]]<-mean(100*(abs(as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-48,end=66)[36:46])-
                                                 as.vector(window(liste.forecast[[i]][[j]],start=66-48,end=66)))[36:46]/as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-48,end=66)))[36:46],na.rm=TRUE)
    
  }
  
  
  
  
}

liste.erreur.LPAV[nom]

#Modele 1 court : 43-48 sur 48 mois 

liste.erreur.LPAV<-liste.forecast

for(i in 1:length(liste.forecast))
  
{
  
  
  for(j in 1:length(liste.forecast[[i]]))
    
  {
    
    liste.erreur.LPAV[[i]][[j]]<-mean(100*(abs(as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-48,end=66)[43:48])-
                                                 as.vector(window(liste.forecast[[i]][[j]],start=66-48,end=66))[43:48])/as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-48,end=66)))[43:48],na.rm=TRUE)
    
  }
  
  
  
  
}

liste.erreur.LPAV[nom]

#Modele 1 long : 40-48 sur 48 mois 

liste.erreur.LPAV<-liste.forecast

for(i in 1:length(liste.forecast))
  
{
  
  
  for(j in 1:length(liste.forecast[[i]]))
    
  {
    
    liste.erreur.LPAV[[i]][[j]]<-mean(100*(abs(as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-48,end=66)[40:48])-
                                                 as.vector(window(liste.forecast[[i]][[j]],start=66-48,end=66))[40:48])/as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-48,end=66)))[40:48],na.rm=TRUE)
    
  }
  
  
  
  
}

#Modele 1 long : 40-48 sur 48 mois 

liste.erreur.LPAV<-liste.forecast

for(i in 1:length(liste.forecast))
  
{
  
  
  for(j in 1:length(liste.forecast[[i]]))
    
  {
    
    liste.erreur.LPAV[[i]][[j]]<-mean(100*(abs(as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-24,end=66)[11:23])-
                                                 as.vector(window(liste.forecast[[i]][[j]],start=66-24,end=66))[11:23])/as.vector(window(liste.reel[[names(liste.forecast)[i]]][[names(liste.forecast[[i]])[j]]][,2],start=66-24,end=66)))[11:23],na.rm=TRUE)
    
  }
  
  
  
  
}
liste.erreur.LPAV[nom]


#--------------Volumetrie------------------------

volumetrie.totale<-0

for (i in 1:length(liste.pn))
  
{
  
  
  volumetrie.totale<-volumetrie.totale+sum(as.vector(window(liste.pn[[i]][,2],start=length(liste.pn[[i]][,1])-48,end=length(liste.pn[[i]][,1])))[40:48],na.rm=TRUE)
  
  
  
}




volumetrie<-liste.pn

for (i in 1:length(liste.pn))
  
{
  
  
  volumetrie[[i]]<-100*(sum(as.vector(window(liste.pn[[i]][,2],start=length(liste.pn[[i]][,1])-48,end=length(liste.pn[[i]][,1])))[40:48],na.rm=TRUE)/volumetrie.totale)
  
  
}

volumetrie[nom]



#--------------------------------------------------------------------Liste finale maille PN zone contrats CPL et SUP----------------------------------------

vente_pn_2<-subset(vente_pn,!CM=="20")

dv<-data.frame(vente_pn_2$Date,vente_pn_2$PN,vente_pn_2$VENTES,vente_pn_2$ZONE)
dr<-data.frame(retour_pn$Date,retour_pn$PN,retour_pn$compteur,retour_pn$Zone)

colnames(dv)<-c("Date","PN","Ventes","Zone")
colnames(dr)<-c("Date","PN","Retours","Zone")


dV<-aggregate(dv$Vente,by=list(dv$Date,dv$PN,dv$Zone),FUN=sum)
dR<-aggregate(dr$Retours,by=list(dr$Date,dr$PN,dr$Zone),FUN=sum)
colnames(dV)<-c("Date","PN","Zone","Ventes")
colnames(dR)<-c("Date","PN","Zone","Retours")


liste.CPL.vente.zone<-list()
nom<-c()

for(j in 1:length(unique(dV$PN)))
  
{
  
  
  liste.CPL.vente.zone[[j]]<-subset(dV, PN==unique(dV$PN)[j])[,-2]
  nom<-c(nom,as.character(unique(dV$PN)[j]))
  names(liste.CPL.vente.zone)<-nom
  
  
  
  
}


liste.CPL.retour.zone<-list()
nom<-c()

for(j in 1:length(unique(dR$PN)))
  
{
  
  
  liste.CPL.retour.zone[[j]]<-subset(dR, PN==unique(dR$PN)[j])[,-2]
  nom<-c(nom,as.character(unique(dR$PN)[j]))
  names(liste.CPL.retour.zone)<-nom
  
  
  
  
}

liste.CPL.pn.retour.zone<-list(list())
length(liste.CPL.pn.retour.zone)=length(liste.CPL.retour.zone)
nom<-c()


for(j in 1:length(liste.CPL.retour.zone))
  
{
  
  for(i in 1:length(unique(liste.CPL.retour.zone[[j]][,2])))
    
  {
    
    liste.CPL.pn.retour.zone[[j]][[i]]<-subset(liste.CPL.retour.zone[[j]], Zone==as.vector(unique(liste.CPL.retour.zone[[j]][,2]))[i])[,-2]
    
    
  }
  
  
}

names(liste.CPL.pn.retour.zone)<-as.character(names(liste.CPL.retour.zone))


for(k in 1:length(names(liste.CPL.pn.retour.zone)))
  
{
  
  names(liste.CPL.pn.retour.zone[[k]])<-as.vector(unique(liste.CPL.retour.zone[[k]][,2]))
  
  
}



liste.CPL.pn.vente.zone<-list(list())
length(liste.CPL.pn.vente.zone)=length(liste.CPL.vente.zone)
nom<-c()


for(j in 1:length(liste.CPL.vente.zone))
  
{
  
  for(i in 1:length(unique(liste.CPL.vente.zone[[j]][,2])))
    
  {
    
    liste.CPL.pn.vente.zone[[j]][[i]]<-subset(liste.CPL.vente.zone[[j]], Zone==as.vector(unique(liste.CPL.vente.zone[[j]][,2]))[i])[,-2]
    
    
  }
  
  
}

names(liste.CPL.pn.vente.zone)<-as.character(names(liste.CPL.vente.zone))


for(k in 1:length(names(liste.CPL.pn.vente.zone)))
  
{
  
  names(liste.CPL.pn.vente.zone[[k]])<-as.vector(unique(liste.CPL.vente.zone[[k]][,2]))
  
  
}

Date<-c(
  as.Date("2010-04-01")
  ,as.Date("2010-05-01")
  ,as.Date("2010-06-01")
  ,as.Date("2010-07-01")
  ,as.Date("2010-08-01")
  ,as.Date("2010-09-01")
  ,as.Date("2010-10-01")
  ,as.Date("2010-11-01")
  ,as.Date("2010-12-01")
  ,as.Date("2011-01-01")
  ,as.Date("2011-02-01")
  ,as.Date("2011-03-01")
  ,as.Date("2011-04-01")
  ,as.Date("2011-05-01")
  ,as.Date("2011-06-01")
  ,as.Date("2011-07-01")
  ,as.Date("2011-08-01")
  ,as.Date("2011-09-01")
  ,as.Date("2011-10-01")
  ,as.Date("2011-11-01")
  ,as.Date("2011-12-01")
  ,as.Date("2012-01-01")
  ,as.Date("2012-02-01")
  ,as.Date("2012-03-01")
  ,as.Date("2012-04-01")
  ,as.Date("2012-05-01")
  ,as.Date("2012-06-01")
  ,as.Date("2012-07-01")
  ,as.Date("2012-08-01")
  ,as.Date("2012-09-01")
  ,as.Date("2012-10-01")
  ,as.Date("2012-11-01")
  ,as.Date("2012-12-01")
  ,as.Date("2013-01-01")
  ,as.Date("2013-02-01")
  ,as.Date("2013-03-01")
  ,as.Date("2013-04-01")
  ,as.Date("2013-05-01")
  ,as.Date("2013-06-01")
  ,as.Date("2013-07-01")
  ,as.Date("2013-08-01")
  ,as.Date("2013-09-01")
  ,as.Date("2013-10-01")
  ,as.Date("2013-11-01")
  ,as.Date("2013-12-01")
  ,as.Date("2014-01-01")
  ,as.Date("2014-02-01")
  ,as.Date("2014-03-01")
  ,as.Date("2014-04-01")
  ,as.Date("2014-05-01")
  ,as.Date("2014-06-01")
  ,as.Date("2014-07-01")
  ,as.Date("2014-08-01")
  ,as.Date("2014-09-01")
  ,as.Date("2014-10-01")
  ,as.Date("2014-11-01")
  ,as.Date("2014-12-01")
  ,as.Date("2015-01-01")
  ,as.Date("2015-02-01")
  ,as.Date("2015-03-01")
  ,as.Date("2015-04-01")
  ,as.Date("2015-05-01")
  ,as.Date("2015-06-01")
  ,as.Date("2015-07-01")
  ,as.Date("2015-08-01")
  ,as.Date("2015-09-01")
)

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


for(j in 1:length(liste.CPL.pn.retour.zone))
  
{
  
  for(i in 1:length(liste.CPL.pn.retour.zone[[j]]))
    
  {
    
    liste.CPL.pn.retour.zone[[j]][[i]]<-remplacerparNA(liste.CPL.pn.retour.zone[[j]][[i]],"Retours")
    
  }
  
}  



for(j in 1:length(liste.CPL.pn.vente.zone))
  
{
  
  for(i in 1:length(liste.CPL.pn.vente.zone[[j]]))
    
  {
    
    liste.CPL.pn.vente.zone[[j]][[i]]<-remplacerparNA(liste.CPL.pn.vente.zone[[j]][[i]],"Ventes")
    
  }
  
}  



liste.CPL.pn.zone<-liste.CPL.pn.retour.zone

for(j in 1:length(liste.CPL.pn.zone))
  
{
  
  for(i in 1:length(liste.CPL.pn.zone[[j]]))
    
  {
    
    
    
    liste.CPL.pn.zone[[j]][[i]]$Ventes<-liste.CPL.pn.vente.zone[[match(names(liste.CPL.pn.zone)[j],names(liste.CPL.pn.vente.zone))]][[match(names(liste.CPL.pn.zone[[j]])[i],names(liste.CPL.pn.vente.zone[[match(names(liste.CPL.pn.zone)[j],names(liste.CPL.pn.vente.zone))]]))]][,2]
    
  }
  
}


#test


nom<-c("M01103","M08201","M05102","M12301","M18602","M13901","M10001","M20101","M07601")


valid.var.CPL<-liste.CPL.pn.zone[nom]

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 1:length(liste.CPL.pn.zone[nom]))
  
{
  
  
  for (j in 1:length(liste.CPL.pn.zone[nom][[i]]))
    
    
  {
    
    
    
   
    if( (ncol(liste.CPL.pn.zone[nom][[i]][[j]])<3)& (j<3) )
      
    {
      j<-j+1
    }
    
    if( (ncol(liste.CPL.pn.zone[nom][[i]][[j]])<3)& (j==3) )
      
    {
      valid.var.CPL[[i]][[j]]<-NA
  
    }
    
    if( (ncol(liste.CPL.pn.zone[nom][[i]][[j]])==3) )
      
    {
      
      x=NULL
      y=NULL
      v=NULL
      
    x=remplacerparNA(liste.CPL.pn.zone[nom][[i]][[j]][,-3],"Retours")[,2] 
    
    y=remplacerparNA(liste.CPL.pn.zone[nom][[i]][[j]][,-2],"Ventes")[,2]
    x=replace(x,which(is.na(x)),0)
    y=replace(y,which(is.na(y)),0)
    
    try( v<-validation(vente=x,retour=y,kmin=2,n.ahead = 3,coupemin=23,coupemax=35,fenetre=36) )
    
    valid.var.CPL[[i]][[j]]<-list(v$mean,v$ordremin)  
    
    }
    
  }
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.CPL.pn.zone[nom])))))
  setWinProgressBar(pb, 100*(i/(length(liste.CPL.pn.zone[nom]))), label=info)
  
  
}

valid.var.CPL$M01103$NAM
valid.var.CPL$M08201$NAM
valid.var.CPL$M05102$NAM
valid.var.CPL$M05102$NAM
valid.var.CPL$M12301$NAM
valid.var.CPL$M18602$NAM
valid.var.CPL$M20101$NAM
valid.var.CPL$M07601$NAM


#--------------------------------------Valid.Var avec fenetre de deux ans-------------------------


valid.var.2ans<-liste.pn.zone[nom]

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 1:length(liste.pn.zone[nom]))
  
{
  
  
  for (j in 1:length(liste.pn.zone[nom][[i]]))
    
    
  {
    
    if( (ncol(liste.pn.zone[nom][[i]][[j]])<3)& (j<3) )
      
    {
      j<-j+1
    }
    
    if( (ncol(liste.pn.zone[nom][[i]][[j]])<3)& (j==3) )
      
    {
      valid.var.2ans[[i]][[j]]<-NA
      
    }
    
    if( (ncol(liste.pn.zone[nom][[i]][[j]])==3) )
      
    {
      
      x=NULL
      y=NULL
      v=NULL
      
      x=remplacerparNA(liste.pn.zone[nom][[i]][[j]][,-3],"Retours")[,2] 
      
      y=remplacerparNA(liste.pn.zone[nom][[i]][[j]][,-2],"Ventes")[,2]
      x=replace(x,which(is.na(x)),0)
      y=replace(y,which(is.na(y)),0)
      
      try( v<-validation(vente=x,retour=y,kmin=2,kmax=9,n.ahead = 3,coupemin=11,coupemax=23,fenetre=24) )
      
      valid.var.2ans[[i]][[j]]<-list(v$mean,v$ordremin)  
    
  }
  
  info <- sprintf("%d%% done", round(100*(i/(length(liste.pn.zone)))))
  setWinProgressBar(pb, 100*(i/(length(liste.pn.zone))), label=info)
  
  
  }
  
  
}




#--------------vente


validation.v<-function(vente,retour,n.ahead,coupemin=34,coupemax=46,kmin=6,kmax=15,fenetre=48)
  
{
  
  result<-list(mean=NA,sd=NA,ordremin=NA)  
  mean<-c()
  sd<-c()
  
  
  for( k in kmin:kmax)
    
  {
    
    
    
    
    
    evolution.erreur<-c()
    
    for(i in coupemin:coupemax)
      
    {
      
      var=NULL
      var=modeleVAR(vente,retour,coupe=i,ordre=k,n.ahead = n.ahead,fenetre=fenetre)
      
      evolution.erreur[i]<-var$mean.error.v
      i<-i+1
      
    }
    
    
    
    mean[k]<-mean(evolution.erreur,na.rm=TRUE)
    sd[k]<-sd(evolution.erreur,na.rm=TRUE)
    k=k+1
    
  }
  
  result[[1]]=mean
  result[[2]]=sd
  result[[3]]=which.min(mean)
  
  return(result)
  
}




#------




valid.var.3ans.v<-liste.pn.zone[nom]

pb <- winProgressBar(title="Progress bar", label="0% done", min=0, max=100, initial=0)

for( i in 1:length(liste.pn.zone[nom]))
  
{
  
  
  for (j in 1:length(liste.pn.zone[nom][[i]]))
    
    
  {
    
    if( (ncol(liste.pn.zone[nom][[i]][[j]])<3)& (j<3) )
      
    {
      j<-j+1
    }
    
    if( (ncol(liste.pn.zone[nom][[i]][[j]])<3)& (j==3) )
      
    {
      valid.var.2ans[[i]][[j]]<-NA
      
    }
    
    if( (ncol(liste.pn.zone[nom][[i]][[j]])==3) )
      
    {
      
      x=NULL
      y=NULL
      v=NULL
      
      x=remplacerparNA(liste.pn.zone[nom][[i]][[j]][,-3],"Retours")[,2] 
      
      y=remplacerparNA(liste.pn.zone[nom][[i]][[j]][,-2],"Ventes")[,2]
      x=replace(x,which(is.na(x)),0)
      y=replace(y,which(is.na(y)),0)
      
      try( v<-validation.v(vente=x,retour=y,kmin=2,n.ahead = 3,coupemin=23,coupemax=35,fenetre=36) )
      
      valid.var.2ans[[i]][[j]]<-list(v$mean,v$ordremin)  
      
    }
    
    info <- sprintf("%d%% done", round(100*(i/(length(liste.pn.zone[nom])))))
    setWinProgressBar(pb, 100*(i/(length(liste.pn.zone[nom]))), label=info)
    
    
  }
  
  
}




vecteur.actuals.et.forecast<-c()
vecteur.dates<-c()

data.frame.entree<-liste.pn.zone$M01103$EMA[,2]   #on prend le vecteur qui correspond aux retours carcasses : c'est la deuxième colonne dans la liste liste.pn.zone$M01103$EMA
modele<-calculmodele2(data.frame.entree,coupe=24,n.ahead=3,train=12,fenetre=36)
vecteur.actuals.et.forecast<-c( liste.pn.zone$M01103$EMA[,2] , modele$mod[(length(modele$mod)-(3-1)):length(modele$mod)] )
vecteur.dates<-c(liste.pn.zone$M01103$EMA[,1],as.Date(sapply(1:3,function(k){as.Date(paste(substr(liste.pn.zone$M01103$EMA[length(liste.pn.zone$M01103$EMA[,1]),1]+31*k,1,8),"01",sep=""),format="%Y-%m-%d")})))


data.frame.sortie<-data.frame(Date = as.Date(rep(0,length(vecteur.actuals.et.forecast)), origin = liste.pn.zone$M01103$EMA[1,1] ),Retours = numeric(length(vecteur.actuals.et.forecast)))
data.frame.sortie[,1]<- vecteur.dates                          
data.frame.sortie[,2]<-vecteur.actuals.et.forecast
data.frame.sortie

v=NULL

for(i in 1:length(liste.pn.zone[nom[1:4]]))
{
  for(j in 1:length(liste.pn.zone[nom[1:4]][[i]]))
    
  {
    
    
    v<-c(v,liste.pn.zone[nom[1:4]][[i]][[j]][,2])
    
  }
  
  
}

V<-matrix(data=c(NA),nrow=length(v),ncol=2)
V[,2]<-v
V[,1]<-10*c(1:792)/792
plot(V,type='l')
b=spectrum(V[,2])

V<-matrix(data=c(NA),nrow=length(donnees$TAT),ncol=2)

V[,2]<-donnees$TAT
V[,1]<-10*c(1:length(donnees$TAT))/length(donnees$TAT)
abs(fft(V[,2]))
musicMatrix(V,"audio.wav")



b<-read.csv("M01103-02")
w<-c()

for(i in 1:length(b[,-1]))
{
  
  w[i]<-b[1,-1][i] 
  
  
}

unlist(w)




as.Date(paste(substr(substr(names(b)[2],2,10),nchar(substr(names(b)[2],2,10))-3,nchar(substr(names(b)[2],2,10))),
              paste("0",substr(substr(names(b)[2],2,10),1,nchar(substr(names(b)[2],2,10))-5),sep=""),"01",sep="-"))

as.Date(sapply(2:length(names(b)),function(i){as.Date(paste(substr(substr(names(b)[i],2,10),nchar(substr(names(b)[i],2,10))-3,nchar(substr(names(b)[i],2,10))),
                                                            paste("",substr(substr(names(b)[i],2,10),1,nchar(substr(names(b)[i],2,10))-5),sep=""),"01",sep="-"))}))



as.Date

(
  
  paste(substr(substr(names(b)[2],2,10),nchar(substr(names(b)[2],2,10))-3,nchar(substr(names(b)[2],2,10))),
  paste("",substr(substr(names(b)[2],2,10),1,nchar(substr(names(b)[2],2,10))-5),sep=""),"01",sep="-")
  
)


mot<-c()
for(k in 2:12)
{
  
mot[k]<-paste(substr(substr(names(b)[k],2,10),nchar(substr(names(b)[k],2,10))-3,nchar(substr(names(b)[k],2,10))),
paste("",substr(substr(names(b)[k],2,10),1,nchar(substr(names(b)[k],2,10))-5),sep=""),"01",sep="-")

}



data.frame.entree <- read.csv("M01103-02.csv")


vecteur.actuals<-c()

for(i in 1:length(data.frame.entree[,-1]))
{
  
  vecteur.actuals[i]<-data.frame.entree[1,-1][i] 
  
  
}

vecteur.actuals<-unlist(vecteur.actuals)


vecteurs.dates.actuals<-as.Date(sapply(2:length(names(data.frame.entree )),function(i){as.Date(paste(substr(substr(names(data.frame.entree )[i],2,10),nchar(substr(names(data.frame.entree )[i],2,10))-3,nchar(substr(names(data.frame.entree )[i],2,10))),
                                                                                    paste("",substr(substr(names(data.frame.entree )[i],2,10),1,nchar(substr(names(data.frame.entree )[i],2,10))-5),sep=""),"01",sep="-"))}))



vecteur.actuals.et.forecast<-c()
vecteur.dates<-c()




modele<-calculmodele2(vecteur.actuals,coupe=24,n.ahead=3,train=12,fenetre=36)
vecteur.actuals.et.forecast<-c( vecteur.actuals , modele$mod[(length(modele$mod)-(3-1)):length(modele$mod)] )
vecteur.dates<-c(vecteurs.dates.actuals,as.Date(sapply(1:3,function(k){as.Date(paste(substr(vecteurs.dates.actuals[length(vecteurs.dates.actuals)]+31*k,1,8),"01",sep=""),format="%Y-%m-%d")})))


data.frame.sortie<-data.frame(Date = as.Date(rep(0,length(vecteur.actuals.et.forecast)), origin = vecteur.dates[1] ),Retours = numeric(length(vecteur.actuals.et.forecast)))
data.frame.sortie[,1]<- vecteur.dates                          
data.frame.sortie[,2]<-vecteur.actuals.et.forecast
data.frame.sortie #Afficher le data.frame






#------------- Exemple sur le M01103 sur zone EMA----------------------------------------




data.frame.entree<- read.csv2(file="M01103-02.2.csv", sep=";", dec=".", header=FALSE,as.is=TRUE)
View(data.frame.entree)


data.frame.entree2<-as.vector(data.frame.entree[2,])
names(data.frame.entree2)<-data.frame.entree[1,]
data.frame.entree2  


remplacer.vide<-function(dF,charact,datedebut,datefin)
  
{
  Date<-seq(as.Date(datedebut),as.Date(datefin),by="month")
  dF.2<-data.frame(matrix(nrow=length(Date),ncol=2))
  dF.2[,1]<-Date
  
  
  for(i in 1:length(Date))
    
  {
    
    if(!is.na(match(Date[i],as.Date(dF[,1],format="%Y-%m-%d")))){dF.2[i,2]<-dF[match(Date[i],as.Date(dF[,1],format="%Y-%m-%d")),2]}
    else(dF.2[i,2]<-0)
    
  }
  
  colnames(dF.2)<-c("Date",charact)
  return(dF.2)
  
}


vecteur.actuals<-c()

for(i in 1:length(data.frame.entree2[,-1]))
{
  
  vecteur.actuals[i]<-data.frame.entree2[1,-1][i] 
  
  
}

vecteur.actuals<-as.numeric(unlist(vecteur.actuals))



vecteurs.dates.actuals<-as.Date(sapply(2:length(names(data.frame.entree2)),function(i){as.Date(names(data.frame.entree2)[i],format="%Y-%m-%d")}  ))                                     
vecteurs.dates.actuals<-sub("31","01",vecteurs.dates.actuals)
vecteurs.dates.actuals<-sub("30","01",vecteurs.dates.actuals)
vecteurs.dates.actuals<-sub("29","01",vecteurs.dates.actuals)
vecteurs.dates.actuals<-sub("28","01",vecteurs.dates.actuals)
vecteurs.dates.actuals<-as.Date(vecteurs.dates.actuals)


df.actuals<-data.frame(Date = as.character(rep(0,length(vecteurs.dates.actuals)), origin = vecteurs.dates.actuals[1] ),Retours = numeric(length(vecteur.actuals)))
df.actuals[,1]<-vecteurs.dates.actuals
df.actuals[,2]<-vecteur.actuals

df.actuals<-remplacer.vide(df.actuals,"Retours",as.Date(vecteurs.dates.actuals[1]),as.Date(vecteurs.dates.actuals[length(vecteurs.dates.actuals)]))



vecteur.forecast<-c()
vecteur.dates.forecast<-c()
                                       
                                       
modele<-calculmodele2(df.actuals[,2],coupe=24,n.ahead=3,train=12,fenetre=36)

                                       
vecteur.dates.forecast<-sapply(1:3,function(k){paste(as.Date(paste(substr(df.actuals[,1][length(df.actuals[,1])]+31*k,1,8),"01",sep=""),format="%Y-%m-%d"),"T","00",":","00",":","00","Z",sep="")})
vecteur.forecast<-modele$mod[(length(modele$mod)-(3-1)):length(modele$mod)]
                                       
                                       
                                       
data.frame.sortie<-data.frame(Date = as.character(rep(0,length(vecteur.dates.forecast)), origin = vecteur.dates.forecast[1] ),Retours = numeric(length(vecteur.forecast)))
data.frame.sortie[,1]<-vecteur.dates.forecast                        
data.frame.sortie[,2]<-vecteur.forecast
data.frame.sortie #Afficher le data.frame
                                   


vecteur.actuals.vente<-as.numeric(unlist(vecteur.actuals.vente))

vecteurs.dates.actuals.retour<-as.Date(sapply(2:length(names(dataset1)),function(i){as.Date(names(dataset1)[i],format="%Y-%m-%d")}))                                     

retour<-as.data.frame(matrix(data=c(NA),ncol=2,nrow=length(vecteurs.dates.actuals.retour)))
retour[,1]<-vecteurs.dates.actuals.retour
retour[,2]<-vecteur.actuals.retour

vecteur.actuals.retour<-remplacer.vide(retour,"Retours",datedebut=vecteurs.dates.actuals.retour[1],datefin=vecteurs.dates.actuals.retour[length(vecteurs.dates.actuals.retour)])[,2]

vecteurs.dates.actuals.vente<-as.Date(sapply(2:length(names(dataset2)),function(i){as.Date(names(dataset2)[i],format="%Y-%m-%d")})) 

vente<-as.data.frame(matrix(data=c(NA),ncol=2,nrow=length(vecteurs.dates.actuals.vente)))
vente[,1]<-vecteurs.dates.actuals.vente
vente[,2]<-vecteur.actuals.vente

vecteur.actuals.vente<-remplacer.vide(vente,"Ventes",datedebut=vecteurs.dates.actuals.vente[1],datefin=vecteurs.dates.actuals.vente[length(vecteurs.dates.actuals.vente)])[,2]




##################


forecast.LPAV<-cbind(Date,forecast.LPAV[1:66,])
d<-seq(from=as.Date("2014-09-01"),to=as.Date("2015-09-01"),by="month")
forecast.LPAV.1<-forecast.LPAV[which(forecast.LPAV[,1]%in%d),]
somme<-apply(forecast.LPAV.1[,-1],2,sum)
sum(somme[1:3])/sum(liste.pn$M01103[which(liste.pn$M01103[,1]%in%d),2])
sum(somme[4:6])/sum(liste.pn$M08201[which(liste.pn$M08201[,1]%in%d),2])
sum(somme[7:9])/sum(liste.pn$M05102[which(liste.pn$M05102[,1]%in%d),2])
sum(somme[10:12])/sum(liste.pn$M12301[which(liste.pn$M12301[,1]%in%d),2])
sum(somme[13:15])/sum(liste.pn$M18602[which(liste.pn$M18602[,1]%in%d),2])
sum(somme[16:18],na.rm=TRUE)/sum(liste.pn$M13901[which(liste.pn$M13901[,1]%in%d),2])
sum(somme[19:21],na.rm=TRUE)/sum(liste.pn$M10001[which(liste.pn$M10001[,1]%in%d),2])
sum(somme[22:24])/sum(liste.pn$M20101[which(liste.pn$M20101[,1]%in%d),2])
sum(somme[25:27],na.rm=TRUE)/sum(liste.pn$M07601[which(liste.pn$M07601[,1]%in%d),2])
sum(somme[28:30])/sum(liste.pn$M16004[which(liste.pn$M16004[,1]%in%d),2])


